
is_v2 = true

-- AHK LPeg lexer.

-- Debug
io.stdout:setvbuf("no")
io.stderr:setvbuf("no")

local lpeg = require('lpeg')
local l = require('lexer')
local word_match = l.word_match
local P, R, S, B, V = lpeg.P, lpeg.R, lpeg.S, lpeg.B, lpeg.V
local lpeg_match, lpeg_Cmt, lpeg_Ct = lpeg.match, lpeg.Cmt, lpeg.Ct
local lpeg_type = lpeg.type

local M = {_NAME = 'ahk'..(is_v2 and 2 or 1)}

-- This seems to be the only way to prevent large scripts from crashing
-- due to "stack overflow (too many captures)" or having other strange
-- behaviour.  Unfortunately it prevents patterns from spanning lines:
-- M._LEXBYLINE = true
M._INCREMENTAL = true

-- local is_v2 = true --string.find(l.property['ahk.platform'] or '', 'v2') and true

local token = l.token
local tunpack = table.unpack or unpack
local tinsert = table.insert
local tremove = table.remove

local function starts_line(p)
  return l.starts_line(S(' \t')^0 * p)
end

local function token_op(op)
  return token(l.OPERATOR, op)
end

-- Select subpattern based on matched word : CASE INSENSITIVE!
local function word_switch(word_patt)
  local word_list = {}
  local filter
  local function add_words(words, new_filter)
    filter = new_filter
    for word, patt in pairs(words) do
      word_list[word:lower()] = lpeg_type(patt) == 'pattern' and lpeg_Ct(patt) or patt
    end
  end
  return lpeg_Cmt(word_patt, function(input, word_end, word, ...)
    word = word:lower()
    local patt = word_list[word]
    if patt then
      if not lpeg_type(patt) then
        -- This saves a bit on startup time by allowing patterns for each
        -- word to be constructed on demand, the first time they are needed.
        patt = lpeg_Ct(filter(patt))
        word_list[word] = patt
      end
      local res = lpeg_match(patt, input, word_end)
      if res then
        -- Return position to resume matching, followed by captures
        -- (captures are token names and positions for styling).
        local end_index = res[#res] or word_end
        if #res == 1 then res = {} end -- No captures (empty string?).
        -- Merge word_patt captures with patt captures.
        local prefix = {...}
        for i = 1, #prefix do
          tinsert(res, i, prefix[i])
        end
        return end_index, tunpack(res)
      end
    end
    return nil
  end), add_words
end

local wordchar = l.alnum + (is_v2 and '_' or S'#_@$') + R'\128\255'

local function insensitive(word)
  local patt = '' -- Testing shows ('' * P(x)) == x if x is a pattern.
  for i = 1, #word do
    local ch = word:sub(i,i)
    patt = patt * S(ch:lower()..ch:upper())
  end
  return patt
end
local function keyword(word)
  return -B(wordchar) * insensitive(word) * -(wordchar)
end

local at_eol = -l.nonnewline -- No non-newline chars here => at end of line.

-- Whitespace.
local ws1 = token(l.WHITESPACE, S(' \t')^1)
local ws = ws1^-1
local ws0 = S' \t'^0 -- no token

-- Comments.
--   Should be (?<!\S), but (?<![\x21-\xFF]) is pretty close:
local line_comment = (';' - B(R('\033\255'))) * l.nonnewline^0
local block_comment
  = starts_line('/*') * l.nonnewline^0
  * (l.newline * -starts_line('*/') * l.nonnewline^0)^0
  * (l.newline * starts_line('*/'))^-1
local comment = token(l.COMMENT, line_comment + block_comment + starts_line('*/'))

local ws_multiline = (comment^-1 * token(l.WHITESPACE, S' \t\r\n'^1))^0
local otb_at_eol = token_op'{' * ws * (comment + at_eol)

local number = token(l.NUMBER, (l.float + l.integer) * -wordchar)

local function string_patt(breakers, subpatt, ddq_esc)
  -- breakers: chars which break segments of string, either as the end of
  --   the string or the start of a sub-pattern; ' " , %
  -- ddq_esc: nil or literal pair of double quotes ('""'); the latter
  --   activates v1 semantics, so "" must be used for a literal ", not `".
  -- subpatt: a pattern to match derefs or other things embedded in the string.
  breakers = (breakers or '')..'`\r\n;'
  local normchars = token(l.STRING, (l.any - S(breakers))^1)
  -- ` is always an escape char, but if followed by ender_esc or \r\n,
  -- it becomes a null escape sequence (i.e. the escape char is ignored
  -- and the ender_esc or \r\n is not escaped).  So ^-1.
  local esc = token('escape', P('`') * (l.any - S((ddq_esc or '')..'\r\n'))^-1)
  local patt = normchars + esc + comment + token(l.STRING, ';')
  if ddq_esc then patt = patt + token('escape', ddq_esc) end
  if subpatt then patt = patt + subpatt end
  return patt^0
end

local pct = token_op'%'
local comma = token_op','

local identifier_patt = wordchar^1
local identifier = token(l.IDENTIFIER, identifier_patt)

local exp_keywords = {}
exp_keywords['and'] = l.KEYWORD
exp_keywords['or'] = l.KEYWORD
exp_keywords['not'] = l.KEYWORD
exp_keywords['in'] = l.ERROR -- Reserved/likely user error.
exp_keywords['contains'] = l.ERROR
if is_v2 then
  exp_keywords['is'] = l.KEYWORD
else
  exp_keywords['is'] = l.ERROR
end
exp_keywords['byref'] = l.KEYWORD -- It's only a keyword in function definitions, but since they can't be detected reliably...
exp_keywords['this'] = l.KEYWORD -- Not really a keyword, but feels like one.
--[[
local function exp_word_highlighter(allow_keywords)
  local function select_token(word, word_end, is_function)
    local token_name = exp_keywords[word:lower()]
    if not token_name
      or (token_name == l.FUNCTION) and not is_function
      or (token_name == l.KEYWORD and not allow_keywords) then
      token_name = l.IDENTIFIER
    end
    return token_name, word_end
  end
  return lpeg.C(identifier_patt) * lpeg.Cp()
    * (#P'(' * lpeg.Cc(true) + lpeg.Cc(false))
    / select_token
  
  -- local function select_token(input, word_end, word)
    -- local token_name = exp_keywords[word:lower()]
    -- if not token_name
      -- or (token_name == l.FUNCTION) and not (input:sub(word_end, word_end) == '(')
      -- or (token_name == l.KEYWORD and not allow_keywords) then
      -- token_name = l.IDENTIFIER
    -- end
    -- return word_end, token_name, word_end
  -- end
  -- return lpeg_Cmt(lpeg.C(identifier_patt)
    -- , select_token)
end
local exp_word = B'.' * identifier + exp_word_highlighter(true)
--]]
local function exp_word_highlighter(word_patt, words)
  local def_token = l.IDENTIFIER
  return lpeg.C(word_patt) * lpeg.Cp() /
    function(word, word_end)
      return words[word:lower()] or def_token, word_end
    end
end
local function def_keywords(word_set, token_name, array)
  for i, word in ipairs(array) do
    word_set[word:lower()] = token_name
  end
end
local function_words, nonfunction_words, variable_words = {}, {}, {}
for word, token_name in pairs(exp_keywords) do
  function_words[word] = token_name
  nonfunction_words[word] = token_name
end
setmetatable(nonfunction_words, {__index = variable_words})

local plain_variable = exp_word_highlighter(identifier_patt, variable_words)
local plain_function = exp_word_highlighter(identifier_patt * #P'(', function_words)

local exp_word = B'.' * (token(l.KEYWORD, keyword'base') + identifier)
  + plain_function
  + token(l.KEYWORD, keyword'base') * #S'.['
  + exp_word_highlighter(identifier_patt, nonfunction_words) -- var or keyword

local deref = pct * (is_v2 and V'expression_until_pct' or plain_variable) * pct

local dot_prop = ws * token_op'.' * (token(l.NUMBER, l.integer) + identifier)
local double_deref = identifier^-1 * (deref * identifier^-1)^1
local variable = double_deref + plain_variable
-- local variable_or_property = variable * dot_prop^0
-- local property = variable * dot_prop^1
local property = exp_word * dot_prop^1
local variable_or_property = property + variable

if is_v2 then
  -- New is unconditionally a keyword in v2, but still affects the interpretation
  -- of the following word; i.e. xxx in 'new xxx()' is a variable, not a function.
  exp_word = (token(l.KEYWORD, keyword'new') * (ws1 * variable)^-1) + exp_word
else
  exp_word = (token(l.KEYWORD, keyword'new') * ws1 * variable) + exp_word
end

-- For now, single-quoted strings have v2 semantics (they don't exist in v1)
-- and double-quoted strings have v1 semantics (for testing/proof of concept).
-- Make the end-quote optional so highlighting kicks in sooner.
local sq = token_op"'"
local dq = token_op'"'
local q_str
if is_v2 then
  local sq_str = sq * string_patt("'%", deref) * sq^-1
  local dq_str = dq * string_patt('"%', deref) * dq^-1
  q_str = sq_str + dq_str
else
  local dq_str = dq * string_patt('"', nil, '""') * dq^-1
  q_str = dq_str
end

local parenex
  = token_op'(' * V'expression'^-1 * token_op')'^-1
  + token_op'{' * V'expression'^-1 * token_op'}'^-1
  + token_op'[' * V'expression'^-1 * token_op']'^-1

--[[
local exp_keywords = keyword'and' + keyword'or' + keyword'not' + keyword'new'
local exp_reserved = keyword'in' + keyword'contains'
if is_v2 then
  exp_keywords = exp_keywords + keyword'is'
else
  -- v1 doesn't actually reserve these keywords, so they can be used as variables.
  -- However, it's more likely to be user error in this context, so treat as error.
  exp_reserved = exp_reserved + keyword'is'
end
local keywordex = token(l.KEYWORD, exp_keywords) + token(l.ERROR, exp_reserved)
--]]

local exp_common = ws1 + q_str + parenex + number + exp_word

-- expression, expression : can include or exclude comma
-- cmd % expression, arg : expression excludes commas; ends at ,
-- x := %expression% : expression excludes percent signs; ends at %
-- fn(expression) : expression includes commas and percent signs; ends at )
local function expr_patt(ender)
  local operators = '+-/*<>~!=^&|?~:,.'
  if ender then operators = operators:gsub('%'..ender, '') end
  local patt = token(l.OPERATOR, S(operators)) + exp_common
  if ender ~= '%' then patt = patt + deref end
  return patt^1
end

local r = {}

-- GR:
--    Resolve open references such as V'expression'.  It benchmarks
--    faster using GR() at the last moment rather than resolving common
--    patterns (expression = GR(expression)) and reusing them.
local function GR(patt)
  r[1] = patt
  return P(r)
end

r.expression_until_comma = expr_patt(',')
r.expression_until_pct = expr_patt('%')
r.expression = expr_patt()

-- Patterns used as command args.
--
local function args(...)
  local aa = {...}
  local a = tremove(aa)
  if a then a = a^-1 end
  while #aa > 0 do
    a = tremove(aa)^-1 * (comma * a)^-1
  end
  return GR(a)
end

local arg_normal = ws * (
      ( pct * ws1 * r.expression_until_comma )
    + ( string_patt(',%', deref) ) )
  + #P','

local arg_last = ws * (
      ( pct * ws1 * r.expression )
    + ( string_patt('%', deref) ) )

local arg_expression = ws * (( pct * ws1 )^-1 * r.expression_until_comma + #P',')
local arg_expression_last = ws * (( pct * ws1 )^-1 * r.expression + #P',')

local arg_var = ws * (variable * ws + #P',')

-- v1.1.21+ supports % expressions in InputVars (except for If).
local arg_var_ex = ws * ( pct * ws1 * r.expression_until_comma ) + arg_var

local cmd_delim = (ws * comma * ws) + ws1 + at_eol

-- Commands.
local command, def_commands = word_switch(
  lpeg.C(token(l.FUNCTION, l.alnum^2)) * cmd_delim
)

-- Control flow (with command-like rules).
local flow1, def_flow_cmd = word_switch(
  lpeg.C(token(l.KEYWORD, l.alnum^2)) * cmd_delim
)
-- Control flow (allowing parenthesis).
local flow2, def_flow_par = word_switch(
  lpeg.C(token(l.KEYWORD, l.alnum^2)) * (cmd_delim + #P'(')
)
-- Control flow (allowing OTB without space).
local flow3, def_flow_otb = word_switch(
  lpeg.C(token(l.KEYWORD, l.alnum^2)) * (cmd_delim + token_op'{')
)

-- Pattern matching any command.
command = command + flow1 + flow2 + flow3

local stm_propdecl = (token(l.KEYWORD, keyword'get' + keyword'set') + identifier)
    * ws * (otb_at_eol + ws * token(l.WHITESPACE, l.newline) * #(ws * '{'))
-- hack: 'propertyName {' sits between 'Loop {' and 'UDF {text}'.  In reality,
--  only propdecl should be considered inside the body of a class, and never outside.
command = command + stm_propdecl

if is_v2 then
  local def = def_commands
  def_commands = function(words, new_filter)
    def(words, new_filter)
    local tn = l.FUNCTION
    -- Add all commands to the expression keyword table.
    for word in pairs(words) do
      function_words[word:lower()] = tn
    end
  end
  -- Add user-defined commands.
  command = command
    + (property + identifier) * cmd_delim * (
      arg_normal * (comma * arg_normal)^0
    )
end

-- Directives.
local directive, def_directives = word_switch(
  lpeg.C(token(l.PREPROCESSOR, l.alnum^2)) * cmd_delim
)
directive = token(l.PREPROCESSOR, P'#') * directive
local directive_args = token(l.STRING, (l.nonnewline - comment)^1)^-1
local no_args = P''

-- Other statements.

local stm_assign = variable_or_property * ws
  * token(l.OPERATOR, (S(':+-*|&^/.') * '=') + '<<=' + '>>=')
  * ws * r.expression^-1

if not is_v2 then
  -- Legacy = assignment.
  stm_assign = stm_assign
    + (variable * ws * token_op'=' * ws * string_patt('%', deref))
end

local inc_dec = token(l.OPERATOR, P'++' + P'--')
local stm_inc_dec = (
    (inc_dec * ws * variable_or_property)
  + (variable_or_property * ws * inc_dec)
  ) * r.expression^-1

local stm_fncall = (double_deref * #P'(' + plain_function) * r.expression
local start_invoke = (
  property * ((ws * (comment + at_eol)) + #S'([') +
  variable * P'['
)
local start_ternary = (1 - S'?\r\n')^0 * P'?' * (1 - S':\r\n')^0 * ':'
local start_parenex = P'('
-- These are combined for efficiency.  Invoke is detected with lookahead to ensure
-- it is styled correctly as an expression.  (By contrast, the left hand of assignment
-- should be styled as a variable even if it matches a keyword.)
local stm_exp_misc = #(start_invoke + start_ternary + start_parenex) * r.expression

local stm_vardecl = token(l.KEYWORD, keyword'local' + keyword'global' + keyword'static')
  * ((ws1 * #wordchar * r.expression^-1) + ws * (comment + at_eol))
local stm_class = token(l.KEYWORD, keyword'class') * ws * identifier * ws
  * (token(l.KEYWORD, keyword'extends') * ws * (variable_or_property * ws)^-1)^-1
  * token_op'{'^-1

r.statement
  = stm_vardecl + stm_class
  -- Assignment takes precedence over command for 'MsgBox = not a command.'
  + stm_assign
  -- new takes precedence over user-defined commands (v2).
  + #(insensitive'new' * S' \t') * r.expression
  -- command takes precedence over stm_fncall for if() and while()
  -- command takes precedence over stm_inc_dec for MsgBox ++ (with space).
  + command
  + stm_fncall + stm_inc_dec + stm_exp_misc
  + token(l.OPERATOR, S'{}') * ws * V'statement'^-1

local label = token(l.LABEL, (1 - S'\t ,:\r\n')^1 * ':')
  -- Labels can't coexist with other code (except comments) on the same line.
  * ((ws * comment) + -(ws0 * l.nonnewline))

local keyname = (l.word + (1 - S' \t\r\n' - comment)) -- A word or any single non-space character (and not the start of a comment, but x::; is okay).
local hotkey_mods = (S'<>*~$!^+#' * -P'::')^0
local hotkey
  = ((P'~'^-1 * keyname * ws0 * B' ' * '& ' * ws0 * P'~'^-1) + hotkey_mods)
  * keyname * (S' \t'^0 * insensitive'up')^-1 * '::'
hotkey = token(l.LABEL, hotkey)
  * ws * (r.statement + token(l.CONSTANT, hotkey_mods * keyname)^-1)

local hotstring = token(l.LABEL, ':' * (R('AZ','az','09') + S('?* \t-'))^0 * ':')
  * ( token('escape', P'`' * l.nonnewline)
    + token(l.STRING, (1 - (S'`\r\n' + P'::'))^1) )^0
  * token(l.LABEL, P'::')
  * string_patt()

-- Continuation sections.
-- These are styled simplistically for two main reasons:
--  1) So styling doesn't break when the user edits inside the section.
--     When a line changes, Scintilla requests styling for only a small portion
--     of the code.  If the section is a single chunk of text which is all the
--     same style (like a multi-line comment or string in any other language)
--     we're given the whole section; otherwise, we get just a fragment, which
--     ends up styled incorrectly.
--  2) Incorrect highlighting is misleading and/or annoying, and correctly
--     highlighting continuation sections would require a very complex grammar.
local continuation_section = token(l.DEFAULT,
  -- (Header and options
  S'(' * (
    keyword'join' * (1 - S' \t\r\n')^1 -- Ignore any ')' in a Join option
    + (1 - S') \t\r\n')^1 -- Allow any character except ')'
    + S' \t'^1 -- Whitespace between options
  )^0 * l.newline *
  -- Content
  (-(ws0 * ')') * l.nonnewline^0 * l.newline)^0 *
  -- )Footer: Must be optional, since Scintill(u)a might pass an incomplete
  --   fragment for lexing while the user is typing in a continuation section.
  (ws0 * ')' * l.nonnewline^0)^-1
)

-- Continuation lines.
-- FIXME: These operators can be used in any context, but we're assuming
-- they are expressions for now.  This is a hack to give sensible styling
-- even though we don't really support continuation.
local continuation_line = #S'+-/*<>~!=^&|?~:,.' * r.expression
  * (token(l.OPERATOR, S')]}')^1 * r.expression^-1)^0

local prefix = l.starts_line(ws) * (
  continuation_section
  + directive + hotkey + hotstring + label
  + continuation_line
  + r.statement 
)

r[1]
  = comment
  + prefix
  + token(l.WHITESPACE, S' \t\r\n')
  + token(l.ERROR, l.any)

M._rules = {
  {'all', P(r)}
}

M._tokenstyles = {
  escape = l.STYLE_REGEX,
}

local function args2(...)
  local aa = {...}
  a = tremove(aa)
  for i = 1, #aa do
    a = tremove(aa) * (comma * a)^-1
  end
  return GR(a)
end
local arg_map = (((
  ('S' * (-1 * lpeg.Cc(arg_last^-1) + lpeg.Cc(arg_normal^-1))) +
  ('E' * (-1 * lpeg.Cc(arg_expression_last^-1) + lpeg.Cc(arg_expression^-1))) +
  ('I' * lpeg.Cc(arg_var_ex^-1)) +
  (S'VO' * lpeg.Cc(arg_var^-1))
)^1 * -l.any) / args2) + lpeg.Cc(no_args)
local arg_cache = {}
local function map_args(args)
  local patt = arg_cache[v]
  if not patt then
    -- Construct a pattern from this argstring.
    patt = lpeg_match(arg_map, args)
    -- Cache it for all commands which use this arg combination.
    arg_cache[args] = patt
  end
  return patt
end

if is_v2 then
def_commands ({
  Abs = 'OS',
  ACos = 'OS',
  Array = 'OSSSSSSSSSS',
  ASin = 'OS',
  ATan = 'OS',
  BlockInput = 'S',
  Ceil = 'OS',
  Chr = 'OS',
  Click = 'SSSSSS',
  ClipWait = 'SS',
  ComObjActive = 'OS',
  ComObjArray = 'OSSSSSSSSS',
  ComObjConnect = 'SS',
  ComObjCreate = 'OSS',
  ComObject = 'OSSS',
  ComObjError = 'OS',
  ComObjFlags = 'OSSS',
  ComObjGet = 'OS',
  ComObjQuery = 'OSSS',
  ComObjType = 'OSS',
  ComObjValue = 'OS',
  Control = 'SSSSSSS',
  ControlClick = 'SSSSSSSS',
  ControlFocus = 'SSSSS',
  ControlGet = 'OSSSSSSS',
  ControlGetFocus = 'OSSSS',
  ControlGetPos = 'OOOOSSSSS',
  ControlGetText = 'OSSSSS',
  ControlMove = 'SSSSSSSSS',
  ControlSend = 'SSSSSS',
  ControlSendRaw = 'SSSSSS',
  ControlSetText = 'SSSSSS',
  CoordMode = 'SS',
  Cos = 'OS',
  Critical = 'S',
  DateAdd = 'OSSS',
  DateDiff = 'OSSS',
  Deref = 'OS',
  DetectHiddenText = 'S',
  DetectHiddenWindows = 'S',
  DirCopy = 'SSS',
  DirCreate = 'S',
  DirDelete = 'SS',
  DirExist = 'OS',
  DirMove = 'SSS',
  DirSelect = 'OSSS',
  DllCall = 'OSSSSSSSSSS',
  Download = 'SS',
  Drive = 'SSS',
  DriveGet = 'OSS',
  Edit = '',
  EnvGet = 'OS',
  EnvSet = 'SS',
  Exception = 'OSSS',
  Exit = 'S',
  ExitApp = 'S',
  Exp = 'OS',
  FileAppend = 'SSS',
  FileCopy = 'SSS',
  FileCreateShortcut = 'SSSSSSSSS',
  FileDelete = 'S',
  FileEncoding = 'S',
  FileExist = 'OS',
  FileGetAttrib = 'OS',
  FileGetShortcut = 'SOOOOOOO',
  FileGetSize = 'OSS',
  FileGetTime = 'OSS',
  FileGetVersion = 'OS',
  FileInstall = 'SSS',
  FileMove = 'SSS',
  FileOpen = 'OSSS',
  FileRead = 'OS',
  FileRecycle = 'S',
  FileRecycleEmpty = 'S',
  FileSelect = 'OSSSS',
  FileSetAttrib = 'SSS',
  FileSetTime = 'SSSS',
  Floor = 'OS',
  Format = 'OSSSSSSSSSS',
  FormatTime = 'OSS',
  Func = 'OS',
  GetKeyName = 'OS',
  GetKeySC = 'OS',
  GetKeyState = 'OSS',
  GetKeyVK = 'OS',
  GroupActivate = 'SS',
  GroupAdd = 'SSSSS',
  GroupClose = 'SS',
  GroupDeactivate = 'SS',
  Gui = 'SSSS',
  GuiControl = 'SSS',
  GuiControlGet = 'OSSS',
  Hotkey = 'SSS',
  IL_Add = 'OSSSS',
  IL_Create = 'OSSS',
  IL_Destroy = 'OS',
  ImageSearch = 'OOSSSSS',
  IniDelete = 'SSS',
  IniRead = 'OSSSS',
  IniWrite = 'SSSS',
  Input = 'OSSS',
  InputBox = 'OSSSS',
  InStr = 'OSSSSS',
  IsByRef = 'OS',
  IsFunc = 'OS',
  IsLabel = 'OS',
  IsObject = 'OSSSSSSSSSS',
  KeyHistory = 'SS',
  KeyWait = 'SS',
  ListHotkeys = '',
  ListLines = 'S',
  ListVars = '',
  Ln = 'OS',
  LoadPicture = 'OSSS',
  Log = 'OS',
  LTrim = 'OSS',
  LV_Add = 'OSSSSSSSSSS',
  LV_Delete = 'OS',
  LV_DeleteCol = 'OS',
  LV_GetCount = 'OS',
  LV_GetNext = 'OSS',
  LV_GetText = 'OSSS',
  LV_Insert = 'OSSSSSSSSSS',
  LV_InsertCol = 'OSSS',
  LV_Modify = 'OSSSSSSSSSS',
  LV_ModifyCol = 'OSSS',
  LV_SetImageList = 'OSS',
  Menu = 'SSSSSS',
  MenuGetHandle = 'OS',
  MenuGetName = 'OS',
  MenuSelect = 'SSSSSSSSSSS',
  Mod = 'OSS',
  MonitorGet = 'SSSSS',
  MonitorGetCount = 'O',
  MonitorGetName = 'OS',
  MonitorGetPrimary = 'O',
  MonitorGetWorkArea = 'SSSSS',
  MouseClick = 'SSSSSSS',
  MouseClickDrag = 'SSSSSSS',
  MouseGetPos = 'OOOOS',
  MouseMove = 'SSSS',
  MsgBox = 'SSSS',
  NumGet = 'OSSS',
  NumPut = 'OSSSS',
  ObjAddRef = 'OS',
  ObjBindMethod = 'OSSSSSSSSSS',
  ObjClone = 'OS',
  ObjDelete = 'OSSS',
  Object = 'OSSSSSSSSSS',
  ObjGetAddress = 'OSS',
  ObjGetCapacity = 'OSS',
  ObjHasKey = 'OSS',
  ObjInsertAt = 'SSSSSSSSSSS',
  ObjLength = 'OS',
  ObjMaxIndex = 'OS',
  ObjMinIndex = 'OS',
  ObjNewEnum = 'OS',
  ObjPop = 'OS',
  ObjPush = 'OSSSSSSSSSS',
  ObjRawSet = 'SSS',
  ObjRelease = 'OS',
  ObjRemoveAt = 'OSSS',
  ObjSetCapacity = 'OSSS',
  OnClipboardChange = 'SS',
  OnExit = 'SS',
  OnMessage = 'SSS',
  Ord = 'OS',
  OutputDebug = 'S',
  Pause = 'SS',
  PixelGetColor = 'OSSS',
  PixelSearch = 'OOSSSSSSS',
  PostMessage = 'SSSSSSSS',
  ProcessClose = 'OS',
  ProcessExist = 'OS',
  ProcessSetPriority = 'SS',
  ProcessWait = 'OSS',
  ProcessWaitClose = 'OSS',
  Random = 'OSS',
  RegDelete = 'SS',
  RegDeleteKey = 'S',
  RegExMatch = 'OSSSS',
  RegExReplace = 'OSSSSSS',
  RegisterCallback = 'OSSSS',
  RegRead = 'OSS',
  RegWrite = 'SSSS',
  Reload = '',
  Round = 'OSS',
  RTrim = 'OSS',
  Run = 'SSSO',
  RunAs = 'SSS',
  RunWait = 'SSSO',
  SB_SetIcon = 'OSSS',
  SB_SetParts = 'OSSSSSSSSSS',
  SB_SetText = 'OSSS',
  Send = 'S',
  SendEvent = 'S',
  SendInput = 'S',
  SendLevel = 'S',
  SendMessage = 'SSSSSSSSS',
  SendMode = 'S',
  SendPlay = 'S',
  SendRaw = 'S',
  SetCapslockState = 'S',
  SetControlDelay = 'S',
  SetDefaultMouseSpeed = 'S',
  SetKeyDelay = 'SSS',
  SetMouseDelay = 'SS',
  SetNumlockState = 'S',
  SetRegView = 'S',
  SetScrollLockState = 'S',
  SetStoreCapslockMode = 'S',
  SetTimer = 'SSS',
  SetTitleMatchMode = 'S',
  SetWinDelay = 'S',
  SetWorkingDir = 'S',
  Shutdown = 'S',
  Sin = 'OS',
  Sleep = 'S',
  Sort = 'OSS',
  SoundBeep = 'SS',
  SoundGet = 'OSSS',
  SoundPlay = 'SS',
  SoundSet = 'SSSS',
  SplitPath = 'SOOOOO',
  Sqrt = 'OS',
  StatusBarGetText = 'OSSSSS',
  StatusBarWait = 'SSSSSSSS',
  StrGet = 'OSSS',
  StringCaseSense = 'S',
  StrLen = 'OS',
  StrLower = 'OSS',
  StrPut = 'OSSSS',
  StrReplace = 'OSSSOS',
  StrSplit = 'OSSS',
  StrUpper = 'OSS',
  SubStr = 'OSSS',
  Suspend = 'S',
  SysGet = 'OS',
  Tan = 'OS',
  Thread = 'SSS',
  ToolTip = 'SSSS',
  TrayTip = 'SSSS',
  Trim = 'OSS',
  TV_Add = 'OSSS',
  TV_Delete = 'OS',
  TV_Get = 'OSS',
  TV_GetChild = 'OS',
  TV_GetCount = 'O',
  TV_GetNext = 'OSS',
  TV_GetParent = 'OS',
  TV_GetPrev = 'OS',
  TV_GetSelection = 'O',
  TV_GetText = 'OSS',
  TV_Modify = 'OSSS',
  TV_SetImageList = 'OSS',
  Type = 'OS',
  VarSetCapacity = 'OSSS',
  WinActivate = 'SSSS',
  WinActivateBottom = 'SSSS',
  WinActive = 'OSSSS',
  WinClose = 'SSSSS',
  WinExist = 'OSSSS',
  WinGetClass = 'OSSSS',
  WinGetControls = 'OSSSS',
  WinGetControlsHwnd = 'OSSSS',
  WinGetCount = 'OSSSS',
  WinGetExStyle = 'OSSSS',
  WinGetID = 'OSSSS',
  WinGetIDLast = 'OSSSS',
  WinGetList = 'OSSSS',
  WinGetMinMax = 'OSSSS',
  WinGetPID = 'OSSSS',
  WinGetPos = 'OOOOSSSS',
  WinGetProcessName = 'OSSSS',
  WinGetProcessPath = 'OSSSS',
  WinGetStyle = 'OSSSS',
  WinGetText = 'OSSSS',
  WinGetTitle = 'OSSSS',
  WinGetTransColor = 'OSSSS',
  WinGetTransparent = 'OSSSS',
  WinHide = 'SSSS',
  WinKill = 'SSSSS',
  WinMaximize = 'SSSS',
  WinMinimize = 'SSSS',
  WinMinimizeAll = '',
  WinMinimizeAllUndo = '',
  WinMove = 'SSSSSSSS',
  WinMoveBottom = 'SSSS',
  WinMoveTop = 'SSSS',
  WinRedraw = 'SSSS',
  WinRestore = 'SSSS',
  WinSetAlwaysOnTop = 'SSSSS',
  WinSetEnabled = 'SSSSS',
  WinSetExStyle = 'SSSSS',
  WinSetRegion = 'SSSSS',
  WinSetStyle = 'SSSSS',
  WinSetTitle = 'SSSSS',
  WinSetTransColor = 'SSSSS',
  WinSetTransparent = 'SSSSS',
  WinShow = 'SSSS',
  WinWait = 'SSSSS',
  WinWaitActive = 'SSSSS',
  WinWaitClose = 'SSSSS',
  WinWaitNotActive = 'SSSSS',
}, map_args)
else
def_commands ({
  AutoTrim = 'S',
  BlockInput = 'S',
  Click = 'S',
  ClipWait = 'EE',
  Control = 'SSSSSSS',
  ControlClick = 'SSSSESSS',
  ControlFocus = 'SSSSS',
  ControlGet = 'OSSSSSSS',
  ControlGetFocus = 'OSSSS',
  ControlGetPos = 'OOOOSSSSS',
  ControlGetText = 'OSSSSS',
  ControlMove = 'SEEEESSSS',
  ControlSend = 'SSSSSS',
  ControlSendRaw = 'SSSSSS',
  ControlSetText = 'SSSSSS',
  CoordMode = 'SS',
  Critical = 'S',
  DetectHiddenText = 'S',
  DetectHiddenWindows = 'S',
  Drive = 'SSS',
  DriveGet = 'OSS',
  DriveSpaceFree = 'OS',
  Edit = '',
  EnvAdd = 'OES',
  EnvDiv = 'OE',
  EnvGet = 'OS',
  EnvMult = 'OE',
  EnvSet = 'SS',
  EnvSub = 'OES',
  EnvUpdate = '',
  Exit = 'E',
  ExitApp = 'E',
  FileAppend = 'SSS',
  FileCopy = 'SSE',
  FileCopyDir = 'SSE',
  FileCreateDir = 'S',
  FileCreateShortcut = 'SSSSSSSEE',
  FileDelete = 'S',
  FileEncoding = 'S',
  FileGetAttrib = 'OS',
  FileGetShortcut = 'SOOOOOOO',
  FileGetSize = 'OSS',
  FileGetTime = 'OSS',
  FileGetVersion = 'OS',
  FileInstall = 'SSE',
  FileMove = 'SSE',
  FileMoveDir = 'SSS',
  FileRead = 'OS',
  FileReadLine = 'OSE',
  FileRecycle = 'S',
  FileRecycleEmpty = 'S',
  FileRemoveDir = 'SE',
  FileSelectFile = 'OSSSS',
  FileSelectFolder = 'OSES',
  FileSetAttrib = 'SSEE',
  FileSetTime = 'ESSEE',
  FormatTime = 'OSS',
  GetKeyState = 'OSS',
  GroupActivate = 'SS',
  GroupAdd = 'SSSSSS',
  GroupClose = 'SS',
  GroupDeactivate = 'SS',
  Gui = 'SSSS',
  GuiControl = 'SSS',
  GuiControlGet = 'OSSS',
  Hotkey = 'SSS',
  ImageSearch = 'OOEEEES',
  IniDelete = 'SSS',
  IniRead = 'OSSSS',
  IniWrite = 'SSSS',
  Input = 'OSSS',
  InputBox = 'OSSSEEEESES',
  KeyHistory = 'SS',
  KeyWait = 'SS',
  ListHotkeys = '',
  ListLines = 'S',
  ListVars = '',
  Menu = 'SSSSSS',
  MouseClick = 'SEEEESS',
  MouseClickDrag = 'SEEEEES',
  MouseGetPos = 'OOOOE',
  MouseMove = 'EEES',
  MsgBox = 'SSSS',
  OnExit = 'SS',
  OutputDebug = 'S',
  Pause = 'SS',
  PixelGetColor = 'OEES',
  PixelSearch = 'OOEEEEEES',
  PostMessage = 'EEESSSSS',
  Process = 'SSS',
  Progress = 'SSSSSS',
  Random = 'OEE',
  RegDelete = 'SSS',
  RegRead = 'OSSS',
  RegWrite = 'SSSSS',
  Reload = '',
  Run = 'SSSO',
  RunAs = 'SSS',
  RunWait = 'SSSO',
  Send = 'S',
  SendEvent = 'S',
  SendInput = 'S',
  SendLevel = 'E',
  SendMessage = 'EEESSSSSE',
  SendMode = 'S',
  SendPlay = 'S',
  SendRaw = 'S',
  SetBatchLines = 'S',
  SetCapslockState = 'S',
  SetControlDelay = 'E',
  SetDefaultMouseSpeed = 'E',
  SetEnv = 'OS',
  SetFormat = 'SS',
  SetKeyDelay = 'EES',
  SetMouseDelay = 'ES',
  SetNumlockState = 'S',
  SetRegView = 'S',
  SetScrollLockState = 'S',
  SetStoreCapslockMode = 'S',
  SetTimer = 'SSE',
  SetTitleMatchMode = 'S',
  SetWinDelay = 'E',
  SetWorkingDir = 'S',
  Shutdown = 'E',
  Sleep = 'E',
  Sort = 'IS',
  SoundBeep = 'EE',
  SoundGet = 'OSSE',
  SoundGetWaveVolume = 'OE',
  SoundPlay = 'SS',
  SoundSet = 'ESSE',
  SoundSetWaveVolume = 'EE',
  SplashImage = 'SSSSSSS',
  SplashTextOff = '',
  SplashTextOn = 'EESS',
  SplitPath = 'IOOOOO',
  StatusBarGetText = 'OESSSS',
  StatusBarWait = 'SEESSESS',
  StringCaseSense = 'S',
  StringGetPos = 'OISSE',
  StringLeft = 'OIE',
  StringLen = 'OI',
  StringLower = 'OIS',
  StringMid = 'OIEES',
  StringReplace = 'OISSS',
  StringRight = 'OIE',
  StringSplit = 'SISSS',
  StringTrimLeft = 'OIE',
  StringTrimRight = 'OIE',
  StringUpper = 'OIS',
  Suspend = 'S',
  SysGet = 'OSSS',
  Thread = 'SEE',
  ToolTip = 'SEEE',
  Transform = 'OSSS',
  TrayTip = 'SSEE',
  URLDownloadToFile = 'SS',
  WinActivate = 'SSSS',
  WinActivateBottom = 'SSSS',
  WinClose = 'SSESS',
  WinGet = 'OSSSSS',
  WinGetActiveStats = 'OOOOO',
  WinGetActiveTitle = 'O',
  WinGetClass = 'OSSSS',
  WinGetPos = 'OOOOSSSS',
  WinGetText = 'OSSSS',
  WinGetTitle = 'OSSSS',
  WinHide = 'SSSS',
  WinKill = 'SSESS',
  WinMaximize = 'SSSS',
  WinMenuSelectItem = 'SSSSSSSSSSS',
  WinMinimize = 'SSSS',
  WinMinimizeAll = '',
  WinMinimizeAllUndo = '',
  WinMove = args(arg_expression, arg_expression) * -P',' + map_args('SSEEEESS'),
  WinRestore = 'SSSS',
  WinSet = 'SSSSSS',
  WinSetTitle = 'SSSSS',
  WinShow = 'SSSS',
  WinWait = 'SSESS',
  WinWaitActive = 'SSESS',
  WinWaitClose = 'SSESS',
  WinWaitNotActive = 'SSESS',
}, map_args)
end

local If_args
if is_v2 then
  If_args = (arg_expression * (comma * ws * r.statement)^-1)^-1
else
  If_args =
  (
    arg_var *
    (
      ( -- if var op value
        token(l.OPERATOR, (S'!<>'^-1 * '=') + '<>' + S'<>') * arg_last
      )
      +
      (token(l.KEYWORD, keyword'not') * ws1)^-1 *
      ( -- if var [not] ...
        (token(l.KEYWORD, keyword'between')
          * string_patt('% \t', deref + ws1 * -keyword'and')
          * ws1 * token(l.KEYWORD, keyword'and')
          * arg_last 
        )
        +
        (token(l.KEYWORD, keyword'contains' + keyword'in')
          * arg_last
        )
      )
      +
      (token(l.KEYWORD, keyword'is' * (ws0 * keyword'not')^-1)
        * arg_last
      )
    )
  )
  + arg_expression^-1
end

local Loop_args =
  (token(l.KEYWORD, keyword'Parse')
    * ws * comma * args(is_v2 and arg_normal or arg_var_ex, arg_normal, arg_normal)) +
  (token(l.KEYWORD, keyword'Reg' + keyword'Files' + keyword'Read')
    * ws * comma * args(arg_normal, arg_normal))

if is_v2 then
  Loop_args = Loop_args +
    (arg_expression^-1)
else
  Loop_args = Loop_args +
    (arg_normal * comma * args(arg_normal, arg_normal, arg_normal)) +
    ((number * ws)^-1 * otb_at_eol) +
    (arg_normal^-1) -- Technically this one supports OTB too, but seems not worth the trouble.
end

local in_keyword = token(l.KEYWORD, keyword'in')

local For_args = arg_var * (comma * ws * (#in_keyword + arg_var))^-1
  * ws * in_keyword * ws1 * r.expression

local args_n = args(arg_normal)
local args_e = args(arg_expression_last)

def_flow_cmd {
  Break = args_n,
  Catch = args(arg_var) * ws * token_op'{'^-1,
  Continue = args_n,
  For = GR(For_args),
  Gosub = args_n,
  Goto = args_n,
  Loop = GR(Loop_args),
  Return = args_e,
  Throw = args_e,
  Until = args_e,
}

if is_v2 then
  local args_nn = map_args'SS'
  def_flow_cmd {
    LoopFiles = args_nn,
    LoopReg = args_nn,
    LoopRead = args_nn,
    LoopParse = map_args'SSS',
  }
else
  local arg_nc = (arg_normal * comma * ws * (command + token(l.OPERATOR, S'{}')))
  local arg_nc_l = arg_nc + arg_last
  local arg_nc_nnnc_nnl = arg_nc + args(arg_normal, arg_normal, arg_nc_l)

  local IfN_args = args(arg_nc_l)
  local IfVN_args = args(arg_var, arg_nc_l)
  local IfNNNN_args = args(arg_normal, arg_nc_nnnc_nnl)

  def_flow_cmd {
    If = GR(If_args),
    IfEqual = IfVN_args,
    IfExist = IfN_args,
    IfGreater = IfVN_args,
    IfGreaterOrEqual = IfVN_args,
    IfInString = IfVN_args,
    IfLess = IfVN_args,
    IfLessOrEqual = IfVN_args,
    IfMsgBox = IfN_args,
    IfNotEqual = IfVN_args,
    IfNotExist = IfN_args,
    IfNotInString = IfVN_args,
    IfWinActive = IfNNNN_args,
    IfWinExist = IfNNNN_args,
    IfWinNotActive = IfNNNN_args,
    IfWinNotExist = IfNNNN_args,
  }
end

def_flow_par {
  If = is_v2 and GR(If_args) or args_e,
  While = args_e,
}

def_flow_otb {
  Else = GR(r.statement^-1),
  Try = GR(r.statement^-1),
  Finally = GR(r.statement^-1),
  -- Fallback rules for 'Cmd{' (commands also listed above):
  Catch = P'',
  Loop = P'',
}

-- Directives common to v1 and v2.
def_directives {
  ClipboardTimeout = directive_args,
  ErrorStdOut = no_args,
  HotkeyInterval = directive_args,
  HotkeyModifierTimeout = directive_args,
  Hotstring = directive_args,
  If = args(arg_expression),
  IfTimeout = directive_args,
  IfWinActive = directive_args,
  IfWinExist = directive_args,
  IfWinNotActive = directive_args,
  IfWinNotExist = directive_args,
  Include = directive_args,
  IncludeAgain = directive_args,
  InputLevel = directive_args,
  InstallKeybdHook = no_args,
  InstallMouseHook = no_args,
  KeyHistory = directive_args,
  MaxHotkeysPerInterval = directive_args,
  MaxThreads = directive_args,
  MaxThreadsBuffer = directive_args,
  MaxThreadsPerHotkey = directive_args,
  MenuMaskKey = directive_args,
  NoTrayIcon = no_args,
  Persistent = no_args,
  SingleInstance = directive_args,
  UseHook = no_args,
  Warn = directive_args,
  WinActivateForce = no_args,
}
if not is_v2 then
  -- Directives for v1 only.
  def_directives {
    CommentFlag = directive_args,
    Delimiter = directive_args,
    DerefChar = directive_args,
    EscapeChar = directive_args,
    LTrim = directive_args,
    MaxMem = directive_args,
    NoEnv = no_args,
  }
end

-- Variables common to v1 and v2.
def_keywords(variable_words, l.VARIABLE, {
  'a_ahkpath', 'a_ahkversion', 'a_appdata', 'a_appdatacommon',
  'a_caretx', 'a_carety', 'a_computername', 'a_controldelay', 'a_coordmodecaret', 'a_coordmodemenu', 'a_coordmodemouse', 'a_coordmodepixel', 'a_coordmodetooltip', 'a_cursor',
  'a_dd', 'a_ddd', 'a_dddd', 'a_defaultgui', 'a_defaultlistview', 'a_defaultmousespeed', 'a_defaulttreeview', 'a_desktop', 'a_desktopcommon', 'a_detecthiddentext', 'a_detecthiddenwindows',
  'a_endchar', 'a_eventinfo',
  'a_fileencoding',
  'a_gui', 'a_guicontrol', 'a_guicontrolevent', 'a_guievent', 'a_guiheight', 'a_guiwidth', 'a_guix', 'a_guiy',
  'a_hour',
  'a_iconfile', 'a_iconhidden', 'a_iconnumber', 'a_icontip', 'a_index', 'a_ipaddress1', 'a_ipaddress2', 'a_ipaddress3', 'a_ipaddress4', 'a_is64bitos', 'a_isadmin', 'a_iscompiled', 'a_iscritical', 'a_ispaused', 'a_issuspended', 'a_isunicode',
  'a_keydelay', 'a_keydelayplay', 'a_keyduration', 'a_keydurationplay',
  'a_language', 'a_lasterror', 'a_linefile', 'a_linenumber', 'a_loopfield', 'a_loopfileattrib', 'a_loopfiledir', 'a_loopfileext', 'a_loopfilefullpath', 'a_loopfilename', 'a_loopfileshortname', 'a_loopfileshortpath', 'a_loopfilesize', 'a_loopfilesizekb', 'a_loopfilesizemb', 'a_loopfiletimeaccessed', 'a_loopfiletimecreated', 'a_loopfiletimemodified', 'a_loopreadline', 'a_loopregkey', 'a_loopregname', 'a_loopregsubkey', 'a_loopregtimemodified', 'a_loopregtype',
  'a_mday', 'a_min', 'a_mm', 'a_mmm', 'a_mmmm', 'a_mon', 'a_mousedelay', 'a_mousedelayplay', 'a_msec', 'a_mydocuments',
  'a_now', 'a_nowutc',
  'a_osversion',
  'a_priorhotkey', 'a_priorkey', 'a_programfiles', 'a_programs', 'a_programscommon', 'a_ptrsize',
  'a_regview',
  'a_screendpi', 'a_screenheight', 'a_screenwidth', 'a_scriptdir', 'a_scriptfullpath', 'a_scripthwnd', 'a_scriptname', 'a_sec', 'a_sendlevel', 'a_sendmode', 'a_space', 'a_startmenu', 'a_startmenucommon', 'a_startup', 'a_startupcommon', 'a_storecapslockmode', 'a_stringcasesense',
  'a_tab', 'a_temp', 'a_thisfunc', 'a_thishotkey', 'a_thislabel', 'a_thismenu', 'a_thismenuitem', 'a_thismenuitempos', 'a_tickcount', 'a_timeidle', 'a_timeidlephysical', 'a_timesincepriorhotkey', 'a_timesincethishotkey', 'a_titlematchmode', 'a_titlematchmodespeed',
  'a_username',
  'a_wday', 'a_windelay', 'a_windir', 'a_workingdir',
  'a_yday', 'a_year', 'a_yweek', 'a_yyyy',
  'clipboard', 'clipboardall', 'false', 'programfiles', 'true',
})
def_keywords(variable_words, l.KEYWORD, {
  'this' -- It's a variable, but feels like a keyword. This is for contexts which only accept variables, not real keywords.
})
if is_v2 then
  -- Variables for v2 only.
  def_keywords(variable_words, l.VARIABLE, {
    'a_comspec',
    'a_initialworkingdir',
    'a_loopfilepath',
    'a_msgboxresult',
  })
  -- Functions for v2 are the same as commands (already set up).
  -- Only these few are handled this way because () forces expression mode.
  def_keywords(function_words, l.KEYWORD, {
    'loop', 'loopfiles', 'loopparse', 'loopread', 'loopreg',
  })
else
  -- Variables for v1 only.
  def_keywords(variable_words, l.VARIABLE, {
    'a_autotrim',
    'a_batchlines',
    'a_exitreason',
    'a_formatfloat', 'a_formatinteger',
    'a_loopfilelongpath',
    'a_numbatchlines',
    'a_ostype',
    'comspec',
  })
  -- Functions for v1 (does not include commands).
  def_keywords(function_words, l.FUNCTION, {
    'abs', 'acos', 'array', 'asc', 'asin', 'atan',
    'ceil', 'chr', 'comobjactive', 'comobjarray', 'comobjconnect', 'comobjcreate', 'comobject', 'comobjerror', 'comobjflags', 'comobjget', 'comobjquery', 'comobjtype', 'comobjvalue', 'cos',
    'dllcall',
    'exception', 'exp',
    'fileexist', 'fileopen', 'floor', 'format', 'func',
    'getkeyname', 'getkeysc', 'getkeystate', 'getkeyvk',
    'il_add', 'il_create', 'il_destroy', 'instr', 'isbyref', 'isfunc', 'islabel', 'isobject',
    'ln', 'loadpicture', 'log', 'ltrim', 'lv_add', 'lv_delete', 'lv_deletecol', 'lv_getcount', 'lv_getnext', 'lv_gettext', 'lv_insert', 'lv_insertcol', 'lv_modify', 'lv_modifycol', 'lv_setimagelist',
    'menugethandle', 'menugetname', 'mod',
    'numget', 'numput',
    'objaddref', 'objbindmethod', 'objclone', 'objdelete', 'object', 'objgetaddress', 'objgetcapacity', 'objhaskey', 'objinsert', 'objinsertat', 'objlength', 'objmaxindex', 'objminindex', 'objnewenum', 'objpop', 'objpush', 'objrawset', 'objrelease', 'objremove', 'objremoveat', 'objsetcapacity', 'onclipboardchange', 'onexit', 'onmessage', 'ord',
    'regexmatch', 'regexreplace', 'registercallback', 'round', 'rtrim',
    'sb_seticon', 'sb_setparts', 'sb_settext', 'sin', 'sqrt', 'strget', 'strlen', 'strput', 'strreplace', 'strsplit', 'substr',
    'tan', 'trim', 'tv_add', 'tv_delete', 'tv_get', 'tv_getchild', 'tv_getcount', 'tv_getnext', 'tv_getparent', 'tv_getprev', 'tv_getselection', 'tv_gettext', 'tv_modify', 'tv_setimagelist',
    'varsetcapacity',
    'winactive', 'winexist'
  })
end

M._foldsymbols = {
  [l.OPERATOR] = {['{'] = 1, ['}'] = -1},
  [l.COMMENT] = {
    ['/*'] = 1, ['*/'] = -1, -- Block comments
    [';{'] = 1, [';}'] = -1, -- User-defined regions
  },
  _patterns = {';?[{}]', '/%*', '%*/', }
}

return M