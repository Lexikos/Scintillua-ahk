
-- AutoHotkey v2 LPeg lexer.

-- Debug
io.stdout:setvbuf("no")
io.stderr:setvbuf("no")

local lpeg = require('lpeg')
local l = require('lexer')
local word_match = l.word_match
local P, R, S, B, V = lpeg.P, lpeg.R, lpeg.S, lpeg.B, lpeg.V
local lpeg_match, lpeg_Cmt, lpeg_Ct = lpeg.match, lpeg.Cmt, lpeg.Ct
local lpeg_type = lpeg.type

local M = {_NAME = 'ahk2'}

-- This seems to be the only way to prevent large scripts from crashing
-- due to "stack overflow (too many captures)" or having other strange
-- behaviour.  Unfortunately it prevents patterns from spanning lines:
-- M._LEXBYLINE = true
M._INCREMENTAL = true

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

local wordchar = l.alnum + '_' + R'\128\255'

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

local function string_patt(breakers, subpatt, ddq_esc, str_token)
  -- breakers: chars which break segments of string, either as the end of
  --   the string or the start of a sub-pattern; ' " , %
  -- ddq_esc: nil or literal pair of double quotes ('""'); the latter
  --   activates v1 semantics, so "" must be used for a literal ", not `".
  -- subpatt: a pattern to match derefs or other things embedded in the string.
  breakers = (breakers or '')..'`\r\n;'
  local normchars = token(str_token or l.STRING, (l.any - S(breakers))^1)
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
exp_keywords['is'] = l.KEYWORD
exp_keywords['byref'] = l.KEYWORD -- It's only a keyword in function definitions, but since they can't be detected reliably...
exp_keywords['this'] = l.KEYWORD -- Not really a keyword, but feels like one.

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

local deref = pct * V'expression_until_pct' * pct

local dot_prop = ws * token_op'.' * (token(l.NUMBER, l.integer) + identifier)
local double_deref = identifier^-1 * (deref * identifier^-1)^1
local variable = double_deref + plain_variable
-- local variable_or_property = variable * dot_prop^0
-- local property = variable * dot_prop^1
local property = exp_word * dot_prop^1
local variable_or_property = property + variable

-- New is unconditionally a keyword in v2, but still affects the interpretation
-- of the following word; i.e. xxx in 'new xxx()' is a variable, not a function.
exp_word = (token(l.KEYWORD, keyword'new') * (ws1 * variable)^-1) + exp_word
-- exp_word = (token(l.KEYWORD, keyword'new') * ws1 * variable) + exp_word

-- Make the end-quote optional so highlighting kicks in sooner.
local sq = token_op"'"
local dq = token_op'"'
local q_str = (sq * string_patt("'%", deref) * sq^-1)
            + (dq * string_patt('"%', deref) * dq^-1)
-- local q_str = dq * string_patt('"', nil, '""') * dq^-1

local parenex
  = token_op'(' * V'expression'^-1 * token_op')'^-1
  + token_op'{' * V'expression'^-1 * token_op'}'^-1
  + token_op'[' * V'expression'^-1 * token_op']'^-1

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

local arg_text = ws * (string_patt(',%', deref) + #P',')

local arg_label = ws * string_patt(',% \t', deref, nil, l.LABEL)
local arg_litlabel = ws * token(l.LABEL, (l.any - S' \t,`\r\n%')^1)

local arg_expression = ws * (r.expression_until_comma + #P',')

local arg_var = ws * (variable * ws + #P',')

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
-- hack: 'propertyName {' sits between 'Loop {' and 'UDF {x:y}'.  In reality,
--  only propdecl should be considered inside the body of a class, and never outside.
command = command + stm_propdecl

do
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
    + (property + identifier) * cmd_delim
      * r.expression^0 -- Since all args are expression, use expression vs. a list of args.
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
  ('S' * lpeg.Cc(arg_text^-1)) +
  ('E' * lpeg.Cc(arg_expression^-1)) +
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

def_commands ({
  Abs = 'OE',
  ACos = 'OE',
  Array = 'OEEEEEEEEEE',
  ASin = 'OE',
  ATan = 'OE',
  BlockInput = 'E',
  Ceil = 'OE',
  Chr = 'OE',
  Click = 'EEEEEE',
  ClipWait = 'EE',
  ComObjActive = 'OE',
  ComObjArray = 'OEEEEEEEEE',
  ComObjConnect = 'EE',
  ComObjCreate = 'OEE',
  ComObject = 'OEEE',
  ComObjError = 'OE',
  ComObjFlags = 'OEEE',
  ComObjGet = 'OE',
  ComObjQuery = 'OEEE',
  ComObjType = 'OEE',
  ComObjValue = 'OE',
  ControlAddItem = 'EEEEEE',
  ControlChoose = 'EEEEEE',
  ControlChooseString = 'EEEEEE',
  ControlClick = 'EEEEEEEE',
  ControlDeleteItem = 'EEEEEE',
  ControlEditPaste = 'EEEEEE',
  ControlFindItem = 'OEEEEEE',
  ControlFocus = 'EEEEE',
  ControlGetChecked = 'OEEEEE',
  ControlGetChoice = 'OEEEEE',
  ControlGetCurrentCol = 'OEEEEE',
  ControlGetCurrentLine = 'OEEEEE',
  ControlGetEnabled = 'OEEEEE',
  ControlGetExStyle = 'OEEEEE',
  ControlGetFocus = 'OEEEE',
  ControlGetHwnd = 'OEEEEE',
  ControlGetLine = 'OEEEEEE',
  ControlGetLineCount = 'OEEEEE',
  ControlGetList = 'OEEEEEE',
  ControlGetPos = 'OOOOEEEEE',
  ControlGetSelected = 'OEEEEE',
  ControlGetStyle = 'OEEEEE',
  ControlGetTab = 'OEEEEE',
  ControlGetText = 'OEEEEE',
  ControlGetVisible = 'OEEEEE',
  ControlHide = 'EEEEE',
  ControlHideDropDown = 'EEEEE',
  ControlMove = 'EEEEEEEEE',
  ControlSend = 'EEEEEE',
  ControlSendRaw = 'EEEEEE',
  ControlSetChecked = 'EEEEEE',
  ControlSetEnabled = 'EEEEEE',
  ControlSetExStyle = 'EEEEEE',
  ControlSetStyle = 'EEEEEE',
  ControlSetTab = 'EEEEEE',
  ControlSetText = 'EEEEEE',
  ControlShow = 'EEEEE',
  ControlShowDropDown = 'EEEEE',
  CoordMode = 'EE',
  Cos = 'OE',
  Critical = 'E',
  DateAdd = 'OEEE',
  DateDiff = 'OEEE',
  Deref = 'OE',
  DetectHiddenText = 'E',
  DetectHiddenWindows = 'E',
  DirCopy = 'EEE',
  DirCreate = 'E',
  DirDelete = 'EE',
  DirExist = 'OE',
  DirMove = 'EEE',
  DirSelect = 'OEEE',
  DllCall = 'OEEEEEEEEEE',
  Download = 'EE',
  DriveEject = 'EE',
  DriveGetCapacity = 'OE',
  DriveGetFilesystem = 'OE',
  DriveGetLabel = 'OE',
  DriveGetList = 'OE',
  DriveGetSerial = 'OE',
  DriveGetSpaceFree = 'OE',
  DriveGetStatus = 'OE',
  DriveGetStatusCD = 'OE',
  DriveGetType = 'OE',
  DriveLock = 'E',
  DriveSetLabel = 'EE',
  DriveUnlock = 'E',
  Edit = '',
  EnvGet = 'OE',
  EnvSet = 'EE',
  Exception = 'OEEE',
  Exit = 'E',
  ExitApp = 'E',
  Exp = 'OE',
  FileAppend = 'EEE',
  FileCopy = 'EEE',
  FileCreateShortcut = 'EEEEEEEEE',
  FileDelete = 'E',
  FileEncoding = 'E',
  FileExist = 'OE',
  FileGetAttrib = 'OE',
  FileGetShortcut = 'EOOOOOOO',
  FileGetSize = 'OEE',
  FileGetTime = 'OEE',
  FileGetVersion = 'OE',
  FileInstall = 'EEE',
  FileMove = 'EEE',
  FileOpen = 'OEEE',
  FileRead = 'OE',
  FileRecycle = 'E',
  FileRecycleEmpty = 'E',
  FileSelect = 'OEEEE',
  FileSetAttrib = 'EEE',
  FileSetTime = 'EEEE',
  Floor = 'OE',
  Format = 'OEEEEEEEEEE',
  FormatTime = 'OEE',
  Func = 'OE',
  GetKeyName = 'OE',
  GetKeySC = 'OE',
  GetKeyState = 'OEE',
  GetKeyVK = 'OE',
  GroupActivate = 'EE',
  GroupAdd = 'EEEEE',
  GroupClose = 'EE',
  GroupDeactivate = 'EE',
  GuiCreate = 'OEEE',
  GuiCtrlFromHwnd = 'OE',
  GuiFromHwnd = 'OEE',
  Hotkey = 'EEE',
  IL_Add = 'OEEEE',
  IL_Create = 'OEEE',
  IL_Destroy = 'OE',
  ImageSearch = 'OOEEEEE',
  IniDelete = 'EEE',
  IniRead = 'OEEEE',
  IniWrite = 'EEEE',
  Input = 'OEEE',
  InputBox = 'OEEEE',
  InStr = 'OEEEEE',
  IsByRef = 'OE',
  IsFunc = 'OE',
  IsLabel = 'OE',
  IsObject = 'OEEEEEEEEEE',
  KeyHistory = 'EE',
  KeyWait = 'EE',
  ListHotkeys = '',
  ListLines = 'E',
  ListVars = '',
  Ln = 'OE',
  LoadPicture = 'OEEE',
  Log = 'OE',
  LTrim = 'OEE',
  Menu = 'EEEEEE',
  MenuGetHandle = 'OE',
  MenuGetName = 'OE',
  MenuSelect = 'EEEEEEEEEEE',
  Mod = 'OEE',
  MonitorGet = 'EEEEE',
  MonitorGetCount = 'O',
  MonitorGetName = 'OE',
  MonitorGetPrimary = 'O',
  MonitorGetWorkArea = 'EEEEE',
  MouseClick = 'EEEEEEE',
  MouseClickDrag = 'EEEEEEE',
  MouseGetPos = 'OOOOE',
  MouseMove = 'EEEE',
  MsgBox = 'EEEE',
  NumGet = 'OEEE',
  NumPut = 'OEEEE',
  ObjAddRef = 'OE',
  ObjBindMethod = 'OEEEEEEEEEE',
  ObjClone = 'OE',
  ObjDelete = 'OEEE',
  Object = 'OEEEEEEEEEE',
  ObjGetAddress = 'OEE',
  ObjGetCapacity = 'OEE',
  ObjHasKey = 'OEE',
  ObjInsertAt = 'EEEEEEEEEEE',
  ObjLength = 'OE',
  ObjMaxIndex = 'OE',
  ObjMinIndex = 'OE',
  ObjNewEnum = 'OE',
  ObjPop = 'OE',
  ObjPush = 'OEEEEEEEEEE',
  ObjRawSet = 'EEE',
  ObjRelease = 'OE',
  ObjRemoveAt = 'OEEE',
  ObjSetCapacity = 'OEEE',
  OnClipboardChange = 'EE',
  OnExit = 'EE',
  OnMessage = 'EEE',
  Ord = 'OE',
  OutputDebug = 'E',
  Pause = 'EE',
  PixelGetColor = 'OEEE',
  PixelSearch = 'OOEEEEEEE',
  PostMessage = 'EEEEEEEE',
  ProcessClose = 'OE',
  ProcessExist = 'OE',
  ProcessSetPriority = 'EE',
  ProcessWait = 'OEE',
  ProcessWaitClose = 'OEE',
  Random = 'OEE',
  RegDelete = 'EE',
  RegDeleteKey = 'E',
  RegExMatch = 'OEEEE',
  RegExReplace = 'OEEEEEE',
  RegisterCallback = 'OEEEE',
  RegRead = 'OEE',
  RegWrite = 'EEEE',
  Reload = '',
  Round = 'OEE',
  RTrim = 'OEE',
  Run = 'EEEO',
  RunAs = 'EEE',
  RunWait = 'EEEO',
  Send = 'E',
  SendEvent = 'E',
  SendInput = 'E',
  SendLevel = 'E',
  SendMessage = 'EEEEEEEEE',
  SendMode = 'E',
  SendPlay = 'E',
  SendRaw = 'E',
  SetCapslockState = 'E',
  SetControlDelay = 'E',
  SetDefaultMouseSpeed = 'E',
  SetKeyDelay = 'EEE',
  SetMouseDelay = 'EE',
  SetNumlockState = 'E',
  SetRegView = 'E',
  SetScrollLockState = 'E',
  SetStoreCapslockMode = 'E',
  SetTimer = 'EEE',
  SetTitleMatchMode = 'E',
  SetWinDelay = 'E',
  SetWorkingDir = 'E',
  Shutdown = 'E',
  Sin = 'OE',
  Sleep = 'E',
  Sort = 'OEE',
  SoundBeep = 'EE',
  SoundGet = 'OEEE',
  SoundPlay = 'EE',
  SoundSet = 'EEEE',
  SplitPath = 'EOOOOO',
  Sqrt = 'OE',
  StatusBarGetText = 'OEEEEE',
  StatusBarWait = 'EEEEEEEE',
  StrGet = 'OEEE',
  StringCaseSense = 'E',
  StrLen = 'OE',
  StrLower = 'OEE',
  StrPut = 'OEEEE',
  StrReplace = 'OEEEOE',
  StrSplit = 'OEEE',
  StrUpper = 'OEE',
  SubStr = 'OEEE',
  Suspend = 'E',
  SysGet = 'OE',
  Tan = 'OE',
  Thread = 'EEE',
  ToolTip = 'EEEE',
  TrayTip = 'EEEE',
  Trim = 'OEE',
  Type = 'OE',
  VarSetCapacity = 'OEEE',
  WinActivate = 'EEEE',
  WinActivateBottom = 'EEEE',
  WinActive = 'OEEEE',
  WinClose = 'EEEEE',
  WinExist = 'OEEEE',
  WinGetClass = 'OEEEE',
  WinGetControls = 'OEEEE',
  WinGetControlsHwnd = 'OEEEE',
  WinGetCount = 'OEEEE',
  WinGetExStyle = 'OEEEE',
  WinGetID = 'OEEEE',
  WinGetIDLast = 'OEEEE',
  WinGetList = 'OEEEE',
  WinGetMinMax = 'OEEEE',
  WinGetPID = 'OEEEE',
  WinGetPos = 'OOOOEEEE',
  WinGetProcessName = 'OEEEE',
  WinGetProcessPath = 'OEEEE',
  WinGetStyle = 'OEEEE',
  WinGetText = 'OEEEE',
  WinGetTitle = 'OEEEE',
  WinGetTransColor = 'OEEEE',
  WinGetTransparent = 'OEEEE',
  WinHide = 'EEEE',
  WinKill = 'EEEEE',
  WinMaximize = 'EEEE',
  WinMinimize = 'EEEE',
  WinMinimizeAll = '',
  WinMinimizeAllUndo = '',
  WinMove = 'EEEEEEEE',
  WinMoveBottom = 'EEEE',
  WinMoveTop = 'EEEE',
  WinRedraw = 'EEEE',
  WinRestore = 'EEEE',
  WinSetAlwaysOnTop = 'EEEEE',
  WinSetEnabled = 'EEEEE',
  WinSetExStyle = 'EEEEE',
  WinSetRegion = 'EEEEE',
  WinSetStyle = 'EEEEE',
  WinSetTitle = 'EEEEE',
  WinSetTransColor = 'EEEEE',
  WinSetTransparent = 'EEEEE',
  WinShow = 'EEEE',
  WinWait = 'EEEEE',
  WinWaitActive = 'EEEEE',
  WinWaitClose = 'EEEEE',
  WinWaitNotActive = 'EEEEE',
}, map_args)

local args_e = map_args'E'
local args_ee = map_args'EE'
local args_eee = map_args'EEE'

local args_label = args(arg_label)
local args_litlabel = args(arg_litlabel)

local If_args = (arg_expression * (comma * ws * r.statement)^-1)^-1

local Loop_args =
  (token(l.KEYWORD, keyword'Parse')
    * ws * comma * args_eee) +
  (token(l.KEYWORD, keyword'Reg' + keyword'Files' + keyword'Read')
    * ws * comma * args_ee) +
  (arg_expression^-1)

local in_keyword = token(l.KEYWORD, keyword'in')

local For_args = arg_var * (comma * ws * (#in_keyword + arg_var))^-1
  * ws * in_keyword * ws1 * r.expression

def_flow_cmd {
  Break = args_litlabel,
  Catch = args(arg_var) * ws * token_op'{'^-1,
  Continue = args_litlabel,
  For = GR(For_args),
  Gosub = args_label,
  Goto = args_label,
  Loop = GR(Loop_args),
  Return = args_e,
  Throw = args_e,
  Until = args_e,
  LoopFiles = args_ee,
  LoopReg = args_ee,
  LoopRead = args_ee,
  LoopParse = args_eee,
}

def_flow_par {
  If = GR(If_args),
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

-- Directives
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

def_keywords(variable_words, l.VARIABLE, {
  -- v1 & v2 variables
  'a_ahkpath', 'a_ahkversion', 'a_appdata', 'a_appdatacommon',
  'a_caretx', 'a_carety', 'a_computername', 'a_controldelay', 'a_coordmodecaret', 'a_coordmodemenu', 'a_coordmodemouse', 'a_coordmodepixel', 'a_coordmodetooltip', 'a_cursor',
  'a_dd', 'a_ddd', 'a_dddd', 'a_defaultmousespeed', 'a_desktop', 'a_desktopcommon', 'a_detecthiddentext', 'a_detecthiddenwindows',
  'a_endchar', 'a_eventinfo',
  'a_fileencoding',
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
  -- v2 variables
  'a_comspec',
  'a_initialworkingdir',
  'a_loopfilepath',
  'a_msgboxresult',
})
def_keywords(variable_words, l.KEYWORD, {
  'this' -- It's a variable, but feels like a keyword. This is for contexts which only accept variables, not real keywords.
})
-- Functions for v2 are the same as commands (already set up).
-- Only these few are handled this way because () forces expression mode.
def_keywords(function_words, l.KEYWORD, {
  'loop', 'loopfiles', 'loopparse', 'loopread', 'loopreg',
})

M._foldsymbols = {
  [l.OPERATOR] = {['{'] = 1, ['}'] = -1},
  [l.COMMENT] = {
    ['/*'] = 1, ['*/'] = -1, -- Block comments
    [';{'] = 1, [';}'] = -1, -- User-defined regions
  },
  _patterns = {';?[{}]', '/%*', '%*/', }
}

return M