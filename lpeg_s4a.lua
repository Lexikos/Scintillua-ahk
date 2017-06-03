RegisterEvents({
    -- Override lexer.
    OnClear = function()
        local ext = props.FileExt:lower()
        local lexer = 'lpeg_ahk'
            .. ((ext:find('2') or props["ahk.platform"]:find("v2")) and '2' or '1')
        if props["lexer.$(file.patterns.ahk)"] ~= lexer then
            props["lexer.$(file.patterns.ahk)"] = lexer
            if ext:find('ahk') then
                editor:StartStyling(0, 0)
            end
        end
    end,
    -- Fix auto-indent for lpeg_ahk.
    OnChar = function(ch)
        if editor.LexerLanguage ~= "lpeg" then return end
        if    ch == "\n" then  return AutoIndent_OnNewLine()
        elseif ch == "{" then         AutoIndent_OnOpeningBrace()
        elseif ch == "}" then         AutoIndent_OnClosingBrace()
        end
        return false
    end
})