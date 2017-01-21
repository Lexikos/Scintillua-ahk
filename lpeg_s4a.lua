-- Override lexer.
RegisterEvents({
    OnClear = function()
        local lexer = 'lpeg_ahk' .. (props["ahk.platform"]:find("v2") and '2' or '1')
        if props["lexer.$(file.patterns.ahk)"] ~= lexer then
            props["lexer.$(file.patterns.ahk)"] = lexer
            if props.FileExt:lower() == 'ahk' then
                editor:StartStyling(0, 0)
            end
        end
    end
})