local function checkSeenVersion()
    local v = props['ahk.version']
    -- This assumes the properties for setting lpeg lexer based on version are in effect.
    -- We only check ahk.version and not Language because the latter is inaccurate for OnClear.
    -- buffer is sometimes nil (why??), so we just do nothing in that case.
    if buffer and buffer.seenVersion ~= v then
        if buffer.seenVersion then
            editor:StartStyling(0, 0)
        end
        buffer.seenVersion = v
    end
end

RegisterEvents({
    OnClear = checkSeenVersion,
    OnSwitchFile = checkSeenVersion,
})