function Header(h)
    -- Surround headers with centering raw LaTeX
    -- if they have a center{,ed,ing} property set to "t".
    if (h.attributes.centering == "t" or
        h.attributes.centered == "t" or
        h.attributes.center == "t") then
        table.insert(h.content, 1, pandoc.RawInline('latex', '\\centering{'))
        table.insert(h.content, pandoc.RawInline('latex', '}'))
        return h
    end
end
