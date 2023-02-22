--[[
All org-mode images get wrapped in a Para
when parsed by pandoc. We don't want that,
because the Revealjs .r-stretch class only
works on *direct* descendants of the heading.
Since we add the .r-stretch class here, it
doesn't need to be added manually in org-mode
(so you can just use a bare image).
]]
function Para(elem)
  if elem.c[1].t == "Image" then
    table.insert(elem.c[1].classes, "r-stretch")
    return elem.c[1]
  end
end
