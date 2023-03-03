--[[
All org-mode images get wrapped in a Para
when parsed by pandoc. We don't want that,
because the Revealjs .r-stretch class only
works on *direct* descendants of the heading.
Since we add the .r-stretch class here, it
doesn't need to be added manually in org-mode
(so you can just use a bare image).
]]
function Para(p)
  local el = p.c[1]
  if el.t == "Image" then
    table.insert(el.classes, "r-stretch")
    return el
  end
end
