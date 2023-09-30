--[[
All org-mode images get wrapped in a Para
when parsed by pandoc. We don't want that,
because the Revealjs .r-stretch class only
works on *direct* descendants of the heading.
Since we add the .r-stretch class here, it
doesn't need to be added manually in org-mode
(so you can just use a bare image).

If you don't want a given image to be stretched,
add the "nostretch" attribute to it.
Org syntax:
#+ATTR_HTML: :nostretch
(The actual value of the attribute is ignored,
 so it can be anything.)
]]
function Para(p)
  local el = p.content[1]
  if el.tag == "Image" then
    if el.attributes.nostretch then
      -- Remove flag as it is no longer needed
      el.attributes.nostretch = nil
      return p
    else
      table.insert(el.classes, "r-stretch")
      return el
    end
  end
end
