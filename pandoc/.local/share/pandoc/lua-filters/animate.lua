--[[
This is for using org syntax to add RevealJS animations
(see https://revealjs.com/auto-animate/).
The syntax is a simple :animate: tag on the heading.
Note that right now there can only be one tag per heading:
if there is a second one, it will overwrite the first.
]]
function Header(h)
  local c = h.content
  -- null check, otherwise filter will crash
  if next(c) ~= nil and c[#c].attr and
      c[#c].attributes["tag-name"] == "animate" then
    -- For some reason tags in headings get interpreted as SmallCaps
    -- and appear in the generated file. We don't want that,
    -- so we delete the last two items in the heading (tag and preceding space).
    table.remove(c, #c)
    table.remove(c, #c)
    h.attributes["auto-animate"] = "true"
    return h
  end
end
