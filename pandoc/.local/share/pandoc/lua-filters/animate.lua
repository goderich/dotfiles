function Header(h)
  local c = h.content
  if c[#c].attr and
      c[#c].attributes["tag-name"] == "animate" then
    table.remove(c, #c)
    table.remove(c, #c)
    h.attributes["auto-animate"] = "true"
    return h
  end
end
