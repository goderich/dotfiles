function Header(elem)
  if elem.c[1].t == "Image" then
    local attrs =
      { ["background-image"]    = elem.c[1].src,
        ["background-size"]     = "contain",
        ["background-repeat"]   = "no-repeat",
        ["background-position"] = "center",
        ["identifier"]          = "section" }
    local h = pandoc.Header(2, pandoc.List(), attrs)
    return h
  end
end
