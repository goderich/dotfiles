function Header(elem)
  if elem.c[1].t == "Image" then
    return pandoc.Header(2,
                         pandoc.List(),
                         {["background-image"]    = elem.c[1].src,
                          ["background-size"]     = "contain",
                          ["background-repeat"]   = "no-repeat",
                          ["background-position"] = "center"})
  end
end
