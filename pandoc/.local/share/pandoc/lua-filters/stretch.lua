function Para(elem)
  if elem.c[1].t == "Image" then
    table.insert(elem.c[1].classes, "r-stretch")
    return elem.c[1]
  end
end
