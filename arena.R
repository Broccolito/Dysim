create_arena = function(g = TRUE, gd.x = 0, gd.y = -1, gd.z = 0, gc = 0.01,
                        w = TRUE, wd.x = 1, wd.y = 0, wd.z = 0, wc = 0.01,
                        border.x = c(-100,100), border.y = c(-100,100), border.z = c(-100,100)){
  return(list(
    gravity = g,
    gravity_direction = data.frame(x = gd.x, y = gd.y, z = gd.z),
    gravity_constant = gc,
    wind = w,
    wind_direction = data.frame(x = wd.x, y = wd.y, z = wd.z),
    wind_constant = wc,
    border = data.frame(x = border.x, y = border.y, z = border.z)
   ))
}