create_particle = function(pos.x = 1, pos.y = 1, pos.z = 1,
                           vel.x = 1, vel.y = 1, vel.z = 1,
                           acc.x = 0, acc.y = 0, acc.z = 0,
                           rad = 1, m = 1){
  return(list(
    radius = rad,
    mass = m,
    position = data.frame(x = pos.x, y = pos.y, z = pos.z),
    velocity = data.frame(x = vel.x, y = vel.y, z = vel.z),
    acceleration = data.frame(x = acc.x, y = acc.y, z = acc.z),
    energy = 0.5*m*(vel.x^2+vel.y^2+vel.z^2),
    momentum = m*(vel.x^2+vel.y^2+vel.z^2)^0.5
  ))
}