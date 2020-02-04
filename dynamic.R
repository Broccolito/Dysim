get_distance = function(p1, p2){
  return(((p1$position$x - p2$position$x)^2 + 
            (p1$position$y - p2$position$y)^2 + 
            (p1$position$z - p2$position$z)^2)^0.5)
}

collide_particle = function(p1, p2){
  print(get_distance(p1, p2))
  return(get_distance(p1, p2) <= (p1$radius + p2$radius))
}

collide_border_x = function(p, a){
  return(
    any(abs(p$position$x - a$border$x[1]) <= p$radius, 
        abs(p$position$x - a$border$x[2]) <= p$radius)
  )
}

collide_border_y = function(p, a){
  return(
    any(abs(p$position$y - a$border$y[1]) <= p$radius, 
        abs(p$position$y - a$border$y[2]) <= p$radius)
  )
}

collide_border_z = function(p, a){
  return(
    any(abs(p$position$z - a$border$z[1]) <= p$radius, 
        abs(p$position$z - a$border$z[2]) <= p$radius)
  )
}

apply_collision_particle = function(p1, p2, loss = 0.01){
  remain = 1 - loss
  p2_v.x = (-(p1$velocity$x * p1$mass)/p2) * loss
  p2_v.y = (-(p1$velocity$y * p1$mass)/p2) * loss
  p2_v.z = (-(p1$velocity$z * p1$mass)/p2) * loss
  p1_v.x = (-(p2$velocity$x * p2$mass)/p1) * loss
  p1_v.y = (-(p2$velocity$y * p2$mass)/p1) * loss
  p1_v.z = (-(p2$velocity$z * p2$mass)/p1) * loss
  new_p1 = p1
  new_p1$velocity$x = p1_v.x
  new_p1$velocity$y = p1_v.y
  new_p1$velocity$y = p1_v.z
  new_p2 = p2
  new_p2$velocity$x = p2_v.x
  new_p2$velocity$y = p2_v.y
  new_p2$velocity$y = p2_v.z
  return(list(
    p1 = new_p1,
    p2 = new_p2
  ))
}

apply_collision_border = function(p, a, loss = 0.01){
  remain = 1 - loss
  p_new = p
  if(collide_border_x(p, a)){
    p_new$velocity$x = -p$velocity$x * remain
  }
  if(collide_border_y(p ,a)){
    p_new$velocity$y = -p$velocity$y * remain
  }
  if(collide_border_z(p, a)){
    p_new$velocity$z = -p$velocity$z * remain
  }
  return(p_new)
}

apply_wind = function(p, a){
  p_new = p
  if(a$wind){
    p_new$acceleration$x = p_new$acceleration$x + a$wind_direction$x*a$wind_constant
    p_new$acceleration$y = p_new$acceleration$y + a$wind_direction$y*a$wind_constant
    p_new$acceleration$z = p_new$acceleration$z + a$wind_direction$z*a$wind_constant
  }
  return(p_new)
}

apply_gravity = function(p, a){
  p_new = p
  if(a$gravity){
    p_new$acceleration$x = p_new$acceleration$x + a$gravity_direction$x*a$gravity_constant
    p_new$acceleration$y = p_new$acceleration$y + a$gravity_direction$y*a$gravity_constant
    p_new$acceleration$z = p_new$acceleration$z + a$gravity_direction$z*a$gravity_constant
  }
  return(p_new)
}

time_passage = function(p, a){
  p_new = p
  p_new$position$x = p_new$position$x + p_new$velocity$x
  p_new$position$y = p_new$position$y + p_new$velocity$y
  p_new$position$z = p_new$position$z + p_new$velocity$z
  p_new$velocity$x = p_new$velocity$x + p_new$acceleration$x
  p_new$velocity$y = p_new$velocity$y + p_new$acceleration$y
  p_new$velocity$z = p_new$velocity$z + p_new$acceleration$z
  p_new$energy = 0.5*p_new$mass*(p_new$velocity$x^2 + p_new$velocity$y^2 + p_new$velocity$z^2)
  return(p_new)
}