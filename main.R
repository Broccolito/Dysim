graphics.off()
rm(list = ls())
source("particle.R")
source("arena.R")
source("dynamic.R")
source("operate.R")

set.seed(1120)

stage = create_arena()

particle_count = 100
particle_list = list()
for(i in paste0("p", 1:particle_count)){
  particle_list = c(particle_list, 
                    list(create_particle(pos.x = 50*rnorm(1),
                                         pos.y = 50*rnorm(1),
                                         pos.z = 50*rnorm(1))))
}

particle_list = lapply(particle_list, apply_gravity, stage)
for(tp in 1:70){
  
  particle_list = lapply(particle_list, apply_wind, stage)
  
  for(i in 1:particle_count){
    particle_collision_list = which(unlist(lapply(particle_list, collide_particle, 
                                                  particle_list[[i]])))
    particle_collision_list = particle_collision_list[particle_collision_list != i]
    if(length(particle_collision_list)>0){
      for(j in particle_collision_list){
        particle_list[[i]] = apply_collision_particle(particle_list[[i]],
                                                      particle_list[[j]])
        cat("\n particle collision detected")
      }
    }
  }
  
  border_collision_list = c(which(unlist(lapply(particle_list, collide_border_x, stage))),
                            which(unlist(lapply(particle_list, collide_border_y, stage))),
                            which(unlist(lapply(particle_list, collide_border_z, stage))))
  if(length(border_collision_list)>0){
    particle_list[border_collision_list] = lapply(particle_list[border_collision_list],
                                                    apply_collision_border, stage)
    cat("\n border collision detected")
  }
  
  particle_list = lapply(particle_list, time_passage, stage)
  windows()
  plot(matrix(unlist(lapply(particle_list, get_xy)),ncol = 2, byrow = TRUE),
       xlim = c(-100, 100), ylim = c(-100, 100))
}