library(plotrix)
library(magick)


screensize_x <- 150
screensize_y <- 50
#functions
create_point <- function(){
  x <- screensize_x / 2
  y <- screensize_y / 2
  while (x > -3 && y > -3 && x < (screensize_x + 3) && y < (screensize_y + 3)) {
    x <- runif(1, min = screensize_x * (-0.2), max = screensize_x * 1.2)
    y <- runif(1, min = screensize_y * (-0.2), max = screensize_y * 1.2)
  }
  return(c(x, y))
}
create_vector <- function(){
  vector_pos <- c(runif(1, min = -1, max = 1), runif(1, min = -1, max = 1))
  return(c(vector_pos[1], vector_pos[2]))
}
create_letter <- function(){
  random_num <- as.integer(runif(1, min = 0, max = 4))
  if(random_num == 0){
    AGTC_letter <- "A"
  } else if (random_num == 1){
    AGTC_letter <- "G"
  } else if (random_num == 2){
    AGTC_letter <- "T"
  } else {
    AGTC_letter <- "C"
  }
  return(AGTC_letter)
}
measure_distance <- function(x1, y1, x2, y2){
  distance <- sqrt(abs(x1-x2) + abs(y2-y1))
  return(distance)
}
#create lists
point_list <- NULL
vector_list <- NULL
letter_list <- NULL
#fill lists
for(i in 1:15){
  point_list <- c(point_list, create_point())
  vector_list <- c(vector_list, create_vector())
  letter_list <- c(letter_list, create_letter())
}
for(j in 1:200){
  if(!file.exists("Output")){
    dir.create("Output")
  }
  png(paste("./Output/output_", paste(rep(0, 5-nchar(j)), collapse = ""), j, ".png", sep = ""), width = 1920, height = 576, res = 300)
  par(mar = c(0, 0, 0, 0))
  plot(NA,xlim = c(0,screensize_x), ylim = c(0,screensize_y), xaxt = "n", yaxt = "n", ann = FALSE)
  #show points
  for(i in 1:(length(point_list)/2)){
    for(k in (i):(length(point_list)/2)){
      if(measure_distance(point_list[i*2-1], point_list[i*2], point_list[k*2-1], point_list[k*2]) <= 6){
        lines(c(point_list[i*2-1], point_list[k*2-1]), c(point_list[i*2], point_list[k*2]))
      }
    }
  }
  for(i in 1:(length(point_list)/2)){
    draw.circle(point_list[i*2-1], point_list[i*2],2.5, lwd = 1.5, col = "white")
    text(point_list[i*2-1], point_list[i*2], letter_list[i], cex = 1)
  }
  #move points
  for(i in 1:(length(point_list))){
    point_list[i] <- as.numeric(point_list[i]) + as.numeric(vector_list[i])
  }
  #remove and add points
  for(i in 1:(length(point_list)/2)){
    if(point_list[i*2-1] < screensize_x * (-0.2) || 
       point_list[i*2-1] > screensize_x * 1.2) || 
       point_list[i*2] < screensize_y * (-0.2) || 
       point_list[i*2] > screensize_y * 1.2)){
      new_point <- create_point()
      point_list[i*2-1] <- new_point[1]
      point_list[i*2] <- new_point[2]
      new_vector <- create_vector()
      vector_list[i*2-1] <- new_vector[1]
      vector_list[i*2] <- new_vector[2]
      letter_list[i] <- create_letter()
    }
  }
  dev.off()
}


#unlink("movie.gif")
movie <- image_read(list.files("./Output/", full.names = TRUE))
movie <- image_animate(movie)
image_write(movie, "movie5.gif")