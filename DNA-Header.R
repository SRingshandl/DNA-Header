library(plotrix)
library(magick)
library(scales)

#define elemental variables
screensize_x <- 200
screensize_y <- 50
moving_letter_number <- 25
speedfactor <- 0.6
distance_line <- 7
create_image_number <- 1500

#functions for later use
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
  vector_pos <- vector_pos * speedfactor
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

#create empty vector lists
point_list <- NULL
vector_list <- NULL
letter_list <- NULL

#fill lists with points, vectors and letters
for(i in 1:moving_letter_number){
  point_list <- c(point_list, create_point())
  vector_list <- c(vector_list, create_vector())
  letter_list <- c(letter_list, create_letter())
}

for(j in 1:create_image_number){
  
  #check if Output folder exists, if not create it
  if(!file.exists("Output")){
    dir.create("Output")
  }
  
  #simple progress indicator
  if(j %% 50 == 0){
    print(paste("Finished images: ", j, sep = ""))
  }
  
  #start of saving image, first create empty plot without any whitespace surrounding it
  png(paste("./Output/output_", paste(rep(0, 5-nchar(j)), collapse = ""), j, ".png", sep = ""), width = 1920, height = 1920/screensize_x*screensize_y, res = 300)
  par(mar = c(0, 0, 0, 0))
  plot(NA,xlim = c(0,screensize_x), ylim = c(0,screensize_y), xaxt = "n", yaxt = "n", ann = FALSE)
  
  #draw connecting lines before drawing circles with letters inside them 
  for(i in 1:(length(point_list)/2)){
    for(k in (i):(length(point_list)/2)){
      if(measure_distance(point_list[i*2-1], point_list[i*2], point_list[k*2-1], point_list[k*2]) <= distance_line){
        color_transparency <- (distance_line - measure_distance(point_list[i*2-1], point_list[i*2], point_list[k*2-1], point_list[k*2]))/distance_line
        lines(c(point_list[i*2-1], point_list[k*2-1]), c(point_list[i*2], point_list[k*2]), col=alpha(rgb(0,0,0), color_transparency))
      }
    }
  }
  
  #draw circles with letters inside them on top of lines
  for(i in 1:(length(point_list)/2)){
    draw.circle(point_list[i*2-1], point_list[i*2],3, lwd = 1.0, col = "white")
    text(point_list[i*2-1], point_list[i*2], letter_list[i], cex = 1)
  }
  #move points with corresponding vector and specified vector multiplier
  for(i in 1:(length(point_list))){
    point_list[i] <- as.numeric(point_list[i]) + as.numeric(vector_list[i])
  }
  
  #remove points when outside grid and send in new ones
  for(i in 1:(length(point_list)/2)){
    if(point_list[i*2-1] < screensize_x * (-0.2) || 
       point_list[i*2-1] > screensize_x * 1.2 || 
       point_list[i*2] < screensize_y * (-0.2) || 
       point_list[i*2] > screensize_y * 1.2){
          new_point <- create_point()
          point_list[i*2-1] <- new_point[1]
          point_list[i*2] <- new_point[2]
          new_vector <- create_vector()
          vector_list[i*2-1] <- new_vector[1]
          vector_list[i*2] <- new_vector[2]
          letter_list[i] <- create_letter()
      }
  }
  #complete image and save
  dev.off()
}

#send system command to run python script images_to_gif
#this generates a gif file with 50 fps so 1000 images --> 20s runtime
#PIL/PILLOW in python is incredibly more performant than imagemagick packages in R
#still VERY RAM hungry as data compression is undone in between steps --> 4 channels x 8 bit x pixel x images
system(paste("python ",getwd(), "/images_to_gif.py", sep = ""))
print("if a 0 appeared above the program finished successfully!")
