# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

hello <- function() {
  print("Hello, R class.")

}


simplePerceptron <- function(x,y,init_w,eta,max_epoch,accepted_error){


  if(missing(init_w)){
    weight <- rep(0, dim(x)[2]+1)
  } else {
    weight <- init_w
  }

  if(missing(eta)){
    eta <- 0.1
  }
  if(missing(accepted_error)){
    accepted_error <- 0.0
  }
  if(missing(max_epoch)){
    max_epoch <- 1000
  }

  for(e in 1:max_epoch){
    errors <- 0

    for(i in 1:length(y)){

      z <- sum(weight[2:length(weight)] * as.numeric(x[i, ])) + weight[1]

      if(z < 0) {
        yc <- 0
      } else {
        yc <- 1
      }

      #update weight
      weightdiff <- eta * (y[i] - yc) * c(1, as.numeric(x[i, ]))
      weight <- weight + weightdiff

      # add error
      if (y[i] != yc) {
        errors <- errors + 1
      }

    }

    #calcurate error ratio
    errors <- errors/length(y)

    if(errors <= accepted_error)
      break
  }

  result <- list(weight=weight,error=errors,epoch=e,eta=eta)
  result

}
