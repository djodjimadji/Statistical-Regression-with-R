# Some implementations without using built-in functions

data_simulated <- read.csv("data_simulated.csv")

cor_comput<-function(x,y)
  {
  n <- length(x)
  x_bar <- mean(x)
  y_bar <- mean(y)
  var_x <- sum((x-x_bar)^2)/(n-1)
  var_y <- sum((y-y_bar)^2)/(n-1)
  covar_x_y <- sum((x-x_bar)*(y-y_bar))/(n-1)
  cor_x_y <- covar_x_y/(var_x*var_y)^0.5
  return(cor_x_y)
}
