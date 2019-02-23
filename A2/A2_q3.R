###################################################################################
################################    QUESTION #3    ################################
###################################################################################
rm(list=ls())

#' Title: dx()
#'
#' @description generates a random number in the range from a to b
#' @param a lower limit
#' @param b upper limit
#'
#' @return dx the amount to add / subtract to next candidate choice

dx <- function(a, b) {runif(1, a, b)}


#' Title obj.func()
#'
#' @description evaluates the y value for a given x
#' @param x 
#'
#' @return y value

obj.func <- function(x) {exp(-x/4) * cos(2*x)}



#' Title iters()
#'
#' @description determines the number of iterations required to properly 'cool'
#'              down the annealing based on k and starting temp
#' @param k boltzman's constant
#' @param tau_0 starting temperature
#'
#' @return number of iterations required

# iters <- function(k, tau_0) {
#   tau <- tau_0
#   epoch <- 0
#   cushion <- .00001
#   while(tau > cushion) {
#     tau <- tau*k
#     epoch <- epoch + 1
#   }
#   return(epoch)
# }



#' Title sa() (SIMULATED ANNEALING ALGORITHM)
#'
#' @description attempts to find the global minimum of the objective function based on 
#'              a variety of variables
#' @param x_0 starting x value
#' @param tau_0 starting temperature
#' @param a lower bound of amount to add / subtract to find next candidate solution
#' @param b upper bound of amount to add / subtract to find next candidate solution
#' @param k boltzman's constant (amount temperature decreases each iteration)
#' @param lower lower bound of possible x values
#' @param upper upper bound of possible x values
#'
#' @return the best approximation for the x value that corresponds 
#'         to the lowest y value of the objective function based on the given starting 
#'         x value

sa <- function(x_0, tau_0=10, a=-1, b=1, k, iter, lower=0, upper=10) {
  
  # set up necessary containers and initial values
  f_0 <- obj.func(x_0)
  result <- c(10,10)
  tau = 0.2
  m.j = 2000

  l.0 <- x_0
  
  for(i in 1:iter) {
    for(j in 1:m.j) {
      # keep looking for the next candidate solution until we find one within the given
      # limits
      repeat {
        l.1 <- l.0 + dx(a, b)
        if((l.1 > lower) && (l.1 < upper)) {break}
      }
      
      # REMOVE BEFORE SUBMITTING!!!!!!!!!!!!!!! DON'T FORGET, YOU IDIOT!
      # inner_results[i, 1] <- i
      # inner_results[i, 2] <- l.0
      # inner_results[i, 3] <- obj.func(l.0)
      # inner_results[i, 4] <- l.1
      # inner_results[i, 5] <- obj.func(l.1)
      
      if(obj.func(l.1) < obj.func(l.0)) {
        l.0 <- l.1
      } else {
        p <- min(1, exp( (obj.func(l.1 - l.0)/tau) ))
        
        if(rbinom(1,1,p)) {l.0 <- l.1}
      }
    }
    tau = k*tau
    if(obj.func(l.0) < result[2]) {
      result[1] = l.0
      result[2] = obj.func(l.0)
    }
  }
  # temp <- list(l.0, inner_results)
  return(result[1])
}

# first, we look at a plot of the objective function
p <- ggplot(data = data.frame(x = 0), mapping = aes(x = x))
p + stat_function(fun = obj.func) + xlim(0,10)

 
num_starts <- 20
random_start_results <- matrix(NA, nrow=num_starts, 3)
colnames(random_start_results) <- c("start location", "x", "y")

for(i in 1:num_starts) {
  start_pos <- runif(1, 0, 10)
  result <- sa(x_0=start_pos, k=.9, iter=300)
  random_start_results[i,1] <- start_pos
  random_start_results[i,2] <- result[[1]]
  random_start_results[i,3] <- obj.func(result[[1]])
}
random_start_results
