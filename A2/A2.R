###################################################################################
###############################     STAT 464      #################################
###############################   ASSIGNMENT #2   #################################
#' @author: Steve Hof
#' @date:   February 15, 2019


rm(list=ls())
setwd("~/school/math/Computational_Statistics_464/assignments/A2")
library(dplyr)
library(tidyverse)

###################################################################################
################################    QUESTION #2    ################################
###################################################################################

# first, we start with the example from class


f.nu <- function(n, u) {
  
  for(i in 1:21) {
    v <- c(1, u[i], u[i]^2)
    total = total + n[i]*v%*%t(v)
  }
  total <- total * 1/12
  # total <- det(total)
  return(det(total))
}

sim_ann <- function() {
  u <- seq(-1, 1, by=.1)
  n <- vector(length=21)
  
  ones <- rep(1,12)
  zeros <- rep(0,9)
  n <- append(ones, zeros)
  n <- sample(n)
  
  # nis <- c(0, 0)
  
  while(nis[1]=0) {
    nis = sample(n,2)
  }
  
  new_ni1 <- nis[1] - 1
  new_ni2 <- nis[2] + 1
}

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

iters <- function(k, tau_0) {
  tau <- tau_0
  epoch <- 0
  cushion <- .00001
  while(tau > cushion) {
    tau <- tau*k
    epoch <- epoch + 1
  }
  return(epoch)
}



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

sa <- function(x_0, tau_0=10, a=-1, b=1, k, lower=0, upper=10) {
  
  # set up necessary containers and initial values
  f_0 <- obj.func(x_0)
  max_epoch <- iters(k, tau_0)
  tau <- rep(tau_0, max_epoch)
  inner_results <- matrix(, nrow=length(tau), ncol=5)
  colnames(inner_results) <- c("epoch", "curr_x", "curr_y", "poss_x", "poss_y")
  
  # fill in the tau vector which contains the cooling schedule for the algorithm
  for(i in 2:max_epoch) {tau[i] <- k*tau[i-1]}
  curr_x <- x_0
  
  # keep looking for the next candidate solution until we find one within the given
  # limits
  for(i in 1:length(tau)) {
    repeat {
      poss_x <- curr_x + dx(a, b)
      if((poss_x > lower) && (poss_x < upper)) {break}
    }
    
    # REMOVE BEFORE SUBMITTING!!!!!!!!!!!!!!! DON'T FORGET, YOU IDIOT!
    # inner_results[i, 1] <- i
    # inner_results[i, 2] <- curr_x
    # inner_results[i, 3] <- obj.func(curr_x)
    # inner_results[i, 4] <- poss_x
    # inner_results[i, 5] <- obj.func(poss_x)
    
    if(obj.func(poss_x) < obj.func(curr_x)) {
      curr_x <- poss_x
    } else {
      p <- min(1, exp( (obj.func(poss_x - curr_x)/tau[i]) ))
      
      if(rbinom(1,1,p)) {curr_x <- poss_x}
    }
  }
  temp <- list(curr_x, inner_results)
  return(temp)
}

# first, we look at a plot of the objective function
p <- ggplot(data = data.frame(x = 0), mapping = aes(x = x))
p + stat_function(fun = obj.func) + xlim(0,10)

# 
num_starts <- 20
random_start_results <- matrix(, nrow=num_starts, 3)
colnames(random_start_results) <- c("start location", "x", "y")

for(i in 1:num_starts) {
  start_pos <- runif(1, 0, 10)
  result <- sa(x_0=start_pos, k=.9)
  random_start_results[i,1] <- start_pos
  random_start_results[i,2] <- result[[1]]
  random_start_results[i,3] <- obj.func(result[[1]])
}
random_start_results





