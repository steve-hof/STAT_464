#########################################################################
### EXAMPLE 3.5 -- GENETIC ALGORITHM
#########################################################################
# baseball.dat 	= entire data set
# baseball.sub 	= matrix of all predictors
# salary.log 	= response, log salary
# n 		= number of observations in the data set
# m 		= number of predictors in the data set
# runs 		= matrix of P individuals for a generation where each row
# 		  is a vector of the parameters included for the model
# 		  (1 = included, 0 = omitted)
# runs.next 	= matrix of P individuals for the new generation where each
# 		  row is a vector of the parameters included for the model
# 		  (1 = included, 0 = omitted)
# runs.aic 	= AIC values for the models of each generation
# P 		= size of each generation
# itr 		= number of generations to run
# m.rate 	= mutation rate
# r 		= ranks of AICs for a generation
# phi 		= fitness values for a generation
# run 		= vector of the parameters included in the best model found
# 		  (1 = included, 0 = omitted)
# best.aic 	= AIC value for the best model found
# aics 		= AIC values for the models at each step
# 		  (used for plotting)
#########################################################################
rm(list=ls())
setwd("~/school/math/Computational_Statistics_464/assignments/A2")

## INITIAL VALUES
path <- "~/school/math/Computational_Statistics_464/data_sets_and_textbook_R_code/datasets/baseball.dat"

baseball.dat = read.table(path,header=TRUE)
baseball.dat$freeagent = factor(baseball.dat$freeagent)
baseball.dat$arbitration = factor(baseball.dat$arbitration)
baseball.sub = baseball.dat[,-1]
salary.log = log(baseball.dat$salary)
n = length(salary.log)
m = length(baseball.sub[1,])
P = 20
itr = 100
m.rate = .01
r = matrix(0,P,1)
phi = matrix(0,P,1)
runs = matrix(0,P,m)
runs.next = matrix(0,P,m)
runs.aic = matrix(0,P,1)
aics = matrix(0,P,itr)
run = NULL
best.aic = 0
best.aic.gen = rep(0,itr)

# INITIALIZES STARTING GENERATION, FITNESS VALUES
set.seed(3219553) 
for(i in 1:P){
  # runs is originally a matrix of zeros. this randomly changes certain elements 
  # in the first row to 1
  runs[i,] = rbinom(m,1,.5)
  
  # selects certain columns of regressor matrix based off of which columns have a 1
  # in the first row 
  run.vars = baseball.sub[,runs[i,]==1]
  
  # perform linear regression on the subset of regressors
  g = lm(salary.log~.,run.vars)
  
  # determine the AIC for the first fitted model
  runs.aic[i] = extractAIC(g)[2]
  
  # places AIC values in the first column (aka first generation)
  aics[i,1] = runs.aic[i]
  
  
  if(runs.aic[i] < best.aic){
    # keep track of which predictors were used
    run = runs[i,]
    # keep track of the AIC value for this set of predictors
    best.aic = runs.aic[i]
  }
}

# calculates the rank of the vector containing the AIC values
r = rank(-runs.aic)

# calculate fitness level to be used for selecting which will become parents
phi = 2*r/(P*(P+1))

best.aic.gen[1]=best.aic

## MAIN
for(j in 1:itr-1){
  
  # BUILDS THE NEW GENERATION, SELECTING FIRST PARENT BASED ON
  # FITNESS AND THE SECOND PARENT AT RANDOM
  for(i in 1:10){
    # select first parent based on fitness
    parent.1 = runs[sample(1:P,1,prob=phi),]
    # select second parent randomly
    parent.2 = runs[sample(1:P,1),]
    
    # randomly select one number between 1 and 26 (not 27 because soon we'll be adding 
    # one to this number). pos is short for position. soon it will be the boundary
    # that separates where we select the first parent's genes and the second parent's genes
    pos = sample(1:(m-1),1)
    
    # set elements of mutate equal to 0 or 1 w/ probability m.rate
    mutate = rbinom(m,1,m.rate)
    
    # FINALLY, THE TWO LUCKY PARENTS GET TO BANG
    # here, the ith row of runs.next is populated with a string of ones
    # and zeros from each parent
    runs.next[i,] = c(parent.1[1:pos],parent.2[(pos+1):m])
    
    # here's where the (possible) mutation occurrs. the %%2 is to ensure
    # the resulting ?allele? is either a zero or a one
    runs.next[i,] = (runs.next[i,]+mutate)%%2
    
    # create a new mutate vector the same way as a few lines ago
    mutate = rbinom(m,1,m.rate)
    
    # more sex! but this time parent.2 is dominant and parent.1 is 
    # the submissive
    runs.next[P+1-i,] = c(parent.2[1:pos],parent.1[(pos+1):m])
    
    # and another round of mutation
    runs.next[P+1-i,] = (runs.next[P+1-i,]+mutate)%%2
    
  }
  # 
  runs = runs.next
  debug = 12
  
  # UPDATES AIC VALUES, FITNESS VALUES FOR NEW GENERATION
  
  # this chunk repeats the code that initialized the first generation. Here we 
  for(i in 1:P){
    run.vars = baseball.sub[,runs[i,]==1]
    g = lm(salary.log~.,run.vars)
    runs.aic[i] = extractAIC(g)[2]
    aics[i,j+1] = runs.aic[i]
    if(runs.aic[i] < best.aic){
      run = runs[i,]
      best.aic = runs.aic[i]
    }
  }
  best.aic.gen[j+1]=best.aic
  r = rank(-runs.aic)
  phi = 2*r/(P*(P+1))
}

## OUTPUT
run 		# BEST LIST OF PREDICTORS FOUND
best.aic 	# AIC VALUE

## PLOT OF AIC VALUES
plot(-aics,xlim=c(0,itr),ylim=c(50,425),type="n",ylab="Negative AIC",
     xlab="Generation",main="AIC Values For Genetic Algorithm")
for(i in 1:itr){points(rep(i,P),-aics[,i],pch=20)}
