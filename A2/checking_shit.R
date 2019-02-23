setwd("~/school/math/Computational_Statistics_464/assignments/A2")

load(file="part_a_results.Rda")
part_a_results

load(file="part_b_results.Rda")
pops_results
pops_results=pops_results[1:2]
pops_results


# save in case
if(selection=="tournament") {
  sub_size = 3
  # create sequence of indices
  idxs = seq(1,P,by=1)
  # permute indices
  idxs = sample(idxs)
  phis = phi[idxs]
  # split indxs vector into sub_size number of sub *lists*
  sub_idxs = split(idxs, ceiling(seq_along(idxs)/sub_size))
  paste("sub_idxs:", sub_idxs)
  sub_phis = split(phi, ceiling(seq_along(phi)/sub_size))
  # order each sub group based on fitness
  # order(phi)
  debug=12
  for(sl in 1:length(sub_idxs)) {
    order(sub_phis[[sl]])
  }
  debug=12
  for(sl in 1:length(sub_idxs)) {
    #2 order(phi[sub_idxs[[sl]]])'
    order(sub_phis[[sl]][sub_idxs[[sl]]])
  }
  debug=12
  for(sl in 1:length(sub_idxs)) {
    #3 sort(order(phi)[x])
    sort(order(sub_phis[[sl]][sub_idxs[[sl]]]))
  }
  debug=12
  for(sl in 1:length(sub_idxs)) {
    #4 phi[sort(order(phi)[sublist])]
    sub_phis[[sl]][sort(order(sub_phis[[sl]][sub_idxs[[sl]]]))]
  }
  
  debug=12
  
  
  
  
  #1 order(y)             #We want to sort by y, so order() gives us the sorting order
  #2 order(y)[x]          #looks up the sorting order for each x
  #3 sort(order(y)[x])    #sorts by that order
  #4 y[sort(order(y)[x])] #converts orders back to numbers from orders
  
  # append the first element (winner) of each sub-list to the winners vector 
  debug=12
}



orig = c(8,4,2,7)
order(orig)
rank(orig)






a = c(0,1,0,1)
b = c(.9, .2, .8, .4)
mat = matrix(a, b, byrow = F)
long = c(1, 9, 4, 2, 7, 4, 2, 6, 2, 6, 7)
split(long)
?split


fuck_off = matrix(1:12, nrow=4)
f2 = as.data.frame(fuck_off)
f2[2,3] = 4
f2[order(f2$V3),]

ad = c(4,2,7,5,8,3,1,4,4)
matt = matrix(ad, nrow=3, byrow=T)
matt
gimp = which.max(rank(matt[,3]))
gimp

#pd = pd[order(pd[,28]),]
?apply

matt
sapply(matt, which.max)

asd = c(0,1)

sample(asd,1)