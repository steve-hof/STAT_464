#July 2, 2014
#Simulated annealing algorithm

#Example 2

#Objective function f(x)
u0=c(-10:10)/10
N=length(u0)
points=c(1:N)

f=function(n,u=u0)
{ A0=matrix(0,nrow=3,ncol=3)
N=length(u)
for (ii in c(1:N))
{ A0=A0+n[ii]*c(1,u[ii],u[ii]^2)%*%t(c(1,u[ii],u[ii]^2))  }
A0=A0/12
# print(A0)
-det(A0)
}



#Minimizing f(n) over n=(n1, ..., n21),
#                where     n1, ..., n21 are nonnegative integers,
#                     n1+...+n21=12

m=2000
k=100
T0=0.005

set.seed(2009813)
#set.seed(3018653)

n0=rep(0,N)
n0[sample(points,12)]=1   #initial vector for n
sum(n0)   #This should be 12.

l0=f(n0)
result=c(n0,l0)

for (i in c(1:k))
{    for (j in c(1:m))
{  l0=f(n0)
s1=points[c(n0>0)]
i1=sample(s1,1)
s2=points[-i1]
i2=sample(s2,1)
n1=n0
n1[i1]=n1[i1]-1
n1[i2]=n1[i2]+1

l1=f(n1)

if (l1 < l0) 
{  n0=n1
l0=l1 
result=rbind(result, c(n0,l0))}

if (l1 > l0)
{ p=runif(1,0,1)
if (p < exp(-(l1-l0)/T0)) 
{  n0=n1
l0=l1
result=rbind(result, c(n0,l0))}
}
}
  T0=0.9*T0
  # print(i)
}

mm=length(result[,1])
print(mm)

par(mfrow=c(2,1))
plot(c(1:mm), result[,22],type="l",xlab="iteration number",ylab="f")
plot(u0, result[mm,c(1:21)], xlab="design points", ylab="frequency",type="h",
     ylim=c(0,6))


print(result[mm,c(1:21)])  #minimizer
class(mm)
length(mm)
vec = c(4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4)
f(vec)
