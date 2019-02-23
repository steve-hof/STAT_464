#July 2, 2014
#Simulated annealing algorithm

#Example 1

#Objective function f(x)
f=function(x)
{ x^4-11*x^3+36*x^2-36*x  }

xx=c(1:6500)/1000

f1=f(xx)

par(mfrow=c(2,2))
plot(xx,f1,xlab="x",ylab="f",type="l")



#Minimizing f(x) over x in [0, 6.5]

n=2000
k=100
T0=0.2
N=n*k
d=1.0

#set.seed(2009813)
#set.seed(3018653)
x0=runif(1,0,6.5)
l0=f(x0)
result=c(x0,l0)

for (i in c(1:k))
{    for (j in c(1:n))
{  l0=f(x0)
dx=runif(1, -d, d)

if ((x0+dx < 6.5) & (x0+dx > 0)) x1=x0+dx
if (x0+dx > 6.5)  x1=x0-dx
if (x0+dx < 0)    x1=x0-dx

l1=f(x1)

if (l1 < l0) 
{  x0=x1
l0=l1 
result=rbind(result, c(x0,l0))}

if (l1 > l0)
{ p=runif(1,0,1)
if (p < exp(-(l1-l0)/T0)) 
{  x0=x1
l0=l1
result=rbind(result, c(x0,l0))}
}
}
  T0=0.9*T0
}

m=length(result[,1])
print(m)

plot(xx,f1,xlab="x",ylab="f",type="l")
abline(v=result[m,1], lty=2, col="blue")
abline(h=result[m,2], lty=3, col="blue")
plot(c(1:m), result[,2],type="l",xlab="iteration number",ylab="f", ylim=c(-31,-25))
plot(c(1:m), result[,1],type="l",xlab="iteration number",ylab="x",ylim=c(4.5,6.5))

print(result[m,1])  #minimizer: 5.02353
print(result[m,2])  #minimum value of function f: -30.01179 