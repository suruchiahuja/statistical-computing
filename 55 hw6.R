###################################
#########
library(base)
x=seq(-10,10,length=100)
 c <- function(x,theta){
   sqrt(2/(pi*theta^2))*exp((theta^2)/2)
 }
theta <- 1
x11()
plot(x,c(x,theta = 1))

c1 <- 








########################################################
library(base)
#Initializing given data
data = c(rep(0,7840),rep(1,1317),rep(2,239),rep(3,42),rep(4,14),rep(5,4),rep(6,4),7)
beta=3.2;

m=mean(data);
alpha.mom=m*beta;
alpha.mom

#################
data = c(rep(0,7840),rep(1,1317),rep(2,239),rep(3,42),rep(4,14),rep(5,4),rep(6,4),7)
beta=3.2;

logfn1 <- function (alpha){
  ba <- beta ^alpha
  dr <- (beta+1)^(alpha+data)
  return(-sum(log((ba)*gamma(alpha+data)/(factorial(data)*gamma(alpha)*dr))))
}
a <-c()
y <- c()
for(i in 1:100){
  a[i] <- 1/i
  y[i] <- logfn1(a[i])
}
z=-y
x11()
plot(a,z, xlab="x", ylab="l(x)",'l');
#finding the maximum of the function
mle_alpha <- nlminb(0.5,logfn1)$par
mle_alpha

fn <- function(alpha,data){
  ba <- beta ^alpha
  dr <- (beta+1)^(alpha+data)
  return(-sum(log(ba*gamma(alpha+data)/(factorial(data)*gamma(alpha)*dr))))
}

#finding confidence intervals
n=length(data);
B=100
mle_alpha_boot=c()
for(i in 1:B){
  boot_obs=sample(1:n,n,replace=T)
  y_boot = data[boot_obs] 
  for (j in 1:10)
  {
    a[j]=1/j
    y[j]=fn(a[j],y_boot)
  }
  mle_alpha_boot[i]=nlminb(0.5,fn,data=y_boot)[[1]];
}
se = sqrt(var(mle_alpha_boot))
#Calculating the Normal confidence interval
Normal = c(mle_alpha_boot-2*se, mle_alpha_boot+2*se)


###############
n=length(data);
prob <- data[data[]==2]
#calculating the observed probability of more than 2 claims
prob <- length(prob)/n;
#finding confidence intervals using Bootstrap technique

prob.boot=c();
pb=c();
for(i in 1:100){
  x <- sample(1:n,n,replace=T)
  x.bootstrap <- data[x] 
  pb <- x.bootstrap[x.bootstrap[]>2]
  prob.boot[i] <- length(prob.boot)/n;
}
serror = sqrt(var(prob.boot))
#Calculating the Normal confidence interval
Normal = c(prob-2*serror, prob+2*serror)

#######################################################################################
############################################################################
##################################################################################

#Creating two column vectors of size 100 with all values 3. 
N1=c(rep(3,100))
N2=c(rep(3,100))
for(k in 1:100)
{
  #let N1[k] be the random variable that counts the number of trials needed to get a HTT at kth trial
  y=round(runif(N1[k],0,1))#simulation for a coin toss
  while((y[N1[k]-2]!=1)||(y[N1[k]-1]!=0)||(y[N1[k]]!=0))
    #The condition in the while loop is to make sure we get a 1 0 0 pattern (an analog to HTT pattern)
  {
    #Simulating a new coin toss each till the desired pattern is achieved
    N1[k]=N1[k]+1;
    y[N1[k]]=round(runif(1,0,1))
  }
  
  #let N2[k] be the random variable that counts the number of trials needed to get a HTH at kth trial
  y=round(runif(N2[k],0,1))#simulation for a coin toss
  while((y[N2[k]-2]!=1)||(y[N2[k]-1]!=0)||(y[N2[k]]!=1))
    #The condition in the while loop is to make sure we get a 1 0 1 pattern
  {
    #Simulating a new coin toss each till the desired pattern is achieved
    N2[k]=N2[k]+1;
    y[N2[k]]=round(runif(1,0,1))
  }
}
#Gives mean number of tosses required to observe each pattern
m1=mean(N1)
m2=mean(N2)
x11()
bp=barplot(c(m1,m2), names.arg=c("HTT","HTH"),ylab="Average number of tosses")
text(bp,0,c(m1,m2),cex=1,pos=3)


######################################################################################

x=round(runif(100000,0,1));
a=0;
b=0;
#Checking for Patterns HTT and HTH
for(i in 3:100000)
{
  if((x[i-2]==1)&(x[i-1]==0)&(x[i]==0))
    a=a+1
  if((x[i-2]==1)&(x[i-1]==0)&(x[i]==1))
    b=b+1
}
#Opens a blank .pdf file
pdf("hw.pdf")
#saves a barplot in the .pdf file
bp1=barplot(c(a,b), names.arg=c("HTT","HTH"),ylab="Number of occurrences")
text(bp1,0,c(a,b),cex=1,pos=3)














