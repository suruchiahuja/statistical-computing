
#question 2
u <- runif(100,0,1) #generating random samples

b=c(rep(0,100))
count=c(rep(0,100));


for(i in 1:100)
{
  x0=0.5;
  x1=1;
   x1=x0-(pbeta(x0,3,5)-u[i])/dbeta(x0,3,5)
  
   count[i]=count[i]+1
  
   if(abs(x1-x0)<0.05)
   break
 
   if(abs(x1-x0)>0.05)
 {
   x0=x1;
 }

b[i]=x1
}
hist(b,main="",xlab="Accepted observation")

mean(count)


#question 3-part a
t=4;
countcs=c(rep(0,4));
countks=c(rep(0,4));
#various sample sizes
x=c(10,25,50,100)
for(t in 1:4) #iteration for each sample size
{
  i=0;
  for(i in 1:500)
  {
    random=runif(x[t],0,1) #generating a random samples from U[0,1]
    c=hist(random,plot=F)$counts #adding the counts and breaks for different intervals
    
    #checking if the p value is less than 0.05 in chisq test
    if(chisq.test(c)$p.value < 0.05)
      countchi[t]=countchi[t]+1;
    
    #checking if p value is less than 0.05 in ks test
    if(ks.test(random,"punif",min=0,max=1)$p.value < 0.05)
      countks[t]=countks[t]+1;
  }
}
#creating a table with sample size as the rows and tests' counts as the columns
table = cbind(Sample_size=x,Chi_squared_test=countcs,KS_test=countks)
table


#Question 3 - part b
t=4;
countcs = c(rep(0,4));
countks = c(rep(0,4));
#various sample sizes
x=c(10,25,50,100)
for(t in 1:4) #iteration for each sample size
{
  i=0;
  for(i in 1:500)
  {
    random = rbeta(x[t],0.05,1) #generating a random samples from Beta[0.01,1]
    c=hist(random,plot=F)$counts#finding the counts for different intervals
    
    #checking if p value is less than 0.05 in chisq test
    if(chisq.test(c)$p.value>0.05)
      countcs[t]=countcs[t]+1;
    
    #checking if p value is less than 0.05 in ks test
    if(ks.test(random,"punif",min=0,max=1)$p.value>0.05)
      countks[t]=countks[t]+1;
  }
}
#creating a table with sample size as the rows and tests' counts as the columns
table=cbind(Sample_size=z,Chi_squared_test=ccountcs,KS_test=countks)
table


