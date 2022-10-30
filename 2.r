# pop = vector of variate values for the population given in Table 4.1
pop<-c(rep(1,times=210),rep(2,times=127),rep(3,times=66),rep(4,times=39),
       rep(5,times=23),rep(6,times=13),rep(7,times=11),rep(8,times=7),
       rep(9,times=3),rep(10,times=1))
hist(pop,breaks=seq(1,10,1),col="cyan",main="",xlab="Variate Value")
mu<-mean(pop) # population mean
mu
(499*var(pop)/500)^0.5 # population standard deviation
k<-10000 # number of simulations
n<-15 # sample size
sim<-rep(0,k) # vector to store sample means
# Calculate k sample means for samples of size n drawn from population pop
for (i in 1:k)
  sim[i]=mean(sample(pop,n,replace=F))
hist(sim,freq=F,col="cyan",xlab="Sample Mean",main="")
# percentage of times sample mean is within 0.5 of true mean mu
mean(abs(sim-mu)<0.5)
