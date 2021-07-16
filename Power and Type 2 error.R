n = 16                # sample size 
sigma = 3.8         # population standard deviation 
sem = sigma/sqrt(n); sem   # standard error 

#We next compute the upper bound of sample means for which 
#the null hypothesis ?? ??? 65 would not be rejected.

alpha = .05           # significance level 
mu0 = 65               # hypothetical upper bound 
q = qnorm(alpha, mean=mu0, sd=sem, lower.tail=FALSE); q 


#Therefore, as long as the sample mean is less than 66.56261  
#the null hypothesis will not be rejected. 
#Since we assume that the actual population mean is 68.2, 
#we can compute the probability of the sample mean below 66.56261, 
#and thus the probability of type II error.

mu = 68.2             # assumed actual mean 
t2error<-pnorm(q, mean=mu, sd=sem)
power<- 1-t2error
power




###################################################################################################
# simulation

type2error <- function(mu0, TRUEmu, sigma, n, alpha, iterations=10000){
  pval <- rep(NA,iterations)
  for( i in 1 : iterations){
    temp.sample <- rnorm(n=n, mean= TRUEmu, sd = sigma)
    temp.mean <- mean(temp.sample)
    temp.sd<- sd(temp.sample)
    pval[i] <- 1- pt((temp.mean-mu0)/(temp.sd/sqrt(n)), df = n-1)
  }
  return(mean(pval>=alpha))
}

power = 1- type2error(mu0=65, TRUEmu = 68.2, sigma=3.8, n=16, alpha=.05, iterations=10000)
power


