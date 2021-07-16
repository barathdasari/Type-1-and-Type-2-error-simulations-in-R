
################################################################################
#       Function for calculating type 1 error                                  #
################################################################################

type1.error<- function(mu0,sigma,n,alpha,k=10000){
  p.value<- rep(NA,k)
  for(i in 1:k){
    random.sample<-rnorm(n = n,mean = mu0,sd = sigma)
    sample.mean<- mean(random.sample)
    sample.sd<- sd(random.sample)
    p.value[i]<- 1 - pt((sample.mean-mu0)/(sample.sd/sqrt(n)), df = n -1)
  }
  return(mean(p.value<alpha))
}

type1.error(mu0 = 8,sigma = .25,n = 100,alpha = .05)


