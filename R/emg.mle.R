emg.mle <- function(x, lower=NA, upper=NA)
{
   # To provide some reasonable defaults
   if(is.na(lower))
   {
     lower <- list(mu=min(x), sigma=sd(x)/10, lambda=0.001/mean(x))
   }
   if(is.na(upper))
   {
     upper <- list(mu=max(x), sigma=(max(x)-min(x))/4, lambda=100/mean(x))
   }
   
   start <- list(
      mu     = mean(x) - sd(x)*((skewness(x)/2)^(1/3)),
      sigma  = sqrt(sd(x)^2*(1 - (skewness(x)/2)^(2/3)) ),
      lambda = 1/(sd(x) * (skewness(x)/2)^(1/3))
   )

   mle(function(mu, sigma, lambda){
     emg.nllik(x, mu, sigma, lambda)},
             method='L-BFGS-B',
             lower=lower,
             upper=upper,
             start=start
       )
}