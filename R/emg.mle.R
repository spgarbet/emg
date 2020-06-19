emg.mle <- function(x, lower=NULL, upper=NULL, start=NULL, ...)
{
   # To provide some reasonable defaults
   if(is.null(lower))
   {
     lower <- list(mu=min(x), sigma=sd(x)/10, lambda=0.001/mean(x))
   }
   if(is.null(upper))
   {
     upper <- list(mu=max(x), sigma=(max(x)-min(x))/4, lambda=100/mean(x))
   }
  
   if(is.null(start))
   {
      start <- list(
        mu     = mean(x) - sd(x)*((skewness(x)/2)^(1/3)),
        sigma  = sqrt(sd(x)^2*(1 - (skewness(x)/2)^(2/3)) ),
        lambda = 1/(sd(x) * (skewness(x)/2)^(1/3))
      )
      
      if(is.nan(start$mu))
      {
        start <- list(
          mu     = (lower$mu+upper$mu)/2,
          sigma  = (lower$sigma+upper$sigma)/2,
          lambda = (lower$lambda+upper$lambda)/2
        )
      }
   }
  
   mle(function(mu, sigma, lambda){
     emg.nllik(x, mu, sigma, lambda)},
             method='L-BFGS-B',
             lower=lower,
             upper=upper,
             start=start,
             ...
       )
}