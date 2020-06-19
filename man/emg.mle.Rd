\name{emg.mle}
\alias{emg.mle}
\title{Maximum Likelihood estimate of parameters}
\description{
  Compute the maximum likelihood model for the parameters given a set of observations. Returns a model with
  estimates for mu, sigma, and lambda.
}
\usage{
  emg.mle(x, lower=NULL, upper=NULL, start=NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ vector of observations to estimate parameters for. }
  \item{lower}{list of lower bounds for parameters.}
  \item{upper}{list of upper bounds for parameters.}
  \item{start}{list of starting parameters for search.}
  \item{...}{optional parameters to pass to `mle`.}
}
\value{
   An object of class \code{\link[stats4]{mle-class}}.
}
\author{ Shawn Garbett }

\seealso{
\code{\link[emg]{EMG}}
\code{\link[emg]{emg.nllik}}
}
\examples{
  emg.mle(remg(200))
  
  ## a example involving fitting
  data(pc9_3um_erlotinib)

  intermitotic.time <- subset(pc9_3um_erlotinib, end.of.movie=='N' & died=='N')$observed

  hist(intermitotic.time, freq=FALSE, main="PC9 in 3um erlotinib", xlab='intermitotic time (hours)')

  fit <- emg.mle(intermitotic.time)
  pdf <- function(x) demg(x, coef(fit)['mu'], coef(fit)['sigma'], coef(fit)['lambda'])
  curve(pdf, from=0, to=170, add=TRUE, col='red')
}