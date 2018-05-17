\name{emg-package}
\alias{emg-package}
\alias{emg}
\docType{package}
\title{Exponentially Modified Gaussian (EMG) Distribution}
\description{
  Provide basic functions for a mixture of gaussian and exponential distributions.
}
\details{
\tabular{ll}{
Package: \tab emg\cr
Type: \tab Package\cr
Date: \tab 2012-01-03\cr
License: \tab GPL 2.0\cr
LazyLoad: \tab yes\cr
}
  Provides basic distribution functions for the EMG model, pemg, demg, qemg and remg. As well as an MLE estimation
  routine emg.mle.
}
\author{
Shawn Garbett, 
Mark Kozdoba, 
Maintainer: Shawn Garbett <Shawn@Garbett.org>
}
\references{
  Gladney H.M., B.F. Dowden, J.D. Swalen. Computer-Assisted Gas-Liquid Chromatography. Anal. Chem., 1969, 41(7):883-8.
  
  Golubev A. Exponentially modified Gaussian (EMG) relevance to distributions related to cell proliferation and differentiation. J Theor Biol. 2010 Jan 21;262(2):257-66.
  
  Grushka E. Charaterization of Exponentially Modified Peaks in Chromatography. Anal. Chem., 1972, 44(11):1733-38. 
}
\keyword{ package }
\seealso{
\code{\link[emg]{EMG}}
\code{\link[emg]{emg.mle}}
\code{\link[stats]{Normal}}
\code{\link[stats]{Exponential}}
}
\examples{
  y <- remg(200)
  hist(y, freq=FALSE, ylim=c(0, 0.35), breaks=20)
  x <- 1:100/100 * 11 - 3
  lines(x, demg(x))
  m <- emg.mle(y)
  sqrt(diag(m@vcov)) # Show stderr in estimate
  ks.test(y, "pemg", 0, 1, 1)
}
