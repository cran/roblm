\name{roblm}
\alias{roblm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ MM-regression estimators  }
\description{
  MM-regression estimators
}
\usage{
roblm(formula, data = list(), weights, na.action, 
	model = TRUE, x = FALSE, y = FALSE, singular.ok = TRUE, 
	contrasts = NULL, offset = NULL, control = roblm.control())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{ formula }
  \item{data}{ data }
  \item{weights}{ weights }
  \item{na.action}{ na.action }
  \item{model}{ model }
  \item{x}{ x }
  \item{y}{ y }
  \item{singular.ok}{ singular.ok }
  \item{contrasts}{ contrasts } 
  \item{offset}{ offset }
  \item{control}{ control }
}
\details{ This function computes and MM-regression estimator
as described in Yohai (1987). It uses a bi-square re-desceding
score function, and by default it returns a highly
robust and highly efficient estimator (with
50\% breakdown point and 95\% asymptotic efficiency
for normal errors). It uses an S-estimator (Rousseeuw 
and Yohai, 1984) for the errors
scale. This estimator is also computed with a 
bi-square score function. This function uses the 
Fast-S algorithm of Salibian-Barrera and Yohai (2005)
to compute the S-estimators.

Standard errors are computed using the stable 
asymptotic formula of Croux, Dhaene and Hoorelbeke
(2004). There is also the option of using the
Robust Bootstrap of Salibian-Barrera and Zamar (2002). 
See \code{roblm.control}. 
}
\value{
  An object of class \code{roblm}. A list that includes the
  following  components:
  \item{coef}{The MM-regression estimator}
  \item{scale}{The S-scale estimator}
  \item{s}{The auxiliary S-regression estimator}
  \item{cov}{The estimated covariance matrix of the regression
  coefficients}
  \item{residuals}{Residuals associated with the MM-estimator}
  \item{fitted.values}{Fitted values associated with the
  MM-estimator}
  \item{converged}{\code{TRUE} if the IRWLS iterations
  converged}
}
\references{ Rousseeuw and Yohai (1984);
Yohai (1987); Salibian-Barrera and Yohai (2005) }
\author{ Matias Salibian-Barrera }
\note{ }
\seealso{ \code{\link{roblm.control}} }
\examples{
data(coleman.dat, package='roblm')
summary( roblm(y~., data=coleman.dat, control = 
	roblm.control(compute.roboot=TRUE) ) )
}
\keyword{ robust }% at least one, from doc/KEYWORDS
\keyword{ regression }% __ONLY ONE__ keyword per line

