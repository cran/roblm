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
and Yohai, 1984) for the errors which
is also computed with a 
bi-square score function. 
The S-estimator is computed using the
Fast-S algorithm of Salibian-Barrera and Yohai (2006).
Standard errors are computed using the 
formulas 
for homoscedastic and independent errors
of Croux, Dhaene and Hoorelbeke
(2003). 
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
\references{ 
Croux, C., Dhaene, G. and Hoorelbeke, D. (2003)
Robust standard errors
for robust estimators, Discussion Papers Series 03.16, K.U. Leuven, CES.

Rousseeuw, P.J. and Yohai, V.J. (1984)
Robust regression by means of 
S-estimators, In Robust and Nonlinear Time Series, J. Franke, W. 
H\"ardle and
R. D.
Martin (eds.). Lectures Notes in Statistics 26, 256-272, Springer Verlag, 
New York.

Salibian-Barrera, M. and Yohai, V.J. (2006) 
A fast algorithm for S-regression estimates, Journal of 
Computational and Graphical Statistics, in press.

Yohai, V.J. (1987)
High breakdown-point and high efficiency estimates for
regression, The Annals of Statistics 15, 642-65.
}
\author{ Matias Salibian-Barrera }
\note{ }
\seealso{ \code{\link{roblm.control}} }
\examples{
data(coleman.dat, package='roblm')
summary( roblm(y~., data=coleman.dat) )
}
\keyword{ robust }% at least one, from doc/KEYWORDS
\keyword{ regression }% __ONLY ONE__ keyword per line

