\name{roblm.S}
\alias{roblm.S}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ S-regression estimators }
\description{
	This function computes the S-regression estimator
}
\usage{
roblm.S(x, y, control)
}
\arguments{
  \item{x}{ Design matrix }
  \item{y}{ Response vector }
  \item{control}{ list as retruned by \code{roblm.control} }
}
\details{
  This function is used by \code{roblm.fit.MM} and not
  intended to be used on its own
}
\value{
	A list containing the following elements:
  \item{coef    }{ Vector of S-regression coefficient estimates }
  \item{cov     }{ Covariance matrix of the coefficient estimators }
  \item{control }{ The control list as given to it }
  \item{scale   }{ The S-scale residual estimate }
  \item{seed    }{ The value of \code{seed} }
}
\references{ 
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

}
\author{ Matias Salibian-Barrera }
\keyword{ robust }% at least one, from doc/KEYWORDS
\keyword{ regression }% __ONLY ONE__ keyword per line
