\name{summary.roblm}
\alias{summary.roblm}
\title{ summary method for roblm }
\description{
  Summary method for elements of class roblm
}
\usage{
\method{summary}{roblm}(object, correlation = FALSE,
                symbolic.cor=FALSE, \dots) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{ an object as created by \code{roblm} }
\item{correlation}{logical variable indicating whether
to compute the correlation matrix of the coefficient
estimators}
\item{symbolic.cor}{logical variable indicating whether
to use symbols to display the correlation matrix of the coefficient
estimators}
\item{\dots}{Extra arguments}
}

% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{ summary.lm }
%\examples{
%a <- roblm(y ~ ., data=stackloss.dat)
%plot(a, which=1)
%}
\keyword{ robust }
\keyword{ regression }% at least one, from doc/KEYWORDS

