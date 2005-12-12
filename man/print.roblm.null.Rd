\name{print.roblm.null}
\alias{print.roblm.null}
\title{ print method for objects of class roblm }
\description{
  Print method for elements of class roblm
}
\usage{
\method{print}{roblm.null}(x, digits = max(3, getOption("digits") - 3), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ an object as created by \code{roblm} }
\item{digits}{ number of digits to use}
\item{...}{optional arguments to be passed to \code{print}}
}

% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{ print.lm }
%\examples{
%a <- roblm(y ~ ., data=stackloss.dat)
%plot(a, which=1)
%}
\keyword{ robust }
\keyword{ regression }% at least one, from doc/KEYWORDS

