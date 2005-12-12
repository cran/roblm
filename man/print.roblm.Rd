\name{print.roblm}
\alias{print.roblm}
\title{ print method for objects of class roblm }
\description{
  Print method for elements of class roblm
}
\usage{
\method{print}{roblm}(x, digits = max(3, getOption("digits") - 3), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ an object as created by \code{roblm} }
\item{digits}{ number of digits to display}
\item{...}{additional arguments}
}

% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{ print.lm }
%\examples{
%a <- roblm(y ~ ., data=stackloss.dat)
%plot(a, which=1)
%}
\keyword{ robust }
\keyword{ regression }% at least one, from doc/KEYWORDS

