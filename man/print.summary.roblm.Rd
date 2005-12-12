\name{print.summary.roblm}
\alias{print.summary.roblm}
\title{ print method for objects of class summary.roblm }
\description{
  Print method for elements of class summary.roblm
}
\usage{
\method{print}{summary.roblm}(x, digits = max(3, getOption("digits") - 3), 
symbolic.cor=
          x$symbolic.cor, signif.stars = getOption("show.signif.stars"),
         \dots) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ an object as created by \code{summary.roblm} }
\item{digits}{number of digits to display}
\item{symbolic.cor}{logical variable indicationg
whether to use symbols to display the
estimated correlation matrix of the coefficient estimators}
\item{signif.stars}{logical variable indicating
whether to use stars to display different levels of
significance in the individual t-tests}
\item{\dots}{extra arguments for print}
}

% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{ print.summary.lm }
%\examples{
%a <- roblm(y ~ ., data=stackloss.dat)
%plot(a, which=1)
%}
\keyword{ robust }
\keyword{ regression }% at least one, from doc/KEYWORDS

