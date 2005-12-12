\name{plot.roblm}
\alias{plot.roblm}
\title{ plot method for roblm }
\description{
  Diagnostic plots for elements of class roblm
}
\usage{
\method{plot}{roblm}(x, 
which = 1:5, caption = c("Standardized residuals vs. Robust Distances",
"Normal Q-Q vs. Residuals", "Response vs. Fitted Values", 
"Residuals vs. Fitted Values" ,
"Sqrt of abs(Residuals) vs. Fitted Vaules"),
panel = points, sub.caption = deparse(x$call), main = "",
ask = prod(par("mfcol")) < length(which) && dev.interactive(),
 ..., p=0.025)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{x}{ an object as created by \code{roblm} }
\item{which}{ integer number between 1 and 5 to specify which
  plot is desired }
\item{caption}{Caption for the different plots}
\item{panel}{Panel}
\item{main}{Main title}
\item{sub.caption}{sub titles}
\item{ask}{waits for user input before displaying each plot }
\item{...}{optional arguments for \code{par}}
\item{p}{threshold for distance-distance plot}
}
\references{ Robust diagnostic plots as in Rousseuw and van Zomeren
(1990) }

% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{ plot.lm }
%\examples{
%a <- roblm(y ~ ., data=stackloss.dat)
%plot(a, which=1)
%}
\keyword{ robust }
\keyword{ regression }% at least one, from doc/KEYWORDS

