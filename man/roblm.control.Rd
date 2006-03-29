\name{roblm.control}
\alias{roblm.control}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Tuning parameters for roblm }
\description{
Tuning parameters for the MM-regression
estimator and the associated S-estimator}
\usage{
roblm.control(seed = 99, Nres = 500, 
	tuning.chi = 1.54764, bb = 0.5, tuning.psi = 4.685061, 
	groups = 5, n.group = 400, k.fast.s = 1,
	max.it = 50,
	compute.rd = TRUE
	) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Nres}{ Number of re-sampling candidates to be
  used to find the initial S-estimator. This parameter 
  is currently set to 500, which works well in most
  situations (see References below). User-choice capability will
  be added in future releases }
  \item{seed}{ Random seed for the re-samples used
  in obtaining candiates for the initial S-estimator.}
  \item{tuning.chi}{ Tuning constant for the S-estimator.
  The choice 1.54764 yields a 50\% breakdown estimator.}
  \item{bb}{ Expected value under the normal model of the
  "chi" function with tuning constant equal to
  \code{tuning.chi}. This is used to compute the S-estimator }
  \item{max.it}{ Maximum number of IRWLS iterations }
  \item{tuning.psi}{ Tuning constant for the 
  re-descending M-estimator. The choice 4.685061 yields
  an estimator with asymptotic efficiency of 95\% for
  normal errors.}
  \item{compute.rd}{ If TRUE robust distances (based on 
  the MCD robust covariance matrix) are computed 
  for the robust diagnostic plots. This may take some
  time to finish, specially for large data sets.}
  \item{groups}{ This parameter is for the fast-S algorithm.
  Number of random subsets to 
  use when the data set is large.}
  \item{n.group}{ This parameter is for the fast-S algorithm.
  Size of each of the \code{groups} above.}
  \item{k.fast.s}{This parameter is for the fast-S algorithm. 
  Number of local improvement steps for each re-sampling
  candidate. }
}
\author{ Matias Salibian-Barrera }
\seealso{ \code{\link{roblm}} }
%\examples{
%a <- roblm(y~., data=coleman.dat, control = 
%	roblm.control(compute.roboot=FALSE) )
%}
\keyword{ robust }% at least one, from doc/KEYWORDS
\keyword{ regression }% __ONLY ONE__ keyword per line

