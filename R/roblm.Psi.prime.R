"roblm.Psi.prime" <-
function(x, cc)
{
tmp <- x / cc
tmp2 <- (1 - tmp^2)*(1 - 5 * tmp^2) / cc
tmp2[ abs(tmp) > 1 ] <- 0
return(tmp2)
}

