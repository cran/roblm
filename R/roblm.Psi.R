"roblm.Psi" <-
function(x, cc)
{
tmp <- x / cc
tmp2 <- tmp*(1 - tmp^2)^2
tmp2[ abs(tmp) > 1 ] <- 0
return(tmp2)
}

