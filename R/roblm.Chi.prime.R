"roblm.Chi.prime" <-
function(x, cc)
{
tmp <- x / cc
tmp2 <- 6*tmp*(1-tmp^2)^2/cc
tmp2[ abs(tmp) > 1 ] <- 0
return(tmp2)
}

