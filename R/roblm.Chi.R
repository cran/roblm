"roblm.Chi" <-
function(x, cc)
{ 

tmp <- x / cc
tmp2 <- 3 * tmp^2 - 3 * tmp^4 + tmp^6
tmp2[ abs(tmp) > 1 ] <- 1
return(tmp2)
}

