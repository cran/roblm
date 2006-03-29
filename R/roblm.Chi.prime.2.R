"roblm.Chi.prime.2" <-
function(x, cc)
{
tmp <- x / cc
tmp2 <- ((1-tmp^2)^2 - 4*tmp^2*(1-tmp^2))*6/cc/cc
tmp2[ abs(tmp) > 1 ] <- 0
return(tmp2)
}

