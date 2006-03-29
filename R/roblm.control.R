"roblm.control" <-
function(seed = 99, Nres = 500,
		   tuning.chi = 1.54764, bb=0.5, tuning.psi = 4.685061, 
		   groups = 5, n.group = 400, k.fast.s=1, max.it=50,
		   compute.rd = TRUE)
{
 return(list(seed=seed, Nres = Nres,
             tuning.chi=tuning.chi, bb=bb, tuning.psi=tuning.psi,
			 groups=groups, n.group=n.group,
			 k.fast.s=k.fast.s, max.it=max.it,
			 compute.rd=compute.rd))
}

