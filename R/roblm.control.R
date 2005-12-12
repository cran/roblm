"roblm.control" <-
function(M = 2000, Nres = NA, seed = 99, 
		   fixed = FALSE, tuning.chi = 1.54764, tuning.psi = 4.685061, 
		   compute.roboot=FALSE, compute.rd=TRUE, max.it = 50,
		   groups = 5, n.group = 400, k.fast.s=1)
{
# for the fast-s algorithm we need
 return(list(M=M, Nres=Nres, seed=seed, fixed=fixed,
             tuning.chi=tuning.chi, tuning.psi=tuning.psi,
			 compute.roboot=compute.roboot, max.it=max.it,
			 compute.rd=compute.rd, groups=groups, n.group=n.group,
			 k.fast.s=k.fast.s))
}
