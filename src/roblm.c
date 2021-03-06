
/* write header */
/* Robust MM regression estimates  */
/* comment code */
/* adapt other sampler */
/* check pointers returned by malloc */
/* replace abort for too many singular resamples by
 * returning the number of singular ones
 */

/* file R-roblm.c
// version 0.6
// Includes the stable correct asymptotic variance estimators
// of Croux, Dhaene, Hoorelbeke
// Includes the fast-s algorithm
*/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <R.h>

#define EPS 1e-7
#define ZERO 1e-10
#define INFI 1e+20
#define MAX_ITER_FAST_S 50
#define MAX_NO_RESAMPLES 500
#define MAX_ITER_FIND_SCALE 200
#define TOL_INVERSE ZERO


/* This function computes an S-regression estimator */
void R_roblm_S(double *X, double *y, int *n, int *P, 
		int *nres, double *scale, double *beta_s, 
		int *seed_rand, double *C, 
		double *bb, int *Groups, int *N_group, int *K_fast_s)
{
void fast_s_large_n(double *X, double *y,
 		int *nn, int *pp, int *NN, int *K,
		int *ggroups, int *nn_group,
		int *bbest_r, double *bb, double *rrhoc,
		double *bbeta, double *sscale);
void fast_s(double *X, double *y, 
		int *nn, int *pp, int *NN, int *K, 
		int *bbest_r, double *bb,	
		double *rrhoc, double *bbeta, double *sscale);
int Nres, N, p, bbest_r = 2, ggroups = *Groups, 
	nn_group = *N_group, k_fast_s = *K_fast_s;
double b = *bb, rrhoc = *C;
Nres = *nres; N = *n; p = *P;
srand((long)*seed_rand); 
if( *n > 2000 )
	fast_s_large_n(X, y, n, P, nres, &k_fast_s,
			&ggroups, &nn_group, &bbest_r,
			&b, &rrhoc, beta_s, scale);
else
	fast_s(X, y, n, P, nres, &k_fast_s, 
			&bbest_r, &b, &rrhoc, beta_s, scale);

}

/* This function performs RWLS iterations starting
 * from an S-regression estimator (and associated 
 * residual scale) 
 */
void R_roblm_MM(double *X, double *y, int *n, int *P, 
		double *beta_initial, double *scale, 
		double *beta_m,
		int *max_it,
		double *Psi_c,
		int *converged)
{
int rwls(double **, int, int, double *, double *, double,
		double, int, double);
double **x; /* scale */
int N = *n, p = *P, i, j;
x = (double **) malloc( N * sizeof(double*) );
for(i=0;i<N;i++) 
        x[i]= (double *) malloc( (p+1) * sizeof(double) );
/* rearranges X into a matrix of n x p */
for(i=0;i<N;i++) {
        for(j=0;j<p;j++)
                x[i][j]=X[j*N+i];
        x[i][p]=y[i]; 
        };
/* starting from the S-estimate (beta_initial), use
 * irwls to compute the MM-estimate (beta_m)  */
*converged = 1; 
if ( rwls(x,N,p,beta_m,beta_initial,*scale,EPS,*max_it,*Psi_c) == 1 )  {
	for(i=0;i<p;i++) beta_m[i]=beta_initial[i];
	*converged = 0;  /* rwls failed to converge */
};

for(i=0;i<N;i++)
	free(x[i]);
free(x);
}


/* this functions solves a linear system of equations
 * it solves for "x"
 * a[, 0:(p-1)] x = a[,p]
 * using the LU decomposition of the p x p matrix
 * in a[0:(p-1), 0:(p-1)] 
 */
int lu(double **a,int *P, double *x)
{
int *pp,p;
register int i,j,k;
double *kk,s;
p = *P;
if ((pp = (int *) malloc(p*sizeof(int)))==NULL)
	  return(1); 
/* pp vector storing the permutations */
for(j=0;j<p;j++)   /* cols */
{ pp[j]=j;
  for(i=j;i<p;i++)   /* rows */
	if ( fabs( a[i][j] ) > fabs( a[pp[j]][j] ) )
		pp[j]=i;
  if ( pp[j] != j )       /* permute rows */
	{ kk=a[j];
	  a[j]=a[pp[j]];
	  a[pp[j]]=kk;
	};
  /* return if singular (det=0)
   * if pivot (j,j) is "zero"  */
    if ( fabs(a[j][j]) < TOL_INVERSE ) {   free(pp);
				return(1);
				};
  for(k=(j+1);k<p;k++)
	a[k][j] = a[k][j] / a[j][j];
  for(k=(j+1);k<p;k++)
	for(i=(j+1);i<p;i++)
		a[k][i] = a[k][i] - a[k][j] * a[j][i];

};    /* end of j for loop*/
for(i=0;i<p;i++)
	{ s=0.0;
	  for(j=0;j<i;j++)
	    s += a[i][j] * x[j];
	    x[i] = a[i][p] - s;          /* y[i]=a[i][p] */
	};
for(i=(p-1);i>=0;i--)
	{ s=0;
	  for(j=(i+1);j<p;j++)
	    s += a[i][j] * x[j];
	  x[i] = (x[i] - s) / a[i][i];
	  };
free(pp);
return(0);
}


double Chi(double x, double c)
{ 
/* // 
// // Tukey's bisquare loss function
// */
double t;
if( fabs(x) > c ) return(1.0);
else { t = x / c;
	return( 3.0*t*t - 3.0*t*t*t*t + t*t*t*t*t*t );
	}
}

double Psi_reg(double x, double c)
{
/* // 
// // First derivative of Tukey's bisquare loss function
// */
if (fabs(x)>c) return(0.0);
else	return( x / c * (1.0-(x/c)*(x/c))*
		(1.0-(x/c)*(x/c))  );
}

double loss_Tukey(double x, double c)
{
double Chi(double x, double c);
return( Chi(x, c) );
/*
if( fabs(x/c) < 1. ) 
	return( (x/c)*(x/c)/2. * 
			( 1 - (x/c)*(x/c) + 
		(x/c)*(x/c)*(x/c)*(x/c)/3. ) );
else
	return( 1. / 6. );
*/
}

double Loss_Tukey(double *x, int n, double c)
{
double loss_Tukey(double,double);
double s=0;
register int i;

for(i=0;i<n;i++) s += loss_Tukey(x[i],c);

return(s);
}

/* this function finds the k-th place in the
 * vector a, in the process it permutes the 
 * elements of a
 */
double kthplace(double *a, int n, int k)
{
int jnc,j;
int l,lr;
double ax,w;
k--;
l=0;
lr=n-1;
while (l<lr)
	{ ax=a[k];
	  jnc=l;
	  j=lr;
	  while (jnc<=j)
		{ while (a[jnc] < ax) jnc++;
		  while (a[j] > ax) j--;
		  if (jnc <= j)
			{ w=a[jnc];
			  a[jnc]=a[j];
			  a[j]=w;
			  jnc++;
			  j--;
			};
		};
	  if (j<k) l=jnc;
	if (k<jnc) lr=j;
	};
return(a[k]);
}

void sampler_i(int n, int *x)
{
/* function to get a random sample of
 * indices (0 to n-1)
 * *x receives the output
 * rand() returns an integer between 0 and RAND_MAX
 */
int i;
for(i=0;i<n;i++) 
	x[i] = (int) ( (double) rand() / RAND_MAX * (double) (n-1) );
}

void sample_n_outof_N(int n, int N, int *x)
{
/* function to get a random sample of size n 
 * of the indices (0 to N) WITHOUT replication
 * *x receives the output
 * rand() returns an integer between 0 and RAND_MAX
 *
 * adapt the other method
 *
 */
register int i,j,flag, cand=0;
if( N < n ) {
	/* printf("\nCant get %d out of %d \
without replication\n", n, N); */
	for(i=0;i<n;i++) x[i] = i;
} else {
for(i=0;i<n;i++) {
	flag=1;
	while (flag==1) {
		flag=0;
		cand = (int) ( (double) rand() / RAND_MAX *
					(double) N );
		for(j=0;j<i;j++)	
			if( cand==x[j] ) flag=1;
		};
	x[i]=cand;
	};
}
}

/* this functions returns ||x-y|| */
double norm_diff(double *x, double *y, int n)
{
double s=0;
register int i;
for(i=0;i<n;i++)
	s += (x[i]-y[i])*(x[i]-y[i]);
return( sqrt(s) );
}

/* C = A + B */
void sum_mat(double **a, double **b, double **c, int n, int m)
{
register int i,j;
for(i=0;i<n;i++)
	for(j=0;j<m;j++) 
		c[i][j] = a[i][j] + b[i][j];
}

/* A = v1 %*% t(v2) */
void matias_vec_vec(double **a, double *v1, double *v2, int n)
{
register int i,j;
for(i=0;i<n;i++)
	for(j=0;j<n;j++)
		a[i][j] = v1[i] * v2[j];
}

/* C = A * b */
void scalar_mat(double **a, double b, double **c, int n, int m)
{
register int i,j;
for(i=0;i<n;i++)
        for(j=0;j<m;j++)
		c[i][j]  = b * a[i][j];
}

/* c = a * b */
void scalar_vec(double *a, double b, double *c, int n)
{
register int i;
for(i=0;i<n;i++)
	c[i]  = b * a[i];
}

/* returns the inner product of a and b, i.e. t(a) %*% b */
double vecprime_vec(double *a, double *b, int n)
{
register int i;
double s = 0.0;
for(i=0;i<n;i++) s += a[i] * b[i];
return(s);
}

/* c = a + b */
void sum_vec(double *a, double *b, double *c, int n)
{
register int i;
for(i=0;i<n;i++) c[i] = a[i] + b[i];
}

/* c = a - b */
void dif_vec(double *a, double *b, double *c, int n)
{
register int i;
for(i=0;i<n;i++) c[i] = a[i] - b[i];
}

/* C = A - B */
void dif_mat(double **a, double **b, double **c, int n, int m)
{
register int i,j;
for(i=0;i<n;i++) 
	for(j=0;j<m;j++) c[i][j] = a[i][j] - b[i][j];
}

/* c = A %*% b */
void mat_vec(double **a, double *b, double *c, int n, int m)
{
register int i,j; 
for(i=0;i<n;i++) 
	for(c[i]=0,j=0;j<m;j++) c[i] += a[i][j] * b[j];
}

/* C = A %*% B */
void mat_mat(double **a, double **b, double **c, int n, 
		int m, int l)
{
register int i,j,k; 
for(i=0;i<n;i++) 
	for(j=0;j<l;j++) {
	c[i][j] = 0; 
	for(k=0;k<m;k++) c[i][j] += a[i][k] * b[k][j];
	};
}

/* RWLS iterations starting from i_estimate */
int rwls(double **a, int n, int p, 
			double *estimate, 
			double *i_estimate,
			double scale, double epsilon, 
			int max_it, double Psi_constant
			)
{
int lu(double **, int *, double *);   
double norm_diff(double *, double *, int);
double Psi_reg(double, double);
double Loss_Tukey(double*, int, double);

double **b,s,*beta1, *beta2, *beta0, *weights, *resid;    
double r,loss1,loss2,lambda;
int iterations=0, iter_lambda;
register int i,j,k;
if ( (b = (double **) malloc ( p * sizeof(double *) ) )==NULL )
	return(1);
for (i=0;i<p;i++)
	if ( (b[i] = (double *) malloc ( (p+1) * sizeof(double) ) )==NULL )
		return(1); 
beta1 = (double *) malloc( p * sizeof(double) );
beta2 = (double *) malloc( p * sizeof(double) );
beta0 = (double *) malloc( p * sizeof(double) );
weights = (double *) malloc( n * sizeof(double) );
resid = (double *) malloc( n * sizeof(double) );
for(i=0;i<p;i++)
	beta2[i] = (beta1[i]=i_estimate[i]) + 1;
/* main loop */
while( (norm_diff(beta1,beta2,p) > epsilon) &&
	( ++iterations < max_it ) ) { 
R_CheckUserInterrupt();	
for(i=0;i<n;i++) {
	s=0;
	for(j=0;j<p;j++)
		s += a[i][j] * beta1[j];
	r = a[i][p]- s;
	if(fabs(r/scale)<1e-7) 
		weights[i] = 1.0 / scale / Psi_constant;
                else 
        	weights[i] = Psi_reg(r/scale, Psi_constant) / r;
};
for(j=0;j<p;j++) beta2[j]=beta1[j];
/* get the residuals and loss for beta2 */
for(i=0;i<n;i++) 
	{ s = 0;
	for(j=0;j<p;j++) 
		s += a[i][j] * beta2[j];
	resid[i] = a[i][p] - s;
	};
loss2 = Loss_Tukey(resid,n,Psi_constant);
/* S+ version of the following code 
 * A <- matrix(0, p, p)
 * Y <- rep(0, p)
 * for(i in 1:n) {
 * A <- A + w[i] * a[i,0:(p-1)] %*% t(a[i,0:(p-1)])
 * Y <- Y + w[i] * a[i,0:(p-1)] * a[i,p]
 * }
 * beta1 <- solve(A, Y)
 */
for(j=0;j<p;j++) 
	for(k=0;k<=p;k++)  {
		b[j][k]=0.0;
		 for(i=0;i<n;i++)
			b[j][k] += a[i][j] * a[i][k] * weights[i];
		};
/* check if system is singular? */
if( lu(b,&p,beta1) == 0 ) {
	/* is beta1 good enough? */
	/* get the residuals and loss for beta1 */
	for(i=0;i<n;i++) 
		{ s = 0;
		for(j=0;j<p;j++) 
			s += a[i][j] * beta1[j];
		resid[i] = a[i][p] - s;
		};
	loss1 = Loss_Tukey(resid,n,Psi_constant);
	for(j=0;j<p;j++) beta0[j] = beta1[j];
	lambda = 1.;
	iter_lambda=0; 
	while( ( loss1 > loss2 ) ) {
		lambda /= 2.;
		for(j=0;j<p;j++) 
			beta0[j] = (1 - lambda) * beta2[j] + lambda * beta1[j];
		/* get the residuals and loss for beta0 */
		for(i=0;i<n;i++) 
			{ s = 0;
			for(j=0;j<p;j++) 
				s += a[i][j] * beta0[j];
			resid[i] = a[i][p] - s;
			};
		loss1 = Loss_Tukey(resid,n,Psi_constant);
		if( ++iter_lambda > 1000) {
			loss1 = loss2; /* force the exit */
			for(j=0;j<p;j++) beta0[j] = beta2[j];
			};
	}; /* end while(loss2 <= loss1 ) */
	} else { 
		iterations = max_it;
	};
}; /* end while(norm_diff(...)   */
for(j=0;j<p;j++) estimate[j]=beta0[j];
free(weights);free(beta1);free(beta2);
free(beta0);free(resid);
for(i=0;i<p;i++) free(b[i]);
free(b);
if( iterations == max_it ) /* MAX_ITER )  */
	return 1; 
	else 
	return 0;
}

/* sets the entries of a matrix to zero */
void reset_mat(double **a, int n, int m)
{
register int i,j;
for(i=0;i<n;i++)
	for(j=0;j<m;j++)
		a[i][j] = 0.0;
}

/* sets the entries of a vector to zero */
void reset_vec(double *a, int n)
{
register int i;
for(i=0;i<n;i++) a[i] = 0.0;
}

/* 
//
// 2004 / 5 -- Matias Salibian-Barrera & Victor Yohai
// Department of Statistics, University of British Columbia
// matias@stat.ubc.ca
// Department of Mathematics, University of Buenos Aires
// vyohai@uolsinectis.com.ar
//
//
// Reference: A fast algorithm for S-regression estimates,
// 2005, Salibian-Barrera and Yohai.


// This function implements the "large n" strategy
*/

void fast_s_large_n(double *X, double *y,
		int *nn, int *pp, int *NN, int *K,
		int *ggroups, int *nn_group,
		int *bbest_r, double *bb, double *rrhoc,
		double *bbeta, double *sscale)
{
/* *X = the design matrix as a vector (as sent by R)
// *y = the response vector
// *nn = the length of y
// *pp = the number of columns in X
// *NN = number of re-sampling candidates to be
//       used in each partition
// *bbest_r = no. of best candidates to be iterated
//            further
// *bb = right-hand side of S-equation (typically 1/2)
// *rrhoc = tuning constant for Tukey's bi-square loss
//          (this should be associated with *bb)
// *bbeta = final estimator
// *sscale = associated scale estimator
// *ggroups = number of groups in which to split the
//            random subsample
// *nn_group = size of each of the (*ggroups) groups
//             to use in the random subsample
*/
void reset_mat(double **a, int n, int m);
double loss_rho(double *r, double scale, int n, int p, double rhoc);
double find_scale(double *r, double b, double rhoc, 
			double initial_scale, int n, int p);
void fast_s_with_memory(double **x, double *y, 
		int *nn, int *pp, int *NN, int *K, 
		int *bbest_r, double *bb,	
		double *rrhoc, 
		double **best_betas, double *best_scales);
void sample_n_outof_N(int n, int N, int *x);
void refine_fast_s(double **x, double *y, double *weights,
			int n, int p, double *res,
			double *tmp, double *tmp2, 
			double **tmp_mat, double **tmp_mat2,
			double *beta_cand, int kk,
			int conv, double b, double rhoc, double *is, double *beta_ref,
			double *scale);
int find_max(double *a, int n);
register int i,j,k,k2;
int n = *nn, p = *pp, kk = *K, *indices; 
int groups = *ggroups, n_group = *nn_group, best_r = *bbest_r;
double **best_betas, *best_scales;
double **final_best_betas, *final_best_scales;
double **x, **xsamp, *ysamp, *res, sc, *beta_ref;
double *tmp, *tmp2, **tmp_mat, **tmp_mat2, *weights;
double best_sc, worst_sc, b = *bb, rhoc = *rrhoc, aux;
int pos_worst_scale, conv;
res = (double *) malloc( n * sizeof(double) );
weights = (double *) malloc( n * sizeof(double) );
tmp  = (double *) malloc( n * sizeof(double) );
tmp2 = (double *) malloc( n * sizeof(double) );
tmp_mat  = (double **) malloc( p * sizeof(double *) );
tmp_mat2 = (double **) malloc( p * sizeof(double *) );
for(i=0;i<p;i++) {
	tmp_mat[i] = (double *) malloc( p * sizeof(double) );
	tmp_mat2[i] = (double *) malloc( (p+1) * sizeof(double) );
};
beta_ref = (double *) malloc( p * sizeof(double) );
final_best_betas = (double **) malloc( best_r * sizeof( double * ) );
for(i=0; i < best_r; i++)
	final_best_betas[i] = (double *) malloc(
				p * sizeof(double) );
final_best_scales = (double *) malloc( best_r * sizeof(double) );
k = best_r * groups;
best_betas = (double **) malloc( k * sizeof( double * ) );
best_scales = (double *) malloc( k * sizeof( double ) );
for(i=0; i < k; i++)
		best_betas[i] = (double*) malloc( p * sizeof(double) );
x = (double**) malloc( n * sizeof(double *) );
for(i=0; i<n; i++) 
	x[i] = (double*) malloc( p * sizeof(double) );
k = n_group * groups;
indices = (int *) malloc( k * sizeof(int) );
xsamp = (double**) malloc( k * sizeof(double *) );
ysamp = (double*) malloc( k * sizeof(double) );
for(i=0;i<k;i++)
	xsamp[i] = (double*) malloc( p * sizeof(double) );
for(i=0;i<n;i++) 
	for(j=0;j<p;j++)
		x[i][j]=X[j*n+i];
/* assume that n > 2000
// k = n_group * groups
// set the seed 
*/
srand((long)37);
/* get a sample of k indices */
sample_n_outof_N(k, n-1, indices);
/* get the sampled design matrix and response */
for(i=0;i<k;i++) {
	for(j=0;j<p;j++)
		xsamp[i][j] = x[indices[i]][j];
		ysamp[i] = y[indices[i]];
};
/* now we go through the groups and get the 
// *bbest_r best betas for each group
*/
for(i=0; i<groups; i++) {
	fast_s_with_memory(xsamp+i*n_group, ysamp+i*n_group,
				&n_group, pp, NN, K,
				bbest_r, bb, rrhoc,
				best_betas+i*best_r,
				best_scales+i*best_r);
};
/* now  iterate (refine) these "best_r * groups"
// best betas in the (xsamp,ysamp) sample
// with kk C-steps
// and keep only the "best_r" best ones
*/
best_sc = INFI;
pos_worst_scale = conv = 0;
for(i=0; i < best_r; i++)
	final_best_scales[i] = INFI;
worst_sc = INFI;
/* set the matrix to zero */
reset_mat(final_best_betas, best_r, p); 
k = n_group * groups;
for(i=0; i< (best_r * groups) ; i++) {
	refine_fast_s(xsamp, ysamp, weights, k, p, res,
			tmp, tmp2, tmp_mat, tmp_mat2,
			best_betas[i], kk, conv, b, rhoc, 
			best_scales+i, beta_ref, 
			&sc);
	if ( loss_rho(res, worst_sc, k, p, rhoc) < b )  {
		/* scale will be better */
		sc = find_scale(res, b, rhoc, sc, k, p);
		k2 = pos_worst_scale;
		final_best_scales[ k2 ] = sc;
		for(j=0;j<p;j++)
			final_best_betas[k2][j] = beta_ref[j];
		pos_worst_scale = find_max(final_best_scales, best_r);
		worst_sc = final_best_scales[pos_worst_scale];
	};
};
/* now iterate the best "best_r" 
// betas in the whole sample (until convergence if possible)
*/
best_sc = INFI;
conv = 1;
for(i=0; i<best_r; i++) {
	refine_fast_s(x, y, weights, n, p, res,
			tmp, tmp2, tmp_mat, tmp_mat2,
			final_best_betas[i], kk, conv, b, rhoc, 
			final_best_scales+i, beta_ref, 
			&aux);
	if(aux < best_sc) {
			*sscale = best_sc = aux;
			for(j=0;j<p;j++)
				bbeta[j] = beta_ref[j];
	};
};
/* Done. Now clean-up. */
for(i=0;i<n;i++) free(x[i]); free(x);
free(best_scales);
k = best_r * groups;
for(i=0;i<k;i++) free( best_betas[i] );
free(best_betas); free(indices); free(ysamp);
k = n_group * groups;
for(i=0;i<k;i++) free(xsamp[i]); 
free(xsamp); free(tmp); free(tmp2);
for(i=0;i<p;i++) {
	free(tmp_mat[i]);
	free(tmp_mat2[i]);
};
free(tmp_mat); free(tmp_mat2); free(weights);
for(i=0;i<best_r;i++)
	free(final_best_betas[i]);
free(final_best_betas);
free(final_best_scales);
free(res);
free(beta_ref);

}

void fast_s_with_memory(double **x, double *y, 
		int *nn, int *pp, int *NN, int *K, 
		int *bbest_r, double *bb,	
		double *rrhoc, 
		double **best_betas, double *best_scales)
{
/*
// same as fast_s, but it returns the best.r best
// betas, and their associated scales
// useful for the adjustment for large "n"
//
// x an n x p design matrix (including intercept if appropriate)
// y and n vector
// *nn = n, *pp = p
// *NN = number of re-sampling candidates to be taken
// *K = number of refining steps for each candidate
// *bbest_r = number of (refined) to be retained for
// 					full iteration 
// 	*bb = right-hand side of the S-equation
// 	*rrhoc  = tuning constant of the \rho function
// 	*bbeta  = returning fast-S estimator
// 	*sscale = returning associated residual scale
*/
void refine_fast_s(double **x, double *y, double *weights,
			int n, int p, double *res,
			double *tmp, double *tmp2, 
			double **tmp_mat, double **tmp_mat2,
			double *beta_cand, int kk,
			int conv, double b, double rhoc,
			double *is,
			double *beta_ref, double *scale);
int find_max(double *a, int n);
double find_scale(double *r, double b, double rhoc, 
			double initial_scale, int n, int p);
double loss_rho(double *r, double s, int n, int p, double rhoc);
double vecprime_vec(double *a, double *b, int n);
void sample_n_outof_N(int n, int N, int *x);
int lu(double **a,int *P, double *x);
register int i,j,k,no_resamples;
int n = *nn, p = *pp, Nres = *NN, kk = *K;
int *b_i, flag, conv;
double **x_samp, *beta_cand, *beta_ref, *res, aux;
double b = *bb, rhoc = *rrhoc, sc, worst_sc = INFI;
double *weights; 
int best_r = *bbest_r, pos_worst_scale;
double *tmp, *tmp2, **tmp_mat2, **tmp_mat;

for(i=0;i<best_r;i++) {
		best_scales[i] = INFI;
};
pos_worst_scale = 0;
res       = (double *) malloc( n * sizeof(double) );
tmp       = (double *) malloc( n * sizeof(double) );
tmp2      = (double *) malloc( n * sizeof(double) );
weights   = (double *) malloc( n * sizeof(double) );
beta_cand = (double *) malloc( p * sizeof(double) );
beta_ref  = (double *) malloc( p * sizeof(double) );
b_i       = (int *) malloc( n * sizeof(int) );
x_samp    = (double **) malloc( n * sizeof(double*) );
tmp_mat   = (double **) malloc( p * sizeof(double*) );
tmp_mat2  = (double **) malloc( p * sizeof(double*) );
for(i=0;i<n;i++) {
	x_samp[i]  = (double *) malloc( (p+1) * sizeof(double) );
}
for(i=0;i<p;i++) {
	tmp_mat[i] = (double *) malloc( p * sizeof(double) );
	tmp_mat2[i] = (double *) malloc( (p+1) * sizeof(double) );
};
/* flag for refine(), conv == 0 means do k refining steps
// conv == 1 means refine until convergence
*/
conv = 0;
aux = -1.0;
/* resampling approximation  */
for(i=0;i<Nres;i++) {
	flag = 1;
	/* find a candidate */
	no_resamples = 0;
	while( flag == 1) {
		R_CheckUserInterrupt();	
		if( (++no_resamples) > MAX_NO_RESAMPLES ) {
			Rprintf("\nToo many singular resamples\nAborting\n\n");
			return;
		};
		/* take a sample of the indices  */
		sample_n_outof_N(p,n-1,b_i);  
		/* build the submatrix */
		for(j=0;j<p;j++) { 	
			for(k=0;k<p;k++)
				x_samp[j][k]=x[b_i[j]][k];
			x_samp[j][p]=y[b_i[j]];
			};	
		/* solve the system, lu = 1 means 
		// matrix is singular
		*/
		flag = lu(x_samp,pp,beta_cand);
	};
	refine_fast_s(x, y, weights, n, p, res,
			tmp, tmp2, tmp_mat, tmp_mat2,
			beta_cand, kk, conv, b, rhoc, 
			&aux, beta_ref, &sc);
	if ( loss_rho(res, worst_sc, n, p, rhoc) < b )  {
		/* scale will be better */
		sc = find_scale(res, b, rhoc, sc, n, p);
		k = pos_worst_scale;
		best_scales[ k ] = sc;
		for(j=0;j<p;j++)
			best_betas[k][j] = beta_ref[j];
		pos_worst_scale = find_max(best_scales, best_r);
		worst_sc = best_scales[pos_worst_scale];
	};

};
/* this function returns all the best_betas
// and best_scales
*/
free(tmp); free(tmp2);
free(res); free(weights); free(beta_cand);
free(beta_ref); free(b_i);
for(i=0; i<n; i++) {
	free(x_samp[i]);
};
for(i=0;i<p;i++) {
	free(tmp_mat[i]);
	free(tmp_mat2[i]);
};
free(x_samp); free(tmp_mat); free(tmp_mat2);
}

void fast_s(double *X, double *y, 
		int *nn, int *pp, int *NN, int *K, 
		int *bbest_r, double *bb,	
		double *rrhoc, double *bbeta, double *sscale)
{
/*
// X an n x p design matrix (including intercept if appropriate)
// y and n vector
// *nn = n, *pp = p
// *NN = number of re-sampling candidates to be taken
// *K = number of refining steps for each candidate
// *bbest_r = number of (refined) to be retained for
// 					full iteration 
// 	*bb = right-hand side of the S-equation
// 	*rrhoc  = tuning constant of the \rho function
// 	*bbeta  = returning fast-S estimator
// 	*sscale = returning associated residual scale
*/
void refine_fast_s(double **x, double *y, double *weights,
			int n, int p, double *res,
			double *tmp, double *tmp2, 
			double **tmp_mat, double **tmp_mat2,
			double *beta_cand, int kk,
			int conv, double b, double rhoc,
			double *is,
			double *beta_ref, double *scale);
/* void disp_mat(double **a, int n, int m);
void disp_vec(double *a, int n); */
int find_max(double *a, int n);
double find_scale(double *r, double b, double rhoc, 
			double initial_scale, int n, int p);
double loss_rho(double *r, double s, int n, int p, double rhoc);
double vecprime_vec(double *a, double *b, int n);
void sample_n_outof_N(int n, int N, int *x);
int lu(double **a,int *P, double *x);
register int i,j,k;
int n = *nn, p = *pp, Nres = *NN, kk = *K, no_resamples;
int *b_i, flag, conv;
double **x, **x_samp, *beta_cand, *beta_ref, *res, aux;
double b = *bb, rhoc = *rrhoc, sc, worst_sc = INFI;
double **best_betas, *best_scales, *weights;
int best_r = *bbest_r, pos_worst_scale;
double *tmp, *tmp2, **tmp_mat2, **tmp_mat, best_sc;
best_betas = (double **) malloc( best_r * sizeof(double*) );
best_scales = (double *) malloc( best_r * sizeof(double) );
for(i=0;i<best_r;i++) {
		best_betas[i] = (double*) malloc( p * sizeof(double) );
		best_scales[i] = INFI; };
pos_worst_scale = 0;
res       = (double *) malloc( n * sizeof(double) );
tmp       = (double *) malloc( n * sizeof(double) );
tmp2      = (double *) malloc( n * sizeof(double) );
weights   = (double *) malloc( n * sizeof(double) );
beta_cand = (double *) malloc( p * sizeof(double) );
beta_ref  = (double *) malloc( p * sizeof(double) );
b_i       = (int *) malloc( n * sizeof(int) );
x         = (double **) malloc( n * sizeof(double*) );
x_samp    = (double **) malloc( n * sizeof(double*) );
tmp_mat   = (double **) malloc( p * sizeof(double*) );
tmp_mat2  = (double **) malloc( p * sizeof(double*) );
for(i=0;i<n;i++) {
	x[i]       = (double *) malloc( p * sizeof(double) );
	x_samp[i]  = (double *) malloc( (p+1) * sizeof(double) );
};
for(i=0;i<p;i++) {
	tmp_mat[i] = (double *) malloc( p * sizeof(double) );
	tmp_mat2[i] = (double *) malloc( (p+1) * sizeof(double) );
};
for(i=0;i<n;i++) 
        for(j=0;j<p;j++)
                x[i][j]=X[j*n+i];

/* disp_mat(x, n, p); */
/* set the seed  */
srand((long)37);
/* flag for refine(), conv == 0 means do k refining steps
// conv == 1 means refine until convergence
*/
conv = 0;
aux = -1.0;
/* resampling approximation  */

for(i=0;i<Nres;i++) {
	flag = 1;
	/* find a candidate */
	no_resamples=0;
	while( flag == 1) {
		R_CheckUserInterrupt();	
		if( (++no_resamples) > MAX_NO_RESAMPLES ) {
			Rprintf("\nToo many singular resamples\nAborting\n\n");
			return;
		};
		/* take a sample of the indices  */
		sample_n_outof_N(p,n-1,b_i);  
		/* build the submatrix */
		for(j=0;j<p;j++) { 	
			for(k=0;k<p;k++)
				x_samp[j][k]=x[b_i[j]][k];
			x_samp[j][p]=y[b_i[j]];
			};	
		/* solve the system, lu = 1 means 
		// matrix is singular
		*/
		flag = lu(x_samp,pp,beta_cand);
	};
	/* disp_vec(beta_cand,p); */
	/* improve the re-sampling candidate */
	refine_fast_s(x, y, weights, n, p, res,
			tmp, tmp2, tmp_mat, tmp_mat2,
			beta_cand, kk, conv, b, rhoc, 
			&aux, beta_ref, &sc);
	/* disp_vec(beta_cand,p);
	disp_vec(beta_ref,p); 
	Rprintf("%f\n", sc); */
	if( fabs(sc) < ZERO) {
		*sscale = sc;
		for(j=0;j<p;j++) bbeta[j] = beta_cand[j];
		free(best_scales); free(tmp); free(tmp2);
		free(res); free(weights); free(beta_cand);
		free(beta_ref); free(b_i);
		for(i=0;i<best_r;i++)
			free(best_betas[i]);
		free(best_betas);
		for(i=0; i<n; i++) {
			free(x[i]);
			free(x_samp[i]);
		};
		for(i=0; i<p; i++) {
			free(tmp_mat[i]);
			free(tmp_mat2[i]);
		};
		free(x); free(x_samp); free(tmp_mat); free(tmp_mat2);
		return;
	};
	if ( loss_rho(res, worst_sc, n, p, rhoc) < b )  {
		/* scale will be better */
		sc = find_scale(res, b, rhoc, sc, n, p);
		k = pos_worst_scale;
		best_scales[ k ] = sc;
		for(j=0;j<p;j++)
			best_betas[k][j] = beta_ref[j];
		pos_worst_scale = find_max(best_scales, best_r);
		worst_sc = best_scales[pos_worst_scale];
	};
};
/* now look for the very best */
best_sc = INFI;
conv = 1;
for(i=0; i<best_r; i++) {
	refine_fast_s(x, y, weights, n, p, res,
			tmp, tmp2, tmp_mat, tmp_mat2,
			best_betas[i], kk, conv, b, rhoc, 
			best_scales+i, beta_ref, 
			&aux);
	if(aux < best_sc) {
			*sscale = best_sc = aux;
			for(j=0;j<p;j++)
				bbeta[j] = beta_ref[j];
	};
};
free(best_scales); free(tmp); free(tmp2);
free(res); free(weights); free(beta_cand);
free(beta_ref); free(b_i);
for(i=0;i<best_r;i++)
	free(best_betas[i]);
free(best_betas);
for(i=0; i<n; i++) {
	free(x[i]);
	free(x_samp[i]);
};
for(i=0; i<p; i++) {
	free(tmp_mat[i]);
	free(tmp_mat2[i]);
};
free(x); free(x_samp); free(tmp_mat); free(tmp_mat2);
}

void refine_fast_s(double **x, double *y, double *weights,
			int n, int p, double *res,
			double *tmp, double *tmp2, 
			double **tmp_mat, double **tmp_mat2,
			double *beta_cand, int kk,
			int conv, double b, double rhoc,
			double *is,
			double *beta_ref, double *scale)
{
/*
// weights = vector of length n
// res = vector of length n
// x = matrix with the data
// y = vector with responses
// tmp = aux vector of length n
// tmp2 = aux vector of length n
// tmp_mat = aux matrix p x p 
// tmp_mat2 = aux matrix p x (p+1)
*/
void fast_s_irwls(double **x, double *y, 
		double *weights, int n, int p, double *beta_ref,
		double **tmp_mat, double *tmp, double *tmp2);
double norm_diff(double *x, double *y, int n);
double norm(double *x, int n);
int lu(double **a,int *P, double *x);
void get_weights_rhop(double *r, double s, int n, 
		double rhoc, double *w);
void r_sum_w_x(double **x, double *w, int n, int p,
			double *tmp,
			double *sum);
void r_sum_w_x_xprime(double **x, double *w, int n, int p,
			double **tmp, double **ans);
double loss_rho(double *r, double scale, int n, int p, double rhoc);
double MAD(double *a, int n, double center, double *tmp,
			double *tmp2);
double vecprime_vec(double *a, double *b, int n);
register int i,j;
int zeroes=0;
double initial_scale = *is, s0;
for(j=0;j<n;j++)
		if( fabs(res[j] = y[j] - vecprime_vec(x[j], beta_cand, p))
				< ZERO ) zeroes++;
/* if "perfect fit", return it with a 0 assoc. scale */
if( zeroes > (((double)n + (double)p)/2.) )
{
	for(i=0;i<p;i++) beta_ref[i] = beta_cand[i];
	*scale = 0.0;
	return;
};
if( initial_scale < 0.0 ) 
	initial_scale = MAD(res, n, 0.0, tmp, tmp2);
s0 = initial_scale;
if( conv > 0 ) 
		kk = MAX_ITER_FAST_S;
if(kk > 0) {
for(i=0; i < kk; i++) {

	/* one step for the scale */
	s0 = s0 * sqrt( loss_rho(res,
					s0, n, p, rhoc) / b ); 
	/* compute weights for IRWLS */
	get_weights_rhop(res, s0, n, rhoc, weights);
	/* compute the matrix for IRWLS */
	r_sum_w_x_xprime(x, weights, n, p, tmp_mat,
				tmp_mat2);
	/* compute the vector for IRWLS */
	for(j=0; j<n; j++)
			weights[j] = weights[j] * y[j];
	r_sum_w_x(x, weights, n, p, tmp, tmp2);
	for(j=0; j<p; j++)
		tmp_mat2[j][p] = tmp2[j];
	/* solve the system for IRWLS */
	lu(tmp_mat2, &p, beta_ref);
	/* check for convergence? */
	if(conv > 0) {
		if(norm_diff(beta_cand, beta_ref, p) /
					norm(beta_cand, p) < EPS )
			break;
	};
	for(j=0;j<n;j++)
		res[j] = y[j] - vecprime_vec(x[j], beta_ref , p);
	for(j=0; j<p; j++)
		beta_cand[j] = beta_ref[j]; 
};
};
*scale = s0;
}


void fast_s_irwls(double **x, double *y, 
		double *weights, int n, int p, double *beta_ref,
		double **tmp_mat, double *tmp, double *tmp2)
{
void mat_prime_vec(double **a, double *b, double *c, int n, int m);
void mat_prime_mat_w(double **a, double *w, double **c, 
		int n, int m);
register int i;
for(i=0;i<n;i++) tmp[i] = weights[i] * y[i];
mat_prime_vec(x, tmp, tmp2, n, p);
mat_prime_mat_w(x, weights, tmp_mat, n, p);
for(i=0;i<p;i++) tmp_mat[i][p] = tmp2[i];
lu(tmp_mat, &p, beta_ref);
}

void get_weights_rhop(double *r, double s, 
		int n, 
		double rhoc, double *w)
{
register int i;
double a;
for(i=0;i<n;i++) {
	a = r[i] / s / rhoc;
	if( fabs(a) > 1 ) 
			w[i] = 0;
	else
			w[i] = (1. - a*a) * (1. - a*a);
};
}

double find_scale(double *r, double b, double rhoc, 
			double initial_scale, int n, int p)
{
double loss_rho(double *r, double scale, int n, int p, double rhoc);
int max_it = MAX_ITER_FIND_SCALE, it = 0;
double e = 1, scale;

while( (++it < max_it) && (fabs(e) > ZERO) )
{
	scale = initial_scale * sqrt(
			loss_rho(r, initial_scale, n, p, rhoc) /
					b ) ;
	e = fabs( scale / initial_scale - 1);
	initial_scale = scale;
};
return(scale);
}

int find_max(double *a, int n)
{
register int i;
int k=0;
double tmp = a[0];
if(n==1) return(0);	
else {
	for(i=1;i<n;i++)
		if(a[i] > tmp) {
				tmp = a[i];
				k = i;
		};
};
return(k);
}

void r_sum_w_x(double **x, double *w, int n, int p,
			double *tmp,
			double *sum)
{
/*
// given a matrix x (n x p) and a vector w of n
// weights, it computes the vector
// \sumin w_i x_i
// need space for p doubles in *tmp
*/
void scalar_vec(double *a, double b, double *c, int n);
void sum_vec(double *a, double *b, double *c, int n);
void reset_vec(double *a, int n);
register int i;


reset_vec(sum, p);

for(i=0; i<n; i++) {
	scalar_vec(x[i], w[i], tmp, p);
	sum_vec(sum , tmp , sum, p);
};


}


void r_sum_w_x_xprime(double **x, double *w, int n, int p,
			double **tmp, double **ans)
{
/*
// given a matrix x (n x p) and a vector w of n
// weights, it computes the matrix
// \sumin w_i x_i x_i'
// need space for p x p "doubles" in tmp
*/
void sum_mat(double **a, double **b, double **c, int n, int m);
void matias_vec_vec(double **a, double *v1, double *v2, int n); 
void scalar_mat(double **a, double b, double **c, int n, int m);
void reset_mat(double **a, int n, int m);
register int i;

reset_mat(ans, p, p);

for(i=0; i<n; i++) {
		matias_vec_vec(tmp, x[i], x[i], p); 
		scalar_mat(tmp, w[i], tmp, p, p); 
		sum_mat(ans, tmp, ans, p, p);
};

}


double loss_rho(double *r, double scale, int n, int p, double rhoc)
{
double Chi(double x, double c);
register int i;
double s = 0;
for(i=0;i<n;i++)
		s += Chi(r[i]/scale, rhoc);
return(s / ( (double) n - (double) p ) );
}

/* ||x|| */
double norm(double *x, int n)
{
double s = 0;
register int i;
for(i=0; i<n; i++) s += x[i] * x[i];
return(sqrt(s));
}

double MAD(double *a, int n, double center, double *b,
			double *tmp)
{
/* if center == 0 then do not center */
double median_abs(double *, int , double *);
int i;
/* if( fabs(center) > 0.0) { */
	for(i=0;i<n;i++) 
		b[i] = a[i] - center;
/* }; */
return( median_abs(b,n,tmp) * 1.4826 );
}

double median(double *x, int n, double *aux) 
{
double kthplace(double *,int,int);
double t;
register int i;
for(i=0;i<n;i++) aux[i]=x[i];
if ( (n/2) == (double) n / 2 )
	t = ( kthplace(aux,n,n/2) + kthplace(aux,n,n/2+1) ) / 2.0 ;
else	t = kthplace(aux,n, n/2+1 ) ;
return(t);
}

double median_abs(double *x, int n, double *aux)
{
double kthplace(double *,int,int);
double t;
register int i;
for(i=0;i<n;i++) aux[i]=fabs(x[i]);
if ( (n/2) == (double) n / 2 )
	t = ( kthplace(aux,n,n/2) + kthplace(aux,n,n/2+1) ) / 2.0 ;
else 	t = kthplace(aux,n, n/2+1 ) ;
return(t);
}



void mat_prime_mat_w(double **a, double *w, double **c, 
		int n, int m)
{
register int i,j,k;
for(i=0;i<m;i++) {
	for(j=0;j<m;j++) {
		c[i][j] = 0;
		for(k=0;k<n;k++) 
			c[i][j] += a[k][i] * w[k] * a[k][j];
	};
};
}

void mat_prime_vec(double **a, double *b, double *c, int n, int m)
{
register int i,j; 
for(i=0;i<m;i++) 
	for(c[i]=0,j=0;j<n;j++) c[i] += a[j][i] * b[j];
}


void disp_vec(double *a, int n)
{
register int i;
Rprintf("\n");
for(i=0;i<n; i++) Rprintf("%lf ",a[i]);
Rprintf("\n");
}


void disp_mat(double **a, int n, int m)
{
register int i,j;
for(i=0;i<n;i++) {
Rprintf("\n");
for(j=0;j<m;j++) Rprintf("%10.8f ",a[i][j]);
};
Rprintf("\n");
}


