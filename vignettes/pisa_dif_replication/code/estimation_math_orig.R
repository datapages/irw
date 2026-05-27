library(fastGHQuad)
library(quantreg)
library(MASS)
library(foreach)
library(doParallel)

gauss=gaussHermiteData(n=35)#We use 35 Gaussian quadrature
w=gauss$w
z=gauss$x

if_zero=function(x.vec){
  # Purpose: Indicator for whether every element of a vector is zero.
  # Arguments:
  #   x.vec - Numeric vector.
  # Returns:
  #   Integer 1 if all elements equal 0, otherwise 0.
  if (all(x.vec==0)){
    return(1)
  }
  else{return(0)}
}

### Estimation ###
tran=function(z, x, mu, sigma){
  # Purpose: Map Gauss-Hermite quadrature nodes to person-specific latent trait values
  #   Theta_{ik} = sqrt(2)*sd_i*z_k + mean_i from group indicators x and (mu, sigma).
  # Arguments:
  #   z - Length-n quadrature nodes (standard normal scale).
  #   x - N-by-p binary group-indicator matrix.
  #   mu - Length-p group means for the latent trait.
  #   sigma - Length-p group SDs for the latent trait.
  # Returns:
  #   Matrix of dimension N by n with transformed quadrature values per person.
  n=length(z)
  N=dim(x)[1]
  p=dim(x)[2]
  mean.vec=apply((rep(1, N)%*%t(mu))*x, 1, sum) #N dim vector
  var.vec=apply((rep(1, N)%*%t(sigma^2))*x, 1, sum) #N dim vector
  sd.vec=var.vec^0.5
  Theta=sqrt(2)*(sd.vec%*%t(rep(1, n)))*(rep(1, N)%*%t(z))+mean.vec%*%t(rep(1, n)) #N by n 
  return(Theta)
}

evaluate_prod=function(v){
  # Purpose: Exponentiated sum of vector elements (product of exponentials), used when
  #   multiplying category probabilities across items in log space.
  # Arguments:
  #   v - Numeric vector (typically log-probabilities with NA allowed).
  # Returns:
  #   Scalar exp(sum(v, na.rm=TRUE)).
  L=length(v)
  return(exp(sum(v, na.rm = TRUE)))
}



quad=function(a, b, g, x, dat, z, mu, sigma, w){ #z is quadrature of dim n
  # Purpose: E-step posterior weights over quadrature nodes for a 2PL-like model with
  #   discriminations a, difficulties b, DIF parameters g, and latent prior from (mu, sigma) and x.
  # Arguments:
  #   a - Length-J item discriminations.
  #   b - Length-J item difficulties (intercepts).
  #   g - J-by-p DIF (group-by-item) parameter matrix.
  #   x - N-by-p binary group matrix.
  #   dat - N-by-J binary item response matrix.
  #   z - Length-n quadrature nodes.
  #   mu - Length-p latent-trait mean by group.
  #   sigma - Length-p latent-trait SD by group.
  #   w - Quadrature weights (length n).
  # Returns:
  #   N-by-n matrix of normalized posterior weights q_{ik} for each person i and quadrature index k.
  n=length(z)
  N=dim(dat)[1]
  J=dim(dat)[2]
  p=dim(x)[2]
  Theta=tran(z, x, mu, sigma) #Transform Gauss Hermite Quadrature z into th, th_i=sqrt(2)*z_i+mu for i=1,...,n
  th=Theta[, 1]
  ta=rep(1, N)%*%t(a)
  tb=rep(1, N)%*%t(b)
  tg=x%*%t(g)
  
  temp=(th%*%t(rep(1, J)))*ta+ tb + tg #N by J
  prob=exp(temp)/(1+exp(temp)) #dim N by J
  q=apply(log((prob^dat)*((1-prob)^(1-dat))), 1, evaluate_prod) #length N vector
  for (k in 2:n){ #matrix evaluate
    th=Theta[, k]
    temp=(th%*%t(rep(1, J)))*ta+tb+tg
    prob=exp(temp)/(1+exp(temp))
    q_c=apply(log((prob^dat)*((1-prob)^(1-dat))), 1, evaluate_prod)
    q=cbind(q, q_c)
  }
  Z=rowSums(q*(rep(1, N)%*%t(w)))
  return((q*(rep(1, N)%*%t(w)))/(Z%*%t(rep(1, n))))
}






target_function1=function(phi_j, x, Y_j, weights, z,  mu_pres, sigma_pres, q){
  # Purpose: Negative weighted average log-likelihood for the anchor item (j=1) as a function of
  #   scalar difficulty b_j; discrimination fixed at 1 and DIF vector fixed at 0.
  # Arguments:
  #   phi_j - Scalar difficulty b_j for the anchor item.
  #   x - N-by-p group indicators.
  #   Y_j - Length-N vector of responses on item j.
  #   weights - Length-N sampling weights.
  #   z - Length-n quadrature nodes.
  #   mu_pres, sigma_pres - Current latent-trait mean and SD by group (length p).
  #   q - N-by-n posterior weights from the E-step.
  # Returns:
  #   Scalar objective (negative normalized weighted log-likelihood) for optim().
  N=dim(x)[1]
  p=dim(x)[2]
  n=length(z)
  b_j=phi_j
  g_j=rep(0, p)
  a_j=1
  
  #Evaluate log-likelihood
  Theta=tran(z, x, mu_pres, sigma_pres)
  temp=Theta*a_j+b_j*(rep(1,N)%*%t(rep(1, n)))+(x%*%g_j)%*%t(rep(1, n)) #N by n
  t2=Y_j%*%t(rep(1, n))*temp-log(1+exp(temp)) #The remaining term in log-likelihood
  log_likelihood=(weights%*%t(rep(1, n)))*t2 #N by n
  res=-sum(q*log_likelihood, na.rm = TRUE)
  return(res/N)
}

target_function_ab=function(ab, g, x, Y_j, weights, z,  mu_pres, sigma_pres, q){
  # Purpose: Negative weighted average log-likelihood for a non-anchor item w.r.t. scalar discrimination a_j
  #   (passed as ab); difficulty b_j fixed at 0 in the predictor; g fixed.
  # Arguments:
  #   ab - Scalar discrimination a_j.
  #   g - Length-p DIF vector for item j.
  #   x - N-by-p group indicators.
  #   Y_j - Length-N vector of responses on item j.
  #   weights - Length-N sampling weights.
  #   z - Length-n quadrature nodes.
  #   mu_pres, sigma_pres - Current latent-trait mean and SD by group (length p).
  #   q - N-by-n posterior weights from the E-step.
  # Returns:
  #   Scalar objective for optim().
  N=dim(x)[1]
  p=dim(x)[2]
  n=length(z)
  a_j=ab
  b_j=0
  g_j=g
  
  #Evaluate log-likelihood
  Theta=tran(z, x, mu_pres, sigma_pres)
  temp=Theta*a_j+b_j*(rep(1,N)%*%t(rep(1, n)))+(x%*%g_j)%*%t(rep(1, n)) #N by n
  t2=Y_j%*%t(rep(1, n))*temp-log(1+exp(temp)) #The remaining term in log-likelihood
  log_likelihood=(weights%*%t(rep(1, n)))*t2 #N by n
  res=-sum(q*log_likelihood, na.rm = TRUE)
  return(res/N)
}

target_function_g1=function(ab, k, gk, gp, x_k, Y_k, weights, z, ind_k, mu_pres, sigma_pres, q){
  # Purpose: Negative weighted average log-likelihood for updating one DIF entry g_{jk} on the anchor item
  #   with discrimination and difficulty fixed (a_j=1, b_j=0 in code).
  # Arguments:
  #   ab - Passed for optim interface consistency (anchor linear predictor uses fixed a,b).
  #   k - Group index whose DIF entry is being updated.
  #   gk - Candidate value for g_{jk}.
  #   gp - Full length-p DIF vector for item j.
  #   x_k, Y_k, ind_k - Group-k covariates, responses, and row indices.
  #   weights - Full-sample sampling weights.
  #   z - Quadrature nodes.
  #   mu_pres, sigma_pres - Latent-trait mean and SD by group.
  #   q - Posterior weights matrix.
  # Returns:
  #   Scalar objective for optim().
  N_k=dim(x_k)[1]
  p=dim(x_k)[2]
  n=length(z)
  a_j=1
  b_j=0
  gp[k]=gk
  g_j=gp
  
  q_k=q[ind_k, ]
  weight_k=weights[ind_k]
  
  #Evaluate log-likelihood
  Theta=tran(z, x_k, mu_pres, sigma_pres)
  temp=Theta*a_j+b_j*(rep(1,N_k)%*%t(rep(1, n)))+(x_k%*%g_j)%*%t(rep(1, n)) #N_k by n
  t2=Y_k%*%t(rep(1, n))*temp-log(1+exp(temp)) #The remaining term in log-likelihood
  log_likelihood=(weight_k%*%t(rep(1, n)))*t2 #N_k by n
  res=-sum(q_k*log_likelihood, na.rm = TRUE)
  return(res/N_k)
}



target_function_g=function(ab, k, gk, gp, x_k, Y_k, weights, z, ind_k, mu_pres, sigma_pres, q){
  # Purpose: Negative weighted average log-likelihood for updating one DIF entry g_{jk} for a non-anchor item.
  # Arguments:
  #   ab - Length-2 vector (a_j, b_j) used in the linear predictor.
  #   k - Group index whose DIF entry is being updated.
  #   gk - Candidate value for g_{jk}.
  #   gp - Full length-p DIF vector for item j; gp[k] overwritten by gk.
  #   x_k - Rows of x for respondents in group k.
  #   Y_k - Responses on item j for those respondents.
  #   weights - Full-sample length-N sampling weights (subset used via ind_k).
  #   z - Length-n quadrature nodes.
  #   ind_k - Row indices in the full sample for group k.
  #   mu_pres, sigma_pres - Current latent-trait mean and SD by group (length p).
  #   q - N-by-n posterior weights; rows ind_k are used.
  # Returns:
  #   Scalar objective for optim().
  N_k=dim(x_k)[1]
  p=dim(x_k)[2]
  n=length(z)
  a_j=ab[1]
  b_j=0
  gp[k]=gk
  g_j=gp
  
  q_k=q[ind_k, ]
  weight_k=weights[ind_k]
  
  #Evaluate log-likelihood
  Theta=tran(z, x_k, mu_pres, sigma_pres)
  temp=Theta*a_j+b_j*(rep(1,N_k)%*%t(rep(1, n)))+(x_k%*%g_j)%*%t(rep(1, n)) #N_k by n
  t2=Y_k%*%t(rep(1, n))*temp-log(1+exp(temp)) #The remaining term in log-likelihood
  log_likelihood=(weight_k%*%t(rep(1, n)))*t2 #N_k by n
  res=-sum(q_k*log_likelihood, na.rm = TRUE)
  return(res/N_k)
}



target_mean=function(para_k, k, x, weights, z, mu_pres, sigma_pres, q){
  # Purpose: Negative weighted average normal log-density for the latent trait, integrated over quadrature,
  #   as a function of group-k mean mu_k.
  # Arguments:
  #   para_k - Candidate value for mu_k.
  #   k - Group index being updated.
  #   x - N-by-p binary group matrix.
  #   weights - Length-N sampling weights.
  #   z - Length-n quadrature nodes.
  #   mu_pres, sigma_pres - Current latent-trait mean and SD by group (length p).
  #   q - N-by-n posterior weights from the E-step.
  # Returns:
  #   Scalar objective for optim().
  N=dim(x)[1]
  p=dim(x)[2]
  n=length(z)
  
  me=mu_pres
  me[k]=para_k
  
  #Evaluate log-likelihood
  Theta=tran(z, x, mu_pres, sigma_pres)
  mean.v=apply(x*(rep(1, N)%*%t(me)), 1, sum, na.rm = TRUE)
  var.v=apply(x*(rep(1, N)%*%t(sigma_pres^2)), 1, sum, na.rm = TRUE)
  sd.v=var.v^0.5
  t1=-0.5*((Theta-mean.v%*%t(rep(1,n)))/(sd.v%*%t(rep(1,n))))^2 #normal term in log-likelihood  #remove prior term
  log_likelihood=(weights%*%t(rep(1,n)))*t1 #N by n
  res=-sum(q*log_likelihood, na.rm = TRUE)
  return(res/N)
}

target_var=function(para_k, k, x, weights, z, mu_pres, sigma_pres, q){
  # Purpose: Negative weighted average log-likelihood for the latent-trait prior including variance normalization,
  #   as a function of group-k SD sigma_k.
  # Arguments:
  #   para_k - Candidate value for sigma_k (positive; optim bounds enforce).
  #   k - Group index being updated.
  #   x - N-by-p binary group matrix.
  #   weights - Length-N sampling weights.
  #   z - Length-n quadrature nodes.
  #   mu_pres, sigma_pres - Current latent-trait mean and SD by group.
  #   q - N-by-n posterior weights from the E-step.
  # Returns:
  #   Scalar objective for optim().
  N=dim(x)[1]
  p=dim(x)[2]
  n=length(z)
  
  sig=sigma_pres
  sig[k]=para_k
  
  #Evaluate log-likelihood
  Theta=tran(z, x, mu_pres, sigma_pres)
  mean.v=apply(x*(rep(1, N)%*%t(mu_pres)), 1, sum, na.rm = TRUE)
  var.v=apply(x*(rep(1, N)%*%t(sig^2)), 1, sum, na.rm = TRUE)
  sd.v=var.v^0.5
  t1=-0.5*((Theta-mean.v%*%t(rep(1,n)))/(sd.v%*%t(rep(1,n))))^2 #N by n
  t2=-0.5*log(2*pi*(var.v)) #N dim vector
  res=sum(t2*weights, na.rm = TRUE)+ sum(q*t1*(weights%*%t(rep(1,n))), na.rm = TRUE)
  return(-res/N)
}





grad_phi1=function(phi_j, x, Y_j, weights, z, mu_pres, sigma_pres, q){
  # Purpose: Gradient of target_function1 w.r.t. scalar difficulty b_j (anchor); sampling weights in the score.
  # Arguments:
  #   phi_j - Scalar difficulty b_j.
  #   x - N-by-p group indicators.
  #   Y_j - Length-N responses on item j.
  #   weights - Length-N sampling weights.
  #   z - Quadrature nodes.
  #   mu_pres, sigma_pres - Latent-trait mean and SD by group.
  #   q - Posterior weights matrix.
  # Returns:
  #   Scalar derivative term passed to optim() (implementation returns -db/N).
  N=dim(x)[1]
  p=dim(x)[2]
  n=length(z)
  b_j=phi_j
  g_j=rep(0, p)
  a_j=1
  
  #Evaluate gradient
  Theta=tran(z, x, mu_pres, sigma_pres)
  temp=Theta*a_j+b_j*(rep(1,N)%*%t(rep(1, n)))+(x%*%g_j)%*%t(rep(1, n)) #N by n
  prob=exp(temp)/(1+exp(temp))
  
  da=sum(Theta*(Y_j%*%t(rep(1, n))-prob)*q*(weights%*%t(rep(1,n))), na.rm = TRUE)
  db=sum((Y_j%*%t(rep(1, n))-prob)*q*(weights%*%t(rep(1,n))), na.rm = TRUE)
  return(-db/N)
}


grad_phi_ab=function(ab, g, x, Y_j, weights, z, mu_pres, sigma_pres, q){
  # Purpose: Gradient of target_function_ab w.r.t. scalar discrimination a_j (b_j fixed at 0 in predictor; weighted).
  # Arguments:
  #   ab - Scalar discrimination a_j.
  #   g - Length-p DIF vector.
  #   x - N-by-p group indicators.
  #   Y_j - Length-N responses on item j.
  #   weights - Length-N sampling weights.
  #   z - Quadrature nodes.
  #   mu_pres, sigma_pres - Latent-trait mean and SD by group.
  #   q - Posterior weights matrix.
  # Returns:
  #   Scalar derivative for optim().
  N=dim(x)[1]
  p=dim(x)[2]
  n=length(z)
  a_j=ab
  b_j=0
  g_j=g
  
  #Evaluate gradient
  Theta=tran(z, x, mu_pres, sigma_pres)
  temp=Theta*a_j+b_j*(rep(1,N)%*%t(rep(1, n)))+(x%*%g_j)%*%t(rep(1, n)) #N by n
  prob=exp(temp)/(1+exp(temp))
  
  da=sum(Theta*(Y_j%*%t(rep(1, n))-prob)*q*(weights%*%t(rep(1,n))), na.rm = TRUE)
  #db=sum((Y_j%*%t(rep(1, n))-prob)*q*(weights%*%t(rep(1,n))), na.rm = TRUE)
  return(-da/N)
}


grad_phi_g1=function(ab, k, gk, gp, x_k, Y_k, weights, z, ind_k,  mu_pres, sigma_pres, q){
  # Purpose: Gradient of target_function_g1 w.r.t. gk for the anchor-item DIF update (weighted).
  # Arguments:
  #   ab, k, gk, gp, x_k, Y_k, weights, z, ind_k, mu_pres, sigma_pres, q - Same roles as in target_function_g1.
  # Returns:
  #   Scalar partial derivative for optim().
  N_k=dim(x_k)[1]
  p=dim(x_k)[2]
  n=length(z)
  a_j=1
  b_j=0
  gp[k]=gk
  g_j=gp
  
  q_k=q[ind_k, ]
  weight_k=weights[ind_k]
  
  #Evaluate gradient
  Theta=tran(z, x_k, mu_pres, sigma_pres)
  temp=Theta*a_j+b_j*(rep(1,N_k)%*%t(rep(1, n)))+(x_k%*%g_j)%*%t(rep(1, n)) #N by n
  prob=exp(temp)/(1+exp(temp))
  dg=sum((Y_k%*%t(rep(1, n))-prob)*q_k*(weight_k%*%t(rep(1, n))), na.rm = TRUE)
  return(-dg/N_k)
}


grad_phi_g=function(ab, k, gk, gp, x_k, Y_k, weights, z, ind_k,  mu_pres, sigma_pres, q){
  # Purpose: Gradient of target_function_g w.r.t. scalar g_{jk} (group-k DIF on item j; weighted).
  # Arguments:
  #   ab, k, gk, gp, x_k, Y_k, weights, z, ind_k, mu_pres, sigma_pres, q - Same roles as in target_function_g.
  # Returns:
  #   Scalar partial derivative for optim().
  N_k=dim(x_k)[1]
  p=dim(x_k)[2]
  n=length(z)
  a_j=ab[1]
  b_j=0
  gp[k]=gk
  g_j=gp
  
  q_k=q[ind_k, ]
  weight_k=weights[ind_k]
  
  #Evaluate gradient
  Theta=tran(z, x_k, mu_pres, sigma_pres)
  temp=Theta*a_j+b_j*(rep(1,N_k)%*%t(rep(1, n)))+(x_k%*%g_j)%*%t(rep(1, n)) #N by n
  prob=exp(temp)/(1+exp(temp))
  dg=sum((Y_k%*%t(rep(1, n))-prob)*q_k*(weight_k%*%t(rep(1, n))), na.rm = TRUE)
  return(-dg/N_k)
}




grad_mean=function(para_k, k, x, weights, z, mu_pres, sigma_pres, q){
  # Purpose: Gradient of target_mean w.r.t. mu_k (weighted normal prior term; latent mean for group k).
  # Arguments:
  #   para_k - Candidate mu_k.
  #   k - Group index.
  #   x - N-by-p group matrix.
  #   weights - Length-N sampling weights.
  #   z - Quadrature nodes.
  #   mu_pres, sigma_pres - Current mean and SD vectors.
  #   q - Posterior weights matrix.
  # Returns:
  #   Scalar derivative for optim().
  N=dim(x)[1]
  p=dim(x)[2]
  n=length(z)
  
  me=mu_pres
  me[k]=para_k
  
  #Evaluate log-likelihood
  Theta=tran(z, x, mu_pres, sigma_pres)
  mean.v=apply(x*(rep(1, N)%*%t(me)), 1, sum, na.rm = TRUE)
  var.v=apply(x*(rep(1, N)%*%t(sigma_pres^2)), 1, sum, na.rm = TRUE)
  dmu_k=sum(((x[,k]/var.v)%*%t(rep(1,n)))*(Theta-mean.v%*%t(rep(1, n)))*q*(weights%*%t(rep(1,n))), na.rm = TRUE)
  return(-dmu_k/N)
}


grad_var=function(para_k, k, x, weights, z, mu_pres, sigma_pres, q){
  # Purpose: Gradient of target_var w.r.t. sigma_k (weighted; latent SD for group k).
  # Arguments:
  #   para_k - Candidate sigma_k.
  #   k - Group index.
  #   x - N-by-p group matrix.
  #   weights - Length-N sampling weights.
  #   z - Quadrature nodes.
  #   mu_pres, sigma_pres - Current mean and SD vectors.
  #   q - Posterior weights matrix.
  # Returns:
  #   Scalar derivative for optim().
  N=dim(x)[1]
  p=dim(x)[2]
  n=length(z)
  
  sig=sigma_pres
  sig[k]=para_k
  
  #Evaluate log-likelihood
  Theta=tran(z, x, mu_pres, sigma_pres)
  mean.v=apply(x*(rep(1, N)%*%t(mu_pres)), 1, sum, na.rm = TRUE)
  var.v=apply(x*(rep(1, N)%*%t(sig^2)), 1, sum, na.rm = TRUE)
  t1=sum(-weights*x[,k]/2/var.v, na.rm = TRUE)
  t2=sum(((x[,k]%*%t(rep(1,n)))*q*(weights%*%t(rep(1,n)))*(Theta-mean.v%*%t(rep(1,n)))^2)/2/((var.v%*%t(rep(1,n)))^2), na.rm = TRUE)
  dsig_k=t1+t2
  return(-dsig_k/N)
}


mml=function(a, b, g, x, dat, weights, z, mu, sigma, w){ #q is quadrature
  # Purpose: Minus weighted log marginal likelihood of responses under the 2PL+DIF model (Gaussian mixing over theta).
  # Arguments:
  #   a, b, g - Item parameters (length J, J, and J-by-p DIF matrix).
  #   x - N-by-p group matrix.
  #   dat - N-by-J item responses.
  #   weights - Length-N sampling weights.
  #   z, w - Quadrature nodes and weights.
  #   mu, sigma - Latent-trait mean and SD by group (length p).
  # Returns:
  #   Scalar negative weighted log marginal likelihood.
  N=dim(x)[1]
  p=dim(x)[2]
  n=length(z)
  J=dim(dat)[2]
  
  #Evaluate log-likelihood
  Theta=tran(z, x, mu, sigma) #N by n
  th=Theta[, 1]
  temp=(th%*%t(a))+rep(1, N)%*%t(b)+x%*%t(g) #N by J
  prob=exp(temp)/(1+exp(temp))
  res=apply((prob^dat)*((1-prob)^(1-dat)), 1, prod, na.rm=TRUE)*w[1] ## summation of log instead
  for (k in 2:n){
    th=Theta[, k]
    temp=(th%*%t(a))+rep(1, N)%*%t(b)+x%*%t(g) #N by J
    prob=exp(temp)/(1+exp(temp))
    res_r=apply((prob^dat)*((1-prob)^(1-dat)), 1, prod, na.rm=TRUE)*w[k]
    res=res+res_r
  }
  res=-sum((weights%*%t(rep(1, J)))*log(res*(pi)^(-0.5)))
  return(res)
}



EM_2PL_inference <- function(a, b, g, x, dat, weights, z, mu, sigma, w, ite, tol = 0.001){
  # Purpose: Weighted EM algorithm for a group-mixture 2PL model with DIF matrix g;
  #   alternates posterior quadrature (quad) and coordinate-wise M-step optimizations.
  # Arguments:
  #   a, b, g - Starting item discriminations, difficulties, and J-by-p DIF matrix.
  #   x - N-by-p group indicators.
  #   dat - N-by-J binary responses.
  #   weights - Length-N sampling weights.
  #   z, w - Global Gauss-Hermite nodes and weights.
  #   mu, sigma - Starting latent-trait mean and SD by group.
  #   ite - Inner iterations over item updates within each EM cycle.
  #   tol - Convergence tolerance on max absolute parameter change.
  # Returns:
  #   Named list: mu, sigma, alpha.vec, beta.vec, gamma.vec, post (normalized posterior weights).
  N=dim(x)[1]
  p=dim(x)[2]
  n=length(z)
  J=dim(dat)[2]
  phi0=matrix(0, J, 2+p)
  phi1=cbind(a, b, g)
  mu0=rep(0, p)
  mu1=mu
  sigma0=rep(0, p)
  sigma1=sigma
  ind_group=list()
  dat_group=list()
  x_group=list()
  for (k in 1:p){
    ind_c=which(x[,k]==1)
    ind_group[[k]]=ind_c
    dat_group[[k]]=dat[ind_c,]
    x_group[[k]]=x[ind_c,]
  }
  while(max(c(abs(phi0-phi1)), abs(mu0-mu1), abs(sigma0-sigma1))>tol){#max
    phi0=phi1
    mu0=mu1
    sigma0=sigma1
    
    #E-step
    quadrature=quad(a=phi0[,1], b=phi0[,2], g=phi0[ ,c(3:(2+p))], x, dat, z, mu0, sigma0, w)
    
    #M-step
    ###Add the sampling weights to optim Y_j
    phi1[1, 1]=0.4738117
    mu1 = rep(0, p)
    phi1[, 2] = rep(0, J)
    
    for(j in 1:J){#update phi_j one by one
      if(j==1){
        for (k in 1:p){
          par_updates_g = optim(par=phi1[j, 2+k], fn=target_function_g1, gr=grad_phi_g1, method = "L-BFGS-B", ab=phi1[j, c(1,2)], k=k, gp=phi1[j, 3:(2+p)], x_k=x_group[[k]], Y_k=dat_group[[k]][,j],ind_k=ind_group[[k]], weights=weights, z=z, mu_pres=mu1,  sigma_pres=sigma1, q=quadrature, lower=-5, upper=5)
          phi1[j, 2+k]=par_updates_g$par
        }
      }
      else{
        for (l in 1:ite){
          par_updates_ab = optim(par=phi1[j, 1], fn=target_function_ab, gr=grad_phi_ab, method = "L-BFGS-B", g=phi1[j, 3:(2+p)], x=x, Y_j=dat[,j], weights=weights, z=z, mu_pres=mu0,  sigma_pres=sigma0, q=quadrature, lower=-5, upper=5)
          phi1[j, 1]=par_updates_ab$par
          for (k in 1:p){
            par_updates_g = optim(par=phi1[j, 2+k], fn=target_function_g, gr=grad_phi_g, method = "L-BFGS-B", ab=phi1[j, c(1,2)], k=k, gp=phi1[j, 3:(2+p)], x_k=x_group[[k]], Y_k=dat_group[[k]][,j], weights=weights, ind_k=ind_group[[k]], z=z, mu_pres=mu0,  sigma_pres=sigma0, q=quadrature, lower=-5, upper=5)
            phi1[j, 2+k]=par_updates_g$par
          }
        }
      }
    }
    #To update mu one by one
    
    #To update sigma one by one
    for (k in 1:p){
      par_updates = optim(par=sigma0[k], fn=target_var, gr=grad_var, method = "L-BFGS-B", k=k,x=x, z=z, weights=weights, mu_pres=mu0,  sigma_pres=sigma0, q=quadrature, lower=0.1, upper=5)
      sigma1[k]=par_updates$par
    }
    
    
    MLL = mml(a=phi1[,1], b=phi1[,2], g=phi1[, c(3:(2+p))], x, dat, weights, z, mu=mu1, sigma=sigma1, w)
    print(MLL) 
  }
  list(mu=mu1, sigma=sigma1, alpha.vec=phi1[, 1], beta.vec=phi1[, 2], gamma.vec = phi1[, c(3:(p+2))], post=quadrature);
}


#Exe
set.seed(6)

#Read in data
dat=read.csv(file = 'PISA_2022_math_sas.csv',  sep = ",", header = T)
dat=dat[,-1]
colnames(dat)=NULL
dat=data.matrix(dat)

x=read.csv(file = 'countries_2022_math_sas.csv', sep = ",", header = T)
x=x[,-1]
colnames(x)=NULL
x=data.matrix(x)
weights=read.csv('PISA_2022_math_samplingweights_sas.csv', header = T)[,1]


N=dim(dat)[1]
J=dim(dat)[2]
p=dim(x)[2]


a_ini=read.csv('a_ini_math_2022.csv', header=F)[,1]
b_ini=rep(0, J)
g_ini=matrix(0, J, p)
mu_ini=rep(0, p)
sigma_ini=runif(p, 0.2, 1)

a=a_ini
b=b_ini
g=as.matrix(g_ini)
mu=mu_ini
sigma=sigma_ini
r=EM_2PL_inference(a=a, b=b, g=g, x=x, dat=dat, weights=weights, z, mu=mu, sigma=sigma, w, ite=1, tol = 0.05)

g_hat=r$gamma.vec
mu_hat=r$mu
sigma_hat=r$sigma
a_hat=r$alpha.vec
b_hat=r$beta.vec

write.table(g_hat, sep=",",  col.names=FALSE, row.names=FALSE, file = 'gamma_math_2022_d0.csv')
write.table(a_hat, sep=",",  col.names=FALSE, row.names=FALSE, file = 'alpha_math_2022_d0.csv')
write.table(b_hat, sep=",",  col.names=FALSE, row.names=FALSE, file = 'beta_math_2022_d0.csv')
write.table(mu_hat, sep=",",  col.names=FALSE, row.names=FALSE, file = 'mu_math_2022_d0.csv')
write.table(sigma_hat, sep=",",  col.names=FALSE, row.names=FALSE, file = 'sigma_math_2022_d0.csv')

