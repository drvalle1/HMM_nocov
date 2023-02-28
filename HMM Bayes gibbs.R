HMM.Bayes.gibbs=function(nclust,y1,y2,nsim){
  nobs=length(y1)
  ncat1=length(unique(y1[!is.na(y1)]))
  ncat2=length(unique(y2[!is.na(y2)]))
  
  #initial param values
  Gamma=matrix(0.01,nclust,nclust)
  diag(Gamma)=1
  Gamma=Gamma/apply(Gamma,1,sum)
  
  z=sample(1:nclust,size=nobs,replace=T)
  ind=which(is.na(y1))
  z[ind]=-99
  
  prob.obs1=matrix(0.1,nclust,ncat1)
  prob.obs1[1,1]=prob.obs1[2,2]=prob.obs1[3,ncat1]=1
  prob.obs1=prob.obs1/apply(prob.obs1,1,sum)
  
  prob.obs2=matrix(0.1,nclust,ncat2)
  prob.obs2[1,1]=prob.obs2[2,2]=prob.obs2[3,ncat2]=1
  prob.obs2=prob.obs2/apply(prob.obs2,1,sum)

  #matrices to store info
  vec.prob.obs1=matrix(NA,nsim,nclust*ncat1)
  vec.prob.obs2=matrix(NA,nsim,nclust*ncat2)
  vec.z=matrix(NA,nsim,nobs)
  vec.Gamma=matrix(NA,nsim,nclust*nclust)
  vec.llk.prior=rep(NA,nsim)
  
  #start Gibbs sampler
  param=list(prob.obs1=prob.obs1,
             prob.obs2=prob.obs2,
             z=z,Gamma=Gamma)
  
  options(warn=2)
  llk.prior=-Inf
  for (i in 1:nsim){
    print(i)
    
    param$prob.obs1=sample.prob.obs(param=param,y=y1,
                                    nclust=nclust,ncat=ncat1)
    # param$prob.obs1=prob.obs.true1
    
    param$prob.obs2=sample.prob.obs(param=param,y=y2,
                                    nclust=nclust,ncat=ncat2)
    # param$prob.obs2=prob.obs.true2
    
    param$z=samplez(Gamma=param$Gamma, 
                    ProbObs1=param$prob.obs1, 
                    ProbObs2=param$prob.obs2, 
                    y1=y1-1, y2=y2-1,z=param$z-1,
                    nobs=nobs, nclust=nclust)+1
    # param$z=ztrue
    
    param$Gamma=sample.Gamma(param=param,nobs=nobs,nclust=nclust)
    # param$Gamma=Gamma.true
    
    #store results
    vec.prob.obs1[i,]=param$prob.obs1
    vec.prob.obs2[i,]=param$prob.obs2
    vec.z[i,]=param$z
    vec.Gamma[i,]=param$Gamma
    vec.llk.prior[i]=calc.llk.prior(param=param,
                                    y1=y1,y2=y2,
                                    nclust=nclust,
                                    nobs=nobs)
    
    #store MAP
    if (vec.llk.prior[i]>llk.prior){
      llk.prior=vec.llk.prior[i]
      z.MAP=param$z
      Gamma.MAP=param$Gamma
      prob.obs1.MAP=param$prob.obs1
      prob.obs2.MAP=param$prob.obs2
      iter.MAP=i
    }
  }
  list(prob.obs1=vec.prob.obs1,
       prob.obs2=vec.prob.obs2,
       z=vec.z,
       Gamma=vec.Gamma,
       llk.prior=vec.llk.prior,
       z.MAP=z.MAP,
       Gamma.MAP=Gamma.MAP,
       prob.obs1.MAP=prob.obs1.MAP,
       prob.obs2.MAP=prob.obs2.MAP,
       iter.MAP=iter.MAP)
}