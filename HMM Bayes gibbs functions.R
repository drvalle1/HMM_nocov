sample.prob.obs=function(param,y,nclust,ncat){
  prob.obs=matrix(NA,nclust,ncat)
  for (i in 1:nclust){
    ysel=y[param$z==i]
    tmp=table(ysel)
    tab1=rep(0,ncat)
    tab1[as.numeric(names(tmp))]=tmp
    prob.obs[i,]=rdirichlet(1,tab1+0.1)
  }
  prob.obs
}
#---------------------------
sample.Gamma=function(param,nobs,nclust){
  Gamma=matrix(NA,nclust,nclust)
  for (i in 1:nclust){
    #tabulate data
    ind=which(param$z[-nobs]==i)
    tmp=table(param$z[ind+1])
    tmp=tmp[names(tmp)!=-99]
    tab1=rep(0,nclust)
    tab1[as.numeric(names(tmp))]=tmp
    Gamma[i,]=rdirichlet(1,tab1+0.1)
  }
  Gamma
}
#---------------------------------
compare.plot=function(true1,estim1){
  rango=range(c(true1,estim1))
  plot(true1,estim1,ylim=rango,xlim=rango)
  lines(rango,rango,col='red')
}
#---------------------------------
calc.llk.prior=function(param,y1,y2,nclust,nobs){
  #get log-likelihood
  llk=0
  for (i in 1:nclust){
    tmp=log(param$prob.obs1[i,])
    y1.sel=y1[param$z==i]
    llk1=sum(tmp[y1.sel])
    
    tmp=log(param$prob.obs2[i,])
    y2.sel=y2[param$z==i]
    llk2=sum(tmp[y2.sel])
    llk=llk+llk1+llk2    
  }

  #get transition probs
  trans.prior=0
  for (i in 1:nclust){
    #tabulate data
    ind=which(param$z[-nobs]==i)
    tmp=table(param$z[ind+1])
    tmp=tmp[names(tmp)!=-99]
    tab1=rep(0,nclust)
    tab1[as.numeric(names(tmp))]=tmp
    tmp=sum(tab1*log(param$Gamma[i,]))
    trans.prior=trans.prior+tmp
  }
  
  #get prior probs
  prior.prob=(0.1-1)*sum(log(param$prob.obs1))+
             (0.1-1)*sum(log(param$prob.obs2))+
             (0.1-1)*sum(log(param$Gamma))

  #return final results
  llk+trans.prior+prior.prob
}