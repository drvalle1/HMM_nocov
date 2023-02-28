rm(list=ls())
set.seed(23)

#observation parameters
nstates=3; ncat1=10; ncat2=7
probs.obs1=matrix(NA,nstates,ncat1)
probs.obs2=matrix(NA,nstates,ncat2)
medias1=c(1,2,8)
medias2=c(7,1,2)
for (i in 1:nstates){
  tmp=dnorm(0:(ncat1-1),mean=medias1[i],sd=1)  
  probs.obs1[i,]=tmp/sum(tmp)
  tmp=dnorm(0:(ncat2-1),mean=medias2[i],sd=1)  
  probs.obs2[i,]=tmp/sum(tmp)
}

#plot patterns
plot(NA,NA,xlim=c(0,ncat1),ylim=c(0,1))
for (i in 1:nstates){
  points(0:(ncat1-1),probs.obs1[i,],col=i)
}
plot(NA,NA,xlim=c(0,ncat2),ylim=c(0,1))
for (i in 1:nstates){
  points(0:(ncat2-1),probs.obs2[i,],col=i)
}

#transition probabilities
Gamma=matrix(runif(nstates*nstates,max=1),nstates,nstates)
diag(Gamma)=c(10,20,30)
Gamma=Gamma/apply(Gamma,1,sum); Gamma
apply(Gamma,1,sum)

#simulate states
nobs=10000
states=rep(NA,nobs)
states[1]=1
for (i in 2:nobs){
  tmp=rmultinom(1,size=1,prob=Gamma[states[i-1],])
  states[i]=which(tmp==1)
}
# plot(states)

#simulate observations
y1=y2=rep(NA,nobs)
for (i in 1:nobs){
  tmp=rmultinom(1,size=1,prob=probs.obs1[states[i],])
  y1[i]=which(tmp==1)
  tmp=rmultinom(1,size=1,prob=probs.obs2[states[i],])
  y2[i]=which(tmp==1)
}

#visualize the generated patterns
plot(1:nobs,y1,type='l',ylim=c(0,10))
lines(1:nobs,y2,col='red')

plot(1:nobs,y1,type='l',ylim=c(0,10))
lines(1:nobs,medias1[states],col='red')
plot(1:nobs,y2,type='l',ylim=c(0,10))
lines(1:nobs,medias2[states],col='red')

#plug in NAs
ind=100:200
y1[ind]=y2[ind]=NA
ind=500:510
y1[ind]=y2[ind]=NA

#export results
setwd('U:\\GIT_models\\HMM_nocov')
write.csv(y1,'sim data obs1.csv',row.names=F)
write.csv(y2,'sim data obs2.csv',row.names=F)
write.csv(states,'sim data states.csv',row.names=F)
write.csv(Gamma,'sim data trans matrix.csv',row.names=F)
write.csv(probs.obs1,'sim data prob obs1.csv',row.names=F)
write.csv(probs.obs2,'sim data prob obs2.csv',row.names=F)