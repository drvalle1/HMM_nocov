rm(list=ls())
set.seed(10)
library('gtools')
library('Rcpp')

#import functions
setwd('U:\\GIT_models\\HMM_nocov')
source('HMM Bayes gibbs functions.R')
source('HMM Bayes gibbs.R')
sourceCpp('aux1.cpp')

#import data
y1=read.csv('sim data obs1.csv')$x
sort(unique(y1))
y2=read.csv('sim data obs2.csv')$x
sort(unique(y2))

#get true values
# ztrue=read.csv('sim data states.csv')$x
# prob.obs.true1=data.matrix(read.csv('sim data prob obs1.csv'))
# prob.obs.true2=data.matrix(read.csv('sim data prob obs2.csv'))
# Gamma.true=data.matrix(read.csv('sim data trans matrix.csv'))

#settings
nclust=3
nsim=10000

#run Gibbs sampler
mod=HMM.Bayes.gibbs(nclust=nclust,y1=y1,y2=y2,nsim=nsim)

#-------------------------------------------
#check convergence
nsim=length(mod$llk.prior)
plot(mod$llk.prior,type='l',
     ylim=c(quantile(mod$llk,0.01),max(mod$llk)))
abline(h=mod$llk.prior[mod$iter.MAP],col='red')
par(mfrow=c(3,3),mar=rep(1,4))
for (i in 1:9) plot(mod$Gamma[,i],type='l')

#compare estimated to true
par(mfrow=c(1,1),mar=rep(3,4))
z.estim=mod$z.MAP
tmp=data.frame(true1=ztrue,estim1=z.estim)
table(tmp)
ordem=c(1,2,3)

prob.obs1.estim=mod$prob.obs1.MAP
prob.obs2.estim=mod$prob.obs2.MAP
compare.plot(true1=prob.obs.true1,estim1=prob.obs1.estim[ordem,])
compare.plot(true1=prob.obs.true2,estim1=prob.obs2.estim[ordem,])

tmp0=mod$Gamma.MAP
tmp=tmp0[ordem,]
tmp1=tmp[,ordem]
compare.plot(true1=Gamma.true,estim1=tmp1)
