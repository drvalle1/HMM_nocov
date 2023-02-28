time1=seq(from=0,to=24,length.out=1000)
bcos=0
bsin=1

res=bcos*cos(2*pi*(time1/24))+bsin*sin(2*pi*(time1/24))

res.sin=sin(2*pi*(time1/24))
res.cos=cos(2*pi*(time1/24))
plot(time1,res.sin,type='l')
lines(time1,res.cos,col='red')

#patterns
plot(NA,NA,xlim=c(0,24),ylim=c(-20,20))
seq1=seq(from=-10,to=10,length.out=5)
combo=expand.grid(bcos=seq1,bsin=seq1)
for (i in 1:nrow(combo)){
  res=combo$bcos[i]*cos(2*pi*(time1/24))+combo$bsin[i]*sin(2*pi*(time1/24))  
  lines(time1,res,col=i)
}

#find minima and maxima of each plot
plot(NA,NA,xlim=c(0,24),ylim=c(-20,20))
seq1=seq(from=-10,to=10,length.out=10)
combo=expand.grid(bcos=seq1,bsin=seq1)
for (i in 1:nrow(combo)){
  res=combo$bcos[i]*cos(2*pi*(time1/24))+combo$bsin[i]*sin(2*pi*(time1/24))  
  ind=which(res==max(res))
  points(time1[ind[1]],max(res))
  ind=which(res==min(res))
  points(time1[ind[1]],min(res),col='red')
}
