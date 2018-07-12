#I use this script to optimize road layout through brute force calculations
rm(list=ls(all=TRUE))
set.seed(1)

setwd('U:\\independent studies\\interactive tools\\conservation tool')
source('optimized functions.R')
grid1=read.csv('fake data gridded.csv',as.is=T)
coef1=read.csv('glm table.csv',as.is=T)
coef2=coef1[,'Estimate']
names(coef2)=coef1$X

#get distance to urban centers
uc=data.frame(x=c(20,20),y=c(20,90))
dist=numeric()
for (i in 1:nrow(uc)){
  x2=(grid1$x-uc$x[i])^2
  y2=(grid1$y-uc$y[i])^2
  dist=cbind(dist,sqrt(x2+y2))
}
grid1$dist_uc=apply(dist,1,min)

#get all combinations of road routes
roads=data.matrix(read.csv('combo coord.csv',as.is=T))
nroads=nrow(roads)

#get all combinations of costs
seq1=0:5
costs=expand.grid(road.cost=seq1,pa.cost=seq1,forest.cost=seq1)
ncost=nrow(costs)

#determine the expected cost for each (cost setup) x (road layout) combination
fim=matrix(NA,ncost*nroads,3)
oo=1
for (i in 1:nroads){
  print(i)
  
  ecost=get.road.cost(x=roads[i,c('x1','x2','x3')],
                      y=roads[i,c('y1','y2','y3')],
                      uc,coef2,
                      costs=costs,grid1=grid1)  
  seq1=oo:(oo+ncost-1)
  fim[seq1,]=cbind(ecost,i,1:ncost)
  oo=oo+ncost
}
fim1=data.frame(ecost=fim[,1],
                road.layout=fim[,2],
                cost.setup=fim[,3])

#select the best road layout for each cost setup
fim2=matrix(NA,ncost,3)
for (i in 1:ncost){
  print(i)
  cond=fim1$cost.setup==i
  fim.tmp=fim1[cond,]
  ind=which(fim.tmp$ecost==min(fim.tmp$ecost))
  if (length(ind)>1) ind=sample(ind,size=1) #if more than one, randomly pick one
  fim2[i,]=unlist(fim.tmp[ind,])
}

colnames(fim2)=colnames(fim1)

#to avoid mismatches, include the original cost setup
fim3=as.data.frame(fim2)
fim3$pa.cost=costs$pa.cost
fim3$road.cost=costs$road.cost
fim3$forest.cost=costs$forest.cost

#to avoid mismatches include original x,y coordinates
fim3$x1=roads[fim3$road.layout,'x1']
fim3$x2=roads[fim3$road.layout,'x2']
fim3$x3=roads[fim3$road.layout,'x3']
fim3$y1=roads[fim3$road.layout,'y1']
fim3$y2=roads[fim3$road.layout,'y2']
fim3$y3=roads[fim3$road.layout,'y3']
write.csv(fim3,'optimized.csv',row.names=F)
