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
cond.pa=grid1$tipo=='PA'
cond.ua=grid1$tipo=='Forest'

#get all combinations of road routes
roads=data.matrix(read.csv('combo coord.csv',as.is=T))
nroads=nrow(roads)

#create matrix to store results
res=matrix(NA,nroads,3)
colnames(res)=c('d.ua','d.pa','l.road') #this should match the output order of calc.stuff()

#determine the expected deforestation in PA and UA and road length
for (i in 1:nroads){
  print(i)
  tmp=calc.stuff(x=roads[i,c('x1','x2','x3')],
                 y=roads[i,c('y1','y2','y3')],
                 uc,coef2,grid1=grid1)  
  res[i,]=tmp
}
roads1=cbind(roads,res)
write.csv(roads1,'optimized tabulate results.csv',row.names=F)
