library(doSNOW)
library(doParallel)
library(tcltk)
cl <- makeSOCKcluster(5)
registerDoSNOW(cl)

#I use this script to optimize road layout through brute force calculations
#This script is implemented with parallel
rm(list=ls(all=TRUE))
set.seed(1)

# setwd('U:\\independent studies\\interactive tools\\conservation tool')
source('dataGen/optimized functions.R')
grid1=read.csv('dataGen/fakeLayout2.csv',as.is=T)
coef1=read.csv('data/glm table.csv',as.is=T)
coef2=coef1[,'Estimate']
names(coef2)=coef1$X

#get distance to urban centers
uc=data.frame(x=c(10,90),y=c(10,90))
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
roads=data.matrix(read.csv('dataGen/comboCoord2.csv',as.is=T))
nroads=nrow(roads)

#progress bar
pb <- tkProgressBar(max = nroads)
progress <- function(n) setTkProgressBar(pb, n)
opts <- list(progress=progress)

#determine the expected deforestation in PA and UA and road length
start.end=data.frame(x=c(10,90), y=c(10,90))
fimDf <- foreach (i=1:nroads, .combine = rbind, .options.snow=opts) %dopar% {
  tmp <- calc.stuff(x=roads[i,c('x1','x2','x3')],
                 y=roads[i,c('y1','y2','y3')],
                 uc, coef2, grid1=grid1, start.end=start.end)
  df <- data.frame(id = i, d.ua = tmp[1], d.pa = tmp[2], l.road = tmp[3])
}

roads1 <- cbind(id=1:nroads, roads)
roads1 <- dplyr::left_join(as.data.frame(roads1), fimDf)
head(roads1)
write.csv(roads1,'dataGen/optimized2.csv',row.names=F)
