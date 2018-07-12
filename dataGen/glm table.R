rm(list=ls(all=TRUE))
setwd('U:\\independent studies\\interactive tools\\prodes')
dat=read.csv('dist_road_uc.csv',as.is=T)

#convert distance measures into km
hist(dat$dist_road)
dat$dist_road=dat$dist_road/1000
dat$dist_uc=dat$dist_uc/1000
range(dat$dist_road)
range(dat$dist_uc)

#model these patterns
res=glm(deforest~dist_road+dist_uc+dist_road*dist_uc,data=dat,family=binomial(link='logit'))
res1=summary(res)

setwd('U:\\independent studies\\interactive tools\\conservation tool')
write.csv(res1$coeff,'glm table.csv')
