rm(list=ls(all=TRUE))
# setwd('U:\\independent studies\\interactive tools\\conservation tool')

x=seq(from=10,to=100,by=10)
y=seq(from=10,to=100,by=10)
combo=expand.grid(x1=x,x2=x,x3=x,y1=y,y2=y,y3=y)
dim(combo)

#always going up
cond=combo$y2 >= combo$y1 & combo$y3 >= combo$y2
combo1=combo[cond,]
dim(combo1)

write.csv(combo1,'dataGen/comboCoord3.csv',row.names=F)

