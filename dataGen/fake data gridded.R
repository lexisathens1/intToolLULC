rm(list=ls(all=TRUE))
library(ggplot2)

combo=expand.grid(x=1:100,y=1:100)
combo$tipo=NA

#PA area
cond=combo$x<40 & combo$y>30 & combo$y<80
sum(cond)
combo$tipo[cond]='PA'

#forested area
b0=-10; b1=1
cond=combo$x>= 40 & (combo$y<80 & combo$y>(b0+b1*combo$x))
sum(cond)
combo$tipo[cond]='Forest'

#pasture
cond=is.na(combo$tipo)
sum(cond)
combo$tipo[cond]='Pasture'

unique(combo$tipo)

#start and end points
coord=data.frame(x=rep(10,2),y=c(10,90))

#plot LULC
res=ggplot() + 
    geom_tile(data = combo, alpha = 0.8,aes(x = x, y = y,fill = tipo)) +
    geom_point(data = coord, aes(x = x,y=y,size=3),show.legend=F) +
    scale_fill_manual(values=c('green','darkgreen','grey'))
res

setwd('U:\\independent studies\\interactive tools\\conservation tool')
write.csv(combo,'fake data gridded.csv',row.names=F)