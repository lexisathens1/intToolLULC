#-------------------
#get nearest distance
get.dist=function(user.coords,grid1){
  dist.mat=matrix(NA,nrow(user.coords)-1,nrow(grid1))
  ngrid=nrow(grid1)
  for (i in 2:nrow(user.coords)){
    y0=user.coords$y[i]
    y1=user.coords$y[i-1]
    x0=user.coords$x[i]
    x1=user.coords$x[i-1]
    delta.x=(x0-x1)
    delta.y=(y0-y1)
    
    #diagonal line
    if (delta.x!=0 & delta.y!=0){
      #key parameters
      b=delta.y/delta.x
      a=y0-b*x0
      s=-1/b
      
      #closest point in line
      xs=((grid1$y-s*grid1$x)-a)/(b-s)
      ys=a+b*xs
      
      #check if within segment
      cond=xs>x0 & xs<x1
      if (delta.x>0) cond=xs<x0 & xs>x1
    }
    
    #vertical line
    if (delta.x==0){ 
      xs=rep(x0,ngrid)
      ys=grid1$y
      
      #check if within segment
      cond=ys>y0 & ys<y1
      if (delta.y>0) cond=ys<y0 & ys>y1
    }
    
    #horizontal line
    if (delta.y==0){ 
      xs=grid1$x
      ys=rep(y0,ngrid)
      
      #check if within segment
      cond=xs>x0 & xs<x1
      if (delta.x>0) cond=xs<x0 & xs>x1
    }
    
    #if within segment
    xsq=(grid1$x[cond]-xs[cond])^2
    ysq=(grid1$y[cond]-ys[cond])^2
    dist.mat[i-1,cond]=sqrt(xsq+ysq)
    
    #if outside segment
    xsq0=(grid1$x[!cond]-x0)^2
    ysq0=(grid1$y[!cond]-y0)^2
    dist0=sqrt(xsq0+ysq0)
    xsq1=(grid1$x[!cond]-x1)^2
    ysq1=(grid1$y[!cond]-y1)^2
    dist1=sqrt(xsq1+ysq1)
    dist.fim=apply(cbind(dist0,dist1),1,min)
    dist.mat[i-1,!cond]=dist.fim
  }
  
  apply(dist.mat,2,min)
}
#-----------------------
#get length
get.length=function(user.coords){
  length1=rep(NA,nrow(user.coords)-1)
  for (i in 2:nrow(user.coords)){
    ydif=user.coords$y[i]-user.coords$y[i-1]
    xdif=user.coords$x[i]-user.coords$x[i-1]
    length1[i-1]=sqrt((ydif^2)+(xdif^2))
  }
  sum(length1)  
}
#-----------------------
#calculate expected cost
get.cost=function(road.cost,pa.cost,forest.cost,grid1,length2){
  cond=grid1$tipo=='PA'
  pa.cost1=sum(pa.cost*grid1$prob[cond])
  pa.def=sum(grid1$prob[cond])/sum(cond)
  road.cost1=length2*road.cost
  cond=grid1$tipo=='Forest'
  forest.cost1=sum(forest.cost*grid1$prob[cond])
  forest.def=sum(grid1$prob[cond])/sum(cond)
  L <- list(road.cost=road.cost1, pa.cost=pa.cost1, forest.cost=forest.cost1,
            total.cost=pa.cost1+forest.cost1+road.cost1,
            pa.def=pa.def, forest.def=forest.def)
  return(L)
}
#-----------------------
#get road cost
get.road.cost=function(x,y,costs,grid1){
  # user.coords=unique(data.frame(x=c(20,x,20),y=c(10,y,90)))
  user.coords=unique(data.frame(x=c(10,x,90),y=c(10,y,90))) # For round plots
  
  #get nearest distance
  grid1$dist=get.dist(user.coords,grid1)
  
  #predict deforestation
  tmp=exp(coef1[1]+coef1[2]*grid1$dist)
  grid1$prob=tmp/(1+tmp)
  
  #get length of road
  length2=get.length(user.coords)
  
  #calculate expected cost
  ecost=rep(NA,ncost)
  for (i in 1:ncost){
    ecost[i]=get.cost(costs$road.cost[i],costs$pa.cost[i],costs$forest.cost[i],grid1,length2)$total.cost  
  }
  ecost
}