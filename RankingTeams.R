rm(list=ls())
load("A:/College/2017 Fall/ECON 524/final.rdata")
ranking=function(year){
  season=data.frame(Name=character(),Season=numeric(),Numwins=numeric(),Numlosses=numeric(),Opponents=list())
  for(i in 1:length(final$Name)){
    if(final$Season[i]==year){
      season=rbind(season,final[i,])
    }
  }
  l=length(season$Name)
  season=cbind(1:l,season)
  colley=matrix(nrow=l,ncol=l)
  for(i in 1:l){
    for(j in 1:l){
      if(i==j){
        colley[i,j]=season$Numwins[i]+season$Numlosses[i]+2
      }
      else{
        colley[i,j]=-1*(length(which(season$Opponents[[i]]==j)))
      }
    }
  }
  x=numeric(l)
  for(i in 1:length(x)){
    x[i]=1+(season$Numwins[i]-season$Numlosses[i])/2
  }
  y=solve(colley,x)
  ranks=data.frame(season$Name,y)
  decreasing=order(ranks$y,decreasing=TRUE)
  solution=ranks[decreasing,]
  return(solution)
}
