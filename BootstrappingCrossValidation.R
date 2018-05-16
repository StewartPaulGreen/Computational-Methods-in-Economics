rm(list=ls())
library(numDeriv)
nd <- read.csv("A:/College/2017 Fall/ECON 524/pddata.csv")

ll1 <- function(b){
  choicea = 1-nd$choice
  choiceb = nd$choice
  aa=numeric(length(nd$id))
  ab=numeric(length(nd$id))
  for (i in 1:length(nd$id)){
    if(nd$round[i]==1){
      aa[i] = 0
      ab[i] = 0
    } else{
      aa[i] = aa[i-1]+choicea[i-1]*nd$epayoff[i-1]
      ab[i] = ab[i-1]+choiceb[i-1]*nd$epayoff[i-1]
    }
    
  }
  r =  b[1]* (choicea * aa) + b[1]*(choiceb * ab) - log(exp(b[1]*aa)+exp(b[1]*ab))
  return(-sum(r))
}


b <- numeric(1)
b[1]<- 10
(result <- nlminb(b,ll1,lower=c(0),upper=c(Inf)))
resultp1 <- result$par
serror1=sqrt(diag(solve(hessian(ll1,resultp1))))

ll2 <- function(b){
  choicea = 1-nd$choice
  choiceb = nd$choice
  aa=numeric(length(nd$id))
  ab=numeric(length(nd$id))
  for (i in 1:length(nd$id)){
    if(nd$round[i]==1){
      aa[i] = 0
      ab[i] = 0
    } else{
      aa[i] = aa[i-1]+choicea[i-1]*nd$payoff[i-1]
      ab[i] = ab[i-1]+choiceb[i-1]*nd$payoff[i-1]
    }
    
  }
  r =  b[1]* (choicea * aa) + b[1]*(choiceb * ab) - log(exp(b[1]*aa)+exp(b[1]*ab))
  return(-sum(r))
}

b=numeric(1)
b[1]<- 10
(result <- nlminb(b,ll2,lower=c(0),upper=c(Inf)))
resultp2 <- result$par
serror2=sqrt(diag(solve(hessian(ll2,resultp2))))

ll3 <- function(b){
  choicea = 1-nd$choice
  choiceb = nd$choice
  aa=numeric(length(nd$id))
  ab=numeric(length(nd$id))
  for (i in 1:length(nd$id)){
    if(nd$round[i]==1){
      aa[i] = 0
      ab[i] = 0
    } else{
      aa[i] = b[2]*aa[i-1]+choicea[i-1]*nd$epayoff[i-1]
      ab[i] = b[2]*ab[i-1]+choiceb[i-1]*nd$epayoff[i-1]
    }
    
  }
  r =  b[1]* (choicea * aa) + b[1]*(choiceb * ab) - log(exp(b[1]*aa)+exp(b[1]*ab))
  return(-sum(r))
}

b <- numeric(2)
b[2]<- 0.5
b[1]<- 10
(result <- nlminb(b,ll3,hessian=TRUE,lower=c(0,0),upper=c(Inf,1)))
resultp3 <- result$par
serror3=sqrt(diag(solve(hessian(ll3,resultp3))))

ll4 <- function(b){
  choicea = 1-nd$choice
  choiceb = nd$choice
  aa=numeric(length(nd$id))
  ab=numeric(length(nd$id))
  for (i in 1:length(nd$id)){
    if(nd$round[i]==1){
      aa[i] = 0
      ab[i] = 0
    } else{
      aa[i] = b[2]*aa[i-1]+choicea[i-1]*nd$pa[i-1]
      ab[i] = b[2]*ab[i-1]+choiceb[i-1]*nd$pb[i-1]
    }
    
  }
  r =  b[1]* (choicea * aa) + b[1]*(choiceb * ab) - log(exp(b[1]*aa)+exp(b[1]*ab))
  return(-sum(r))
}

b <- numeric(2)
b[2]<- 0.5
b[1]<- 10
(result <- nlminb(b,ll4,hessian=TRUE,lower=c(0,0),upper=c(Inf,1)))
resultp4 <- result$par
serror4=sqrt(diag(solve(hessian(ll4,resultp4))))

ll5 <- function(b){
  choicea = 1-nd$choice
  choiceb = nd$choice
  aa=numeric(length(nd$id))
  ab=numeric(length(nd$id))
  for (i in 1:length(nd$id)){
    if(nd$round[i]==1){
      aa[i] = 0
      ab[i] = 0
    } else{
      aa[i] = b[2]*aa[i-1]+b[3]*choicea[i-1]*nd$epayoff[i-1]+(1-b[3])*choicea[i-1]*nd$pa[i-1]
      ab[i] = b[2]*ab[i-1]+b[3]*choiceb[i-1]*nd$epayoff[i-1]+(1-b[3])*choiceb[i-1]*nd$pb[i-1]
    }
    
  }
  r =  b[1]* (choicea * aa) + b[1]*(choiceb * ab) - log(exp(b[1]*aa)+exp(b[1]*ab))
  return(-sum(r))
}

b <- numeric(3)
b[3]=0.5
b[2]<- 0.5
b[1]<- 10
(result <- nlminb(b,ll5,hessian=TRUE,lower=c(0,0,0),upper=c(Inf,1,1)))
resultp5 <- result$par
serror5=sqrt(diag(solve(hessian(ll5,resultp5))))

ll1value=-ll1(resultp1)
ll2value=-ll2(resultp2)
ll3value=-ll3(resultp3)
ll4value=-ll4(resultp4)
ll5value=-ll5(resultp5)

AIC1=-2*ll1value+(2*length(resultp1))
AIC2=-2*ll2value+(2*length(resultp2))
AIC3=-2*ll3value+(2*length(resultp3))
AIC4=-2*ll4value+(2*length(resultp4))
AIC5=-2*ll5value+(2*length(resultp5))
AIC=c(AIC1,AIC2,AIC3,AIC4,AIC5)
AICbest=which(AIC==min(AIC))

n=nrow(nd)

BIC1=-2*ll1value+(length(resultp1)*log(n))
BIC2=-2*ll2value+(length(resultp2)*log(n))
BIC3=-2*ll3value+(length(resultp3)*log(n))
BIC4=-2*ll4value+(length(resultp4)*log(n))
BIC5=-2*ll5value+(length(resultp5)*log(n))
BIC=c(BIC1,BIC2,BIC3,BIC4,BIC5)
BICbest=which(BIC==min(BIC))

lr13=(1-pchisq(2*(ll3value-ll1value),1))
lr15=(1-pchisq(2*(ll5value-ll1value),2))
lr24=(1-pchisq(2*(ll4value-ll2value),1))
lr25=(1-pchisq(2*(ll5value-ll2value),2))
lr35=(1-pchisq(2*(ll5value-ll3value),1))
lr45=(1-pchisq(2*(ll5value-ll4value),1))
lrtest=c(lr13,lr15,lr24,lr25,lr35,lr45)
lrbest=which(lrtest==min(lrtest))
#ll5 model is best all lr tests are significant



k=5
id=length(unique(nd$id))
set.seed(100)
folds=sample(rep(1:k,each=4))
MSE1k=numeric(5)
for(i in 1:k){
  trainassign=which(folds!=i)
  testassign=which(folds==i)
  train=numeric(0)
  test=numeric(0)
  for(j in trainassign){
    train=c(train,which(nd$id==j))
  }
  for(j in testassign){
    test=c(test,which(nd$id==j))
  }
  ll1train=function(b){
    choicea = 1-nd$choice[train]
    choiceb = nd$choice[train]
    aa=numeric(length(train))
    ab=numeric(length(train))
    for (i in 1:length(train)){
      if(nd$round[train][i]==1){
        aa[i] = 0
        ab[i] = 0
      } else{
        aa[i] = aa[i-1]+choicea[i-1]*nd$epayoff[i-1]
        ab[i] = ab[i-1]+choiceb[i-1]*nd$epayoff[i-1]
      }
    }
    r =  b[1]* (choicea * aa) + b[1]*(choiceb * ab) - log(exp(b[1]*aa)+exp(b[1]*ab))
    return(-sum(r))
  }
  result1k=nlminb(c(0),ll1train,lower=c(0),upper=c(Inf))
  ll1MSE=function(b){
    choicea=1-nd$choice[test]
    choiceb=nd$choice[test]
    aa=numeric(length(test))
    ab=numeric(length(test))
    for(i in 1:length(test)){
      if(nd$round[test][i]==1){
        aa[i]=0
        ab[i]=0
      }
      else{
        aa[i]=aa[i-1]+choicea[i-1]*nd$epayoff[i-1]
        ab[i]=ab[i-1]+choiceb[i-1]*nd$epayoff[i-1]
      }
    }
    prob=exp(b[1]*ab)/(exp(b[1]*aa)+exp(b[1]*ab))
    MSE=mean((nd$choice[test]-prob)^2)
    return(MSE)
  }
  MSE1k[i]=ll1MSE(result1k$par)
}
MSE1k=mean(MSE1k)

set.seed(100)
folds=sample(rep(1:k,each=4))
MSE2k=numeric(5)
for(i in 1:k){
  trainassign=which(folds!=i)
  testassign=which(folds==i)
  train=numeric(0)
  test=numeric(0)
  for(j in trainassign){
    train=c(train,which(nd$id==j))
  }
  for(j in testassign){
    test=c(test,which(nd$id==j))
  }
  ll2train=function(b){
    choicea = 1-nd$choice[train]
    choiceb = nd$choice[train]
    aa=numeric(length(train))
    ab=numeric(length(train))
    for (i in 1:length(train)){
      if(nd$round[train][i]==1){
        aa[i] = 0
        ab[i] = 0
      } else{
        aa[i] = aa[i-1]+choicea[i-1]*nd$payoff[i-1]
        ab[i] = ab[i-1]+choiceb[i-1]*nd$payoff[i-1]
      }
    }
    r =  b[1]* (choicea * aa) + b[1]*(choiceb * ab) - log(exp(b[1]*aa)+exp(b[1]*ab))
    return(-sum(r))
  }
  result2k=nlminb(c(0),ll2train,lower=c(0),upper=c(Inf))
  ll2MSE=function(b){
    choicea=1-nd$choice[test]
    choiceb=nd$choice[test]
    aa=numeric(length(test))
    ab=numeric(length(test))
    for(i in 1:length(test)){
      if(nd$round[test][i]==1){
        aa[i]=0
        ab[i]=0
      }
      else{
        aa[i]=aa[i-1]+choicea[i-1]*nd$payoff[i-1]
        ab[i]=ab[i-1]+choiceb[i-1]*nd$payoff[i-1]
      }
    }
    prob=exp(b[1]*ab)/(exp(b[1]*aa)+exp(b[1]*ab))
    MSE=mean((nd$choice[test]-prob)^2)
    return(MSE)
  }
  MSE2k[i]=ll2MSE(result2k$par)
}
MSE2k=mean(MSE2k)

set.seed(100)
folds=sample(rep(1:k,each=4))
MSE3k=numeric(5)
for(i in 1:k){
  trainassign=which(folds!=i)
  testassign=which(folds==i)
  train=numeric(0)
  test=numeric(0)
  for(j in trainassign){
    train=c(train,which(nd$id==j))
  }
  for(j in testassign){
    test=c(test,which(nd$id==j))
  }
  ll3train=function(b){
    choicea = 1-nd$choice[train]
    choiceb = nd$choice[train]
    aa=numeric(length(train))
    ab=numeric(length(train))
    for (i in 1:length(train)){
      if(nd$round[train][i]==1){
        aa[i] = 0
        ab[i] = 0
      } else{
        aa[i] = b[2]*aa[i-1]+choicea[i-1]*nd$epayoff[i-1]
        ab[i] = b[2]*ab[i-1]+choiceb[i-1]*nd$epayoff[i-1]
      }
    }
    r =  b[1]* (choicea * aa) + b[1]*(choiceb * ab) - log(exp(b[1]*aa)+exp(b[1]*ab))
    return(-sum(r))
  }
  result3k=nlminb(c(0,0),ll3train,lower=c(0),upper=c(Inf))
  ll3MSE=function(b){
    choicea=1-nd$choice[test]
    choiceb=nd$choice[test]
    aa=numeric(length(test))
    ab=numeric(length(test))
    for(i in 1:length(test)){
      if(nd$round[test][i]==1){
        aa[i]=0
        ab[i]=0
      }
      else{
        aa[i] = b[2]*aa[i-1]+choicea[i-1]*nd$epayoff[i-1]
        ab[i] = b[2]*ab[i-1]+choiceb[i-1]*nd$epayoff[i-1]
      }
    }
    prob=exp(b[1]*ab)/(exp(b[1]*aa)+exp(b[1]*ab))
    MSE=mean((nd$choice[test]-prob)^2)
    return(MSE)
  }
  MSE3k[i]=ll3MSE(result3k$par)
}
MSE3k=mean(MSE3k)

set.seed(100)
folds=sample(rep(1:k,each=4))
MSE4k=numeric(5)
for(i in 1:k){
  trainassign=which(folds!=i)
  testassign=which(folds==i)
  train=numeric(0)
  test=numeric(0)
  for(j in trainassign){
    train=c(train,which(nd$id==j))
  }
  for(j in testassign){
    test=c(test,which(nd$id==j))
  }
  ll4train=function(b){
    choicea = 1-nd$choice[train]
    choiceb = nd$choice[train]
    aa=numeric(length(train))
    ab=numeric(length(train))
    for (i in 1:length(train)){
      if(nd$round[train][i]==1){
        aa[i] = 0
        ab[i] = 0
      } else{
        aa[i] = b[2]*aa[i-1]+choicea[i-1]*nd$pa[i-1]
        ab[i] = b[2]*ab[i-1]+choiceb[i-1]*nd$pb[i-1]
      }
    }
    r =  b[1]* (choicea * aa) + b[1]*(choiceb * ab) - log(exp(b[1]*aa)+exp(b[1]*ab))
    return(-sum(r))
  }
  result4k=nlminb(c(0,0),ll4train,lower=c(0),upper=c(Inf))
  ll4MSE=function(b){
    choicea=1-nd$choice[test]
    choiceb=nd$choice[test]
    aa=numeric(length(test))
    ab=numeric(length(test))
    for(i in 1:length(test)){
      if(nd$round[test][i]==1){
        aa[i]=0
        ab[i]=0
      }
      else{
        aa[i] = b[2]*aa[i-1]+choicea[i-1]*nd$pa[i-1]
        ab[i] = b[2]*ab[i-1]+choiceb[i-1]*nd$pb[i-1]
      }
    }
    prob=exp(b[1]*ab)/(exp(b[1]*aa)+exp(b[1]*ab))
    MSE=mean((nd$choice[test]-prob)^2)
    return(MSE)
  }
  MSE4k[i]=ll4MSE(result4k$par)
}
MSE4k=mean(MSE4k)

set.seed(100)
folds=sample(rep(1:k,each=4))
MSE5k=numeric(5)
for(i in 1:k){
  trainassign=which(folds!=i)
  testassign=which(folds==i)
  train=numeric(0)
  test=numeric(0)
  for(j in trainassign){
    train=c(train,which(nd$id==j))
  }
  for(j in testassign){
    test=c(test,which(nd$id==j))
  }
  ll5train=function(b){
    choicea = 1-nd$choice[train]
    choiceb = nd$choice[train]
    aa=numeric(length(train))
    ab=numeric(length(train))
    for (i in 1:length(train)){
      if(nd$round[train][i]==1){
        aa[i] = 0
        ab[i] = 0
      } else{
        aa[i] = b[2]*aa[i-1]+b[3]*choicea[i-1]*nd$epayoff[i-1]+(1-b[3])*choicea[i-1]*nd$pa[i-1]
        ab[i] = b[2]*ab[i-1]+b[3]*choiceb[i-1]*nd$epayoff[i-1]+(1-b[3])*choiceb[i-1]*nd$pb[i-1]
      }
    }
    r =  b[1]* (choicea * aa) + b[1]*(choiceb * ab) - log(exp(b[1]*aa)+exp(b[1]*ab))
    return(-sum(r))
  }
  result5k=nlminb(c(0,0,0),ll5train,lower=c(0),upper=c(Inf))
  ll5MSE=function(b){
    choicea=1-nd$choice[test]
    choiceb=nd$choice[test]
    aa=numeric(length(test))
    ab=numeric(length(test))
    for(i in 1:length(test)){
      if(nd$round[test][i]==1){
        aa[i]=0
        ab[i]=0
      }
      else{
        aa[i] = b[2]*aa[i-1]+b[3]*choicea[i-1]*nd$epayoff[i-1]+(1-b[3])*choicea[i-1]*nd$pa[i-1]
        ab[i] = b[2]*ab[i-1]+b[3]*choiceb[i-1]*nd$epayoff[i-1]+(1-b[3])*choiceb[i-1]*nd$pb[i-1]
      }
    }
    prob=exp(b[1]*ab)/(exp(b[1]*aa)+exp(b[1]*ab))
    MSE=mean((nd$choice[test]-prob)^2)
    return(MSE)
  }
  MSE5k[i]=ll5MSE(result5k$par)
}
MSE5k=mean(MSE5k)
MSEk=c(MSE1k,MSE2k,MSE3k,MSE4k,MSE5k)
MSEbest=which(MSEk==min(MSEk)) #5

library(boot)
B=10
set.seed(100)
ll5coef=matrix(0,B,3)
for(i in 1:B){
  I=sample(1:20,20,replace=TRUE)
  newdata=data.frame(choice=numeric(),otherchoice=numeric(),id=numeric(),payoff=numeric(),round=numeric(),pa=numeric(),bp=numeric(),econ=numeric(),epayoff=numeric(),epayoffa=numeric(),epayoffb=numeric())
  for(j in I){
    for(k in 1:2000){
      if(nd$id[k]==I[j]){
        newdata=rbind(newdata,nd[k,])
      }
    }
  }
  ll5boot <- function(b){
    choicea = 1-newdata$choice
    choiceb = newdata$choice
    aa=numeric(length(newdata$id))
    ab=numeric(length(newdata$id))
    for (i in 1:length(nd$id)){
      if(nd$round[i]==1){
        aa[i] = 0
        ab[i] = 0
      } else{
        aa[i] = b[2]*aa[i-1]+b[3]*choicea[i-1]*newdata$epayoff[i-1]+(1-b[3])*choicea[i-1]*newdata$pa[i-1]
        ab[i] = b[2]*ab[i-1]+b[3]*choiceb[i-1]*newdata$epayoff[i-1]+(1-b[3])*choiceb[i-1]*newdata$pb[i-1]
      }
      
    }
    r =  b[1]* (choicea * aa) + b[1]*(choiceb * ab) - log(exp(b[1]*aa)+exp(b[1]*ab))
    return(-sum(r))
  }
  resultB=nlminb(c(0,0,0),ll5boot,lower=c(0),upper=c(Inf))
  coefB=resultB$par
  ll5coef[i,]=coefB
}

ll5beta=colMeans(ll5coef)
ll5se=numeric(3)
for(i in 1:3){
  ll5se[i]=sd(ll5coef[,i])/sqrt(length(ll5coef[,i]))
}
ll5se
p=abs(ll5beta)-1.96*(ll5se)
#all stat sig from 0
