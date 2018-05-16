rm(list=ls())
library(numDeriv)
load("A:/College/2017 Fall/ECON 524/pddata.Rdata")

IA=numeric(2000)
IB=numeric(2000)
for(i in 1:2000){
  if(pddata$round[i]>1){
    if(pddata$choice[i-1]==0){
      IA[i]=1
    }
    else if(pddata$choice[i-1]==1){
      IB[i]=1
    }
  }
}

AGA=numeric(2000)
AGB=numeric(2000)
EGA=numeric(2000)
EGB=numeric(2000)
for(i in 1:2000){
  if(pddata$round[i]>1){
    EGA[i]=pddata$epayoffa[i-1]
    EGB[i]=pddata$epayoffb[i-1]
    AGA[i]=pddata$pa[i-1]
    AGB[i]=pddata$pb[i-1]
  }
}

ll1=function(b){
  QAj=numeric(2000)
  QBj=numeric(2000)
  for(i in 1:2000){
    if(pddata$round[i]>1){
      QAj[i]=(1*QAj[i-1])+(1*IA[i]*EGA[i])+((1-1)*IA[i]*AGA[i])
      QBj[i]=(1*QBj[i-1])+(1*IB[i]*EGB[i])+((1-1)*IB[i]*AGB[i])
    }
  }
  LL=(b[1]*(1-pddata$choice)*QAj)+(b[1]*pddata$choice*QBj)-log(exp(b[1]*QAj)+exp(b[1]*QBj))
  return(-sum(LL))
}

res1=nlminb(c(0),ll1,lower=c(0),upper=c(Inf))
hes1=hessian(ll1,res1$par)
MLESE1=sqrt(diag(solve(hes1)))

ll2=function(b){
  QAj=numeric(2000)
  QBj=numeric(2000)
  for(i in 1:2000){
    if(pddata$round[i]>1){
      QAj[i]=(1*QAj[i-1])+(0*IA[i]*EGA[i])+((1-0)*IA[i]*AGA[i])
      QBj[i]=(1*QBj[i-1])+(0*IB[i]*EGB[i])+((1-0)*IB[i]*AGB[i])
    }
  }
  LL=(b[1]*(1-pddata$choice)*QAj)+(b[1]*pddata$choice*QBj)-log(exp(b[1]*QAj)+exp(b[1]*QBj))
  return(-sum(LL))
}

res2=nlminb(c(0),ll2,lower=c(0),upper=c(Inf))
hes2=hessian(ll2,res2$par)
MLESE2=sqrt(diag(solve(hes2)))

ll3=function(b){
  QAj=numeric(2000)
  QBj=numeric(2000)
  for(i in 1:2000){
    if(pddata$round[i]>1){
      QAj[i]=(b[2]*QAj[i-1])+(1*IA[i]*EGA[i])+((1-1)*IA[i]*AGA[i])
      QBj[i]=(b[2]*QBj[i-1])+(1*IB[i]*EGB[i])+((1-1)*IB[i]*AGB[i])
    }
  }
  LL=(b[1]*(1-pddata$choice)*QAj)+(b[1]*pddata$choice*QBj)-log(exp(b[1]*QAj)+exp(b[1]*QBj))
  return(-sum(LL))
}

res3=nlminb(c(0,0),ll3,lower=c(0,0),upper=c(Inf,1))
hes3=hessian(ll3,res3$par)
MLESE3=sqrt(diag(solve(hes3)))

ll4=function(b){
  QAj=numeric(2000)
  QBj=numeric(2000)
  for(i in 1:2000){
    if(pddata$round[i]>1){
      QAj[i]=(b[2]*QAj[i-1])+(0*IA[i]*EGA[i])+((1-0)*IA[i]*AGA[i])
      QBj[i]=(b[2]*QBj[i-1])+(0*IB[i]*EGB[i])+((1-0)*IB[i]*AGB[i])
    }
  }
  LL=(b[1]*(1-pddata$choice)*QAj)+(b[1]*pddata$choice*QBj)-log(exp(b[1]*QAj)+exp(b[1]*QBj))
  return(-sum(LL))
}

res4=nlminb(c(0,0),ll4,lower=c(0,0),upper=c(Inf,1))
hes4=hessian(ll4,res4$par)
MLESE4=sqrt(diag(solve(hes4)))

ll5=function(b){
  QAj=numeric(2000)
  QBj=numeric(2000)
  for(i in 1:2000){
    if(pddata$round[i]>1){
      QAj[i]=(b[2]*QAj[i-1])+(b[3]*IA[i]*EGA[i])+((1-b[3])*IA[i]*AGA[i])
      QBj[i]=(b[2]*QBj[i-1])+(b[3]*IB[i]*EGB[i])+((1-b[3])*IB[i]*AGB[i])
    }
  }
  LL=(b[1]*(1-pddata$choice)*QAj)+(b[1]*pddata$choice*QBj)-log(exp(b[1]*QAj)+exp(b[1]*QBj))
  return(-sum(LL))
}

res5=nlminb(c(0,0,0),ll5,lower=c(0,0,0),upper=c(Inf,1,1))
hes5=hessian(ll5,res5$par)
MLESE5=sqrt(diag(solve(hes5)))
