rm(list=ls())
w=c(11,28,4,28,3,20)
final=data.frame(Name=character(),Season=numeric(),Numwins=numeric(),Numlosses=numeric(),Opponents=list())
for(n in 1961:2010){ #to loop through each webpage from 1961 to 2010
  d=data.frame(Date=character(),Awayteam=character(),Awayscore=numeric(),Hometeam=character(),Homescore=numeric(),Location=character(),Season=numeric())
  address=paste("http://homepages.cae.wisc.edu/~dwilson/rsfc/history/howell/cf",n,"gms.txt",sep="")
  f=read.fwf(address,w)
  d=rbind(d,data.frame(Date=as.character(unlist(f[1])),Awayteam=as.character(unlist(f[2])),Awayscore=f[3],Hometeam=as.character(unlist(f[4])),Homescore=f[5],Location=as.character(unlist(f[6])),Season=n))
  
  d$Awayteam=as.character(d$Awayteam) #all this is setting up and manipulating the data from the web to work with more
  d$Hometeam=as.character(d$Hometeam)
  d$Location=as.character(d$Location)
  
  d$Awayteam=gsub(" {2,}","",d$Awayteam) 
  d$Hometeam=gsub(" {2,}","",d$Hometeam)
  d$Location=gsub(" {2,}","",d$Location)
  
  hometeams=unique(d$Hometeam)
  awayteams=unique(d$Awayteam)
  allteams=unique(append(hometeams,awayteams))
  numteams=length(allteams)
  div2=numeric()
  for(i in 1:numteams){ #list of teams that are div 2 teams
    count=0
    for(j in 1:nrow(d)){
      if((d$Hometeam[j]==allteams[i])|(d$Awayteam[j]==allteams[i])){ #have to check both home and away against all
        count=count+1
      }
    }
    if(count<6){
      div2=append(div2,i) 
    }
  }

  removediv2=numeric()
  for(i in 1:length(div2)){ #remove lines with div 2 teams
    for(j in 1:nrow(d)){
      if((d$Hometeam[j]==allteams[div2[i]])|(d$Awayteam[j]==allteams[div2[i]])){ #have to check both home and away against all
        removediv2=append(removediv2,j)
      }
    }
  }
  d=d[-removediv2,]
    
  removetie=numeric()
  for(i in 1:nrow(d)){ #find ties
    if(d$V3[i]==d$V5[i]){
      removetie=append(removetie,i)
    }
  } 
  if(length(removetie)!=0){ #remove ties
    d=d[-removetie,]
  }
  
  hometeams=unique(d$Hometeam)
  awayteams=unique(d$Awayteam)
  allteams=unique(append(hometeams,awayteams))
  numteams=length(allteams)
  numwins=numeric()
  numlosses=numeric()
  for(i in 1:numteams){ #counting the number of wins and losses for each team
    wins=0
    losses=0
    for(j in 1:nrow(d)){ #loop through year specific data
      if((d$Hometeam[j]==allteams[i])&(d$V5[j]>d$V3[j])){ #V3 is Away team score and V5 is Home team score
        wins=wins+1
      }
      else if((d$Hometeam[j]==allteams[i])&(d$V5[j]<d$V3[j])){
        losses=losses+1
      }
      else if((d$Awayteam[j]==allteams[i])&(d$V5[j]<d$V3[j])){
        wins=wins+1
      }
      else if((d$Awayteam[j]==allteams[i])&(d$V5[j]>d$V3[j])){
        losses=losses+1
      }
    }
    numwins[i]=wins
    numlosses[i]=losses
  }
  rownames(d)=1:nrow(d)
  opponents=list()
  hometeams=unique(d$Hometeam)
  awayteams=unique(d$Awayteam)
  allteams=unique(append(hometeams,awayteams))
  numteams=length(allteams)
  for(i in 1:numteams){ #for each unique team
    schedule=numeric()
    for(j in 1:nrow(d)){ #go through the whole  year data
      if(d$Hometeam[j]==allteams[i]){ #if the home team col matches the unique team
        for(k in 1:numteams){ #go through each unique team
          if(d$Awayteam[j]==allteams[k]){ #find the index of the team in the away col
            schedule=append(schedule,k)
          }
        }
      }
      else if(d$Awayteam[j]==allteams[i]){ #if the away team col matches the unique team
        for(k in 1:numteams){ #igo through each unique team
          if(d$Hometeam[j]==allteams[k]){ #find the index of the team in the away col
            schedule=append(schedule,k) 
          }
        } #maybe could have done the k loop with a hashtable?
      }
    }
    
    opponents=append(opponents,list(schedule))
  }
  final=rbind(final,data.frame(Name=as.character(allteams),Season=n,Numwins=as.numeric(numwins),Numlosses=as.numeric(numlosses),Opponents=I(opponents)))
}

save(final,file="final.rdata")