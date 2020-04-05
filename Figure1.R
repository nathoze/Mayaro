
figure1A <- function(data){
  Titers = data.frame(may_ratio =exp(data$Y[,1]), chik_ratio = exp(data$Y[,2]))
  
  
  Plot = ggplot()  +   geom_point(data=Titers, aes(x = may_ratio, y=chik_ratio),color = "black", alpha=0.4)
  Plot <- Plot +scale_x_log10()+scale_y_log10()
  
  Plot = Plot+coord_fixed(ratio = 1)
  
  Plot <- Plot +theme(axis.text.x = element_text(size=24),
                      axis.text.y = element_text(size=24),
                      text=element_text(size=24))
  Plot <- Plot+xlab("MAYV RFI")+ylab("CHIKV RFI")
  print(Plot)
  
}

figure1B <- function(data){
  
  Ymax = 0.5
  for(com in  unique(data$location)){
    age_cats=10
    
    W=which(data$location==com)
    titers <- data.frame(age=data$age[W], titerM = data$Y[W,1], titerC = data$Y[W,2])
    A=60
    
    age_categories <- seq(from = 0, to = A, by = age_cats)
    
    age_cats=15
    mM=c()
    c1=c()
    c2=c()
    mC=c()
    c1C=c()
    c2C=c()
     
    Ylim.min = -0.6
    Ylim.max = 1.65
    
    J=0
    Ages=seq(1,A,by=10)
    
    for(k in Ages){
      J=J+1
      ag = which(abs(titers$age-k)<=age_cats/2)
      tM= (titers$titerM[ag] )
      
      mM[J]= mean(tM)
      #c1[J]=quantile(tM,probs = 0.025)
      #c2[J]= quantile(tM,probs = 0.975)
      
      c1[J]=-1.96*sd(tM)/sqrt(length(tM))+mean(tM)
      c2[J]=mean(tM)+1.96*sd(tM)/sqrt(length(tM))
      
      if(c1[J]< Ylim.min){
        c1[J]=Ylim.min
      }
      
      if(c2[J]>Ylim.max){
        c2[J]=Ylim.max
      }
      if(mM[J]>Ylim.max){
        mM[J]=Ylim.max
      }
      
      
      tC= (titers$titerC[ag] )
      
      mC[J]= mean(tC)
      c1C[J]=-1.96*sd(tC)/sqrt(length(tC))+mean(tC)
      c2C[J]=mean(tC)+1.96*sd(tC)/sqrt(length(tC))
      
      if(mC[J]< Ylim.min){
        mC[J]=Ylim.min
      }
      if(mC[J]>Ylim.max){
        mC[J]=Ylim.max
      }
      
      if(c1C[J]< Ylim.min){
        c1C[J]=Ylim.min
      }
      
      if(c2C[J]>Ylim.max){
        c2C[J]=Ylim.max
      }
    }
    
    
    histdataM <- data.frame(age = Ages+5,
                            mean=mM,lower=c1,upper=c2)
    
    
    g = ggplot(histdataM, aes(x=age, y=mean)) +
      geom_point(color = ColorMayaro, size=5)+
      geom_segment(aes(x=age,y=lower, xend= age,yend=upper), color= ColorMayaro, size=3)
    
    
    histdataC <- data.frame(age = Ages+7,
                            mean=mC,lower=c1C,upper=c2C)
    
    
    g  = g+  geom_point(data=histdataC, aes(x=age, y=mean), color = ColorChikungunya, size=5)+
      geom_segment(data=histdataC, aes(x=age,y=lower, xend= age,yend=upper), color= ColorChikungunya, size=3)
    g<- g+scale_y_continuous(limits=c(Ylim.min,Ylim.max),
                             breaks=c(-2.3,0,1.61),
                             labels=c('0.1', '1','>5'))
    
    
    g<- g+scale_x_continuous(limits=c(3,60),
                             breaks=c(5,15,25,35,45,55),
                             labels=c('1-10', '11-20', '21-30','31-40','41-50','>50'))
    
    
    g <- g+theme_classic()+theme(axis.text.x = element_text(size=18, angle=45, hjust=1),
                                 axis.text.y = element_text(size=18),
                                 text=element_text(size=18))
    g <- g+ylab('')+xlab(' ')
    print(g)
    
  }  
}


