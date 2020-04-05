plot_FOI_logscale<-function(F1, Ylim=60, LENGTHYEARS=37){
  
  ## For all communes
  Chains=rstan::extract(F1$fit)
  
  # in log scale -----
  Ymin=0.101
  dim(Chains$PM)
  ages=seq(LENGTHYEARS,1,by=-1)
  for(com in seq(1,NRegions)){
    
    lambdam=Chains$lambdaM[,ages,com]
    par_outM <-  apply(lambdam, 2, function(x)c(mean(x), quantile(x, probs=c(0.025, 0.975))))
    
    
    lambdac=Chains$lambdaC[,ages,com]
    par_outC <-  apply(lambdac, 2, function(x)c(mean(x), quantile(x, probs=c(0.025, 0.975))))
    
    
    pm=par_outM
    pc=par_outC
    
    
    pm=(1-exp(-pm))*100
    pc=(1-exp(-pc))*100 
    
    yrs = seq((2016-LENGTHYEARS+1),2016)
    
    
    meanDataC <- data.frame(x = yrs, y = pc[1, ])
    meanDataM <- data.frame(x= yrs, y = pm[1,] )
    
    
    #  meanDataM <- data.frame(x= yrs, y = rep(pm[1,LENGTHYEARS],LENGTHYEARS))
    
    meanDataM$y[which(meanDataM$y>Ylim)]=Ylim
    meanDataC$y[which(meanDataC$y>Ylim)]=Ylim
    
    meanDataM$y[which(meanDataM$y<Ymin)]=Ymin
    meanDataC$y[which(meanDataC$y<Ymin)]=Ymin
    
    p <- ggplot2::ggplot()
    p <- p + ggplot2::geom_line(data = meanDataM, ggplot2::aes(x = x, y = y), size = 1, color=ColorMayaro)
    p <- p + ggplot2::geom_line(data = meanDataC, ggplot2::aes(x = x, y = y), size = 1, color=ColorChikungunya)
    p <- p + ggplot2::xlab("Year") + ggplot2::ylab("Infection probability (%)")
    p <- p+theme_classic()+ylim(0, Ylim)
    
    p <- p+theme(axis.text.x = element_text(size=15),
                 axis.text.y = element_text(size=15),
                 text=element_text(size=15))
    
    xpoly =c(yrs,rev(yrs))
    
    ypoly = c(pm[2,], rev(pm[3,]))
    
    ypoly[which(ypoly<Ymin)]=Ymin
    ypoly[which(ypoly>Ylim)]=Ylim
    
    
    DataEnvelope = data.frame(x = xpoly, y = ypoly)
    p <- p +ggplot2::geom_polygon(data=DataEnvelope, ggplot2::aes(x, y), fill=ColorMayaro, alpha=0.4)
    
    
    
    xpoly =c(yrs,rev(yrs))
    ypoly = c(pc[2,], rev(pc[3,]))
    ypoly[which(ypoly<Ymin)]=Ymin
    
    DataEnvelope = data.frame(x = xpoly, y = ypoly)
    p <- p +ggplot2::geom_polygon(data=DataEnvelope, ggplot2::aes(x, y), fill=ColorChikungunya, alpha=0.4)
    
    p <- p + scale_y_log10(limits=c(Ymin, Ylim), breaks=c(0.1,1,10), labels=c('<0.1','1','10'))
    
    print(p)
    #ggsave(paste0('FOIlog',com,'.pdf'))
    
  }
  
  
}