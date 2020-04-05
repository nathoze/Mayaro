Chains=rstan::extract(F1$fit)

YMax  =6
# demographic
BM = Chains$BM[,2] # risk of MAYV infection of women vs men
BC = Chains$BC[,2] # risk of CHIKV infection of women vs men
ageM = Chains$ageFactorM # risk of MAYV infection of < 20 y.o. vs >20 y.o.
ageC = Chains$ageFactorC # risk of CHIKV infection of < 20 y.o. vs >20 y.o.

CarbetM = 1/Chains$CarbetM[,2] # risk of MAYV infection in a Carbet
CarbetC = 1/Chains$CarbetC[,2] # risk of CHIKV infection in a Carbet

SocialM = 1/Chains$SocialM[,2] # risk of MAYV infection income
SocialC = 1/Chains$SocialC[,2] # risk of CHIKV infection income

A=cbind(ageM,ageC,  SocialM,SocialC,1/BM,1/BC,CarbetM, CarbetC)
DX=1
dx=0.1

dataf <- data.frame(mean=apply(A,2,mean), ci1=apply(A,2,quan1), ci2=apply(A,2,quan2),
                    x = c(1,1+dx, 1+DX, 1+DX+dx, 1+2*DX, 1+2*DX+dx, 1+3*DX, 1+3*DX+dx),
                    dis = factor(rep(c('M','C'),4)))

g = ggplot(dataf, aes(x=x, y=mean)) + 
  geom_point(aes(color=dis),size=3) +
  geom_errorbar(aes(ymin=ci1, ymax=ci2, color=dis),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9),
                size=1.6)

g= g + ggplot2::geom_hline(yintercept = 1,linetype='dashed')
g=g+scale_colour_manual(values=c( ColorChikungunya,ColorMayaro))
g=g+theme_classic()

g=g+ theme( axis.text.x = element_text(size=16),
            axis.text.y = element_text(size=16),
            text=element_text(size=16),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            panel.grid.major.x =   element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.background = element_rect(fill = "white")  )
g=g+ylim(0,YMax+0.2)+xlab('')+ylab('')
g =  g+scale_y_log10()
 g=  g+scale_x_discrete(limits=c("Children", "Low income", "Males", "Carbet"))
print(g)









 