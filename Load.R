library(rstan)
library(ggplot2)


# load the dataset

dat.guiana = read.csv(file='CHIKVMAYV.csv', sep=';')
data=dat.guiana

# some functions -----
quan1 <-function(X){
  return(quantile(X,probs = 0.025))
}
quan2 <-function(X){
  return(quantile(X,probs = 0.975))
}

odds.ratio <- function(model0){
  
  mean<-as.numeric(exp(coef(model0))[2])
  co= confint(model0,level = 0.95)
  ci1 <- exp(co)[2]
  ci2 <- exp(co)[4]
  A=c(mean, ci1, ci2)
  return(A)
} 

mean.and.ci <-function(s){
  
  mean = as.numeric(s[1])
  lower = as.numeric(attr(x = s,'ci')[1])
  upper = as.numeric(attr(x = s,'ci')[2])
  print(paste0('Mean: ', round(mean,3), ' 2.5%: ', round(lower,3), ' 97.5%: ', round(upper,3)))
  
}

# variables

ColorChikungunya = 'orange'
ColorMayaro = 'purple'

#With the seroneutralization
MayThreshold=2.73
ChikThreshold = 24.9


N = nrow(data)


# Simulated dataset
# transform age -> random within the class

N=dim(data)[1]
age.numeric  = rep(0,N)
for(i in 1:N){
  if(dat.guiana$age[i] == '[1-10]')
    age.numeric[i] = floor(runif(n = 1, min = 1, max = 11))
  
  if(dat.guiana$age[i] == '[11-20]')
    age.numeric[i] = floor(runif(n = 1, min = 11, max = 21))
  
  if(dat.guiana$age[i] == '[21-30]')
    age.numeric[i] = floor(runif(n = 1, min = 21, max = 31))
  
  if(dat.guiana$age[i] == '[31-40]')
    age.numeric[i] = floor(runif(n = 1, min = 31, max = 41))
  if(dat.guiana$age[i] == '[41-50]')
    age.numeric[i] = floor(runif(n = 1, min = 41, max = 51))
  if(dat.guiana$age[i] == '>50')
    age.numeric[i] = floor(runif(n = 1, min = 51, max = 71))
}


data$age =  age.numeric
data$location  = as.numeric(dat.guiana$region)
data$sex  = as.numeric(dat.guiana$sex=="Female")+1 # 1 Male, 2 Female

# the following variables were not included in the dataset to keep the participants anonymous
data$environment = rep(1,N) # a variable about the rural/urban environement of the individuals
data$social = rep(1,N) #1 low income, 2 high income
data$carbet = rep(1,N) # 1 live in a carbet, 2 not in a carbet

data$Y =  log(cbind(data$MAYVRFI,data$CHIKVRFI)) # RFI must be in log scale

NRegions = length(unique(data$region))
NGroups = NRegions