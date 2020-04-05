Infections <- function(F1){
  Chains=rstan::extract(F1$fit)
  
  sigma0M=mean(Chains$sigma_0[,1])
  sigma0C=mean(Chains$sigma_0[,2])
  s1M = mean(Chains$sigma_1[,1])
  s1C =  mean(Chains$sigma_1[,2])
  sCM =  mean(Chains$sigma_2[,1])
  sMC =  mean(Chains$sigma_2[,2])
  epsM = mean( Chains$epsilon[,1])
  epsC = mean(Chains$epsilon[,2])
  
  
  
  CarbetFactorM = Chains$CarbetM
  CarbetFactorC = Chains$CarbetC
  
  
  SexFactorM = Chains$BM
  SexFactorC = Chains$BC
  
  SocialFactorM = Chains$SocialM
  SocialFactorC = Chains$SocialC
  
  
  V = array(0,dim = c(N,4))
  PM = array(0,dim = c(N,1))
  PC = array(0,dim = c(N,1)) 
  
  for(i in 1:N){
    tm = F1$data$Y[i,1]
    tc = F1$data$Y[i,2]
    
    age=F1$data$age[i]
    region=F1$data$location[i]
    carbet=F1$data$carbet[i]
    sex= F1$data$sex[i]
    ocs= F1$data$ocs[i]
    social= F1$data$social[i]
    
    pm =  mean(1-exp(-Chains$cum_foiM[,age,region]*SexFactorM[,sex]*CarbetFactorM[,carbet]*SocialFactorM[,social]))
    pc =  mean(1-exp(-Chains$cum_foiC[,age,region]*SexFactorC[,sex]*CarbetFactorC[,carbet]*SocialFactorC[,social]))
    
    PM[i] = pm
    PC[i] = pc
    
    V[i,1]=dnorm(x = tm, mean=sigma0M , sd = epsM)*dnorm(x = tc, mean=sigma0C , sd = epsC) *(1-pm)*(1-pc)
    V[i,2] =dnorm(x=tm, mean = sigma0M + s1M, sd = epsM)*dnorm(x=tc,mean = sigma0C + sMC*tm, sd = epsC) *(pm)*(1-pc)
    V[i,3] =dnorm(x=tm,mean = sigma0M + sCM*tc,  sd = epsM)*dnorm(x=tc, mean= sigma0C + s1C, sd = epsC) *(1-pm)*(pc)
    V[i,4] =dnorm(x=tm,mean = sigma0M + s1M,  sd = epsM)*dnorm(x=tc, mean= sigma0C + s1C, sd = epsC) *(pm)*(pc)
    
  }
  
  V2=prop.table(V,1)
  
  
  qm = V2[,2]+V2[,4]
  qc = V2[,3]+V2[,4]
  
 
  
  MAX=factor(max.col(V2)) 
  
  
  Infections= data.frame(may_ratio=  exp(data$Y[,1]),
                         chik_ratio =  exp(data$Y[,2]),
                         mayinf= (MAX ==2 | MAX ==4),
                         chikinf= (MAX ==3 | MAX ==4),
                         mayinfProb =  qm,
                         chikinfProb =  qc,
                         monoMay  =  exp( data$Y[,1])>MayThreshold,
                         monoCHIK  =  exp(data$Y[,2])>ChikThreshold,
                         probaMAYV = PM,
                         probaCHIK = PC,
                         region = data$location,
                         weights = data$weight,
                         num_indiv = data$num_indiv)
  
  return(Infections)
}