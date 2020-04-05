
compute_DIC_Guyane_Predictors <- function(F1,...){
  
  
  Chains <- rstan::extract(F1$fit)
  Eps = as.matrix(Chains$epsilon)
  
  M <-   dim(Eps)[1]
  neps = dim(Eps)[2]
  
  ## Compute the LogLikelihood
  
  LogLikelihoods <- matrix(0, nrow = M, ncol = N) 
  Y <- F1$data$Y
  age <- F1$data$age  
  sex=F1$data$sex
  location <- F1$data$location
  carbet <- F1$data$carbet
  social <- F1$data$social

  for (i in seq(1,M)){
    
    cumM = Chains$cum_foiM[i,,]
    cumC = Chains$cum_foiC[i,,]
    BM =Chains$BM[i,]
    BC =Chains$BC[i,]
    
    CarbetM =Chains$CarbetM[i,]
    CarbetC =Chains$CarbetC[i,]
    
    
    SocialM =Chains$SocialM[i,]
    SocialC =Chains$SocialC[i,]
    
    sigma01 = Chains$sigma_0[i,1]
    sigma02 = Chains$sigma_0[i,2]
    sigma11 = Chains$sigma_1[i,1]
    sigma12 = Chains$sigma_1[i,2]
    sigma21 = Chains$sigma_2[i,1]
    sigma22 = Chains$sigma_2[i,2]
    eps1 =Eps[i,1]
    eps2 =Eps[i,2]
    
    
    for(j in 1:N){ 
      
      
      PM= (1-exp(-cumM[age[j],location[j]]*BM[sex[j]]*CarbetM[carbet[j]]*SocialM[social[j]] ) );
      PC= (1-exp(-cumC[age[j],location[j]]*BC[sex[j]]*CarbetC[carbet[j]]*SocialC[social[j]]  ) );
      
      IMC  = PM*PC;
      IM0 = PM*(1-PC);
      IC0  = (1-PM)*PC;
      I00  = (1-PM)*(1-PC);
      
      
      P =  dnorm(mean = sigma01+sigma11,sd = eps1, x = Y[j,1] )*dnorm(mean = sigma02+sigma12,sd = eps2, x = Y[j,2] ) *IMC+
        dnorm(mean = sigma01+sigma11,sd = eps1, x = Y[j,1] )*dnorm(mean = sigma02 + sigma22*Y[j,1] ,sd = eps2, x = Y[j,2] ) *IM0+
        dnorm(mean = sigma01+ sigma21* Y[j,2],sd = eps1, x = Y[j,1] )*dnorm(mean = sigma02+sigma12,sd = eps2, x = Y[j,2] ) *IC0+
        dnorm(mean = sigma01,sd = eps1, x = Y[j,1] )*dnorm(mean = sigma02,sd = eps2, x = Y[j,2] ) *I00
      
      
      
      LogLikelihoods[i,j]  <- log(P)
      
    }
  }
  
  # posterior mean
  
  eps1 = mean(Chains$epsilon[,1])
  if(neps==2){
    eps2 = mean(Chains$epsilon[,2])
  }  else{
    eps1=eps2
  }
  
  sigma01 =mean( Chains$sigma_0[,1])
  sigma02 = mean(Chains$sigma_0[,2])
  
  sigma11 =mean( Chains$sigma_1[,1])
  sigma12 = mean(Chains$sigma_1[,2])
  sigma21 = mean(Chains$sigma_2[,1])
  sigma22 = mean(Chains$sigma_2[,2])
  
  
  BM =colMeans(Chains$BM)
  BC =colMeans(Chains$BC)
  
  CarbetM =colMeans(Chains$CarbetM)
  CarbetC =colMeans(Chains$CarbetC)
  
  SocialM =colMeans(Chains$SocialM)
  SocialC =colMeans(Chains$SocialC)
  
  LogLikelihoodMean <- 0
  for (j in seq(1,N) ){
    
    
    
    cumM = mean(Chains$cum_foiM[,age[j],location[j]])
    cumC = mean(Chains$cum_foiC[,age[j],location[j]])
    
    PM= (1-exp(-cumM*BM[sex[j]]*CarbetM[carbet[j]]*SocialM[social[j]] ) );
    PC= (1-exp(-cumC*BC[sex[j]]*CarbetC[carbet[j]]*SocialC[social[j]] ) );
    
    
    
    IMC  = PM*PC;
    IM0 = PM*(1-PC);
    IC0  = (1-PM)*PC;
    I00  = (1-PM)*(1-PC); 
    
    P =  dnorm(mean = sigma01+sigma11,sd = eps1, x = Y[j,1] )*dnorm(mean =  sigma02+sigma12,sd = eps2, x = Y[j,2] ) *IMC+
      dnorm(mean = sigma01+ sigma11,sd = eps1, x = Y[j,1] )*dnorm(mean = sigma02+sigma22* Y[j,1],sd = eps2, x = Y[j,2] ) *IM0+
      dnorm(mean =  sigma01+sigma21*Y[j,2],sd = eps1, x = Y[j,1] )*dnorm(mean = sigma02+sigma12,sd = eps2, x = Y[j,2] ) *IC0+
      dnorm(mean =  sigma01,sd = eps1, x = Y[j,1] )*dnorm(mean = sigma02,sd = eps2, x = Y[j,2] ) *I00
    
    LogLikelihoodMean <- LogLikelihoodMean +log(P)
    
  }
  
  
  LP <- rowSums(LogLikelihoods)
  
  
  # Compute the DIC
  
  Dbar = -2*mean(LP)
  Dthetabar = -2*LogLikelihoodMean
  pD = Dbar-Dthetabar
  
  DIC = pD+Dbar 
  
  
  
  information_criteria <- list(DIC = DIC,
                               pD = pD,
                               Dbar=Dbar)
  
  
  
  return(information_criteria)
  
}




print.information_criteria <- function(x,...){
  
  cat(sprintf('DIC:  %f, Dbar:  %f, pD:  %f\n' , x$DIC, x$Dbar, x$pD))
  
}


RowVar <- function(x) {
  rowSums((x - rowMeans(x))^2)/(dim(x)[2] - 1)
}

ColVar <- function(x) {
  colSums((x - colMeans(x))^2)/(dim(x)[1] - 1)
}

