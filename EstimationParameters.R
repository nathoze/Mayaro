



EstimationParameters <-function(){
  
  quants  = c(0.025,0.5,0.975)
  quantilestext  = paste0(quants*100,'%')
  quantilestext[4]  = 'mean'
  
  estimate_parameters <-function(distribution,name){
    de<-data.frame(t(quantile(distribution, probs = quants)), mean = mean(distribution))
    names(de) <- quantilestext
    rownames(de)[1] = name
    return(de) 
  }
  
  
  params <- NULL
  
  
  params <-rbind(params, estimate_parameters(Chains$sigma_0[,1],'Sigma0M')) 
  params <-rbind(params, estimate_parameters(Chains$sigma_0[,2],'Sigma0C'))
  params <-rbind(params, estimate_parameters(Chains$sigma_1[,1],'Sigma1M'))
  params <-rbind(params, estimate_parameters(Chains$sigma_1[,2],'Sigma1C'))
  params <-rbind(params, estimate_parameters(Chains$sigma_2[,1],'SigmaCtoM'))
  params <-rbind(params, estimate_parameters(Chains$sigma_2[,2],'SigmaMtoC'))
  params <-rbind(params, estimate_parameters(Chains$epsilon[,1],'epsilonM'))
  params <-rbind(params, estimate_parameters(Chains$epsilon[,2],'epsilonC'))
 
  
  
  params <-rbind(params, estimate_parameters(1/Chains$BM[,2],'Males MAYV')) 
  params <-rbind(params, estimate_parameters(1/Chains$ageFactorM,'Adults MAYV')) 
  params <-rbind(params, estimate_parameters(1/Chains$CarbetM[,2],'Carbet MAYV')) 
  params <-rbind(params, estimate_parameters(Chains$BC[,2],'Females CHIKV')) 
  params <-rbind(params, estimate_parameters(1/Chains$ageFactorC,'Adults CHIKV')) 
  params <-rbind(params, estimate_parameters(1/Chains$CarbetC[,2],'Carbet CHIKV')) 

  
  
  
  XX = NULL
  #print_estimated_parameters <- function(params){
  for(i in seq(1,dim(params)[1])){
    X=paste0( rownames(params)[i],  ' ', round(params$mean[i],digits=2),  ' [', round(params$`2.5%`[i],digits=2),', ', round(params$`97.5%`[i], digits=2) ,']')
    print(X)
    XX[i] = X
  }
  
  
}




