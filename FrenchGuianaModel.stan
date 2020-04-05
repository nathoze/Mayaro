data {
    int <lower=0> A; //the number of age classes

    int <lower=0> NGroups; //the number of foi groups

    int <lower=0> N; //the number of individuals

    int <lower=0> age[N]; // Age 

    int <lower=0> sex[N]; // sexe = 1 for males, 2 for females  

    int <lower=1> location[N]; 

    int <lower=1> carbet[N]; //  = 1 if living in a carbet 2 otherwise

    int <lower=0> social[N]; // social = 1 for lower income, 2 for higher income  

    int <lower=1> NRegions;  // 7  = nb clusters of communes
 
    real Y[N,2]; // log  titer (the new seropositivity)

}

parameters {

    real<lower = 0> alphaM[NRegions] ;
    real<lower = 0> alphaC[NRegions] ;

    real<lower =0.0001> bc;
    real<lower =-1> tc;


    real log_BM;
    real log_BC;
    real log_ageM;
    real log_ageC;
    real log_CarbetM;
    real log_CarbetC;

    real log_SocialM;
    real log_SocialC;


    real sigma_0[2];
    real<lower=1> sigma_1[2];
    real<lower=0> sigma_2[2];
    real<lower=0.001, upper=1> epsilon[2];

}

transformed parameters {

    real<lower=0> lambdaM[A,NRegions]; 
    real<lower=0> lambdaC[A,NRegions]; 

    matrix<lower=0>[A,NRegions]  cum_foiM;   // cumulative foi by age
    matrix<lower=0>[A,NRegions]  cum_foiC;   // cumulative foi by age
 
    real<lower =0> BM[2];
    real<lower =0> BC[2];
    real<lower =0> ageFactorM;
    real<lower =0> ageFactorC;
    real<lower =0> CarbetM[2];
    real<lower =0> CarbetC[2];

    real<lower =0> SocialM[2];
    real<lower =0> SocialC[2];


    real<lower =0.0001> betaC[NRegions] ;
    real<lower = -1> TC[NRegions] ;

    real<lower = 0> S; // Normalization for the CHIKV epidemics 

    BM[1]=1; //males
    BM[2] =  exp(log_BM); //females

    BC[1] =  1; //males
    BC[2] = exp(log_BC); //females

    ageFactorM =  exp(log_ageM);
    ageFactorC =  exp(log_ageC);

    CarbetM[1]=  1; // if lives in a carbet
    CarbetM[2] =  exp(log_CarbetM); // if doesn't live in a carbet

    CarbetC[1] =  1; 
    CarbetC[2] =  exp(log_CarbetC);


    SocialM[1] =  1; 
    SocialM[2] =  exp(log_SocialM);

    SocialC[1] =  1; 
    SocialC[2] =  exp(log_SocialC);


    for(I in 1:NRegions){      
        TC[I]=tc;
    }

    for(I in 1:NRegions){      
       // betaC[I]=bc;
        betaC[I]=1;
    }

   S=0;
    for(j in 1:A){
        S= S+exp(-((j-tc)^2)/(betaC[1])^2);
    }


    for(I in 1:NRegions){      
        for(j in 1:A){
            lambdaM[j,I]  =    alphaM[I];
            lambdaC[j,I]  =   alphaC[I]/S*exp(-((j-TC[I])^2)/(betaC[I])^2);

        }
    }



 


// AGE SWITCH


        for(I in 1:NRegions){  
                for (j in 1:A) {
                    cum_foiM[j,I] = 0;// Here change  : Lconstant added to the cumulative FOI not the FOI
                    if(j<=20){
                        for(k in 1:j){
                            cum_foiM[j,I] = cum_foiM[j,I]+lambdaM[k,I]*ageFactorM;
                        }
                    } 
                    if(j>20 ){
                        for(k in 0:j-1){
                            if(k <=20){
                                cum_foiM[j,I] = cum_foiM[j,I]+lambdaM[j-k,I]*ageFactorM; // as young
                            } 
                            if(k>20 ){
                                cum_foiM[j,I] = cum_foiM[j,I]+lambdaM[j-k,I]; // as mid
                            }
                        }
                    }   
                } 
            
        }
 
 

        for(I in 1:NRegions){  
                for (j in 1:A) {
                    cum_foiC[j,I] = 0;// Here change  : Lconstant added to the cumulative FOI not the FOI
                    if(j<=20){
                        for(k in 1:j){
                            cum_foiC[j,I] = cum_foiC[j,I]+lambdaC[k,I]*ageFactorC;
                        }
                    } 
                    if(j>20 ){
                        for(k in 0:j-1){
                            if(k <=20){
                                cum_foiC[j,I] = cum_foiC[j,I]+lambdaC[j-k,I]*ageFactorC; // as young
                            } 
                            if(k>20 ){
                                cum_foiC[j,I] = cum_foiC[j,I]+lambdaC[j-k,I]; // as mid
                            }
                        }
                    }   
                } 
               
        }

}

model {
    real LPS[4];

    real PM; // infected by CHIKV   
    real PC;  // infected by MAYV 

    real IMC; // infected by both MAYV and CHIKV   
    real IM0;  // infected by  MAYV but not by CHIKV 
    real IC0;    
    real I00; 



  //FOI by group
    for(I in 1:NRegions){   
        alphaM[I] ~ uniform(0,5);
        alphaC[I] ~ uniform(0,5);
    }
  
    tc ~ uniform(0,50);
    bc ~ uniform(0,2);

    sigma_0[1] ~ normal(-0.4,0.01);

    epsilon[1] ~ uniform(0,5);

    sigma_0[2] ~  normal(0,0.01);

    epsilon[2] ~ uniform(0,5);

    sigma_1[1] ~ uniform(0,5);
    sigma_2[1] ~ uniform(0,5);
    sigma_1[1] ~ uniform(0,5);
    sigma_2[1] ~ uniform(0,5);

 
    log_BM ~ normal(0,1.73) ; // prior based on Cauchemez et al., NEJM, 2009; 1.73^2=3
    log_BC ~ normal(0,1.73) ; 
     
    log_CarbetM ~ normal(0,1.73) ; 
    log_CarbetC ~ normal(0,1.73) ; 
    
    log_ageM  ~ normal(0,1.73) ; 
    log_ageC ~ normal(0,1.73) ; 
 
    log_SocialM ~ normal(0,1.73) ; 
    log_SocialC ~ normal(0,1.73) ; 
   



    for(j in 1:N){


        PM= (1-exp(-cum_foiM[age[j],location[j]]*BM[sex[j]]*SocialM[social[j]]*CarbetM[carbet[j]]  ) );
        PC= (1-exp(-cum_foiC[age[j],location[j]]*BC[sex[j]]*SocialC[social[j]]*CarbetC[carbet[j]]  ) );

        IMC  = PM*PC;
        IM0 = PM*(1-PC);
        IC0  = (1-PM)*PC;
        I00  = (1-PM)*(1-PC);


        if(IMC  == 0){
            LPS[1] = -10000;
        }else{
            LPS[1] = log(IMC) + normal_lpdf(Y[j,1] | sigma_0[1] +  sigma_1[1], epsilon[1])+ normal_lpdf(Y[j,2] |  sigma_0[2] +  sigma_1[2] , epsilon[2]);
        }
        if(IM0 == 0){
            LPS[2] = -10000;
        }else{
            LPS[2] = log(IM0) + normal_lpdf(Y[j,1] |  sigma_0[1] +  sigma_1[1] , epsilon[1])+normal_lpdf(Y[j,2] |  sigma_0[2] +  sigma_2[2]*(Y[j,1])  , epsilon[2]);
        }
        if(IC0  == 0){
            LPS[3] = -10000;
        }else{
            LPS[3] = log(IC0) + normal_lpdf(Y[j,1] | sigma_0[1] +  sigma_2[1]*(Y[j,2]) , epsilon[1])+ normal_lpdf(Y[j,2] |  sigma_0[2] + sigma_1[2] , epsilon[2]); 
        }
        if(I00 == 0){
            LPS[4] = -10000;
        }else{
            LPS[4] = log(I00) + normal_lpdf(Y[j,1] |  sigma_0[1]  , epsilon[1])+ normal_lpdf(Y[j,2] | sigma_0[2] , epsilon[2]);
        }

            target += log(sum(exp(LPS)));

    }
}




