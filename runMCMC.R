runMCMC <- function()
{
  stanmodels=NULL
  StanModel=rstan::stan_model("FrenchGuianaModel.stan")
  name = StanModel@model_name
  stanmodels$FrenchGuianaModel= StanModel
  
  
  customFit <-function(model,data, 
                       iter = 5000,
                       chains = 4,
                       warmup = floor(iter/2), 
                       thin = 1,
                       seed = sample.int(.Machine$integer.max, 1),
                       init = 'random',
                       check_data = TRUE,
                       sample_file = NULL,
                       diagnostic_file = NULL,
                       verbose = FALSE,
                       algorithm = c("NUTS", "HMC", "Fixed_param"),
                       control = NULL,
                       include = TRUE,
                       cores = getOption("mc.cores", 1L),
                       open_progress = interactive() && !isatty(stdout()) &&
                         !identical(Sys.getenv("RSTUDIO"), "1"),
                       show_messages = TRUE,
                       ...){
    
    mdls <- model$name

    #NZones <- data$Ncategory
    # change this here
    
    # the variable pars should include the priors and the parameters of the model
    pars =  c(model$priors,
              NRegions=NRegions)
    newdata = append(data,pars)

    F <- rstan::sampling(stanmodels[[mdls]],
                         data= newdata,
                         iter = iter,
                         chains = chains,
                         warmup = warmup, 
                         thin = thin,
                         seed = seed,
                         init = init,
                         check_data = check_data,
                         sample_file = sample_file,
                         diagnostic_file = diagnostic_file,
                         verbose = verbose,
                         #algorithm = 'Fixed_param',
                         algorithm = algorithm,
                         control =  list(adapt_delta = 0.95), # increase adaptive delta
                         #control= control,
                         include = include,
                         cores = cores,
                         open_progress = open_progress,
                         show_messages = show_messages)
    
    out <-list(fit = F, data = data, model = model)
    
    return(out)
    
  }
  customModel <- function(S,
                          A=70,
                          ModelZone=as.array(rep(0,NRegions))){
    
    name = S@model_name
    priors <- list(ModelZone = ModelZone, A=A)
    
    me <- list(name = name,
               priors = priors)
    
    return(me)
    
  }   
  
  model = customModel(S = StanModel)
  F1 = customFit(data=data,model=model,chains=4,iter=20000,cores=4)
  return(F1)
}

