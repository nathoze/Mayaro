# Plot figures and get numbers 

# source scripts in the folder ----
source('runMCMC.R')
source('compute_DIC_Guyane_Predictors.R')
source('Infections.R')
source('Figure1.R')
source('plot_FOI_logscale.R')
source('ContingencyTables.R')
source('EstimationParameters.R')

# Load Data and Parameters ----

source('Load.R')

# Run MCMC
# takes from a few hours -> 1 day
F1=runMCMC()


# compute the deviance information criterion (DIC)
compute_DIC_Guyane_Predictors(F1)

# Make a data.frame with the infection probability of each individuals
Inf.Prob = Infections(F1)


# Figures ----
# Figure 1
#1A
figure1A(data)

#1B
figure1B(data)

 
# Figure 2 -----
# Figure 2A
plot_FOI_logscale(F1)
# 2B 
source('RiskFactorsFOI.R')


# Compare the seroneutralisation results, the luminex, and the model prediction
ContingencyTables(Inf.Prob = Inf.Prob)


# Numbers ----
# Risk Factors Parameters and Antibody model Parameters
EstimationParameters()

