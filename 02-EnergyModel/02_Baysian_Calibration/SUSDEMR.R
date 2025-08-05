#------------------------------------------------------
#SUSDEM:
# STOCHASTIC URBAN SCALE DOMESTIC ENERGY MODEL
# BAYESIAN REGRESSION, CALIBRATION, AND DECISION-MAKING
#------------------------------------------------------
# Andre Neto-Bradley 2021
# Adapted from Original MATLAB (Adam Booth and Ruchi Choudhary, 2013)
# Energy Efficient Cities Initiative (www.eeci.cam.ac.uk)
#-------------------------------------------------------

# OVERVIEW--------------------------------------------------------------
# INPUTS: Energy Performance Certificate / RdSAP inputs
#         for sample dwellings
#         Weather data - monthly temperatures and irradition
#         Energy intensity posteriors from Bayesian regression
#         (See Bayesian Hierarchical Model EnergyFlex files)
#
# OUTPUTS: End-use energy demands
#          Utilities (installation costs; lifetime financial savings;
#         CO2 emissions savings; thermal comfort improvement)
#--------------------------------------------------------------
  
# LOAD SUSDEM Classes and Functions ######## 
# IMPORT INPUTS FOR CONSTRUCTION/DESIGN PARAMETERS
# FROM EPC INPUT DATA
# LOAD WEATHER DATA FOR REGION

source("SUSDEM_fns.R")
source("SUSDEM_Cls.R")

#load('SalfordEPCMarch2012.mat')
#load('SalfordWeather.mat')

haringey_sample <- read.csv("Haringey_Test_Sample.csv")
designParam <- haringey_sample


# N = nrow(designParam) #N = number of sample dwellings # changed this because size(designParam,1) is not working in R
# dwellingtype = designParam$DwellingType # matrix for Dwelling type
# dwellingposition = designParam$DwellingPosition #matrix for Dwelling position
# dwellingage = designParam$AgeBandCode #matrix for Age band of dwelling
# 
# groundfloorareas = designParam$GroundFloorArea #matrix for Ground floor area
# firstfloorareas = designParam$FirstFloorArea #matrix for First floor area
# secondfloorareas = designParam$SecondFloorArea #matrix for Second floor area
# totalfloorareas = groundfloorareas+firstfloorareas+secondfloorareas #matrix for Total floor area


#SPECIFY NUMBER OF CLUSTERS
# C = length(unique())# C = number of building classes (i.e. clusters) for housing stock analysis; Five for case study


#------------------------------------------------
# LOAD POSTERIORS OF ENERGY INTENSITY
# FROM BAYESIAN REGRESSION
#------------------------------------------------
# FOR GAMMA POSTERIORS
# Original SUSDEM used results from Bayesian regression analysis with a mixture of two priors,
# including errors in variables model
# See paper: Booth, Choudhary, and Spiegelhalter (2013), "A hierarchical
# Bayesian framework for calibrating micro-level models with macro-level
# data", Journal of Building Performance Simulation, DOI: 10.1080/19401493.2012.723750
#
# ORIGINAL SECTION HAS BEEN REMOVED AND REPLACED WITH HIERARCHICAL MODEL FOR ESTIMATING
# LOCAL ENERGY INTENSITY.

# specify number of samples for the Bayesian calibration
samples = 10; 
# i.e. number of simulations and number of samples from posteriors of energy intensity

# RUN ANALYSIS FOR EACH BUILDING CLASS
# parpool(C) %run parallel analysis for each cluster
#for i = 1:C

# DATA LOADING #####
# We now need to load the relevant data and carry out some data cleaning before we can prepare the input files for the bayesian calibration.
# - We need to load the posteriors from the energy intensity estimation step (Step 2 on EnergyFlex)
# - We need to load relevant weather data for the local area
# For the test case of Haringey the LA number is E09000014

library(data.table)
library(dplyr)


# Load the EUI posteriors from previous EFlex Module
load(file='haringey_prior_dist.rda')

# Load relevant weather data (this needs monthly temperture and irradiance data)
local_weather <- read.csv("Londmon_dat.csv")
weather_data <- Weather_input(local_weather,51.5)
weather_IO <- weather_data$weather

# OPTION 1: The below loads the raw RdSAP data for Haringey we used and processed it using the custom 
# import function. If you are only interested in testing the calibration itself load the pre-processed 
# data in OPTION 2 below.
  
# NEEDS DEBUGGING STILL
# haringey_sample <- ParityData("Haringey_Raw_Data.csv")

# OPTION 2: Load pre-processed sample of data for testing purposes.
haringey_sample <- read.csv("Haringey_Test_Sample.csv")

# In both cases you will have NAs in place for non-relevant values (e.g. a single floor flat 
# would have 'SecondFloorArea' marked as NA). You need to replace these NAs with zeros to allow the the function to run.



RDSAP_NA_Cleanser <- function(designParam){
  designParam$SecondFloorHeight[which(is.na(designParam$SecondFloorHeight))] <- 0
  designParam$FirstFloorHeight[which(is.na(designParam$FirstFloorHeight))] <- 0
  designParam$SecondFloorArea[which(is.na(designParam$SecondFloorArea))] <- 0
  designParam$FirstFloorArea[which(is.na(designParam$FirstFloorArea))] <- 0
  designParam$SecondFloorPerimeter[which(is.na(designParam$SecondFloorPerimeter))] <- 0
  designParam$FirstFloorPerimeter[which(is.na(designParam$FirstFloorPerimeter))] <- 0
  designParam$RoofInsulation[which(is.na(designParam$RoofInsulation))] <- 0
  designParam$LELpercentage[which(is.na(designParam$LELpercentage))] <- 0
  
  return(designParam)
}

haringey_sample_clean <- RDSAP_NA_Cleanser(haringey_sample)
#-----------------------------------------------------
# CALCULATE INPUTS AND OUTPUTS FOR BAYESIAN CALIBRATION
#-----------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# xf: Design points corresponding to field trials
# xc,tc: Design points corresponding to computer trials
# (tc is calibration parameters, xc is known parameters)
# yf: Response from field experiments
# yc: Response from computer simulations

# IO_out can take a while to run - 20-40 seconds not unusual.
IO_out = IO(samples, filter(haringey_sample_clean,group==13), haringey_prior_dist[which(haringey_prior_dist$group==13),2], weather_data$weather)

xf = as.data.frame(IO_out[1:(samples)])
yf = as.data.frame(IO_out[(1+samples):(2*(samples))])
xc = as.data.frame(IO_out[(1+(2*samples)):(3*(samples))])
yc = as.data.frame(IO_out[(1+(3*samples)):(4*(samples))])
tc= data.frame(IO_out[(1+(4*samples)):(5*(samples))], # This is the prior for Heating Set Point
                #IO_out[(1+(5*samples)):(6*(samples))], # This is the the prior Fraction Heated
                IO_out[(1+(6*samples)):(7*(samples))]#,  # This is the prior for Infiltration 
                #IO_out[(1+(7*samples)):(8*(samples))], # This is the prior for the Heating System efficiency
                #IO_out[(1+(8*samples)):(9*(samples))], # This is the prior for the Window to Wall Ratio
                #IO_out[(1+(9*samples)):(10*(samples))]  # This is the Double Glazing U-value
  )



#ALT xc and xf

# xc$`IO_out[(1 + (2 * samples)):(3 * (samples))]` <- rnorm(100,1,1)
# xf$`IO_out[1:(samples)]`<-rnorm(100,1,1)
#xf <- data.frame(xf[sample(nrow(xf),10),])
#yf <- data.frame(yf[sample(nrow(yf),10),])

#------------------------
# RUN BAYESIAN CALIBRATION
#------------------------
# See paper: Booth, Choudhary, and Spiegelhalter (2013), "Handling
# uncertainty in housing stock models", Building and Environment, DOI: 10.1016/j.buildenv.2011.08.016
library(rstan) # change to cmdstan
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Return posterior realizations and params structure
# pvals: samples from joint posterior distribution of calibration params
# params: structure with info about parameters
stan_post_cmdtest = standriver(yf,yc,xf,xc,tc);
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Basic extract
calibration_outputs <- extract(stan_post_cmdtest)


# plots
library(bayesplot)
mcmc_trace(as.array(stan_post_cmdtest), pars = params_to_plot)
mcmc_dens_overlay(as.array(stan_post_cmdtest), pars = params_to_plot)
mcmc_rhat(rhat(stan_post_cmdtest))
mcmc_neff(neff_ratio(stan_post_cmdtest))
mcmc_acf(as.array(stan_post_cmdtest), pars = c("lambda_eta", "beta_eta[2]", "lp__"))

# NEED TO REVIEW PACKAGE VERSIONS AS NOT WORKING
# library(posterior)
# calibration_outputs <- stan_post_cmdtest$draws_df()

# eta_mu <- mean(yc[,1], na.rm = TRUE) # mean value
# eta_sd <- sd(yc[,1], na.rm = TRUE) # standard deviation
# 
# 
# y_pred <- fitsamples$y_pred * eta_sd + eta_mu 
# 
# 
# buildingData(i).pvals = pvals
# buildingData(i).params = params

#---------------------------------------------
# RUN RETROFIT ANALYSIS USING CALIBRATED MODEL
#---------------------------------------------
# See paper: Booth and Choudhary (2013), "Decision making under uncertainty
# in the retrofit analysis of the UK housing stock: Implications for the
# Green Deal", Energy and Buildings, DOI: 10.1016/j.enbuild.2013.05.014

#[Demand, Utility] = retrofitAnalysis(pvals, params, samples, buildingData(i).inputs, texts, weather);

#buildingData(i).Demand = Demand #End-use energy demands
#buildingData(i).Utility = Utility #Utilities (installation costs; lifetime financial savings; CO2 emissions
                                               #savings; thermal comfort improvement)


