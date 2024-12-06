################################################################################
################################################################################
### Title: Functions for Simulation Project
### Author: Daniel C Posmik (daniel_posmik@brown.edu)
### Date: 2024-12-05
################################################################################
################################################################################

# Libraries ====================================================================
library(knitr)
library(kableExtra)
library(tidyverse)
library(lme4)
library(purrr)

################################################################################
### Function: gen_beta 
################################################################################

#` Define the function to generate the beta estimates
#` 
#` @param G Number of clusters
#` @param alpha Fixed effect 
#` @param beta Treatment effect 
#` @param c1 Cost of sampling the first individual in a cluster 
#` @param c2 Cost of sampling each additional individual in a cluster
#` @param B Total budget
#` @param gammaSq Variance of the within-cluster noise 
#` @param sigmaSq Variance of the between-cluster noise
#` @param p Probability of treatment assignment
#` @return betaHat Estimated treatment effect
gen_beta <- function(G, alpha, beta, c1, c2, B, gammaSq, sigmaSq, p){
# Initialize variables
xAll <- numeric()
yAll <- numeric()
id <- numeric()

# Generate data over clusters
for (g in 1:G) {
  # Calculate the number of individuals in a cluster
  R <- (B/g - c1)/(c2) + 1 
  R <- max(floor(R),1)

  # Generate the treatment vector for a cluster
  x <- rbinom(1, 1, p)
  xVec <- rep(x, R)
      
  # Generate cluster-level effects and outcomes
  mu.i0 <- alpha + beta * x
  mu.i <- rnorm(R, mean = mu.i0, sd = sqrt(gammaSq))
  y.i <- rnorm(R, mean = mu.i, sd = sqrt(sigmaSq))
      
  # Append generated data
  xAll <- c(xAll, xVec)
  yAll <- c(yAll, y.i)
  id <- c(id, rep(g, R))
}

# Create a data frame
dfSim <- data.frame(G = id, x = xAll, y = yAll)
dfSim

# Fit the model
lmerMod <- lme4::lmer(y ~ x + (1 | G), data = dfSim)

# Extract the beta coefficient
betaHat <- fixef(lmerMod)[2]
return(betaHat)
}

################################################################################
### Function: gen_var 
################################################################################

#` Define the function to generate the variance of the beta estimates
#`
#` @param G Number of clusters
#` @param alpha Fixed effect
#` @param beta Treatment effect
#` @param c1 Cost of sampling the first individual in a cluster
#` @param c2 Cost of sampling each additional individual in a cluster
#` @param B Total budget
#` @param gammaSq Variance of the within-cluster noise
#` @param sigmaSq Variance of the between-cluster noise
#` @param p Probability of treatment assignment
#` @param nSim Number of simulations
#` @return var_beta Variance of the beta estimates
gen_var <- function(G, alpha, beta, c1, c2, B, gammaSq, sigmaSq, p, nSim){
  # Replicate the simulation nSim times
  betaHat <- replicate(nSim, gen_beta(G, alpha = alpha, beta = beta, 
    c1 = c1, c2 = c2, B = B, 
    gammaSq = gammaSq, sigmaSq = sigmaSq, 
    p = p))

  # Calculate the variance of the beta estimates
  var_beta <- var(betaHat %>% na.omit)

  # Return the variance
  return(var_beta)
}

################################################################################
### Function: gen_varPoi 
################################################################################

#` Define the function to generate the variance of the beta estimates for Poisson
#`
#` @param G Number of clusters
#` @param alpha Fixed effect
#` @param beta Treatment effect
#` @param c1 Cost of sampling the first individual in a cluster
#` @param c2 Cost of sampling each additional individual in a cluster
#` @param B Total budget
#` @param gammaSq Variance of the within-cluster noise
#` @param sigmaSq Variance of the between-cluster noise
#` @param p Probability of treatment assignment
#` @param nSim Number of simulations
#` @return var_beta Variance of the beta estimates
gen_varPoi <- function(G, alpha, beta, c1, c2, B, gammaSq, sigmaSq, p, nSim){
  # Replicate the simulation nSim times
  betaHat <- replicate(nSim, gen_betaPoi(G, alpha = alpha, beta = beta, 
                                      c1 = c1, c2 = c2, B = B, 
                                      gammaSq = gammaSq, sigmaSq = sigmaSq, 
                                      p = p))
  
  # Calculate the variance of the beta estimates
  var_beta <- var(betaHat %>% na.omit)
  
  # Return the variance
  return(var_beta)
}

################################################################################
### Function: gen_betaPoi 
################################################################################

#` Define the function to generate the beta estimates from Poisson distirbution
#`
#` @param G Number of clusters
#` @param alpha Fixed effect
#` @param beta Treatment effect
#` @param c1 Cost of sampling the first individual in a cluster
#` @param c2 Cost of sampling each additional individual in a cluster
#` @param B Total budget
#` @param gammaSq Variance of the within-cluster noise
#` @param sigmaSq Variance of the between-cluster noise
#` @param p Probability of treatment assignment
gen_betaPoi <- function(G, alpha, beta, c1, c2, B, gammaSq, sigmaSq, p){
  # Initialize variables
  xAll <- numeric()
  yAll <- numeric()
  id <- numeric()

  # Generate data over clusters
  for (g in 1:G) {
    # Calculate the number of individuals in a cluster
    R <- (B/g - c1)/(c2) + 1 
    R <- max(floor(R),1)

    # Generate the treatment vector for a cluster
    x <- rbinom(1, 1, p)
    xVec <- rep(x, R)
      
    # Generate cluster-level effects and outcomes
    mu.i0 <- alpha + beta * x
    log_mu.i <- rnorm(R, mean = mu.i0, sd = sqrt(gammaSq))
    mu.i <- exp(log_mu.i)
    y.i <- rpois(R, lambda = R * mu.i)
      
    # Append generated data
    xAll <- c(xAll, xVec)
    yAll <- c(yAll, y.i)
    id <- c(id, rep(g, R))
  }

  # Create a data frame
  dfSim <- data.frame(G = id, x = xAll, y = yAll)

  # Fit the model
  lmerMod <- lme4::glmer(y ~ x + (1 | G), 
                         data = dfSim, 
                         family = poisson)

  # Extract the beta coefficient
  betaHat <- fixef(lmerMod)[2]
  return(betaHat)
}

################################################################################
### Function: sample_strategy 
################################################################################

#` Define the function to generate the optimal sample strategy
#`
#` @param rho Intra-cluster correlation
#` @param c1 Cost of sampling the first individual in a cluster
#` @param c2 Cost of sampling each additional individual in a cluster
#` @param B Total budget
#` @return df Data frame with the optimal sample strategy
sample_strategy <- function(rho, c1, c2, B){
  # Calculate optimal number of samples in a given cluster 
  ROpt <- sqrt((1-rho)/(rho))*sqrt(c1/c2) + 1
  ROpt <- floor(ROpt)
  
  # Calculate optimal number of clusters
  GOpt <- B/((ROpt - 1)*c2 + c1) 
  GOpt <- floor(GOpt)
  
  # Calculate the variance of the treatment effect estimator
  var_beta <- 4*(rho + (1-rho)/ROpt)/(ROpt*GOpt)
  
  # Return the results
  df <- data.frame(rho = rho, 
                   c2 = c2, 
                   ROpt = ROpt, GOpt = GOpt, 
                   var_beta = var_beta)
  return(df)
}

