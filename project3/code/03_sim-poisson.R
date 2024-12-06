################################################################################
################################################################################
### Title: Simulate Optimal Cluster Size for Poisson Data
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

# Set-Up =======================================================================
source("/Users/posmikdc/Documents/brown/classes/php2550-pda/php2550-projects/project3/code/00_func.R")

################################################################################
### Optimal Study Design in Poisson Case 
################################################################################

# Set seed
set.seed(321)

# Simulate the results for different G 
nSim <- 50
G <- seq(2, 100, by = 1)

# Initialize an empty data frame to store results
dfPoi <- data.frame(G = numeric(0), var_beta = numeric(0))

for (g in G){
  # Calculate results for each iteration
  resultsPoi <- gen_varPoi(G = g, alpha = 1, beta = 0.5, 
    c1 = 50, c2 = 2, B = 500, 
    gammaSq = 1, sigmaSq = 1.5, 
    p = 0.5, nSim = nSim)
  # Bind results to a data frame
  dfPoi <- rbind(dfPoi, data.frame(G = g, var_beta = resultsPoi))
}

# Find the point of minimum variance 
varPoi.min <- dfPoi[which.min(dfPoi$var_beta), ] 

# Plot the results
p.varpoi <- ggplot(dfPoi, aes(x = G, y = var_beta)) +
  geom_line() +
  geom_point() +
  geom_point(aes(x = varPoi.min$G, y = varPoi.min$var_beta), 
             color = "red", size = 3) +
  geom_text(aes(x = varPoi.min$G, y = varPoi.min$var_beta, 
                label = paste0("(", round(varPoi.min$var_beta, 3), ")")),
            hjust = 0.1, vjust = -7, color = "red") + # Add the label
  labs(title = "Variance of the Estimator Across Number of Clusters (Poisson Case)",
       x = "Number of Clusters",
       y = "Variance of the Causal Estimator") +
  theme_minimal()

# Save the plot as .png 
ggsave("/Users/posmikdc/Documents/brown/classes/php2550-pda/php2550-projects/project3/img/variance-sim-poisson.png", 
  plot = p.varpoi, width = 10, height = 5)

################################################################################
### Sensitivity of the Poisson case to varying c_2
################################################################################

# Fixed parameters
c1 <- 50 # Cost of sampling the first individual in a cluster
B <- 500 # Budget
sigmaSq <- 1 # Across-cluster variance

# Varying parameters
alphaVec <- 1 # Fixed effect
betaVec <- 0.5 # Treatment effect
c2Vec <- c(2, 10, 40) # Cost of sampling the i-th individual in a cluster, i = 2, ..., n 
pVec <- 0.5 # Treatment assignment probability
gammaVec <- 1 # Within-cluster variance

# Simulation parameters 
nSim <- 50 # Number of simulations
G <- seq(2, 100, by = 1) # Number of clusters

# Create all combinations of the varying parameters
dfVary <- expand.grid(
  alpha = alphaVec,
  beta = betaVec,
  c2 = c2Vec,
  p = pVec,
  gamma = gammaVec,
  sigma = sigmaSq,
  B = B,
  c1 = c1
)

# Initialize an empty data frame to store results
dfResults <- data.frame()

# Loop over each row of the parameter grid
for (n in 1:nrow(dfVary)) {
  # Extract parameters for the current iteration
  params <- dfVary[n, ]
  
  for (i in seq_along(G)) {
    # Set g 
    g <- G[i]
    
    # Calculate results for each iteration
    resultsVary <- gen_varPoi(
      G = g, 
      alpha = params$alpha, 
      beta = params$beta, 
      c1 = params$c1, 
      c2 = params$c2, 
      B = params$B, 
      gammaSq = params$gamma, 
      sigmaSq = params$sigma, 
      p = params$p, 
      nSim = nSim
    )
    
    # Append results to dfResults
    dfResults <- rbind(
      dfResults,
      data.frame(
        G = g, 
        var_beta = resultsVary, 
        alpha = params$alpha, 
        beta = params$beta, 
        c1 = params$c1, 
        c2 = params$c2, 
        B = params$B, 
        gamma = params$gamma, 
        sigma = params$sigma, 
        p = params$p
      )
    )
  }
}

# Save the long-format results as .csv
write.csv(
  dfResults, 
  "/Users/posmikdc/Documents/brown/classes/php2550-pda/php2550-projects/project3/output/varyc2-sim-poisson.csv", 
  row.names = FALSE
)

# Create summary output: c2
dfPoiC2 <- dfResults %>%
  group_by(G, c2) %>%
  summarise(
    var_beta = mean(var_beta, na.rm = TRUE)
  ) %>%
  select(var_beta, G, c2)

min_var_c2 <- dfPoiC2 %>%
  group_by(c2) %>%
  slice_min(var_beta, n = 1, with_ties = FALSE) %>%
  mutate(label = paste0("(", G, ", ", round(var_beta, 2), ")"))

p.c2 <- ggplot(dfPoiC2, aes(x = G, y = var_beta, group = c2, color = c2)) +
  geom_line() +
  geom_point() +
  geom_point(data = min_var_c2, aes(x = G, y = var_beta), 
             shape = 17, size = 3, color = "red") + 
  geom_text(data = min_var_c2, aes(x = G, y = var_beta, label = label), 
            hjust = 1, vjust = -5, color = "red", size = 3.5) +
  labs(
    title = "Variance of the Estimator (Poisson) Across Different Individual Costs",
    x = "Number of Clusters",
    y = "Variance of the Causal Estimator"
  ) +
  theme_minimal() +
  theme(legend.position = "none") +
  facet_wrap(~ c2, scales = "free_y") 

# Save the plot as .png
ggsave(
  filename = "/Users/posmikdc/Documents/brown/classes/php2550-pda/php2550-projects/project3/img/poisson-c2-plot.png",
  plot = p.c2,
  width = 10, height = 5
)




