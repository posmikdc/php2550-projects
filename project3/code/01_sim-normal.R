################################################################################
################################################################################
### Title: Simulate Optimal Cluster Size for Normal Data
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
### Optimal Study Design in Normal Case 
################################################################################

# Simulate the results for different G 
nSim <- 200
G <- seq(2, 100, by = 1)

# Initialize an empty data frame to store results
dfSumm <- data.frame(G = numeric(0), var_beta = numeric(0))

for (g in G){
  # Calculate results for each iteration
  results <- gen_var(G = g, alpha = 1, 
    beta = 0.5, c1 = 50, c2 = 2, 
    B = 500, gammaSq = 1, 
    sigmaSq = 1.5, p = 0.5, 
    nSim = nSim)
  # Bind results to a data frame
  dfSumm <- rbind(dfSumm, data.frame(G = g, var_beta = results))
}

# Find the point of minimum variance 
var.min <- dfSumm[which.min(dfSumm$var_beta), ] 

# Plot the results
p.var <- ggplot(dfSumm, aes(x = G, y = var_beta)) +
  geom_line() +
  geom_point() +
  geom_point(aes(x = var.min$G, y = var.min$var_beta), 
             color = "red", size = 3) + # Highlight the minimum variance point
  geom_text(aes(x = var.min$G, y = var.min$var_beta, 
                label = paste0("(", round(var.min$var_beta, 3), ")")),
            hjust = 0.1, vjust = -7, color = "red") + # Add the label
  labs(title = "Variance of the Estimator Across Number of Clusters",
       x = "Number of Clusters",
       y = "Variance of the Causal Estimator") +
  theme_minimal()

# Save the plot as .png 
ggsave("/Users/posmikdc/Documents/brown/classes/php2550-pda/php2550-projects/project3/img/variance-sim.png", 
  plot = p.var, width = 10, height = 5)
