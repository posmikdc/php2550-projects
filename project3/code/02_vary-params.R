################################################################################
################################################################################
### Title: Vary Parameters for Sensitivity in Optimal Cluster Size 
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
### Varying Parameters for Sensitivity Analysis
################################################################################

# Fixed parameters
c1 <- 50 # Cost of sampling the first individual in a cluster
B <- 500 # Budget
sigmaSq <- 1 # Across-cluster variance

# Varying parameters
alphaVec <- c(0.25, 1.75) # Fixed effect
betaVec <- c(0.25, 1.75, 3) # Treatment effect
c2Vec <- c(2, 10, 40) # Cost of sampling the i-th individual in a cluster, i = 2, ..., n 
pVec <- c(0.25, 0.75) # Treatment assignment probability
gammaVec <- c(1.25, 2.5, 4) # Within-cluster variance

# Simulation parameters 
nSim <- 100 # Number of simulations
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
    resultsVary <- gen_var(
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
  "/Users/posmikdc/Documents/brown/classes/php2550-pda/php2550-projects/project3/output/df-varying-parameters.csv", 
  row.names = FALSE
)

################################################################################
### Create summary output
################################################################################

# Create summary output: Alpha 
dfAlpha <- dfResults %>%
  group_by(G, alpha) %>%
  summarise(
    var_beta = mean(var_beta, na.rm = TRUE)
  ) %>%
  select(var_beta, G, alpha)

min_var_alpha <- dfAlpha %>%
  group_by(alpha) %>%
  slice_min(var_beta, n = 1, with_ties = FALSE) %>%
  mutate(label = paste0("(", G, ", ", round(var_beta, 2), ")"))

p.alpha <- ggplot(dfAlpha, aes(x = G, y = var_beta, group = alpha, color = alpha)) +
  geom_line() +
  geom_point() +
  geom_point(data = min_var_alpha, aes(x = G, y = var_beta), 
             shape = 17, size = 3, color = "red") + 
  geom_text(data = min_var_alpha, aes(x = G, y = var_beta, label = label), 
            hjust = 1, vjust = -5, color = "red", size = 3.5) +
  labs(
    title = "Variance of the Estimator Across Different Fixed Effects",
    x = "Number of Clusters",
    y = "Variance of the Causal Estimator"
  ) +
  theme_minimal() +
  theme(legend.position = "none") +
  facet_wrap(~ alpha, scales = "free_y") 

# Save the plot as .png
ggsave(
  filename = "/Users/posmikdc/Documents/brown/classes/php2550-pda/php2550-projects/project3/img/alpha-plot.png",
  plot = p.alpha,
  width = 10, height = 5
)

# Create summary output: Beta
dfBeta <- dfResults %>%
  group_by(G, beta) %>%
  summarise(
    var_beta = mean(var_beta, na.rm = TRUE)
  ) %>%
  select(var_beta, G, beta)

min_var_beta <- dfBeta %>%
  group_by(beta) %>%
  slice_min(var_beta, n = 1, with_ties = FALSE) %>%
  mutate(label = paste0("(", G, ", ", round(var_beta, 2), ")"))

p.beta <- ggplot(dfBeta, aes(x = G, y = var_beta, group = beta, color = beta)) +
  geom_line() +
  geom_point() +
  geom_point(data = min_var_beta, aes(x = G, y = var_beta), 
             shape = 17, size = 3, color = "red") + 
  geom_text(data = min_var_beta, aes(x = G, y = var_beta, label = label), 
            hjust = 1, vjust = -5, color = "red", size = 3.5) +
  labs(
    title = "Variance of the Estimator Across Different Treatment Effects",
    x = "Number of Clusters",
    y = "Variance of the Causal Estimator"
  ) +
  theme_minimal() +
  theme(legend.position = "none") +
  facet_wrap(~ beta, scales = "free_y")

# Save the plot as .png
ggsave(
  filename = "/Users/posmikdc/Documents/brown/classes/php2550-pda/php2550-projects/project3/img/beta-plot.png",
  plot = p.beta,
  width = 10, height = 5
)

# Create summary output: p
dfP <- dfResults %>%
  group_by(G, p) %>%
  summarise(
    var_beta = mean(var_beta, na.rm = TRUE)
  ) %>%
  select(var_beta, G, p)

min_var_p <- dfP %>%
  group_by(p) %>%
  slice_min(var_beta, n = 1, with_ties = FALSE) %>%
  mutate(label = paste0("(", G, ", ", round(var_beta, 2), ")"))

p.p <- ggplot(dfP, aes(x = G, y = var_beta, group = p, color = p)) +
  geom_line() +
  geom_point() +
  geom_point(data = min_var_p, aes(x = G, y = var_beta), 
             shape = 17, size = 3, color = "red") + 
  geom_text(data = min_var_p, aes(x = G, y = var_beta, label = label), 
            hjust = 1, vjust = -5, color = "red", size = 3.5) +
  labs(
    title = "Variance of the Estimator Across Different Treatment Assignment Probabilities",
    x = "Number of Clusters",
    y = "Variance of the Causal Estimator"
  ) +
  theme_minimal() +
  theme(legend.position = "none") +
  facet_wrap(~ p, scales = "free_y")

# Save the plot as .png
ggsave(
  filename = "/Users/posmikdc/Documents/brown/classes/php2550-pda/php2550-projects/project3/img/p-plot.png",
  plot = p.p,
  width = 10, height = 5
)

# Create summary output: c2
dfC2 <- dfResults %>%
  group_by(G, c2) %>%
  summarise(
    var_beta = mean(var_beta, na.rm = TRUE)
  ) %>%
  select(var_beta, G, c2)

min_var_c2 <- dfC2 %>%
  group_by(c2) %>%
  slice_min(var_beta, n = 1, with_ties = FALSE) %>%
  mutate(label = paste0("(", G, ", ", round(var_beta, 2), ")"))

p.c2 <- ggplot(dfC2, aes(x = G, y = var_beta, group = c2, color = c2)) +
  geom_line() +
  geom_point() +
  geom_point(data = min_var_c2, aes(x = G, y = var_beta), 
             shape = 17, size = 3, color = "red") + 
  geom_text(data = min_var_c2, aes(x = G, y = var_beta, label = label), 
            hjust = 1, vjust = -5, color = "red", size = 3.5) +
  labs(
    title = "Variance of the Estimator Across Different Costs of Sampling Individuals",
    x = "Number of Clusters",
    y = "Variance of the Causal Estimator"
  ) +
  theme_minimal() +
  theme(legend.position = "none") +
  facet_wrap(~ c2, scales = "free_y")

# Save the plot as .png
ggsave(
  filename = "/Users/posmikdc/Documents/brown/classes/php2550-pda/php2550-projects/project3/img/c2-plot.png",
  plot = p.c2,
  width = 10, height = 5
)

# Create summary output: gamma
dfGamma <- dfResults %>%
  group_by(G, gamma) %>%
  summarise(
    var_beta = mean(var_beta, na.rm = TRUE)
  ) %>%
  select(var_beta, G, gamma)

min_var_gamma <- dfGamma %>%
  group_by(gamma) %>%
  slice_min(var_beta, n = 1, with_ties = FALSE) %>%
  mutate(label = paste0("(", G, ", ", round(var_beta, 2), ")"))

p.gamma <- ggplot(dfGamma, aes(x = G, y = var_beta, group = gamma, color = gamma)) +
  geom_line() +
  geom_point() +
  geom_point(data = min_var_gamma, aes(x = G, y = var_beta), 
             shape = 17, size = 3, color = "red") + 
  geom_text(data = min_var_gamma, aes(x = G, y = var_beta, label = label), 
            hjust = 1, vjust = -5, color = "red", size = 3.5) +
  labs(
    title = "Variance of the Estimator Across Different Within-Cluster Variances",
    x = "Number of Clusters",
    y = "Variance of the Causal Estimator"
  ) +
  theme_minimal() +
  theme(legend.position = "none") +
  facet_wrap(~ gamma, scales = "free_y")

# Save the plot as .png
ggsave(
  filename = "/Users/posmikdc/Documents/brown/classes/php2550-pda/php2550-projects/project3/img/gamma-plot.png",
  plot = p.gamma,
  width = 10, height = 5
)



