## ----set-up, eval=TRUE--------------------------------------------------------
# Example: create Beta priors from TEK counts (k out of n informants say 'harvest causes decline')
tek_k <- 7   # informants saying 'decline'
tek_n <- 10  # total informants
# Beta posterior (Laplace smoothing): Beta(k+1, n-k+1)
alpha <- tek_k + 1
beta  <- tek_n - tek_k + 1

# draw Monte Carlo samples for that probability
set.seed(123)
p_samples <- rbeta(1000, alpha, beta)

# Inspect distribution
library(ggplot2)
qplot(p_samples, geom = "density") + ggtitle("TEK-derived prior for P(decline | harvest)")

## ----sim-data, eval=TRUE------------------------------------------------------
set.seed(42)
n_inf <- 20
# create a data frame
informants <- data.frame(
  id = 1:n_inf,
  # binomial distribution with parameters n, size and prob
  says_decline = rbinom(n = n_inf, size = 1, prob = 0.6),
  # for abundance, take a sample of the specified size from the elements of x using either with or without replacement.
  abundance = sample(x= c("High","Medium","Low"), 
                     size = n_inf, 
                     replace=TRUE, 
                     prob = c(0.4,0.35,0.25)),
  # take a sample again for confidence
  confidence = sample(3:5, 
                      size = n_inf, 
                      replace=TRUE,
                      prob=c(0.2,0.6,0.2))
)

knitr::kable(head(informants, 8))

## ----bn-mc-workflow, eval=requireNamespace("bnlearn", quietly = TRUE) && requireNamespace("gRain", quietly = TRUE)----
if (requireNamespace("bnlearn", quietly = TRUE) && requireNamespace("gRain", quietly = TRUE)) {
  library(bnlearn); library(gRain); library(purrr)
} else {
  message("Skipping BN + gRain workflow: 'bnlearn' and/or 'gRain' not available")
}

# define simple TEK priors (ensure alpha/beta are available during knitting)
tek_k <- 7   # number of informants saying 'decline'
tek_n <- 10  # total informants
alpha <- tek_k + 1
beta  <- tek_n - tek_k + 1

# define structure
model_string <- "[Decision][Abundance|Decision][Impact|Abundance:Decision]"
net <- model2network(model_string)

# a single Monte Carlo iteration (pseudo-code)
sample_one <- function(){
  # sample p(Abundance=Low | Decision=Harvest) from TEK-derived Beta
  p_decline_if_harvest <- rbeta(1, alpha, beta)

  # create CPTs (values must be in the order expected by gRain)
  # Example CPTs (toy values)
  cpt_decision <- cptable(~Decision, values=c(0.5,0.5), levels=c("Harvest","Conserve"))
  # Abundance|Decision: P(High|Harvest)=1 - p_decline_if_harvest, P(High|Conserve)=0.9, etc.
  cpt_abundance <- cptable(~Abundance|Decision, values=c(1-p_decline_if_harvest, p_decline_if_harvest, 0.9, 0.1), levels=c("High","Low"))
  # Impact|Abundance: simple mapping
  cpt_impact <- cptable(~Impact|Abundance:Decision, values=c(0.95,0.05, 0.2,0.8), levels=c("Recovery","Decline"))

  plist <- compileCPT(list(cpt_decision, cpt_abundance, cpt_impact))
  grain_net <- grain(plist)
  query <- querygrain(grain_net, nodes=c("Impact"), type="marginal")
  return(query)
}

# run Monte Carlo many times (here, do 500 iterations)
results <- map_dfr(1:500, ~as.data.frame(sample_one()))

# summarize distributions for decisions (extract expected probability of Recovery/Decline for each decision)



## ----agg-simple, eval=TRUE----------------------------------------------------
# Binary: counts for 'says_decline'
k <- sum(informants$says_decline)
n <- nrow(informants)
alpha <- k + 1; beta <- n - k + 1
cat(sprintf("P(decline) prior ~ Beta(%d, %d) -> mean = %.2f\n", alpha, beta, alpha/(alpha+beta)))

# Multinomial: counts for abundance categories -> Dirichlet
ab_counts <- table(informants$abundance)
ab_dirichlet_alpha <- as.numeric(ab_counts) + 1
cat("Abundance counts:\n")
print(ab_counts)

## ----agg-weighted, eval=TRUE--------------------------------------------------
# Use confidence as weight (simple rescaling to pseudo-counts)
weights <- informants$confidence / sum(informants$confidence)
# Weighted count for binary outcome
weighted_k <- sum(informants$says_decline * weights) * nrow(informants)
# Convert to pseudo-counts (floor/round) if needed for Dirichlet/Beta
wk <- round(weighted_k)
walpha <- wk + 1; wbeta <- n - wk + 1
cat(sprintf("Weighted Beta prior approx: Beta(%d, %d) -> mean = %.2f\n", walpha, wbeta, walpha/(walpha+wbeta)))

# Weighted multinomial: use weighted frequencies
weighted_ab <- tapply(weights, informants$abundance, sum)
weighted_ab_counts <- round(weighted_ab * nrow(informants))
cat("Weighted abundance pseudo-counts:\n")
print(weighted_ab_counts)

## ----agg-qualitative, eval=TRUE-----------------------------------------------
# Map High/Medium/Low to pseudo-counts reflecting stronger beliefs
map_counts <- c(High=5, Medium=3, Low=1)
qual_counts <- sapply(c("High","Medium","Low"), function(lvl) sum(map_counts[lvl] * (informants$abundance == lvl)))
cat("Mapped pseudo-counts for abundance (qualitative -> counts):\n")
print(qual_counts)
# Convert to Dirichlet alpha
qual_alpha <- qual_counts + 1
print(qual_alpha)

## ----generate-simulated-tek-data, eval=TRUE-----------------------------------
# Set random seed for reproducibility
set.seed(42)
# Set number of informants
n_informants <- 50

# Create a data frame with informant characteristics
# Create informant IDs from 1 to n_informants
tek_survey_data <- data.frame(
  informant_id = 1:n_informants,
  # Generate years of experience from normal distribution: mean=25, sd=15, minimum=5
  # rnorm: draw from normal distribution with specified mean and standard deviation
  # pmax: ensure no values below 5
  years_experience = pmax(5, round(rnorm(n_informants, mean = 25, sd = 15))),
  # Generate primary use category: subsistence, commercial, or cultural
  # sample: take a sample of the specified size from the elements without replacement
  primary_use = sample(
    x = c("Subsistence", "Commercial", "Cultural"),
    size = n_informants,
    replace = TRUE,
    prob = c(0.5, 0.2, 0.3)
  ),
  # Initialize columns for responses (will fill in loops below)
  says_harvest_declines = NA,
  abundance_status = NA,
  management_recommendation = NA,
  confidence = NA
)

# Fill binary outcome: probability of saying "harvesting causes decline" increases with experience
# Loop through each informant
for (i in 1:n_informants) {
  # Calculate probability based on years of experience (ranges from ~0.3 to 0.7)
  p_decline <- 0.3 + (tek_survey_data$years_experience[i] / 100) * 0.4
  # rbinom: draw from binomial distribution with size=1 (binary outcome) and probability p_decline
  tek_survey_data$says_harvest_declines[i] <- rbinom(1, size = 1, prob = p_decline)
}

# Fill categorical abundance assessment: correlated with perception of harvest decline
# Create a matrix of probabilities for abundance (High, Medium, Low) given each response
# rows: "says decline"=1, "does not say decline"=0; columns: High, Medium, Low probabilities
abundance_probs <- matrix(
  c(0.3, 0.4, 0.3,  # if says decline: P(High)=0.3, P(Medium)=0.4, P(Low)=0.3
    0.5, 0.3, 0.2), # if does not say decline: P(High)=0.5, P(Medium)=0.3, P(Low)=0.2
  nrow = 2,
  byrow = TRUE
)

# Loop through each informant to assign abundance status
for (i in 1:n_informants) {
  # Select probability row based on their answer about decline (add 1 to convert 0/1 to row 1/2)
  row_idx <- tek_survey_data$says_harvest_declines[i] + 1
  # sample: select one abundance category with probabilities from the appropriate row
  tek_survey_data$abundance_status[i] <- sample(
    x = c("High", "Medium", "Low"),
    size = 1,
    prob = abundance_probs[row_idx, ]
  )
}

# Fill management recommendation: correlated with perception of decline and current abundance
# Loop through each informant
for (i in 1:n_informants) {
  # If they say decline AND perceive low abundance: recommend conservation
  if (tek_survey_data$says_harvest_declines[i] == 1 && 
      tek_survey_data$abundance_status[i] == "Low") {
    # sample: select one management option with specified probabilities
    tek_survey_data$management_recommendation[i] <- sample(
      c("Conserve strictly", "Harvest with guidelines", "Let it recover, then harvest"),
      size = 1,
      prob = c(0.6, 0.3, 0.1)
    )
  # Else if they perceive high abundance: less restrictive recommendations
  } else if (tek_survey_data$abundance_status[i] == "High") {
    tek_survey_data$management_recommendation[i] <- sample(
      c("Harvest freely", "Harvest with guidelines", "Conserve strictly"),
      size = 1,
      prob = c(0.4, 0.4, 0.2)
    )
  # Otherwise: balanced distribution of recommendations
  } else {
    tek_survey_data$management_recommendation[i] <- sample(
      c("Harvest freely", "Harvest with guidelines", "Conserve strictly", "Let it recover, then harvest"),
      size = 1,
      prob = c(0.2, 0.4, 0.25, 0.15)
    )
  }
}

# Fill confidence scores: higher for more experienced informants
# Loop through each informant
for (i in 1:n_informants) {
  # Calculate probability of high confidence based on years of experience
  p_high_conf <- 0.3 + (tek_survey_data$years_experience[i] / 100) * 0.3
  # Assign confidence based on this probability
  # runif: draw from uniform distribution (0 to 1)
  if (runif(1) < p_high_conf) {
    # High confidence: choose between 4 or 5
    tek_survey_data$confidence[i] <- sample(4:5, 1, prob = c(0.4, 0.6))
  } else {
    # Lower confidence: choose between 2 or 3
    tek_survey_data$confidence[i] <- sample(2:3, 1, prob = c(0.4, 0.6))
  }
}

# Display first 10 rows of the dataset
cat("TEK Survey Dataset - First 10 rows:\n\n")
print(knitr::kable(head(tek_survey_data, 10)))

# Print summary statistics
cat("\n\nSummary of TEK Survey Data (n=", n_informants, ")\n")
cat("================================\n")
# mean: calculate average; round to 1 decimal place
# min/max: find minimum and maximum values
cat("Years of experience: Mean =", round(mean(tek_survey_data$years_experience), 1), 
    ", Range =", min(tek_survey_data$years_experience), "-", 
    max(tek_survey_data$years_experience), "\n")
# table: count frequency of each category
cat("Primary use breakdown:\n")
print(table(tek_survey_data$primary_use))
cat("\n\nBinary outcome (says harvesting declines):\n")
print(table(tek_survey_data$says_harvest_declines))
cat("\n\nAbundance status:\n")
print(table(tek_survey_data$abundance_status))
cat("\n\nManagement recommendations:\n")
print(table(tek_survey_data$management_recommendation))
cat("\n\nConfidence scores:\n")
print(table(tek_survey_data$confidence))

## ----use-simulated-for-aggregation, eval=TRUE---------------------------------
# === Simple pooling: binary outcome ===
# sum: count the total number of informants who said harvesting causes decline (1 = yes, 0 = no)
k <- sum(tek_survey_data$says_harvest_declines)
# nrow: count the total number of rows (informants)
n <- nrow(tek_survey_data)
# Beta distribution parameters with Laplace smoothing (add 1 to each)
alpha_simple <- k + 1
beta_simple <- n - k + 1
cat("Binary outcome aggregation (Simple pooling):\n")
# sprintf: format and print the Beta distribution parameters
cat(sprintf("  P(harvesting causes decline) ~ Beta(%d, %d)\n", alpha_simple, beta_simple))
# Calculate mean probability of the Beta distribution: alpha / (alpha + beta)
cat(sprintf("  Mean probability: %.3f\n", alpha_simple / (alpha_simple + beta_simple)))

# === Simple pooling: categorical outcome (abundance) ===
# table: count frequency of each abundance category
abundance_counts <- table(tek_survey_data$abundance_status)
# Convert counts to Dirichlet parameters by adding 1 to each count
abundance_alpha <- as.numeric(abundance_counts) + 1
cat("\nAbundance status aggregation (Simple pooling):\n")
cat("  Category counts:\n")
print(abundance_counts)
cat("  Dirichlet alpha (for Dirichlet prior):\n")
print(abundance_alpha)

# === Weighted pooling: by informant confidence ===
# Divide each confidence score by the sum to create normalized weights (sum to 1)
weights <- tek_survey_data$confidence / sum(tek_survey_data$confidence)
# Multiply weighted responses by informant count and sum to get weighted pseudo-count
# sum: add up the products of (informant response) * (normalized weight)
weighted_k_decline <- sum(tek_survey_data$says_harvest_declines * weights) * nrow(tek_survey_data)
# round: convert to integer for use as Beta parameter
wk_decline <- round(weighted_k_decline)
# Calculate Beta parameters with Laplace smoothing
walpha_decline <- wk_decline + 1
wbeta_decline <- n - wk_decline + 1
cat("\nBinary outcome aggregation (Confidence-weighted):\n")
cat(sprintf("  P(harvesting causes decline) ~ Beta(%d, %d)\n", walpha_decline, wbeta_decline))
# Calculate mean probability of the weighted Beta distribution
cat(sprintf("  Mean probability: %.3f\n", walpha_decline / (walpha_decline + wbeta_decline)))

## ----example-load-own-data, eval=FALSE----------------------------------------
# # Load your survey data (replace 'your_data.csv' with your file)
# # read.csv: read a comma-separated values file and create a data frame
# my_tek_data <- read.csv("your_data.csv")
# 
# # Check structure of your data
# # str: display internal structure of an R object, showing column names, types, and sample values
# str(my_tek_data)
# 
# # Summarize binary outcome
# cat("Binary outcome:\n")
# # table: create a frequency table of the responses
# print(table(my_tek_data$says_harvest_declines))
# 
# # Aggregate binary outcome using the methods from Section 5.1
# # sum: count the number of "yes" responses (1 = yes)
# k <- sum(my_tek_data$says_harvest_declines)
# # nrow: count the total number of rows (informants)
# n <- nrow(my_tek_data)
# # Calculate Beta distribution parameters with Laplace smoothing
# alpha <- k + 1; beta <- n - k + 1
# # sprintf: format the prior as a string and print it
# cat(sprintf("Prior: Beta(%d, %d)\n", alpha, beta))

