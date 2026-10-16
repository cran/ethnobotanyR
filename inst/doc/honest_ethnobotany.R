## -----------------------------------------------------------------------------
# Suppose 12 citations out of 20 informants
k <- 12  # citations
n <- 20  # informants

# Posterior parameters for Beta(1,1) prior
alpha_post <- k + 1
beta_post <- n - k + 1

# Posterior mean
mean_uv <- alpha_post / (alpha_post + beta_post)
# mean
mean_uv

# 95% credible interval
ci_uv <- qbeta(c(0.025, 0.975), alpha_post, beta_post)

# lower
ci_uv[1]
# upper
ci_uv[2]

