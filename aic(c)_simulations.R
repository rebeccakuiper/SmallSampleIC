
###############################################################################
## Load required packages & functions
###############################################################################

# Install if not done previously
# lapply(c("MASS",    "purrr",    "magrittr", "dplyr", "furrr",
#          "ggplot2", "devtools", "jtools",   "tidyr"),
#        function(x) install.packages(x))

lapply(c("purrr",   "magrittr", "dplyr",  "furrr",
         "ggplot2", "devtools", "jtools", "tidyr"),
       function(x) library(x, character.only = TRUE))

# Install if not done previously
# install_github("LeonardV/restriktor")
library(restriktor)

source("functions.R")

###############################################################################
## Simulation specifications (sim 1(a), 1(b) and 2)
###############################################################################

# Set seed for reproducibility
set.seed(123)

## Number of simulations, sample size, effect size, correlations and relative
## strength of regression coefficients (the same for all simulations)

nsim    <- 1000
weights <- list(c(1, 2, 3, 0, 0, 0, 0))
N       <- c(10, 20)
R2      <- .933
rho     <- 0

###############################################################################
## Simulation 1(a) (replicating H&T)
###############################################################################


## Hypothesis set of H&T

hypos1a <- list(H1 = "X2 == 0; X3 == 0; X4 == 0; X5 == 0; X6 == 0; X7 == 0",
                H2 = "X3 == 0; X4 == 0; X5 == 0; X6 == 0; X7 == 0",
                H3 = "X4 == 0; X5 == 0; X6 == 0; X7 == 0",
                H4 = "X5 == 0; X6 == 0; X7 == 0",
                H5 = "X6 == 0; X7 == 0",
                H6 = "X7 == 0")

# Run simulation in parallel
plan(multisession)

# Obtain the output (replication of H&T, note that here no intercept is fitted)
out1a <- future_map_dfr(1:nsim, 
                       ~expand_grid(N = N, R2 = R2, rho = rho, weights = weights) %>%
                         mutate(cormat = map2(rho, weights, ~make.cormat(.x, .y)),
                                coefs  = pmap(list(R2 = R2, cormat = cormat, weights = weights),
                                              function(R2, cormat, weights) make.coefs(R2, cormat, weights)),
                                hypos = list(hypos1a)) %>%
                         mutate(output = pmap(list(N = N, R2 = R2, coefs = coefs, cormat = cormat, hypos = hypos),
                                              function(N, R2, coefs, cormat, hypos) {
                                                make.data(N, R2, coefs, cormat) %>%
                                                  fit.aic(hypos = hypos, data = ., formula = Y ~ . - 1)
                                                })) %>%
                        select(-c(weights, cormat, coefs, hypos)),
                      .id = "NSIM",
                      .progress = TRUE,
                      .options = furrr_options(seed = TRUE),
                      .env_globals = globalenv())


###############################################################################
## Simulation 1(b1), extending the simulation by H&T by incorporating other
## sample sizes, effect sizes and correlations between predictors
## NOTE THAT AN INTERCEPT IS FITTED HERE, WHICH WAS NOT DONE IN THE PREVIOUS
## SIMULATIONS
###############################################################################

N       <- c(10, 20, 50, 80, 160, 250)
f2      <- c(0.02, 0.15, 0.35)
R2      <- c(f2 / (1 + f2), .933)
rho     <- c(0, 0.25, 0.5)

## Hypothesis set of H&T (same as in simulation 1a)

hypos1b1 <- list(H1 = "X2 == 0; X3 == 0; X4 == 0; X5 == 0; X6 == 0; X7 == 0",
                 H2 = "X3 == 0; X4 == 0; X5 == 0; X6 == 0; X7 == 0",
                 H3 = "X4 == 0; X5 == 0; X6 == 0; X7 == 0",
                 H4 = "X5 == 0; X6 == 0; X7 == 0",
                 H5 = "X6 == 0; X7 == 0",
                 H6 = "X7 == 0")

# Obtain the output
out1b1 <- future_map_dfr(1:nsim, 
                        ~expand_grid(N = N, R2 = R2, rho = rho, weights = weights) %>%
                          mutate(cormat = map2(rho, weights, ~make.cormat(.x, .y)),
                                 coefs  = pmap(list(R2 = R2, cormat = cormat, weights = weights),
                                               function(R2, cormat, weights) make.coefs(R2, cormat, weights)),
                                 hypos = list(hypos1b1)) %>%
                          mutate(output = pmap(list(N = N, R2 = R2, coefs = coefs, cormat = cormat, hypos = hypos),
                                               function(N, R2, coefs, cormat, hypos) {
                                                 make.data(N, R2, coefs, cormat) %>%
                                                   fit.aic(hypos = hypos, data = ., formula = Y ~ .)
                                               })) %>%
                          select(-c(weights, cormat, coefs, hypos)),
                        .id = "NSIM",
                        .progress = TRUE,
                        .options = furrr_options(seed = TRUE),
                        .env_globals = globalenv())

###############################################################################
## Simulation 1(b2), adding the classical null hypothesis, setting all 
## coefficients to zero.
###############################################################################

## Add classical null hypothesis to previous set
hypos1b2 <- c(H0 = "X1 == 0; X2 == 0; X3 == 0; X4 == 0; X5 == 0; X6 == 0; X7 == 0",
              c(hypos1b1)) %>%
  as.list()

## Run simulation 1(b1)
out1b2 <- future_map_dfr(1:nsim,
                         ~expand_grid(N = N, R2 = R2, rho = rho, weights = weights) %>%
                           mutate(cormat = map2(rho, weights, ~make.cormat(.x, .y)),
                                  coefs  = pmap(list(R2 = R2, cormat = cormat, weights = weights),
                                                function(R2, cormat, weights) make.coefs(R2, cormat, weights)),
                                  hypos = list(hypos1b2)) %>%
                           mutate(output = pmap(list(N = N, R2 = R2, coefs = coefs, cormat = cormat, hypos = hypos),
                                                function(N, R2, coefs, cormat, hypos) {
                                                  make.data(N, R2, coefs, cormat) %>%
                                                    fit.aic(hypos = hypos, data = ., formula = Y ~ .)
                                                })) %>%
                           select(-c(weights, cormat, coefs, hypos)),
                         .id = "NSIM",
                         .progress = TRUE,
                         .options = furrr_options(seed = TRUE),
                         .env_globals = globalenv())

###############################################################################
## Simulation 1(b3), evaluating all possible hypotheses in the set
###############################################################################

hypos1b3 <- "all"

## Run simulation 1(b1)
out1b3 <- future_map_dfr(1:nsim,
                         ~expand_grid(N = N, R2 = R2, rho = rho, weights = weights) %>%
                           mutate(cormat = map2(rho, weights, ~make.cormat(.x, .y)),
                                  coefs  = pmap(list(R2 = R2, cormat = cormat, weights = weights),
                                                function(R2, cormat, weights) make.coefs(R2, cormat, weights)),
                                  hypos = list(hypos1b3)) %>%
                           mutate(output = pmap(list(N = N, R2 = R2, coefs = coefs, cormat = cormat, hypos = hypos),
                                                function(N, R2, coefs, cormat, hypos) {
                                                  make.data(N, R2, coefs, cormat) %>%
                                                    fit.aic(hypos = hypos, data = ., formula = Y ~ .)
                                                })) %>%
                           select(-c(weights, cormat, coefs, hypos)),
                         .id = "NSIM",
                         .progress = TRUE,
                         .options = furrr_options(seed = TRUE),
                         .env_globals = globalenv())


###############################################################################
## Simulation 2(a), confirmatory simulation where true hypothesis is the least
## complex hypothesis in the set (and thus has the smallest penalty)
###############################################################################

set.seed(124)

plan(multisession)

hypos2a <- list(H1 = "X4 == 0; X5 == 0; X6 == 0; X7 == 0")

out2a <- future_map_dfr(1:nsim, 
                        ~expand_grid(N = N, R2 = R2, rho = rho, weights = weights) %>%
                          mutate(cormat = map2(rho, weights, ~make.cormat(.x, .y)),
                                 coefs  = pmap(list(R2 = R2, cormat = cormat, weights = weights),
                                               function(R2, cormat, weights) make.coefs(R2, cormat, weights)),
                                 hypos = list(hypos2a),
                                 output = pmap(list(N = N, R2 = R2, coefs = coefs, cormat = cormat, hypos = hypos),
                                               function(N, R2, coefs, cormat, hypos) {
                                                 make.data(N, R2, coefs, cormat) %>%
                                                   fit.aic(hypos = hypos, data = ., formula = Y ~ .)
                                               }))  %>%
                          select(-c(weights, cormat, coefs, hypos)), 
                        .id = "NSIM", 
                        .progress = TRUE, 
                        .options = furrr_options(seed = TRUE),
                        .env_globals = globalenv())

###############################################################################
## Simulation 2(b), confirmatory simulation where true hypothesis is the most
## complex hypothesis in the set (and thus has the largest penalty)
###############################################################################

hypos2b <- list(H1 = "X1 == 0; X2 == 0; X3 == 0; X4 == 0; X5 == 0; X6 == 0; X7 == 0")

out2b <- future_map_dfr(1:nsim, 
                        ~expand_grid(N = N, R2 = R2, rho = rho, weights = weights) %>%
                          mutate(cormat = map2(rho, weights, ~make.cormat(.x, .y)),
                                 coefs  = pmap(list(R2 = R2, cormat = cormat, weights = weights),
                                               function(R2, cormat, weights) make.coefs(R2, cormat, weights)),
                                 hypos = list(hypos2b),
                                 output = pmap(list(N = N, R2 = R2, coefs = coefs, cormat = cormat, hypos = hypos),
                                               function(N, R2, coefs, cormat, hypos) {
                                                 make.data(N, R2, coefs, cormat) %>%
                                                   fit.aic(hypos = hypos, data = ., formula = Y ~ .)
                                               })) %>%
                          select(-c(weights, cormat, coefs, hypos)), 
                        .id = "NSIM", 
                        .progress = TRUE, 
                        .options = furrr_options(seed = TRUE),
                        .env_globals = globalenv())

###############################################################################
## Simulation 2(c), confirmatory simulation where true hypothesis is the most
## complex hypothesis in the set (and thus has the largest penalty)
###############################################################################

hypos2c <- list(H1 = "X1 == 0; X2 == 0; X3 == 0")

out2c <- future_map_dfr(1:nsim, 
                        ~expand_grid(N = N, R2 = R2, rho = rho, weights = weights) %>%
                          mutate(cormat = map2(rho, weights, ~make.cormat(.x, .y)),
                                 coefs  = pmap(list(R2 = R2, cormat = cormat, weights = weights),
                                               function(R2, cormat, weights) make.coefs(R2, cormat, weights)),
                                 hypos = list(hypos2c),
                                 output = pmap(list(N = N, R2 = R2, coefs = coefs, cormat = cormat, hypos = hypos),
                                               function(N, R2, coefs, cormat, hypos) {
                                                 make.data(N, R2, coefs, cormat) %>%
                                                   fit.aic(hypos = hypos, data = ., formula = Y ~ .)
                                               })) %>%
                          select(-c(weights, cormat, coefs, hypos)), 
                        .id = "NSIM", 
                        .progress = TRUE, 
                        .options = furrr_options(seed = TRUE),
                        .env_globals = globalenv())

###############################################################################
## save output of all simulations
###############################################################################

list("out1a" = out1a, 
     "out1b1" = out1b1, 
     "out1b2" = out1b2,
     "out1b3" = out1b3,
     "out2a" = out2a, 
     "out2b" = out2b,
     "out2c" = out2c) %>%
  Map(f = function(x, n) readr::write_rds(x, file = paste0("results//aicc_", n, ".rds")), ., names(.))


