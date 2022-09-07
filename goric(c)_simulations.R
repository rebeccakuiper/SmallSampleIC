
###############################################################################
## Load required packages & functions
###############################################################################


# lapply(c("MASS",    "purrr",    "magrittr", "dplyr", "furrr",
#          "ggplot2", "devtools", "jtools",   "tidyr"),
#        function(x) install.pacOkages(x))

lapply(c("purrr",   "magrittr", "dplyr",  "furrr",
         "ggplot2", "devtools", "jtools", "tidyr"),
       function(x) library(x, character.only = TRUE))

# install_github("LeonardV/restriktor")
library(restriktor)

source("functions.R")

###############################################################################
## Simulation specifications (sim 1(a), 1(b) and 2)
###############################################################################

# Set seed for reproducibility
set.seed(1234)

## Number of simulations, sample size, effect size, correlations and relative
## strength of regression coefficients (the same for all simulations)

nsim    <- 1000
N       <- c(10, 20, 50, 80, 160, 250)
f2      <- c(0.02, 0.15, 0.35)
R2      <- c(f2 / (1 + f2), .933)
rho     <- c(0, 0.25, 0.5)
weights <- list(c(1, 2, 3, 4), c(5, 6, 7, 8))
model   <- list(hypo1 = list(hypos = list(H1 = "X1 < X2 < X3 < X4"), 
                             comparison = "unconstrained", 
                             Htrue = "H1"),
                hypo2 = list(hypos = list(H1 = "X1 < X2 < X3 < X4"), 
                             comparison = "complement", 
                             Htrue = "H1"),
                hypo3 = list(hypos = list(H1 = "X1 > X2 > X3 > X4"), 
                             comparison = "unconstrained", 
                             Htrue = "unconstrained"),
                hypo4 = list(hypos = list(H1 = "X1 > X2 > X3 > X4"), 
                             comparison = "complement", 
                             Htrue = "complement"),
                hypo5 = list(hypos = list(H1 = "X1 > X2 < X3 < X4"), 
                             comparison = "unconstrained", 
                             Htrue = "unconstrained"),
                hypo6 = list(hypos = list(H1 = "X1 > X2 < X3 < X4"), 
                             comparison = "complement", 
                             Htrue = "complement"),
                hypo7 = list(hypos = list(H1 = "X1 < X2 < X3 < X4", H2 = "X1 > X2 < X3 < X4"), 
                             comparison = "unconstrained",
                             Htrue = "H1"),
                hypo8 = list(hypos = list(H1 = "X1 < X2 < X3 < X4", H2 = "X1 == X1 == X3 == X4"), 
                             comparison = "unconstrained",
                             Htrue = "H1"))

plan(multisession)

goric_out <- future_map_dfr(1:nsim, ~expand_grid(N, R2, rho, weights, model) %>%
                              unnest_wider(model) %>%
                              mutate(cormat = map2(rho, weights, ~make.cormat(.x, .y)),
                                     coefs  = pmap(list(R2 = R2, cormat = cormat, weights = weights),
                                                   function(R2, cormat, weights) make.coefs(R2, cormat, weights))) %>%
                              mutate(output = pmap(list(N, R2, coefs, cormat, hypos, comparison),
                                                   function(N, R2, coefs, cormat, hypos, comparison) {
                                                     make.data(N, R2, coefs, cormat) %>%
                                                       fit.goric(hypos = hypos, data = ., formula = Y ~ ., comparison = comparison)
                                                     })) %>%
                              select(-c(cormat)),
                            .id = "NSIM", 
                            .progress = TRUE, 
                            .options = furrr_options(seed = TRUE),
                            .env_globals = globalenv())

readr::write_rds(goric_out, file = "results//goric_out.rds")
