
c("aicc_out1a.rds", "aicc_out1b1.rds", "aicc_out1b2.rds", "aicc_out1b3.rds",
  "aicc_out2a.rds", "aicc_out2b.rds", "aicc_out2c.rds") %>%
  lapply(function(x) {
    readRDS(paste0("results//", x)) %>%
      assign(substr(x, 1, nchar(x) - 4), ., envir = globalenv())
  })

# lapply(c("MASS",    "purrr",    "magrittr", "dplyr", "furrr",
#          "ggplot2", "devtools", "jtools",   "tidyr"),
#        function(x) install.pacOkages(x))

lapply(c("purrr",   "magrittr", "dplyr",  "furrr",
         "ggplot2", "devtools", "jtools", "tidyr"),
       function(x) library(x, character.only = TRUE))

# install_github("LeonardV/restriktor")
library(restriktor)


source("functions.R")

list("aicc_out1a"  = aicc_out1a, 
     "aicc_out1b1" = aicc_out1b1, 
     "aicc_out1b2" = aicc_out1b2, 
     "aicc_out1b3" = aicc_out1b3,
     "aicc_out2a"  = aicc_out2a, 
     "aicc_out2b"  = aicc_out2b,
     "aicc_out2c"  = aicc_out2c) %>%
  Map(f = function(results, true.hypo, n) {
    thr <- format.data(results, true.hypo) %>%
      filter(N > 10) %>%
      plot.thr.se(true.hypo, c(16, 17, 18), c("AIC THR", "AICc THR", "BIC THR")) +
      scale_x_continuous(breaks = c(20, 50, 80, 160, 250)) +
      ylab("THR") +
      ylim(0, 1)
    
    weights <- format.data(results, true.hypo) %>%
      filter(N > 10) %>%
      plot.weights.se(c(16, 17, 18), c("Mean AIC weight", "Mean AICc weight", "Mean BIC weight")) +
      scale_x_continuous(breaks = c(20, 50, 80, 160, 250)) +
      ylab("Mean weight") +
      ylim(0, 1)
    
    ggsave(paste0("figures_bic//", n, "_thr.pdf"),        plot = thr,         device = "pdf", width = 10, height = 7, dpi = 800)
    ggsave(paste0("figures_bic//", n, "_weight.pdf"),     plot = weights,     device = "pdf", width = 10, height = 7, dpi = 800)
  }, 
  results = ., 
  true.hypo = list("H3", "H3", "H3", "H98", 
                   "H1", "unconstrained", "unconstrained"),
  n = names(.))


list("aicc_out1a"  = aicc_out1a, 
     "aicc_out1b1" = aicc_out1b1, 
     "aicc_out1b2" = aicc_out1b2, 
     "aicc_out1b3" = aicc_out1b3,
     "aicc_out2a"  = aicc_out2a, 
     "aicc_out2b"  = aicc_out2b,
     "aicc_out2c"  = aicc_out2c) %>%
  Map(f = function(results, true.hypo, n) {
    
    weights <- format.data(results, true.hypo) %>%
      filter(N > 10) %>%
      plot.weights.boxplot(c("Mean AIC weight", "Mean AICc weight", "Mean BIC weight")) +
      ylab("Mean weight") +
      ylim(0, 1)
    
    ggsave(paste0("figures_bic//", n, "_weight_boxplot.pdf"), plot = weights,     device = "pdf", width = 10, height = 7, dpi = 800)
  }, 
  results = ., 
  true.hypo = list("H3", "H3", "H3", "H98", 
                   "H1", "unconstrained", "unconstrained"),
  n = names(.))