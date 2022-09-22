
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
      filter(Method != "BIC", N > 10) %>%
      plot.thr.se(true.hypo, c(16, 17), c("AIC THR", "AICc THR")) +
      scale_x_continuous(breaks = c(20, 50, 80, 160, 250)) +
      ylab("THR") +
      ylim(0, 1)
    
    weights <- format.data(results, true.hypo) %>%
      filter(Method != "BIC", N > 10) %>%
      plot.weights.se(c(16, 17), c("Mean AIC weight", "Mean AICc weight")) +
      scale_x_continuous(breaks = c(20, 50, 80, 160, 250)) +
      ylab("Mean weight") +
      ylim(0, 1)
    
    thr_weights <- format.data(results, true.hypo) %>%
      filter(Method != "BIC", N > 10) %>%
      plot.thr.weights.se(true.hypo) +
      scale_x_continuous(breaks = c(20, 50, 80, 160, 250)) +
      ylab("THR / Mean weight") +
      ylim(0, 1)
    
    ggsave(paste0("figures_unc//", n, "_thr.pdf"),        plot = thr,         device = "pdf", width = 10, height = 7, dpi = 800)
    ggsave(paste0("figures_unc//", n, "_weight.pdf"),     plot = weights,     device = "pdf", width = 10, height = 7, dpi = 800)
    ggsave(paste0("figures_unc//", n, "_thr_weight.pdf"), plot = thr_weights, device = "pdf", width = 10, height = 7, dpi = 800)
  }, 
  results = ., 
  true.hypo = list("H3", "H3", "H3", "H98", 
                   "H1", "unconstrained", "unconstrained"),
  n = names(.))


goric_out_data <- readRDS("results//goric_out.rds")

goric_out_data <- goric_out_data %>%
  mutate(hyposet = rep(paste0("Set", 1:8), nrow(.) / 8),
         sim_con = map2_chr(weights, hyposet, ~paste0("Ratio", paste0(.x, collapse = ":"),"", .y)))


goric_out_data %$%
  sim_con %>%
  unique() %>%
  map(function(condition) {
    d <- goric_out_data %>%
      filter(sim_con == condition) %>%
      format.data(unique(.$Htrue))
    
    thr <- plot.thr.se(d, unique(d$Htrue), c(16, 17), c("GORIC THR", "GORICc THR")) +
      ylab("THR") +
      scale_x_continuous(breaks = c(10, 20, 50, 80, 160, 250)) +
      theme(axis.text.x = element_text(size = 7)) +
      ylim(0, 1)
    
    weights <- plot.weights.se(d, c(16, 17), c("Mean GORIC weight", "Mean GORICc weight")) +
      ylab("Mean weight") +
      scale_x_continuous(breaks = c(10, 20, 50, 80, 160, 250)) +
      theme(axis.text.x = element_text(size = 7)) +
      ylim(0, 1)
    
    thr_weights <- plot.thr.weights.se(d, unique(d$Htrue)) +
      ylab("THR / Mean weight") +
      scale_x_continuous(breaks = c(10, 20, 50, 80, 160, 250)) +
      theme(axis.text.x = element_text(size = 7)) +
      ylim(0, 1)
    
    
    ggsave(paste0("figures_unc//goricc", gsub("[^[:alnum:] ]", "", condition), "_thr.pdf"),        plot = thr,         device = "pdf", width = 10, height = 7, dpi = 800)
    ggsave(paste0("figures_unc//goricc", gsub("[^[:alnum:] ]", "", condition), "_weight.pdf"),     plot = weights,     device = "pdf", width = 10, height = 7, dpi = 800)
    ggsave(paste0("figures_unc//goricc", gsub("[^[:alnum:] ]", "", condition), "_thr_weight.pdf"), plot = thr_weights, device = "pdf", width = 10, height = 7, dpi = 800)
    
    
  })

goric_out_data %>%
  filter(sim_con == "Ratio1:2:3:4Set2") %>%
  format.data("H1") %>%
  plot.thr.weights.se("H1") +
  ylab("THR / Mean weight") +
  scale_x_continuous(breaks = c(10, 20, 50, 80, 160, 250)) +
  theme(axis.text.x = element_text(size = 7)) +
  ylim(0.5, 1)
  
ggsave(paste0("figures_unc//goricc", gsub("[^[:alnum:] ]", "", "Ratio1:2:3:4Set2"), "_thr_weight_adjusted_scale.pdf"), device = "pdf", width = 10, height = 7, dpi = 800)
