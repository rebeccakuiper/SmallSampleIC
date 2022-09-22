# lapply(c("MASS",    "purrr",    "magrittr", "dplyr", "furrr",
#          "ggplot2", "devtools", "jtools",   "tidyr"),
#        function(x) install.pacOkages(x))

lapply(c("purrr",   "magrittr", "dplyr",  "furrr",
         "ggplot2", "devtools", "jtools", "tidyr"),
       function(x) library(x, character.only = TRUE))

c("aicc_out1a.rds", "aicc_out1b1.rds", "aicc_out1b2.rds", "aicc_out1b3.rds",
  "aicc_out2a.rds", "aicc_out2b.rds", "aicc_out2c.rds") %>%
  lapply(function(x) {
    readRDS(paste0("results//", x)) %>%
      assign(substr(x, 1, nchar(x) - 4), ., envir = globalenv())
  })

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
    
    weights <- format.data(results, true.hypo) %>%
      filter(Method != "BIC", N > 10) %>%
      plot.weights.boxplot(c("Mean AIC weight", "Mean AICc weight")) +
      ylab("Mean weight") +
      ylim(0, 1)
    
    ggsave(paste0("weight_boxplots//", n, "_weight_boxplot.pdf"), plot = weights,     device = "pdf", width = 10, height = 7, dpi = 800)
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
    
    weights <- plot.weights.boxplot(d, c("Mean GORIC weight", "Mean GORICc weight")) +
      ylab("Mean weight") +
      theme(axis.text.x = element_text(size = 7)) +
      ylim(0, 1)
    
    ggsave(paste0("weight_boxplots//goricc", gsub("[^[:alnum:] ]", "", condition), "_weight_boxplot.pdf"), plot = weights, device = "pdf", width = 10, height = 7, dpi = 800)

    
  })
