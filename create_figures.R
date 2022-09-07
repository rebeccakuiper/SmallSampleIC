
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
      plot.thr(true.hypo) +
      scale_x_continuous(breaks = c(20, 50, 80, 160, 250)) +
      ylab("THR")
    
    weights <- format.data(results, true.hypo) %>%
      filter(Method != "BIC", N > 10) %>%
      plot.weights() +
      scale_x_continuous(breaks = c(20, 50, 80, 160, 250)) +
      ylab("Mean weight")
    
    thr_weights <- format.data(results, true.hypo) %>%
      filter(Method != "BIC", N > 10) %>%
      plot.thr.weights(true.hypo) +
      scale_x_continuous(breaks = c(20, 50, 80, 160, 250)) +
      ylab("THR / Mean weight")
    
    ggsave(paste0("figures20//", n, "_thr.pdf"),        plot = thr,         device = "pdf", width = 10, height = 7, dpi = 800)
    ggsave(paste0("figures20//", n, "_weight.pdf"),     plot = weights,     device = "pdf", width = 10, height = 7, dpi = 800)
    ggsave(paste0("figures20//", n, "_thr_weight.pdf"), plot = thr_weights, device = "pdf", width = 10, height = 7, dpi = 800)
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
    
    thr         <- plot.thr(d, unique(d$Htrue)) +
      ylab("THR") +
      scale_x_continuous(breaks = c(10, 20, 50, 80, 160, 250)) +
      theme(axis.text.x = element_text(size = 7))
    
    weights     <- plot.weights(d) +
      ylab("Mean weight") +
      scale_x_continuous(breaks = c(10, 20, 50, 80, 160, 250)) +
      theme(axis.text.x = element_text(size = 7))
    
    thr_weights <- plot.thr.weights(d, unique(d$Htrue)) +
      ylab("THR / Mean weight") +
      scale_x_continuous(breaks = c(10, 20, 50, 80, 160, 250)) +
      theme(axis.text.x = element_text(size = 7))
    
    
    ggsave(paste0("figures20//goricc", gsub("[^[:alnum:] ]", "", condition), "_thr.pdf"),        plot = thr,         device = "pdf", width = 10, height = 7, dpi = 800)
    ggsave(paste0("figures20//goricc", gsub("[^[:alnum:] ]", "", condition), "_weight.pdf"),     plot = weights,     device = "pdf", width = 10, height = 7, dpi = 800)
    ggsave(paste0("figures20//goricc", gsub("[^[:alnum:] ]", "", condition), "_thr_weight.pdf"), plot = thr_weights, device = "pdf", width = 10, height = 7, dpi = 800)
    
    
  })

# Obtain true hypothesis rates for replication of H&T simulation
aicc_out1a %>%
  format.data("H3") %>%
  group_by(N, R2, Method) %>%
  summarize(THR = mean(best.hypo == "H3"))

# Obtain true hypothesis rates for replication of H&T simulation (with intercept)
aicc_out1b1 %>%
  format.data("H3") %>%
  filter(N == 20, rho == 0, R2 == .933, Method != "BIC") %>%
  group_by(N, R2, Method) %>%
  summarize(THR = mean(best.hypo == "H3"))
  
# Check which hypotheses are chosen most often in the exploratory simulation
best_hypos <- aicc_out1b3 %>%
  format.data("H98") %>%
  group_by(N, R2, rho, Method) %>%
  summarize(chosen = list(table(best.hypo))) %>%
  mutate(which.hypo = map_chr(chosen, ~ names(.x)[which.max(.x)]),
         count.hypo = map_dbl(chosen, ~ .x[which.max(.x)]))


View(best_hypos %>% filter(N > 10, Method != "BIC"))

# Check how often a difference larger than 3 occurs between best and second best
# hypothesis in the exploratory simulation
aicc_out1b3 %>%
  unnest_longer(output, indices_to = "Method") %>%
  filter(Method != "BIC", N > 10) %>%
  mutate(dif_b_sb = map_dbl(output, function(x) {
    x[order(x$value), ]$value[2] - x[order(x$value), ]$value[1]
  })) %>%
  group_by(N, R2, rho, Method) %>%
  summarize(dif_b_sb = sum(dif_b_sb > 3)) %>%
  View()
