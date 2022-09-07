

make.cormat <- function(rho, preds) {
  cormat <- diag(length(preds)) # create k x k matrix
  cormat[cormat==0] <- rho      # equal off-diagonal elements
  cormat                        # return correlation matrix
}

make.coefs <- function(R2, cormat, weights) {
  # regression coefficients are defined by the effect size R2, the correlations
  # between predictors, and the relative strength of the predictors (i.e., the
  # weights)
  sqrt(R2 / sum(weights %*% t(weights) * cormat)) * weights
}

make.data <- function(N, R2, coefs, cormat) {
  # generate multivariate normal predictors
  X <- MASS::mvrnorm(N, mu = rep(0, length(coefs)), cormat)
  # and generate the outcome based on predictors and regression coefficients
  Y <- X %*% coefs + rnorm(N, 0, sqrt(1 - R2))
  
  data.frame(data.frame(X),
             Y = Y)
}

fit.aic <- function(hypos = c("all", "hypos"), data, formula) {
  
  # number of variables included in the model
  nvar <- (model.frame(formula = formula, data = data) %>% ncol()) - 1
  
  # specify hypotheses if these are provided as a list
  if ((!is.list(hypos)) && (hypos != "all")) {
    stop("hypos must be either a list specifying each hypothesis under consideration
         or 'all', which results in all possible combinations of equality and 
         inequality constraints.")
  }
  if (is.list(hypos)) {
    h <- hypos
  }
  # or create all possible hypotheses (all possible combinations of constrained
  # and unconstrained regression coefficients) if this is asked
  else if (hypos == "all") {
    h <- map(1:nvar, 
             ~combn(1:nvar, .x, simplify = F) %>%
               map(function(y) paste("X", y, "==0", collapse = ";"))) %>%
      unlist() %>%
      as.list()
    
    names(h) <- paste0("H", 1:length(h))
    
  }
  
  # Create hypotheses in environment
  list2env(h, envir = globalenv())
  
  # Fit regression model
  fit <- lm(formula = formula, data = data)
  
  # Specify goric model (reduces to the aic when there are solely inequality
  # constraints in the hypothesis of interest)
  AIC.mod  <- paste0("goric(fit, ",
                     paste0(names(h), collapse = ","),
                     ")")
  
  # Fit goric model
  AIC <- parse(text = AIC.mod) %>% 
    eval() %$% 
    result %>%
    mutate(value = goric,
           weights = goric.weights) %>%
    select(-c(goric, goric.weights))
  
  # Obtain AICc values by adjusting the penaltyterm
  AICc <- AIC %>%
    mutate(penalty = (nrow(data) * penalty) / (nrow(data) - penalty - 1),
           value   = -2 * (loglik - penalty),
           weights = exp(-(value - min(value)) / 2) / sum(exp(-(value - min(value))/2)))
  
  # Obtain BIC values by adjusting the penalty term
  BIC <- AIC %>%
    mutate(penalty = log(nrow(data)) * penalty,
           value   = -2 * (loglik) + penalty,
           weights = exp(-(value - min(value)) / 2) / sum(exp(-(value - min(value)))))
  
  # Return AIC, AICc and BIC output
  list(AIC = AIC, AICc = AICc, BIC = BIC)
  
}

fit.goric <- function(hypos, data, formula, comparison) {
  
  # Create hypotheses in environment
  list2env(hypos, envir = globalenv())
  
  # Fit regression model
  fit <- lm(formula = formula, data = data)
  
  # Specify goric model (reduces to the aic when there are solely inequality
  # constraints in the hypothesis of interest)
  goric.mod  <- paste0("goric(fit, ",
                       paste0(names(hypos), collapse = ","),
                       ", type = 'goric', comparison = '", comparison, "')")
  goricc.mod <- paste0("goric(fit, ",
                       paste0(names(hypos), collapse = ","),
                       ", type = 'goricc', comparison = '", comparison, "')")
  
  # Fit goric model
  GORIC <- parse(text = goric.mod) %>% 
    eval() %$% 
    result %>%
    mutate(value = goric,
           weights = goric.weights) %>%
    select(-c(goric, goric.weights))
  
  # Fit goricc model
  GORICc <- parse(text = goricc.mod) %>%
    eval() %$%
    result %>%
    mutate(value = goricc,
           weights = goricc.weights) %>%
    select(-(c(goricc, goricc.weights)))
  
  # Return AIC, AICc and BIC output
  list(GORIC = GORIC, GORICc = GORICc)
  
}

true.weight <- function(goric.result, true.hypo) {
  # Obtain the IC-weight of the true hypothesis
  goric.result[goric.result$model == true.hypo, ]$weight
}

best.hypo <- function(goric.result) {
  # Obtain the hypothesis with the most support
  goric.result[which.max(goric.result$weights), ]$model
}

best.versus.true <- function(true.hypo, best.hypo, goric.result) {
  
  g <- goric.result
  
  # If the hypothesis with the most support is also the true hypothesis, 
  # calculate the ratio of the IC weights with the second best supported hypothesis
  if(best.hypo == true.hypo) {
    ref.hypo           <- g[order(g$weights, decreasing = TRUE)[2], ]$model
    second.best.weight <- g[order(g$weights, decreasing = TRUE)[2], ]$weights
    
    list(ratio = g[g$model == true.hypo, ]$weights / second.best.weight,
         ref.hypo = ref.hypo)
  } 
  
  # If the hypothesis with the most support is not the true hypothesis,
  # calculate the ratio of the IC weights with the best supported hypothesis
  else {
    best.weight <- g[g$model == best.hypo, ]$weight
    
    list(ratio = g[g$model == true.hypo, ]$weights / best.weight,
         ref.hypo = best.hypo)
  }
}

format.data <- function(results, true.hypo) {
  results %>%
    unnest_longer(output, indices_to = "Method") %>%
    mutate(best.hypo = map_chr(output, best.hypo),
           true.weight = map_dbl(output, true.weight, true.hypo),
           support.ratio = map2(output, best.hypo, ~best.versus.true(true.hypo, .y, .x))) %>%
    unnest_wider(support.ratio)
}


plot.thr <- function(results, true.hypo) {
  # Plot the true hypothesis rate
  results %>%
    group_by(N, R2, rho, Method) %>%
    summarize(THR = mean(best.hypo == true.hypo)) %>%
    mutate(R2 = paste0(expression(italic(R^2)), " == ", round(R2, 2)),
           rho = paste0("rho == ", rho),
           `Sample size` = N) %>%
    ungroup() %>%
    ggplot(aes(x = `Sample size`, y = THR, col = Method)) +
    geom_point() +
    geom_line() +
    scale_color_brewer(palette = "Set1", labels = c("AIC THR", "AICc THR")) +
    jtools::theme_apa() +
    facet_grid(R2 ~ rho, labeller = label_parsed) +
    theme(legend.position = "bottom") +
    ylim(0, 1)
}

plot.weights <- function(results) {
  # Plot the average IC weights
  results %>%
    group_by(N, R2, rho, Method) %>%
    summarize(Weight = mean(true.weight)) %>%
    mutate(R2 = paste0(expression(italic(R^2)), " == ", round(R2, 2)),
           rho = paste0("rho == ", rho),
           `Sample size` = N) %>%
    ungroup() %>%
    ggplot(aes(x = `Sample size`, y = Weight, col = Method)) +
    geom_point(shape = 17) +
    geom_line(linetype = 2) +
    scale_color_brewer(palette = "Set1", labels = c("Mean AIC weight", "Mean AICc weight")) +
    jtools::theme_apa() +
    facet_grid(R2 ~ rho, labeller = label_parsed) +
    theme(legend.position = "bottom") +
    ylim(0, 1)
}

plot.thr.weights <- function(results, true.hypo) {
  # Plot the average IC weights
  results %>%
    group_by(N, R2, rho, Method) %>%
    summarize(Weight = mean(true.weight),
              THR = mean(best.hypo == true.hypo)) %>%
    pivot_longer(c(Weight, THR), 
                 names_to = "Measure",
                 values_to = "Value") %>%
    mutate(R2 = paste0(expression(italic(R^2)), " == ", round(R2, 2)),
           rho = paste0("rho == ", rho),
           `Sample size` = N,
           Method2 = ifelse(Measure == "Weight", 
                            paste0("Mean ", Method, " ", Measure),
                            paste0(Method, " ", Measure))) %>%
    ungroup() %>%
    ggplot(aes(x = `Sample size`, y = Value, col = Method2, linetype = Method2, shape = Method2)) +
    geom_point() +
    geom_line() +
    jtools::theme_apa() +
    facet_grid(R2 ~ rho, labeller = label_parsed) +
    scale_color_manual(values = c("#E41A1C", "#E41A1C", "#377EB8", "#377EB8")) +
    scale_linetype_manual(values = c(1, 2, 1, 2)) +
    scale_shape_manual(values = c(16, 17, 16, 17)) +
    theme(legend.position = "bottom") +
    ylim(0, 1)
}
