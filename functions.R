

# Create correlation matrix from common correlation between predictors.
make.cormat <- function(rho, preds) {
  cormat <- diag(length(preds)) # create k x k matrix
  cormat[cormat==0] <- rho      # equal off-diagonal elements
  cormat                        # return correlation matrix
}

# Create coefficients based on effect size, correlation between predictors and 
# relative strength (weights) of the predictors.
make.coefs <- function(R2, cormat, weights) {
  # regression coefficients are defined by the effect size R2, the correlations
  # between predictors, and the relative strength of the predictors (i.e., the
  # weights)
  sqrt(R2 / sum(weights %*% t(weights) * cormat)) * weights
}

# Simulate the data (predictors - multivariate normal; and Y - univariate normal)
make.data <- function(N, R2, coefs, cormat) {
  # generate multivariate normal predictors
  X <- MASS::mvrnorm(N, mu = rep(0, length(coefs)), cormat)
  # and generate the outcome based on predictors and regression coefficients
  Y <- X %*% coefs + rnorm(N, 0, sqrt(1 - R2))
  
  data.frame(data.frame(X),
             Y = Y)
}

# Fit the AIC, AICc and BIC for either a subset of hypotheses (statistical models)
# or all statistical models (possible given the available set of predictors)
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

# Fit the GORIC or GORICc for a user-defined subset of hypotheses (statistical models)
# (the option to include all possible hypotheses is not feasible here, given that there
# are infinitely many informative hypotheses the goric(c) could evaluate)

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

# Obtain IC weight of the true hypothesis (this also works for aic/bic output)
true.weight <- function(goric.result, true.hypo) {
  # Obtain the IC-weight of the true hypothesis
  goric.result[goric.result$model == true.hypo, ]$weight
}

# Obtain hypothesis that got most support from the data (this also works for aic/bic output)
best.hypo <- function(goric.result) {
  # Obtain the hypothesis with the most support
  goric.result[which.max(goric.result$weights), ]$model
}

# Calculate the relative strength of the best hypothesis with the true hypothesis
# (if the true hypothesis is not the best hypothesis), or of the true hypothesis
# with the second best hypothesis (if the true hypothesis is the best hypothesis)
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

# Format the simulation data to have a nice and insightful output format (that is,
# obtain the required information in a workable format)
format.data <- function(results, true.hypo) {
  results %>%
    unnest_longer(output, indices_to = "Method") %>%
    mutate(best.hypo = map_chr(output, best.hypo),
           true.weight = map_dbl(output, true.weight, true.hypo),
           support.ratio = map2(output, best.hypo, ~best.versus.true(true.hypo, .y, .x))) %>%
    unnest_wider(support.ratio)
}

# create the figures for the true hypothesis rate.
plot.thr <- function(results, true.hypo, labels) {
  # Plot the true hypothesis rate
  results %>%
    group_by(N, R2, rho, Method) %>%
    summarize(THR = mean(best.hypo == true.hypo)) %>%
    mutate(R2 = ifelse(R2 < 0.02 | R2 > .9, 
                       paste0(expression(italic(R^2)), " == ", sub('.', '', paste0(round(R2, 4)))),
                       paste0(expression(italic(R^2)), " == ", sub('.', '', paste0(round(R2, 2))))),
           rho = paste0("rho == ", rho),
           `Sample size` = N) %>%
    ungroup() %>%
    ggplot(aes(x = `Sample size`, y = THR, col = Method)) +
    geom_point() +
    geom_line() +
    scale_color_brewer(palette = "Set1", labels = labels) +
    jtools::theme_apa() +
    facet_grid(R2 ~ rho, labeller = label_parsed) +
    theme(legend.position = "bottom") +
    ylim(0, 1)
}

plot.thr.se <- function(results, true.hypo, shapes, labels) {
  # Plot the true hypothesis rate
  results %>%
    group_by(N, R2, rho, Method) %>%
    summarize(THR = mean(best.hypo == true.hypo),
              lower = max(0, prop.test(sum(best.hypo == true.hypo), length(best.hypo))$conf.int[1]),
              upper = min(1, prop.test(sum(best.hypo == true.hypo), length(best.hypo))$conf.int[2])) %>%
    mutate(R2 = ifelse(R2 < 0.02 | R2 > .9, 
                       paste0(expression(italic(R^2)), " == ", sub('.', '', paste0(round(R2, 4)))),
                       paste0(expression(italic(R^2)), " == ", sub('.', '', paste0(round(R2, 2))))),
           rho = paste0("rho == ", rho),
           `Sample size` = N) %>%
    ungroup() %>%
    ggplot(aes(x = `Sample size`, y = THR, col = Method, fill = Method, shape = Method)) +
    geom_point() +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, col = "transparent") +
    scale_color_brewer(palette = "Set1", labels = labels) +
    scale_fill_brewer(palette = "Set1", labels = labels) +
    scale_shape_manual(values = shapes, labels = labels) +
    theme_apa(remove.y.gridlines = F) +
    facet_grid(R2 ~ rho, labeller = label_parsed) +
    theme(legend.position = "bottom")
}

# create the figures for the IC-weights
plot.weights <- function(results, labels) {
  # Plot the average IC weights
  results %>%
    group_by(N, R2, rho, Method) %>%
    summarize(weight = mean(true.weight)) %>%
    mutate(R2 = ifelse(R2 < 0.02 | R2 > .9, 
                       paste0(expression(italic(R^2)), " == ", sub('.', '', paste0(round(R2, 4)))),
                       paste0(expression(italic(R^2)), " == ", sub('.', '', paste0(round(R2, 2))))),
           rho = paste0("rho == ", rho),
           `Sample size` = N) %>%
    ungroup() %>%
    ggplot(aes(x = `Sample size`, y = weight, col = Method)) +
    geom_point() +
    geom_line(linetype = 2) +
    scale_color_brewer(palette = "Set1", labels = labels) +
    jtools::theme_apa() +
    facet_grid(R2 ~ rho, labeller = label_parsed) +
    theme(legend.position = "bottom") +
    ylim(0, 1)
}

plot.weights.se <- function(results, shapes, labels) {
  # Plot the average IC weights
  results %>%
    group_by(N, R2, rho, Method) %>%
    summarize(weight = mean(true.weight),
              lower = max(0, mean(true.weight) - qnorm(0.975)*(sd(true.weight)/sqrt(n()))),
              upper = min(1, mean(true.weight) + qnorm(0.975)*(sd(true.weight)/sqrt(n())))) %>%
    mutate(R2 = ifelse(R2 < 0.02 | R2 > .9, 
                       paste0(expression(italic(R^2)), " == ", sub('.', '', paste0(round(R2, 4)))),
                       paste0(expression(italic(R^2)), " == ", sub('.', '', paste0(round(R2, 2))))),
           rho = paste0("rho == ", rho),
           `Sample size` = N) %>%
    ungroup() %>%
    ggplot(aes(x = `Sample size`, y = weight, col = Method, fill = Method, shape = Method)) +
    geom_point() +
    geom_line(linetype = 2) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, col = "transparent") +
    scale_color_brewer(palette = "Set1", labels = labels) +
    scale_fill_brewer(palette = "Set1", labels = labels) +
    scale_shape_manual(values = shapes, labels = labels) +
    jtools::theme_apa(remove.y.gridlines = F) +
    facet_grid(R2 ~ rho, labeller = label_parsed) +
    theme(legend.position = "bottom")
}

plot.weights.boxplot <- function(results, labels) {
  # Plot the average IC weights
  results %>%
    mutate(R2 = ifelse(R2 < 0.02 | R2 > .9, 
                       paste0(expression(italic(R^2)), " == ", sub('.', '', paste0(round(R2, 4)))),
                       paste0(expression(italic(R^2)), " == ", sub('.', '', paste0(round(R2, 2))))),
           rho = paste0("rho == ", rho),
           `Sample size` = as.factor(N)) %>%
    ggplot(aes(x = `Sample size`, y = true.weight, col = Method)) +
    geom_boxplot() +
    scale_color_brewer(palette = "Set1", labels = labels) +
    jtools::theme_apa(remove.y.gridlines = F) +
    facet_grid(R2 ~ rho, labeller = label_parsed) +
    theme(legend.position = "bottom")
}

# create the figures for the true hypothesis rate and IC-weights in a single plot
plot.thr.weights <- function(results, true.hypo) {
  # Plot the average IC weights
  results %>%
    group_by(N, R2, rho, Method) %>%
    summarize(weight = mean(true.weight),
              THR = mean(best.hypo == true.hypo)) %>%
    pivot_longer(c(weight, THR), 
                 names_to = "Measure",
                 values_to = "Value") %>%
    mutate(R2 = ifelse(R2 < 0.02 | R2 > .9, 
                       paste0(expression(italic(R^2)), " == ", sub('.', '', paste0(round(R2, 4)))),
                       paste0(expression(italic(R^2)), " == ", sub('.', '', paste0(round(R2, 2))))),
           rho = paste0("rho == ", rho),
           `Sample size` = N,
           Method2 = ifelse(Measure == "weight", 
                            paste0("Mean ", Method, " ", Measure),
                            paste0(Method, " ", Measure))) %>%
    ungroup() %>%
    ggplot(aes(x = `Sample size`, y = Value, col = Method2, linetype = Method2, shape = Method2)) +
    geom_point() +
    geom_line() +
    jtools::theme_apa() +
    facet_grid(R2 ~ rho, labeller = label_parsed) +
    scale_color_manual(values = c("#E41A1C", "#377EB8", "#E41A1C", "#377EB8")) +
    scale_linetype_manual(values = c(1, 1, 2, 2)) +
    scale_shape_manual(values = c(16, 17, 16, 17)) +
    theme(legend.position = "bottom") +
    ylim(0, 1)
}

plot.thr.weights.se <- function(results, true.hypo) {
  # Plot the average IC weights
  results %>%
    group_by(N, R2, rho, Method) %>%
    summarize(mean__weight = mean(true.weight),
              lower__weight = max(0, mean(true.weight) - qnorm(0.975)*(sd(true.weight)/sqrt(n()))),
              upper__weight = min(1, mean(true.weight) + qnorm(0.975)*(sd(true.weight)/sqrt(n()))),
              mean__THR = mean(best.hypo == true.hypo),
              lower__THR = max(0, prop.test(sum(best.hypo == true.hypo), length(best.hypo))$conf.int[1]),
              upper__THR = min(1, prop.test(sum(best.hypo == true.hypo), length(best.hypo))$conf.int[2])) %>%
    pivot_longer(c(mean__weight:upper__THR), 
                 names_to = c(".value", "Measure"),
                 names_pattern = "(.*)__(.*)",
                 values_to = "Value") %>%
    mutate(R2 = ifelse(R2 < 0.02 | R2 > .9, 
                       paste0(expression(italic(R^2)), " == ", sub('.', '', paste0(round(R2, 4)))),
                       paste0(expression(italic(R^2)), " == ", sub('.', '', paste0(round(R2, 2))))),
           rho = paste0("rho == ", rho),
           `Sample size` = N,
           Method2 = ifelse(Measure == "weight", 
                            paste0("Mean ", Method, " ", Measure),
                            paste0(Method, " ", Measure))) %>%
    ungroup() %>%
    ggplot(aes(x = `Sample size`, y = mean, col = Method2, fill = Method2, linetype = Method2, shape = Method2)) +
    geom_point() +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, col = "transparent") +
    jtools::theme_apa(remove.y.gridlines = FALSE) +
    facet_grid(R2 ~ rho, labeller = label_parsed) +
    scale_color_manual(values = c("#E41A1C", "#377EB8", "#E41A1C", "#377EB8")) +
    scale_fill_manual(values = c("#E41A1C", "#377EB8", "#E41A1C", "#377EB8")) +
    scale_linetype_manual(values = c(1, 1, 2, 2)) +
    scale_shape_manual(values = c(16, 17, 16, 17)) +
    theme(legend.position = "bottom")
}
