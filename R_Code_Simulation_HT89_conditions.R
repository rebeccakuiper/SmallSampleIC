#---------------------install/load packages----------------------
# packages necessary to perform simulation study

if (!require("devtools")) install.packages("devtools")
library(devtools) 
install_github("LeonardV/restriktor")
library(restriktor)
#if (!require("restriktor")) install.packages("restriktor") # install this package first (once)
#library(restriktor)
#
if (!require("MASS")) install.packages("MASS") # install this package first (once)
library(MASS)   # to call mvrnorm
if (!require("tidyverse")) install.packages("tidyverse") # install this package first (once)
library(tidyverse) #to plot results
if (!require("ggplot2")) install.packages("ggplot2")
library(ggplot2)
if (!require("gridExtra")) install.packages("gridExtra")
if (!require("cowplot")) install.packages("cowplot")
library(cowplot)
if (!require("jtools")) install.packages("jtools")
library(jtools)
if (!require("DataCombine")) install.packages("DataCombine")
library(DataCombine)
if (!require("dplyr")) install.packages("dplyr")
library(dplyr)
if (!require("ggpubr")) install.packages("ggpubr")
library(ggpubr)
if (!require("scales")) install.packages("scales")
library(scales)
if (!require("ggsci")) install.packages("ggsci")
library(ggsci)
if (!require("extrafont")) install.packages("extrafont")
library(extrafont)
if (!require("hrbrthemes")) install.packages("hrbrthemes")
library(hrbrthemes)
# TO DO is following needed (takes a lot of time):
#font_import() 
#y 
#fonts()


#----------------ADJUST, DOES NOT REMAIN THE SAME DURING EVERY NEW SIMULATION--------------------

#specify number of simulations
nsim <- 10 # 1000

# Paste a condition here; wrt p, hypos etc.
# In this file, we redo simulations of HT89 by inspecting multiple r2 and rho values.
p <- 7
ratio <- c(1, 2, 3, 0, 0, 0, 0)
ratio_name <- "ratio1230000"
length(ratio) == p


# Select on of following four simulations 

# Sim 0 - like HT89 do
HyposetName <- "HTset0"
H01 <- "X2 == 0; X3 == 0; X4 == 0; X5 == 0; X6 == 0; X7 == 0"
H02 <- "X3 == 0; X4 == 0; X5 == 0; X6 == 0; X7 == 0"
H03 <- "X4 == 0; X5 == 0; X6 == 0; X7 == 0"
H04 <- "X5 == 0; X6 == 0; X7 == 0"
H05 <- "X6 == 0; X7 == 0"
H06 <- "X7 == 0"
nrhypos <- 6
indexHtrue <- 3
#
##cat(paste0("H0", 1:nrhypos, ","))
goric_m <- "goric(fit.lm, H01, H02, H03, H04, H05, H06, type = 'goric')"
goricc_m <- "goric(fit.lm, H01, H02, H03, H04, H05, H06, type = 'goricc')"


# Sim 1
HyposetName <- "HTset1"
H01 <- "X3 == 0; X4 == 0; X5 == 0; X6 == 0; X7 == 0"
H02 <- "X4 == 0; X5 == 0; X6 == 0; X7 == 0"
H03 <- "X5 == 0; X6 == 0; X7 == 0"
H04 <- "X6 == 0; X7 == 0"
H05 <- "X7 == 0"
nrhypos <- 5
indexHtrue <- 2
#
##cat(paste0("H0", 1:nrhypos, ","))
goric_m <- "goric(fit.lm, H01, H02, H03, H04, H05, type = 'goric')"
goricc_m <- "goric(fit.lm, H01, H02, H03, H04, H05, type = 'goricc')"



# Sim 2 - inlcudes underfitting
HyposetName <- "HTset2"
H01 <- "X1 == 0; X2 == 0; X3 == 0; X4 == 0; X5 == 0; X6 == 0; X7 == 0"
H02 <- "X2 == 0; X3 == 0; X4 == 0; X5 == 0; X6 == 0; X7 == 0"
H03 <- "X3 == 0; X4 == 0; X5 == 0; X6 == 0; X7 == 0"
H04 <- "X4 == 0; X5 == 0; X6 == 0; X7 == 0"
H05 <- "X5 == 0; X6 == 0; X7 == 0"
H06 <- "X6 == 0; X7 == 0"
H07 <- "X7 == 0"
nrhypos <- 7
indexHtrue <- 4
#
goric_m <- "goric(fit.lm, H01, H02, H03, H04, H05, H06, H07, type = 'goric')"
goricc_m <- "goric(fit.lm, H01, H02, H03, H04, H05, H06, H07, type = 'goricc')"


# Sim 3
HyposetName <- "HTset3"
# Make all combinations of possible =hypotheses
# TO DO make more general!
h <- c('X1 == 0', 'X2 == 0', 'X3 == 0', 'X4 == 0', 'X5 == 0', 'X6 == 0', 'X7 == 0') #specify the variables
#h
#make columns per number of variables in hypothesis
for(i in 1:length(h)){
  assign(paste("col", i, sep = ""), col <- combn(h, i, FUN = function(x){return(paste(x, collapse = '; '))}))    
}
#length of columns "n"
maxcol <- max(length(col1), length(col2), length(col3), length(col4), length(col5), length(col6), length(col7)) # < sum(1:(1+length(h)))
#dataframe with all combinations of variables
hypothesis <- data.frame(col7[1:maxcol],col6[1:maxcol],col5[1:maxcol],col4[1:maxcol],col3[1:maxcol],col2[1:maxcol],col1[1:maxcol])
#hypothesis
#make dataframe without the missings and split in two columns
hypothesis <- gather(hypothesis, na.rm = TRUE, factor_key = TRUE)
#all hypothesis in text, by R
hypothesis$value
#
#number of hypotheses
nrhypos <- sum(!is.na(hypothesis$value))
#nrhypos
#
#create all hypotheses H01 to H0nrhypos
for(i in 1:nrhypos){
  assign(paste("H0", i, sep = ""), hypothesis$value[i])    
}
indexHtrue <- which(hypothesis$value == "X4 == 0; X5 == 0; X6 == 0; X7 == 0") # 64
#
##cat(paste0("H0", 1:nrhypos, ","))
goric_m <- "goric(fit.lm, H01, H02, H03, H04, H05, H06, H07, H08, H09, H010, H011, H012, H013, H014, H015, H016, H017, H018, H019, H020, H021, H022, H023, H024, H025, H026, H027, H028, H029, H030, H031, H032, H033, H034, H035, H036, H037, H038, H039, H040, H041, H042, H043, H044, H045, H046, H047, H048, H049, H050, H051, H052, H053, H054, H055, H056, H057, H058, H059, H060, H061, H062, H063, H064, H065, H066, H067, H068, H069, H070, H071, H072, H073, H074, H075, H076, H077, H078, H079, H080, H081, H082, H083, H084, H085, H086, H087, H088, H089, H090, H091, H092, H093, H094, H095, H096, H097, H098, H099, H0100, H0101, H0102, H0103, H0104, H0105, H0106, H0107, H0108, H0109, H0110, H0111, H0112, H0113, H0114, H0115, H0116, H0117, H0118, H0119, H0120, H0121, H0122, H0123, H0124, H0125, H0126, H0127, type = 'goric')"
goricc_m <- "goric(fit.lm, H01, H02, H03, H04, H05, H06, H07, H08, H09, H010, H011, H012, H013, H014, H015, H016, H017, H018, H019, H020, H021, H022, H023, H024, H025, H026, H027, H028, H029, H030, H031, H032, H033, H034, H035, H036, H037, H038, H039, H040, H041, H042, H043, H044, H045, H046, H047, H048, H049, H050, H051, H052, H053, H054, H055, H056, H057, H058, H059, H060, H061, H062, H063, H064, H065, H066, H067, H068, H069, H070, H071, H072, H073, H074, H075, H076, H077, H078, H079, H080, H081, H082, H083, H084, H085, H086, H087, H088, H089, H090, H091, H092, H093, H094, H095, H096, H097, H098, H099, H0100, H0101, H0102, H0103, H0104, H0105, H0106, H0107, H0108, H0109, H0110, H0111, H0112, H0113, H0114, H0115, H0116, H0117, H0118, H0119, H0120, H0121, H0122, H0123, H0124, H0125, H0126, H0127, type = 'goricc')"



# Sim 4 - only the correct hypothesis
HyposetName <- "HTset4"
H02 <- "X4 == 0; X5 == 0; X6 == 0; X7 == 0"
nrhypos <- 1
indexHtrue <- 1
#
##cat(paste0("H0", 1:nrhypos, ","))
goric_m <- "goric(fit.lm, H02, type = 'goric')"
goricc_m <- "goric(fit.lm, H02, type = 'goricc')"


# Sim 5 - only one hypothesis which is incorrect (H0), but has lower penalty than unc (1+0 vs 1+7)
HyposetName <- "HTset5"
H0 <- "X1 == 0; X2 == 0; X3 == 0; X4 == 0; X5 == 0; X6 == 0; X7 == 0"
nrhypos <- 1
indexHtrue <- 2
#
##cat(paste0("H0", 1:nrhypos, ","))
goric_m <- "goric(fit.lm, H0, type = 'goric')"
goricc_m <- "goric(fit.lm, H0, type = 'goricc')"





set.seed(123) # to obtain same results every time you run the same simulation

#ADJUST SAMPLE SIZE, TELLER R2 AND TELLERRHO, TO EXAMINE VARYING SITUATIONS
#n <- 20 # sample size, adjust each time
#tellerR2 <- 1 # adjust each time
#tellerRho <- 1 # adjust each time
#
# Make loop such that all conditions are run, I do thta below


#----------------DO NOT ADJUST, REMAINS THE SAME DURING EVERY NEW SIMULATION--------------------

#-------------EFFECT SIZE, CORRELATIE EN SAMPLE SIZE------------

f2 <- c(0.02, 0.15, 0.35) # effect sizes f2
r2 <- f2 / (1+f2) # transfrom f2 to R2 # R2 = .0196, .13, .26
r2 <- c(r2, .933) # Special for this simulation, since HT89 use this r2 (then beta = ratio)
rho <- c(0, 0.25, 0.5) # correlation between X's/predictors -- all set equal to rho
N <- c(10, 20, 50, 80, 160, 250) # sample size  # c(10, 20)


#dataframe for simulation results (true hypothesis rate per n) in one table
goricresult <- data.frame(criterion = NA, thr = NA, meanWeight = NA, Htrue = NA, bestH_thr = NA, bestH_meanW = NA, th.abs.best = NA, npred = NA, rpred = NA, ninf = NA, n = NA, cor_pop = NA, ES_pop = NA, meanES = NA)
#goricresult <- data.frame(thr = NA, meanWeight = NA, Htrue = NA, bestH_thr = NA, bestH_meanW = NA, th.abs.best = NA, npred = NA, rpred = NA, ninf = NA, n = NA, cor_pop = NA, ES_pop = NA, meanES = NA)
#goriccresult <- data.frame(thr = NA, meanWeight = NA, Htrue = NA, bestH_thr = NA, bestH_meanW = NA, th.abs.best = NA, npred = NA, rpred = NA, ninf = NA, n = NA, cor_pop = NA, ES_pop = NA, meanES = NA)


# Make loop such that input above wrt n and tellers is not needed
for(n in N){
  for(tellerR2 in 1:length(r2)){
    for(tellerRho in 1:length(rho)){
      
#----------------------------GORIC SIMULATIES -----------------------------

#RUN SIMULATIONS AFTER EVERY ADJUSTMENT

# Make variables for output from simulation
goric <- array(NA, c(nsim, (nrhypos+1)))
goricweights <- array(NA, c(nsim, (nrhypos+1)))
goricc <- array(NA, c(nsim, (nrhypos+1)))
goriccweights <- array(NA, c(nsim, (nrhypos+1)))
r2approx <- array(NA, c(nsim, (1)))

#matrix voor correlatie tussen variabelen
sigma <- matrix(rho[tellerRho], ncol=p, nrow = p)
for(i in 1:p){
  sigma[i,i] <- 1
}


# General formula to calculate population regression coefficients (betas)
betas <- NA
fun <- function(x){sum <- 0; for(i in 1:p){for(j in 1:p){sum <- sum + ratio[i]*x * ratio[j]*x * sigma[i,j]}}; sum - r2[tellerR2]}
x <- uniroot(fun, lower=0, upper=100)$root 
betas <- ratio*x
betas # population value for beta (notably: standardized beta)

#Check:
#sum <- 0; for(i in 1:p){for(j in 1:p){sum <- sum + (ratio[i]*x) * (ratio[j]*x) * sigma[i,j]}}; sum 
#r2[tellerR2]


betas_check <- 0 # As a check to see how simulation went

# Generate goric values = AIC values in case of equalities
# And generate goricc values = AICc values in case of equalities
for (i in 1:nsim) {
  cat("iteration =", p, "... =", i, "\n")
  
  # generate X, that is, create sample of predictor variables
  X <- mvrnorm(n, rep(0,p), sigma, empirical=FALSE)
  #
  # Generate error variable based on desired r2 (because all vars are standardized, var(resid)=1-R2)
  # So, not variance of 1 every time but 1-R2.
  var.epsilon <- 1-r2[tellerR2]
  error <- rnorm(n, sd=sqrt(var.epsilon))
  #
  # Make y based on true coefficients and desired r2
  intercept <- 0
  y <- intercept + X %*% betas + error
  #
  X <- scale(X) # We need this when comparing betas with GORIC(C).
  y <- scale(y) # Not per se needed, but is nice to check the estimated standardized beta values.
  
  #fit model
  fit.lm <- lm(y ~ 1 + X) # regression model, with intercept
  # As a check to see how simulation went:
  r2approx[i,] <- summary(lm(fit.lm))$r.squared
  betas_check <- betas_check + coef(fit.lm)
  
  # GORIC and GORICC
  goricm <- eval(parse(text=goric_m))
  goriccm <- eval(parse(text=goricc_m))
  #
  goric[i,] <- goricm$result[, 4]
  goricweights[i,] <- goricm$result[, 5]
  #
  goricc[i,] <- goriccm$result[, 4]
  goriccweights[i,] <- goriccm$result[, 5]
}

# As check to see how simulation went
# Compare r2approx with r2
#CHECK WHETHER ESTIMATED R2 OF SAMPLE MATCHES WITH POPULATION R2
#mean(r2approx[,1])  # estimated based on n and nsim
#r2[tellerR2]        # population
#
## Compare betas
#betas_check / nsim # estimated based on n and nsim
#betas              # population


#TRUE HYPOTHESIS RATE GORIC/AIC
#sum(apply(goric[,indexHtrue] < goric[,-indexHtrue], 1, all)) / nsim # true hypothesis rate
HRgoric <- matrix(NA, nrow = 1, ncol = (nrhypos))
for(i in 1:(nrhypos+1)){
  if(nrhypos == 1){
    HRgoric[i] <- sum(goric[,i] <= goric[,-i]) / nsim
  }else{
    HRgoric[i] <- sum(apply(goric[,i] <= goric[,-i], 1, all)) / nsim # how often is each hypothesis chosen
  }
}
#HRgoric
#HRgoric[indexHtrue] # true hypothesis rate
#
#
#TRUE HYPOTHESIS RATE GORICc
HRgoricc <- matrix(NA, nrow = 1, ncol = (nrhypos))
for(i in 1:(nrhypos+1)){
  if(nrhypos == 1){
    HRgoricc[i] <- sum(goricc[,i] <= goricc[,-i]) / nsim
  }else{
    HRgoricc[i] <- sum(apply(goricc[,i] <= goricc[,-i], 1, all)) / nsim # how often is each hypothesis chosen
  }
}
#HRgoricc
#HRgoricc[indexHtrue] # true hypothesis rate


# Kijk voor de gevallen waar de ware hypotheses gekozen wordt of daar het absolute verschil in goric waardes groter is dan Z (tov alle andere hypotheses in de set)
Z <- 3
if(nrhypos == 1){
  TH.goric.ab.best <- sum((abs(goric[,indexHtrue] - goric[,-indexHtrue]) > Z)[(goric[,indexHtrue] < goric[,-indexHtrue])]) / nsim
  # Kijk voor de gevallen waar de ware hypothese gekozen wordt, of daar het absolute verschil in goric waardes groter is dan X
  TH.goricc.ab.best <- sum((abs(goricc[,indexHtrue] - goricc[,-indexHtrue]) > Z)[(goricc[,indexHtrue] < goricc[,-indexHtrue])]) / nsim
}else{
  TH.goric.ab.best <- sum(apply(abs(goric[,indexHtrue] - goric[,-indexHtrue]) > Z, 1, all)[apply(goric[,indexHtrue] < goric[,-indexHtrue], 1, all)]) / nsim
  # Kijk voor de gevallen waar de ware hypothese gekozen wordt, of daar het absolute verschil in goric waardes groter is dan X
  TH.goricc.ab.best <- sum(apply(abs(goricc[,indexHtrue] - goricc[,-indexHtrue]) > Z, 1, all)[apply(goricc[,indexHtrue] < goricc[,-indexHtrue], 1, all)]) / nsim
}
#
meanGoricweights <- apply(goricweights, 2, mean) # mean of goric weights
meanGoricweights <- apply(goricweights, 2, mean, na.rm=T) # mean of goric weights# voor als er NA (= missings) zijn
#meanGoricweights
meangoriccweights <- apply(goriccweights, 2, mean) #mean of goricc weights
meangoriccweights <- apply(goriccweights, 2, mean, na.rm=T) #mean of goricc weights voor als er missings zijn
#meangoriccweights
#all(meanGoricweights[indexHtrue] > meanGoricweights[-indexHtrue]) # check whether this average is highest for true hypothesis
#which(meanGoricweights == max(meanGoricweights)) # gives index for hypothesis with highest mean goric weight.
#all(meangoriccweights[indexHtrue] > meangoriccweights[-indexHtrue])
#which(meangoriccweights == max(meangoriccweights))

#table with true hypothesis rate per n=sample size for goric and goricc
goricresult[nrow(goricresult) + 1,]   = list("aic", HRgoric[indexHtrue], meanGoricweights[indexHtrue], indexHtrue, which.max(HRgoric), which.max(meanGoricweights), TH.goric.ab.best, length(ratio), sum(ratio), length(ratio[ratio != 0]), n, rho[tellerRho], r2[tellerR2], mean(r2approx[,1]))
goricresult[nrow(goricresult) + 1,]   = list("aicc", HRgoricc[indexHtrue], meangoriccweights[indexHtrue], indexHtrue, which.max(HRgoricc), which.max(meangoriccweights), TH.goricc.ab.best, length(ratio), sum(ratio), length(ratio[ratio != 0]), n, rho[tellerRho], r2[tellerR2], mean(r2approx[,1]))
#goricresult[nrow(goricresult) + 1,]   = list(HRgoric[indexHtrue], meanGoricweights[indexHtrue], indexHtrue, which.max(HRgoric), which.max(meanGoricweights), TH.goric.ab.best, length(ratio), sum(ratio), length(ratio[ratio != 0]), n, rho[tellerRho], r2[tellerR2], mean(r2approx[,1]))
#goriccresult[nrow(goriccresult) + 1,] = list(HRgoricc[indexHtrue], meangoriccweights[indexHtrue], indexHtrue, which.max(HRgoricc), which.max(meangoriccweights), TH.goricc.ab.best, length(ratio), sum(ratio), length(ratio[ratio != 0]), n, rho[tellerRho], r2[tellerR2], mean(r2approx[,1]))
#goricresult
#goriccresult


} # End of loop for n
}} # End of loops for r2 and rho

goricresult <- goricresult[-1,] # Note: delete first row here, because it consists of NAs
#goriccresult[-1,] # Note: delete first row here, because it consists of NAs


name <- paste0("Output_aic_cond_", ratio_name, "_", HyposetName, ".rds")
# Save an object to a file
saveRDS(goricresult, file = name)
# Restore the object
goricresult <- readRDS(name)



### MAKE PLOTS IF ALL SIMULATIONS ARE DONE! ####

#-----------------------------------plot results four variables-----------------------------


goricresult$cor_pop <- as.factor(goricresult$cor_pop)
#goricresult$cor_pop <- factor(goricresult$cor_pop,
#                              levels = levels(rho),
#                              labels = c(paste("rho == ", rho[1], sep = ""),
#                                         paste("rho == ", rho[2], sep = ""),
#                                         paste("rho == ", rho[3], sep = ""))
#                              )
goricresult$cor_pop <- factor(goricresult$cor_pop,
                              levels = levels(as.factor(rho)),
                              labels = as.vector(paste("rho == ", rho[1:length(rho)], sep = ""))
)

goricresult$ES_pop <- as.factor(goricresult$ES_pop)
#goricresult$ES_pop <- factor(goricresult$ES_pop,
#                             levels = levels(goricresult$ES_pop),
#                             labels = c(paste(expression(italic("R")),"^2 == ", round(r2[1], 2), sep = ""),
#                                        paste(expression(italic("R")),"^2 == ", round(r2[2], 2), sep = ""),
#                                        paste(expression(italic("R")),"^2 == ", round(r2[3], 2), sep = ""),
#                                        paste(expression(italic("R")),"^2 == ", round(r2[4], 2), sep = "")))
goricresult$ES_pop <- factor(goricresult$ES_pop,
                             levels = levels(as.factor(r2)),
                             labels = as.vector(paste(expression(italic("R")),"^2 == ", round(r2[1:length(r2)], 2), sep = ""))
)

plot_HT89_cond <- ggplot(data = goricresult) +
  geom_line(mapping = aes(x = n, y = thr, colour = criterion)) +
  facet_grid(ES_pop ~ cor_pop,
             labeller = label_parsed) + 
  theme_apa() +
  geom_point(mapping = aes(x = n, y = thr, colour = criterion)) + 
  facet_grid(ES_pop ~ cor_pop,
             labeller = label_parsed) + 
  theme_apa() +
  scale_x_log10(breaks = N) +
  ylim(0, 1) +
  theme(legend.position = "bottom") +
  ggtitle("Based on Hurvich and Tsai (1989)") +
  scale_color_jco() +
  xlab("Sample Size") +
  ylab("True Hypothesis Rate") +
  theme(text = element_text(family = "Arial", size=12))

plot_HT89_cond

ggsave(paste0("Plot_aic_cond_", ratio_name, "_", HyposetName, ".png"), plot = last_plot(), device = "png") #voor .png format
ggsave(paste0("Plot_aic_cond_", ratio_name, "_", HyposetName, ".pdf"), plot = last_plot(), device = cairo_pdf) #voor .pdf format
# Close the pdf and jpg file
dev.off() 
