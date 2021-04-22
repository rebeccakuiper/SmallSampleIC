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
nsim <- 1000 # 1000

# Paste a condition here. 
# For AIC, see "Conditions AIC.R" en gebruik "R_Code_Simulation_HT89_conditions.R"
# p, hypos etc
p <- 4
ratio_nr <- 2 # 1 (=1234) or 2 (=5678)


#
# Hypo set 1
HyposetName <- "HypoSet1" # change this name per set of hypotheses you run. 
# Make overview here or somewhere, such that we know what the set numbers refer to (or give other types of names; e.g., HypoSetH1)
H1 <- "X1 < X2; X2 < X3; X3 < X4" # change accordingly!
nrhypos <- 1 # change accordingly!
indexHtrue <- 1 # change accordingly!
#
##cat(paste0("H0", 1:nrhypos, ","))
#goric_m <- "goric(fit.lm, H01, H02, H03, H04, H05, H06, H07, H08, H09, H010, H011, H012, H013, H014, H015, type = 'goric')"
goric_m <- "goric(fit.lm, H1, type = 'goric')"
goricc_m <- "goric(fit.lm, H1, type = 'goricc')"
#goric_m <- "goric(fit.lm, H1, comparison = 'complement', type = 'goric')"
#goricc_m <- "goric(fit.lm, H1, comparison = 'complement', type = 'goricc')"
#
#


# Hypo set 2
HyposetName <- "HypoSet2"
H1 <- "X1 > X2; X2 > X3; X3 > X4" # change accordingly!
nrhypos <- 1 # change accordingly!
indexHtrue <- 2 # change accordingly!
#
# change accordingly:
##cat(paste0("H0", 1:nrhypos, ","))
#goric_m <- "goric(fit.lm, H01, H02, H03, H04, H05, H06, H07, H08, H09, H010, H011, H012, H013, H014, H015, type = 'goric')"
goric_m <- "goric(fit.lm, H1, type = 'goric')"
goricc_m <- "goric(fit.lm, H1, type = 'goricc')"
#goric_m <- "goric(fit.lm, H1, comparison = 'complement', type = 'goric')"
#goricc_m <- "goric(fit.lm, H1, comparison = 'complement', type = 'goricc')" 
#
# 


# Hypo set 3
HyposetName <- "HypoSet3" # change this name per set of hypotheses you run. 
# Make overview here or somewhere, such that we know what the set numbers refer to (or give other types of names; e.g., HypoSetH1)
H1 <- "X1 < X2; X2 < X3; X3 < X4" # change accordingly!
nrhypos <- 1 # change accordingly!
indexHtrue <- 1 # change accordingly!
#
##cat(paste0("H0", 1:nrhypos, ","))
#goric_m <- "goric(fit.lm, H01, H02, H03, H04, H05, H06, H07, H08, H09, H010, H011, H012, H013, H014, H015, type = 'goric')"
goric_m <- "goric(fit.lm, H1, comparison = 'complement', type = 'goric')"
goricc_m <- "goric(fit.lm, H1, comparison = 'complement', type = 'goricc')"
#goric_m <- "goric(fit.lm, H1, comparison = 'complement', type = 'goric')"
#goricc_m <- "goric(fit.lm, H1, comparison = 'complement', type = 'goricc')"
#
#

# Hypo set 4
HyposetName <- "HypoSet4"
H1 <- "X1 > X2; X2 > X3; X3 > X4" # change accordingly!
nrhypos <- 1 # change accordingly!
indexHtrue <- 2 # change accordingly!
#
# change accordingly:
##cat(paste0("H0", 1:nrhypos, ","))
#goric_m <- "goric(fit.lm, H01, H02, H03, H04, H05, H06, H07, H08, H09, H010, H011, H012, H013, H014, H015, type = 'goric')"
goric_m <- "goric(fit.lm, H1, comparison = 'complement', type = 'goric')"
goricc_m <- "goric(fit.lm, H1, comparison = 'complement', type = 'goricc')"
#goric_m <- "goric(fit.lm, H1, comparison = 'complement', type = 'goric')"
#goricc_m <- "goric(fit.lm, H1, comparison = 'complement', type = 'goricc')" 
#
# 


#
# Hypo set 5
HyposetName <- "HypoSet5" # change this name per set of hypotheses you run. 
# Make overview here or somewhere, such that we know what the set numbers refer to (or give other types of names; e.g., HypoSetH1)
H1 <- "X1 > X2; X2 < X3; X3 < X4" # change accordingly!
nrhypos <- 1 # change accordingly!
indexHtrue <- 2 # change accordingly!
#
##cat(paste0("H0", 1:nrhypos, ","))
#goric_m <- "goric(fit.lm, H01, H02, H03, H04, H05, H06, H07, H08, H09, H010, H011, H012, H013, H014, H015, type = 'goric')"
goric_m <- "goric(fit.lm, H1, type = 'goric')"
goricc_m <- "goric(fit.lm, H1, type = 'goricc')"
#goric_m <- "goric(fit.lm, H1, comparison = 'complement', type = 'goric')"
#goricc_m <- "goric(fit.lm, H1, comparison = 'complement', type = 'goricc')"
#
#


# Hypo set 6
HyposetName <- "HypoSet6" # change this name per set of hypotheses you run. 
# Make overview here or somewhere, such that we know what the set numbers refer to (or give other types of names; e.g., HypoSetH1)
H1 <- "X1 > X2; X2 < X3; X3 < X4" # change accordingly!
nrhypos <- 1 # change accordingly!
indexHtrue <- 2 # change accordingly!
#
##cat(paste0("H0", 1:nrhypos, ","))
#goric_m <- "goric(fit.lm, H01, H02, H03, H04, H05, H06, H07, H08, H09, H010, H011, H012, H013, H014, H015, type = 'goric')"
goric_m <- "goric(fit.lm, H1, comparison = 'complement', type = 'goric')"
goricc_m <- "goric(fit.lm, H1, comparison = 'complement', type = 'goricc')"
#goric_m <- "goric(fit.lm, H1, comparison = 'complement', type = 'goric')"
#goricc_m <- "goric(fit.lm, H1, comparison = 'complement', type = 'goricc')"
#
#

# 
# Hypo set 7
HyposetName <- "HypoSet7" # change this name per set of hypotheses you run. 
# Make overview here or somewhere, such that we know what the set numbers refer to (or give other types of names; e.g., HypoSetH1)
H1 <- "X1 < X2; X2 < X3; X3 < X4" # change accordingly!
H2 <- "X1 > X2; X2 < X3; X3 < X4" # change accordingly!
nrhypos <- 2 # change accordingly!
indexHtrue <- 1 # change accordingly!
#
##cat(paste0("H0", 1:nrhypos, ","))
#goric_m <- "goric(fit.lm, H01, H02, H03, H04, H05, H06, H07, H08, H09, H010, H011, H012, H013, H014, H015, type = 'goric')"
goric_m <- "goric(fit.lm, H1, H2, type = 'goric')"
goricc_m <- "goric(fit.lm, H1, H2, type = 'goricc')"
#goric_m <- "goric(fit.lm, H1, comparison = 'complement', type = 'goric')"
#goricc_m <- "goric(fit.lm, H1, comparison = 'complement', type = 'goricc')"
#
#

#
# Hypo set 8
HyposetName <- "HypoSet8" # change this name per set of hypotheses you run. 
# Make overview here or somewhere, such that we know what the set numbers refer to (or give other types of names; e.g., HypoSetH1)
H1 <- "X1 < X2; X2 < X3; X3 < X4" # change accordingly!
H2 <- "X1 == X2; X2 == X3; X3 == X4" # change accordingly!
nrhypos <- 2 # change accordingly!
indexHtrue <- 1 # change accordingly!
#
##cat(paste0("H0", 1:nrhypos, ","))
#goric_m <- "goric(fit.lm, H01, H02, H03, H04, H05, H06, H07, H08, H09, H010, H011, H012, H013, H014, H015, type = 'goric')"
goric_m <- "goric(fit.lm, H1, H2, type = 'goric')"
goricc_m <- "goric(fit.lm, H1, H2, type = 'goricc')"
#goric_m <- "goric(fit.lm, H1, comparison = 'complement', type = 'goric')"
#goricc_m <- "goric(fit.lm, H1, comparison = 'complement', type = 'goricc')"
#
#


if(ratio_nr == 1){
  ratio <- c(1, 2, 3, 4) # 1234, 5678
  ratio_name <- "ratio1234"
  ratio_title <- paste("Ratio of effects is 1:2:3:4")
}else{
  ratio <- c(5, 6, 7, 8)
  ratio_name <- "ratio5678"
  ratio_title <- paste("Ratio of effects is 5:6:7:8")
}
# Check
if(length(ratio) != p){"WARNING: ratio is not of length p"}


set.seed(123) # to obtain same results every time you run the same simulation

#ADJUST SAMPLE SIZE, TELLER R2 AND TELLERRHO, TO EXAMINE VARYING SITUATIONS
#n <- 20 # sample size, adjust each time
#tellerR2 <- 1 # adjust each time
#tellerRho <- 1 # adjust each time
#
# Make loop such that all conditions are run, I do that below


#----------------DO NOT ADJUST, REMAINS THE SAME DURING EVERY NEW SIMULATION--------------------

#-------------EFFECT SIZE, CORRELATIE EN SAMPLE SIZE------------

f2 <- c(0.02, 0.15, 0.35) # effect sizes f2
r2 <- f2 / (1+f2) # transfrom f2 to R2 # R2 = .0196, .13, .26
r2 <- c(r2, .933)
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
goricresult[nrow(goricresult) + 1,]   = list("goric", HRgoric[indexHtrue], meanGoricweights[indexHtrue], indexHtrue, which.max(HRgoric), which.max(meanGoricweights), TH.goric.ab.best, length(ratio), sum(ratio), length(ratio[ratio != 0]), n, rho[tellerRho], r2[tellerR2], mean(r2approx[,1]))
goricresult[nrow(goricresult) + 1,]   = list("goricc", HRgoricc[indexHtrue], meangoriccweights[indexHtrue], indexHtrue, which.max(HRgoricc), which.max(meangoriccweights), TH.goricc.ab.best, length(ratio), sum(ratio), length(ratio[ratio != 0]), n, rho[tellerRho], r2[tellerR2], mean(r2approx[,1]))
#goricresult[nrow(goricresult) + 1,]   = list(HRgoric[indexHtrue], meanGoricweights[indexHtrue], indexHtrue, which.max(HRgoric), which.max(meanGoricweights), TH.goric.ab.best, length(ratio), sum(ratio), length(ratio[ratio != 0]), n, rho[tellerRho], r2[tellerR2], mean(r2approx[,1]))
#goriccresult[nrow(goriccresult) + 1,] = list(HRgoricc[indexHtrue], meangoriccweights[indexHtrue], indexHtrue, which.max(HRgoricc), which.max(meangoriccweights), TH.goricc.ab.best, length(ratio), sum(ratio), length(ratio[ratio != 0]), n, rho[tellerRho], r2[tellerR2], mean(r2approx[,1]))
#goricresult
#goriccresult


}}} # End of loops for n, r2 and rho

goricresult <- goricresult[-1,] # Note: delete first row here, because it consists of NAs
#goriccresult[-1,] # Note: delete first row here, because it consists of NAs


name <- paste0("Output_goric_", ratio_name, "_", HyposetName, ".rds")
# Save an object to a file
saveRDS(goricresult, file = name)
# Restore the object
goricresult <- readRDS(name)


### MAKE PLOTS IF ALL SIMULATIONS ARE DONE! ####

#-----------------------------------plot results four variables-----------------------------

#name_plot <- paste0("Plot_goric_", ratio_name)
#
# Open a pdf file
#pdf(paste0(name_plot, ".pdf")) 
# Open jpeg file
#jpeg(paste0(name_plot, ".jpg"), width = 350, height = "350") # Error


goricresult$cor_pop <- as.factor(goricresult$cor_pop)
goricresult$cor_pop <- factor(goricresult$cor_pop,
                              levels = levels(goricresult$cor_pop),
                              labels = c(paste("rho == ", rho[1], sep = ""),
                                         paste("rho == ", rho[2], sep = ""),
                                         paste("rho == ", rho[3], sep = ""))
)
#goricresult$cor_pop <- factor(goricresult$cor_pop,
#                              levels = levels(as.factor(rho)),
#                              labels = as.vector(paste("rho == ", rho[1:length(rho)], sep = ""))
#)

goricresult$ES_pop <- as.factor(goricresult$ES_pop)
goricresult$ES_pop <- factor(goricresult$ES_pop,
                             levels = levels(goricresult$ES_pop),
                             labels = c(paste(expression(italic("R")),"^2 == ", round(r2[1], 2), sep = ""),
                                        paste(expression(italic("R")),"^2 == ", round(r2[2], 2), sep = ""),
                                        paste(expression(italic("R")),"^2 == ", round(r2[3], 2), sep = ""),
                                        paste(expression(italic("R")),"^2 == ", round(r2[4], 2), sep = "")))
#goricresult$ES_pop <- factor(goricresult$ES_pop,
#                             levels = levels(as.factor(r2)),
#                             labels = as.vector(paste(expression(italic("R")),"^2 == ", round(r2[1:length(r2)], 2), sep = ""))
#)

plot4_goric <- ggplot(data = goricresult) +
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
  ggtitle(ratio_title) +
  scale_color_jco() +
  xlab("Sample Size") +
  ylab("True Hypothesis Rate") +
  theme(text = element_text(family = "Arial", size=12))

plot4_goric

ggsave(paste0("Plot_goric_", ratio_name, "_", HyposetName, ".png"), plot = last_plot(), device = "png") #voor .png format
ggsave(paste0("Plot_goric_", ratio_name, "_", HyposetName, ".pdf"), plot = last_plot(), device = cairo_pdf) #voor .pdf format
# Close the pdf and jpg file
dev.off() 


# TO DO
#Nb nog iets met meanWeights doen? Ook in plot?? 

# TO DO 
# ook een alg AIC sim file maken 
# adhv p_rel en p_irr
# Namen met Tapering en Equal effects en evt ratios er in verwerpen adhv de p's
#
