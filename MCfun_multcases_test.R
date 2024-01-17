# For general exploration (connected patches)

# Parameter scan over d while epsilon is evolving and no patch dependence, a0=0.14 [~35-40 mins]
rm(list = ls())
source("Bagpack-noPD.R")

foldername = "Data/e-var-mean_0p2-sd_0p1-parsweep_d-a0_0p15-noPD"
trait = c("e")
parsweep_trait = c("d")
obj <- MCfun_multcases(a0 = 0.15, b0 = 30, b1 = 500, bins=21,
                       p1 = 0.75, beta = 0.75, vartrait = trait, ToffMinMaxPos = c(0,1,TRUE),
                       ttmin=0, ttmax=0.4, ttmean=0.2, ttsd=0.1,
                       e = 0.2, ec = 0.9, C01 = 0.5, C02 = 0.5, C11 = 50, C12 =50,
                       N01 = 4, N02 = 3, R01 = 10, R02 = 10, foldername = foldername,
                       mu = 0.3, d = seq(0,0.5,0.025), k = 0.003, t0 = 3500, theta = 2, times = seq(0, 8000, 0.5),
                       mutations = T, parasweep=parsweep_trait, GeneticMixing=T, Allee = T, HLatEQ = F, savedata = T)


# Raw plots for observation (not used in the manuscript)
plot_varcomp(foldername, parasweep=parsweep_trait)
plot_HLcomp(foldername, parasweep=parsweep_trait)
plot_LandscapeHLcomp(foldername, parasweep=parsweep_trait)
# plot_multcases(foldername,traits=trait,parasweep=parsweep_trait)
# 
