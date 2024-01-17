# Code to generate data for parameter space plots over d
# patch dependence and independence data as well

# Parameter scan over d while b1 is evolving [~25-30 mins]
rm(list = ls())
source("Bagpack.R")

foldername = "Data/b1-var-mean_500-sd_200-ttmin_100-ttmax_900-parsweep_d"
trait = c("b1")
parsweep_trait = c("d")
obj <- MCfun_multcases(a0 = 0.1, b0 = 30, b1 = 500, bins=21,
                       p1 = 0.75, beta = 0.75, vartrait = trait, ToffMinMaxPos = c(0,1,TRUE),
                       ttmin=100, ttmax=900, ttmean=500, ttsd=200,
                       e = 0.2, ec = 0.9, C01 = 0.5, C02 = 0.5, C11 = 50, C12 =50,
                       N01 = 4, N02 = 3, R01 = 10, R02 = 10, foldername = foldername,
                       mu = 0.3, d = seq(0,0.5,0.025), k = 0.003, t0 = 3500, theta = 2, times = seq(0, 8000, 1),
                       mutations = T, parasweep=parsweep_trait, GeneticMixing=T, Allee = T, HLatEQ = F, savedata = T)


# Parameter scan over d while b1 is evolving and no patch dependence [~35-40 mins]
rm(list = ls())
source("Bagpack-noPD.R")

foldername = "Data/b1-var-mean_500-sd_200-ttmin_100-ttmax_900-parsweep_d-noPD"
trait = c("b1")
parsweep_trait = c("d")
obj <- MCfun_multcases(a0 = 0.1, b0 = 30, b1 = 500, bins=21,
                       p1 = 0.75, beta = 0.75, vartrait = trait, ToffMinMaxPos = c(0,1,TRUE),
                       ttmin=100, ttmax=900, ttmean=500, ttsd=200,
                       e = 0.2, ec = 0.9, C01 = 0.5, C02 = 0.5, C11 = 50, C12 =50,
                       N01 = 4, N02 = 3, R01 = 10, R02 = 10, foldername = foldername,
                       mu = 0.3, d = seq(0,0.5,0.025), k = 0.003, t0 = 3500, theta = 2, times = seq(0, 8000, 0.5),
                       mutations = T, parasweep=parsweep_trait, GeneticMixing=T, Allee = T, HLatEQ = F, savedata = T)

#Plot the figure by sourcing the file which plots it
source("plotcode/SI_Figure_S12_plotfile.R")

