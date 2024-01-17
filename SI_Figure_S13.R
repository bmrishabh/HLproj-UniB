# Code for figures with p-beta positive trade-off

#Generating data for SI figure where p=beta evolves (with N0 Switch)
#a0=0.1 [~1-2 minutes]
rm(list = ls())
source("Bagpack.R")

foldername = "Data/p-beta-pos-tradeoff-mean_0p65-sd_0p17-a0_0p1-N0_switched"
trait = c("p1","beta")
parsweep_trait = c()
obj <- MCfun_multcases(a0 = 0.1, b0 = 30, b1 = 500, bins=21,
                       p1 = 0.75, beta = 0.75, vartrait = trait, ToffMinMaxPos = c(0,1,TRUE),
                       ttmin=0, ttmax=1, ttmean=0.65, ttsd=0.17,
                       e = 0.2, ec = 0.9, C01 = 0.5, C02 = 0.5, C11 = 50, C12 =50,
                       N01 = 3, N02 = 4, R01 = 10, R02 = 10, foldername = foldername,
                       mu = 0.3, d = 0.5, k = 0.003, t0 = 3500, theta = 2, times = seq(0, 8000, 1),
                       mutations = T, parasweep=parsweep_trait, GeneticMixing=T, Allee = T, HLatEQ = F, savedata = T)


#a0=0.11 [~1-2 minutes]
rm(list = ls())
source("Bagpack.R")

foldername = "Data/p-beta-pos-tradeoff-mean_0p65-sd_0p17-a0_0p11-N0_switched"
trait = c("p1","beta")
parsweep_trait = c()
obj <- MCfun_multcases(a0 = 0.11, b0 = 30, b1 = 500, bins=21,
                       p1 = 0.75, beta = 0.75, vartrait = trait, ToffMinMaxPos = c(0,1,TRUE),
                       ttmin=0, ttmax=1, ttmean=0.65, ttsd=0.17,
                       e = 0.2, ec = 0.9, C01 = 0.5, C02 = 0.5, C11 = 50, C12 =50,
                       N01 = 3, N02 = 4, R01 = 10, R02 = 10, foldername = foldername,
                       mu = 0.3, d = 0.5, k = 0.003, t0 = 3500, theta = 2, times = seq(0, 8000, 1),
                       mutations = T, parasweep=parsweep_trait, GeneticMixing=T, Allee = T, HLatEQ = F, savedata = T)


#a0=0.12 [~1-2 minutes]
rm(list = ls())
source("Bagpack.R")

foldername = "Data/p-beta-pos-tradeoff-mean_0p65-sd_0p17-a0_0p13-N0_switched"
trait = c("p1","beta")
parsweep_trait = c()
obj <- MCfun_multcases(a0 = 0.13, b0 = 30, b1 = 500, bins=21,
                       p1 = 0.75, beta = 0.75, vartrait = trait, ToffMinMaxPos = c(0,1,TRUE),
                       ttmin=0, ttmax=1, ttmean=0.65, ttsd=0.17,
                       e = 0.2, ec = 0.9, C01 = 0.5, C02 = 0.5, C11 = 50, C12 =50,
                       N01 = 3, N02 = 4, R01 = 10, R02 = 10, foldername = foldername,
                       mu = 0.3, d = 0.5, k = 0.003, t0 = 3500, theta = 2, times = seq(0, 8000, 1),
                       mutations = T, parasweep=parsweep_trait, GeneticMixing=T, Allee = T, HLatEQ = F, savedata = T)


#a0=0.13 [~1-2 minutes]
rm(list = ls())
source("Bagpack.R")

foldername = "Data/p-beta-pos-tradeoff-mean_0p65-sd_0p17-a0_0p15-N0_switched"
trait = c("p1","beta")
parsweep_trait = c()
obj <- MCfun_multcases(a0 = 0.15, b0 = 30, b1 = 500, bins=21,
                       p1 = 0.75, beta = 0.75, vartrait = trait, ToffMinMaxPos = c(0,1,TRUE),
                       ttmin=0, ttmax=1, ttmean=0.65, ttsd=0.17,
                       e = 0.2, ec = 0.9, C01 = 0.5, C02 = 0.5, C11 = 50, C12 =50,
                       N01 = 3, N02 = 4, R01 = 10, R02 = 10, foldername = foldername,
                       mu = 0.3, d = 0.5, k = 0.003, t0 = 3500, theta = 2, times = seq(0, 8000, 1),
                       mutations = T, parasweep=parsweep_trait, GeneticMixing=T, Allee = T, HLatEQ = F, savedata = T)

#Plot the figure by sourcing the file which plots it
source("plotcode/SI_Figure_S13_plotfile.R")






