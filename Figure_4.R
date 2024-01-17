# Generating data and figure for comparing the variance scenarios when either epsilon or b0 evolves


# Generate data for default values and epsilon evolves [~1-2 minutes]
### Might already have this from previous figure, if not comment out and run the section ###
# rm(list = ls())
# source("Bagpack.R")
# foldername = "Data/e-var-mean_0p2-sd_0p1-a0_0p1"
# trait = c("e")
# parsweep_trait = c()
# obj <- MCfun_multcases(a0 = 0.1, b0 = 30, b1 = 500, bins=21,
#                        p1 = 0.75, beta = 0.75, vartrait = trait, ToffMinMaxPos = c(0,1,TRUE),
#                        ttmin=0, ttmax=0.4, ttmean=0.2, ttsd=0.1,
#                        e = 0.2, ec = 0.9, C01 = 0.5, C02 = 0.5, C11 = 50, C12 =50,
#                        N01 = 4, N02 = 3, R01 = 10, R02 = 10, foldername = foldername,
#                        mu = 0.3, d = 0.5, k = 0.003, t0 = 3500, theta = 2, times = seq(0, 8000, 1),
#                        mutations = T, parasweep=parsweep_trait, GeneticMixing=T, Allee = T, HLatEQ = F, savedata = T)


# Generate data for default values when b0 evolves [~1-2 minutes]
rm(list = ls())
source("Bagpack.R")
foldername = "Data/b0-var-mean_30-sd_7p5-a0_0p1"
trait = c("b0")
parsweep_trait = c()
obj <- MCfun_multcases(a0 = 0.1, b0 = 30, b1 = 500, bins=21,
                       p1 = 0.75, beta = 0.75, vartrait = trait, ToffMinMaxPos = c(0,1,TRUE),
                       ttmin=15, ttmax=45, ttmean=30, ttsd=7.5,
                       e = 0.2, ec = 0.9, C01 = 0.5, C02 = 0.5, C11 = 50, C12 =50,
                       N01 = 4, N02 = 3, R01 = 10, R02 = 10, foldername = foldername,
                       mu = 0.3, d = 0.5, k = 0.003, t0 = 3500, theta = 2, times = seq(0, 8000, 1),
                       mutations = T, parasweep=parsweep_trait, GeneticMixing=T, Allee = T, HLatEQ = F, savedata = T)


# Generate data for default values when b1 evolves [~2-3 minutes]
rm(list = ls())
source("Bagpack.R")
foldername = "Data/b1-var-mean_500-sd_200-ttmin_100-ttmax_900"
trait = c("b1")
parsweep_trait = c()
obj <- MCfun_multcases(a0 = 0.1, b0 = 30, b1 = 500, bins=21,
                       p1 = 0.75, beta = 0.75, vartrait = trait, ToffMinMaxPos = c(0,1,TRUE),
                       ttmin=100, ttmax=900, ttmean=500, ttsd=200,
                       e = 0.2, ec = 0.9, C01 = 0.5, C02 = 0.5, C11 = 50, C12 =50,
                       N01 = 4, N02 = 3, R01 = 10, R02 = 10, foldername = foldername,
                       mu = 0.3, d = 0.5, k = 0.003, t0 = 3500, theta = 2, times = seq(0, 8000, 1),
                       mutations = T, parasweep=parsweep_trait, GeneticMixing=T, Allee = T, HLatEQ = F, savedata = T)


#Plot the figure by sourcing the file which plots it
source("plotcode/Figure_4_plotfile.R")




