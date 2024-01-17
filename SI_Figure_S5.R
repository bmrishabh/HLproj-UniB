# For general exploration (connected patches)

# Generate data for default values when a0=0.1 and initial densities (N02) is varied [~19-20 minutes]
rm(list = ls())
source("Bagpack.R")
foldername = "Data/e-var-mean_0p2-sd_0p1-a0_0p1-parsweep_N02"
trait = c("e")
parsweep_trait = c("N02")
obj <- MCfun_multcases(a0 = 0.1, b0 = 30, b1 = 500, bins=21,
                       p1 = 0.75, beta = 0.75, vartrait = trait, ToffMinMaxPos = c(0,1,TRUE),
                       ttmin=0, ttmax=0.4, ttmean=0.2, ttsd=0.08,
                       e = 0.2, ec = 0.9, C01 = 0.5, C02 = 0.5, C11 = 50, C12 =50,
                       N01 = 4, N02 = seq(2,6,0.2), R01 = 10, R02 = 10, foldername = foldername,
                       mu = 0.3, d = 0.5, k = 0.003, t0 = 3500, theta = 2, times = seq(0, 8000, 1),
                       mutations = T, parasweep=parsweep_trait, GeneticMixing=T, Allee = T, HLatEQ = F, savedata = T)



# Generate data for default values when a0=0.1 and p=0.5 and initial densities (N02) is varied [~19-20 minutes]
rm(list = ls())
source("Bagpack.R")
foldername = "Data/e-var-mean_0p2-sd_0p1-a0_0p1-p_0p5-parsweep_N02"
trait = c("e")
parsweep_trait = c("N02")
obj <- MCfun_multcases(a0 = 0.1, b0 = 30, b1 = 500, bins=21,
                       p1 = 0.5, beta = 0.75, vartrait = trait, ToffMinMaxPos = c(0,1,TRUE),
                       ttmin=0, ttmax=0.4, ttmean=0.2, ttsd=0.08,
                       e = 0.2, ec = 0.9, C01 = 0.5, C02 = 0.5, C11 = 50, C12 =50,
                       N01 = 4, N02 = seq(2,6,0.2), R01 = 10, R02 = 10, foldername = foldername,
                       mu = 0.3, d = 0.5, k = 0.003, t0 = 3500, theta = 2, times = seq(0, 8000, 1),
                       mutations = T, parasweep=parsweep_trait, GeneticMixing=T, Allee = T, HLatEQ = F, savedata = T)


#Plot the figure by sourcing the file which plots it
source("plotcode/SI_Figure_S5_plotfile.R")



