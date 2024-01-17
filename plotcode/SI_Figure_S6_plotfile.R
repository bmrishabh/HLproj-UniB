# We compare equilibrium values over parameter space with different variation cases
rm(list = ls())
library(ggplot2)
library(ggpubr)
library(latex2exp) #useful to use latex symbols
library(reshape2) #for reshaping dataframes
loadRData <- function(fileName){ #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}

###################### Preppeing Data ########################
filname <- "SI_Figure_S6.pdf"

parsweep.curator <- function(filepath, trait) {
  obj <- loadRData(filepath)
  
  cases <- expand.grid(Va=c(T,F),HL=c(T,F),VM=c(T,F))
  cases <- cases[!(cases[,"Va"]==F & cases[,"VM"]==T),]
  
  Data <- data.frame()
  for (j in 1:length(obj)) {
    for (i in 1:nrow(cases)) {
      obj1<-obj[[j]]
      len <- nrow(obj1[[i]]$params)
      tempData <- data.frame(cbind(N1=mean(tail(rowSums(obj1[[i]]$out[, 2:(len+1), drop=F]),n=1)),
                                   N2=mean(tail(rowSums(obj1[[i]]$out[,(len+2):(2*len+1), drop=F]),n=1)),
                                   R1=mean(tail(obj1[[i]]$out[, (2*len+2)],n=1)),
                                   R2=mean(tail(obj1[[i]]$out[, (2*len+3)],n=1))))
      tempData$type <- if (cases[i,"VM"]==TRUE) "Constant Variation" else if (cases[i,"Va"]==FALSE) "No Variation" else "Heritable Variation"
      tempData$HL <- if (cases[i,"HL"]==TRUE) "Habitat Loss" else "No Habitat Loss"
      tempData$param_val <- as.numeric(names(obj)[j])
      Data <- rbind(Data,tempData)
    }
  }
  Data$type <- factor(Data$type,levels=c("No Variation","Heritable Variation","Constant Variation"))
  Data$HL <- factor(Data$HL, levels = c("No Habitat Loss", "Habitat Loss"))
  Data$Trait <- trait
  Data$Trait <- factor(Data$Trait)
  Data$N <- Data$N1 + Data$N2
  Data <- melt(Data, id.vars=c("R1","R2","type","HL","param_val","Trait"),
               variable.name = "patch", value.name="density")
  Data$patch <- factor(Data$patch)
  return(Data)
}

Data.b1 <- parsweep.curator("Data/b1-var-mean_500-ttmin_100-ttmax_900-parsweep_ttsd/ttsd/rawdata/file.RData","b1")
Data.b0 <- parsweep.curator("Data/b0-var-mean_30-ttmin_15-ttmax_45-parsweep_ttsd/ttsd/rawdata/file.RData","b0")
Data.e <- parsweep.curator("Data/e-var-mean_0p2-ttmin_0-ttmax_0p4-parsweep_ttsd/ttsd/rawdata/file.RData","epsilon")


Data <- rbind(Data.e,Data.b0,Data.b1)
levels(Data$Trait)<-list("epsilon"=TeX("$\\epsilon$"),"b[0]"="b0","b[1]"="b1")
levels(Data$patch)<-list("N[1]"="N1","N[2]"="N2","N[1]+N[2]"="N")
rm(Data.b1,Data.b0,Data.e)
pobj <- ggplot(data=Data,aes(x=param_val)) + 
  scale_linetype_manual("",values = c("No Variation"="solid", "Heritable Variation"="dotted",
                                      "Constant Variation"="dashed")) +
  scale_alpha_manual(values = c("No Variation"=0.7, "Heritable Variation"=1,
                                "Constant Variation"=1),guide = "none") +
  scale_discrete_manual(aesthetic = "linewidth",values = c("No Variation"=0.5, "Heritable Variation"=0.5,
                                                           "Constant Variation"=0.8),guide = "none") +
  scale_color_manual("",values=c("No Habitat Loss"="blue","Habitat Loss"="red")) +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5, size=12),
                     # axis.title.x = element_text(size = 15),
                     #axis.title.y = element_text(size = 15),
                     axis.text.x = element_text(angle=90, vjust=0.5),
                     #legend.title = element_text(size = 10),
                     #legend.text = element_text(size = 7),
                     legend.position="bottom",
                     # strip.text = element_text(size = 12),
                     #plot.margin = margin(0.5, 1, 0.5, 0.5, "lines"),
                     panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
                     strip.background = element_rect(fill = "white"),
                     strip.placement = "outside",
                     panel.border = element_rect(colour = "black"),
                     panel.spacing = unit(0.7, "lines")) +
  scale_x_continuous(expand = c(0,0.0001))# + scale_y_continuous(expand = c(0,0),limits = c(0,10))

figure <- pobj + geom_line(aes(y=density, color=HL, linetype=type, alpha=type, linewidth=type)) + 
  facet_grid(patch~Trait,scales = "free_x",labeller = label_parsed, switch="y")+ xlab(TeX("$TT_{sd}$")) + ylab("Equilibrium Consumer Density") +
  scale_y_continuous(expand = c(0.01,0.01)) + ggtitle("Evolving traits")

ggsave(filename = filname, device = "pdf", plot = figure, width = 8, height = 6)


