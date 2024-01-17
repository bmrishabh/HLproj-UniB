# We compare equilibrium values over parameter space with no variation case
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
filname <- "SI_Figure_S5.pdf"

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
  Data <- melt(Data, id.vars=c("R1","R2","type","HL","param_val","Trait"),
                 variable.name = "patch", value.name="density")
  Data$patch <- factor(Data$patch)
  return(Data)
}

Data.a0 <- parsweep.curator("Data/e-var-mean_0p2-sd_0p1-a0_0p1-parsweep_N02/N02/rawdata/file.RData","Default")
Data.b0 <- parsweep.curator("Data/e-var-mean_0p2-sd_0p1-a0_0p1-p_0p5-parsweep_N02/N02/rawdata/file.RData","Outcompetition")


Data <- rbind(Data.a0,Data.b0)
Data <- Data[Data$type=="No Variation",]
Data <- Data[Data$HL=="No Habitat Loss",]
levels(Data$patch)<-list("1"="N1","2"="N2")
Data$param_val <- Data$param_val - 4 # N01=4, thus, this step given us N02-N01
rm(Data.a0,Data.b0)
pobj <- ggplot(data=Data,aes(x=param_val)) + 
  # scale_linetype_manual("Patch",values = c("1"="solid", "2"="dotted")) +
  # scale_alpha_manual(values = c("1"=0.7, "2"=1),guide = "none") +
  # scale_size_manual(values = c("1"=0.5, "2"=0.7),guide = "none") +
  # scale_color_manual("",values=c("No Habitat Loss"="blue","Habitat Loss"="red")) +
  scale_shape_manual("patch", values = c("1" = 1, "2" = 2)) +
  scale_alpha_manual(values = c("1"=0.7, "2"=1),guide = "none") +
  scale_size_manual("",values=c("1"=1.5,"2"=2), guide = "none") +
  scale_color_manual("",values=c("No Habitat Loss"="blue","Habitat Loss"="red")) +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5),
                     axis.title.x = element_text(face = "bold"),
                     axis.title.y = element_blank(),
                     #axis.text.x = element_text(angle=90, vjust=0.5),
                     #legend.title = element_text(size = 10),
                     #legend.text = element_text(size = 7),
                     legend.position="bottom",
                     # strip.text = element_text(size = 12),
                     plot.margin = margin(0.5, 1, 0.5, 0.5, "lines"),
                     panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
                     strip.background = element_blank(),
                     strip.placement = "outside",
                     panel.border = element_rect(colour = "black"),
                     panel.spacing = unit(0.7, "lines")) +
  scale_x_continuous(expand = c(0.01,0.01))# + scale_y_continuous(expand = c(0,0),limits = c(0,10))

p.a0 <- pobj + geom_point(data=Data[Data$Trait=="Default",],aes(y=density, color=HL, shape=patch, alpha=patch, size=patch)) +
  scale_y_continuous(expand = c(0.01,0.01), limits = c(-0.01,23)) + xlab(TeX("$N_{0,2} - N_{0,1}$")) + ggtitle("Default")

p.b0 <- pobj + geom_point(data=Data[Data$Trait=="Outcompetition",],aes(y=density, color=HL, shape=patch, alpha=patch, size=patch)) +
  scale_y_continuous(expand = c(0.01,0.01), limits = c(-0.01,23)) + xlab(TeX("$N_{0,2} - N_{0,1}$")) + ggtitle("Outcompetition")


figure <- ggarrange(p.a0+theme(axis.title.x = element_blank(), axis.title.y = element_blank()),
                    p.b0+theme(axis.title.x = element_blank(), axis.title.y = element_blank()),
                    labels = "auto",
                    ncol = 2, nrow = 1, align = "hv", vjust = 1.2, hjust = 0.2,
                    common.legend = TRUE, legend = "none")
figure <- annotate_figure(figure, left = text_grob("Equilibrium Consumer Density", rot = 90))
figure <- annotate_figure(figure, bottom = text_grob(TeX("($N_{0,2} - N_{0,1}$) where $N_{0,1}=4$")))
figure <- annotate_figure(figure, bottom = get_legend(p.a0))

ggsave(filename = filname, device = "pdf", plot = figure, width = 8, height = 5)

