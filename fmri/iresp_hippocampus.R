library(ggplot2)
library(FSA)

data <- read.csv("/Users/jisu/Documents/Hidden Target/Codes/Python/20190802_hippocampus_smiley_rmWM(20).csv")

Ldata <- data[data$roi_name == "Left Hipp",]
Rdata <- data[data$roi_name == "Right Hipp",]
m.Ldata <- tapply(Ldata$iresp, Ldata$tent, mean)
m.Rdata <- tapply(Rdata$iresp, Rdata$tent, mean)
se.Ldata <- tapply(Ldata$iresp, Ldata$tent, se)
se.Rdata <- tapply(Rdata$iresp, Rdata$tent, se)

dat.s <- data.frame("Iresp" = c(m.Ldata, m.Rdata), "se" = c(se.Ldata, se.Rdata))
dat.s$tent <- rep(c(1:7),2)
dat.s$roi <- rep(c("Left Hippocampus","Right Hippocampus"),each=7)
dat.s$label <- rep(c("Smiley"))


data <- read.csv("/Users/jisu/Documents/Hidden Target/Codes/Python/20190802_hippocampus_unsmiley_rmWM(20).csv")

Ldata <- data[data$roi_name == "Left Hipp",]
Rdata <- data[data$roi_name == "Right Hipp",]
m.Ldata <- tapply(Ldata$iresp, Ldata$tent, mean)
m.Rdata <- tapply(Rdata$iresp, Rdata$tent, mean)
se.Ldata <- tapply(Ldata$iresp, Ldata$tent, se)
se.Rdata <- tapply(Rdata$iresp, Rdata$tent, se)

dat.us <- data.frame("Iresp" = c(m.Ldata, m.Rdata), "se" = c(se.Ldata, se.Rdata))
dat.us$tent <- rep(c(1:7),2)
dat.us$roi <- rep(c("Left Hippocampus","Right Hippocampus"),each=7)
dat.us$label <- rep(c("UnSmiley"))

dat <- rbind(dat.s, dat.us)

p1 <- ggplot(data = dat[dat$roi == "Left Hippocampus",], aes(x = tent, y=Iresp)) + 
  scale_y_continuous(name="Iresp") + 
  scale_x_continuous("TENT",limits=c(1,7),breaks=c(1:7)) +
  geom_point(aes(col=label)) +
  geom_line(aes(x=tent, y=Iresp, col=label)) +
  geom_errorbar(aes(ymin=Iresp-se, ymax=Iresp+se, col=label), width=.2,
                position=position_dodge()) +
  ggtitle("Left Hippocampus Activity (Smiley vs UnSmiley)")
#theme(plot.title=element_text(face="bold", size=20, hjust=0.5))

p2 <- ggplot(data = dat[dat$roi == "Right Hippocampus",], aes(x = tent, y=Iresp)) + 
  scale_y_continuous(name="Iresp") + 
  scale_x_continuous("TENT",limits=c(1,7),breaks=c(1:7)) +
  geom_point(aes(col=label)) +
  geom_line(aes(x=tent, y=Iresp, col=label)) +
  geom_errorbar(aes(ymin=Iresp-se, ymax=Iresp+se, col=label), width=.2,
                position=position_dodge()) +
  ggtitle("Right Hippocampus Activity (Smiley vs UnSmiley)")
#theme(plot.title=element_text(face="bold", size=20, hjust=0.5))


png(paste0("/Users/jisu/Documents/Hidden Target/Visualizations/1908/20190801_Smiley_UnSmiley_rmWM(20).png"), width = 2000, height = 1000, res=220)
multiplot(p1, p2)
dev.off()

png(paste0("/Users/jisu/Documents/Hidden Target/Visualizations/1908/20190801_Smiley_UnSmiley_rmWM2(20).png"), width = 1200, height = 1000, res=220)
p2
dev.off()
