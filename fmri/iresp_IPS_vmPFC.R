library(ggplot2)


multiplot <- function(..., plotlist=NULL, file, cols=2, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  if (numPlots==1) {
    print(plots[[1]])
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}





data <- read.csv("/Users/jisu/Documents/Hidden Target/Codes/Python/20190731_IPS_vmPFC_exploit.csv")

IPSData <- data[data$roi_name == "Left IPS",]
vmPFCdata <- data[data$roi_name == "vmPFC",]
m.IPSData <- tapply(IPSData$iresp, IPSData$tent, mean)
m.vmPFCdata <- tapply(vmPFCdata$iresp, vmPFCdata$tent, mean)
se.IPSData <- tapply(IPSData$iresp, IPSData$tent, se)
se.vmPFCdata <- tapply(vmPFCdata$iresp, vmPFCdata$tent, se)


dat.s <- data.frame("Iresp" = c(m.IPSData, m.vmPFCdata), "se" = c(se.IPSData, se.vmPFCdata))
dat.s$tent <- rep(c(1:7),2)
dat.s$roi <- rep(c("Left IPS","vmPFC"),each=7)
dat.s$label <- rep(c("Exploit"))



data <- read.csv("/Users/jisu/Documents/Hidden Target/Codes/Python/20190731_IPS_vmPFC_explore.csv")

IPSData <- data[data$roi_name == "Left IPS",]
vmPFCdata <- data[data$roi_name == "vmPFC",]
m.IPSData <- tapply(IPSData$iresp, IPSData$tent, mean)
m.vmPFCdata <- tapply(vmPFCdata$iresp, vmPFCdata$tent, mean)

dat.us <- data.frame("Iresp" = c(m.IPSData, m.vmPFCdata), "se" = c(se.IPSData, se.vmPFCdata))
dat.us$tent <- rep(c(1:7),2)
dat.us$roi <- rep(c("Left IPS","vmPFC"),each=7)
dat.us$label <- rep(c("Explore"))

dat <- rbind(dat.s, dat.us)

p1 <- ggplot(data = dat[dat$roi == "Left IPS",], aes(x = tent, y=Iresp)) + 
  scale_y_continuous(name="Iresp") + 
  scale_x_continuous("TENT",limits=c(1,7),breaks=c(1:7)) +
  geom_point(aes(col=label)) +
  geom_line(aes(x=tent, y=Iresp, col=label)) +
  geom_errorbar(aes(ymin=Iresp-se, ymax=Iresp+se, col=label), width=.2,
                position=position_dodge()) +
  ggtitle("Left IPS Activity (Exploit vs Explore)")
  #theme(plot.title=element_text(face="bold", size=20, hjust=0.5))

p2 <- ggplot(data = dat[dat$roi == "vmPFC",], aes(x = tent, y=Iresp)) + 
  scale_y_continuous(name="Iresp") + 
  scale_x_continuous("TENT",limits=c(1,7),breaks=c(1:7)) +
  geom_point(aes(col=label)) +
  geom_line(aes(x=tent, y=Iresp, col=label)) +
  geom_errorbar(aes(ymin=Iresp-se, ymax=Iresp+se, col=label), width=.2,
                position=position_dodge()) +
  ggtitle("vmPFC Activity (Exploit vs Explore)")
#theme(plot.title=element_text(face="bold", size=20, hjust=0.5))


png(paste0("/Users/jisu/Documents/Hidden Target/Visualizations/1908/20190731_exploit_explore.png"), width = 2000, height = 1000, res=220)
multiplot(p1, p2)
dev.off()


