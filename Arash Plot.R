library (ggplot2) #version 3.1.0
library(gridExtra)
library (dplyr)

## adding data in manually
Flow.Rate<-c(0.2,
             0.18,
             0.24,
             0.4,
             0.45,
             0.37,
             0.5,
             0.55,
             0.52,
             0.6,
             0.7,
             0.57,
             0.75,
             0.8,
             0.84,
             0.9,
             0.93,
             0.95,
             0.95,
             0.97,
             0.99,
             0.2,
             0.22,
             0.19,
             0.4,
             0.39,
             0.47,
             0.5,
             0.52,
             0.47,
             0.6,
             0.62,
             0.63,
             0.75,
             0.78,
             0.8,
             0.9,
             0.87,
             0.95,
             1,
             1.1,
             0.97,
             1.25,
             1.15,
             1.28,
             1.5,
             1.38,
             1.52,
             1.75,
             1.66,
             1.83,
             2,
             1.9,
             2.14,
             2.5,
             2.43,
             2.62,
             3,
             2.84,
             3.2,
             0.2,
             0.15,
             0.24,
             0.4,
             0.36,
             0.42,
             0.5,
             0.45,
             0.56,
             0.6,
             0.57,
             0.65,
             0.75,
             0.7,
             0.79,
             0.9,
             0.85,
             0.96,
             1,
             0.96,
             1.1,
             1.25,
             1.17,
             1.32,
             1.5,
             1.4,
             1.6,
             1.75,
             1.75,
             1.76,
             2,
             1.95,
             2.04,
             2.5,
             2.43,
             2.49,
             3,
             3.03,
             2.95,
             4,
             3.96,
             3.98,
             4.5,
             4.47,
             4.52,
             0.2,
             0.21,
             0.15,
             0.4,
             0.36,
             0.43,
             0.5,
             0.48,
             0.55,
             0.6,
             0.54,
             0.65,
             0.75,
             0.69,
             0.8,
             0.9,
             0.84,
             0.93,
             1,
             1.1,
             0.98,
             1.25,
             1.32,
             1.28,
             1.5,
             1.43,
             1.55,
             1.75,
             1.69,
             1.79,
             2,
             1.94,
             2.09,
             2.5,
             2.6,
             2.41,
             3,
             3.1,
             2.85,
             4,
             3.85,
             4.1,
             5,
             4.85,
             5.2,
             7.3,
             7.4,
             7.1)
Voltage<-c(4,
           4,
           4,
           6.5,
           6.5,
           6.5,
           7,
           7,
           7,
           7.8,
           7.8,
           7.8,
           8.8,
           8.8,
           8.8,
           9.6,
           9.6,
           9.6,
           10,
           10,
           10,
           2.7,
           2.7,
           2.7,
           4,
           4,
           4,
           4.4,
           4.4,
           4.4,
           4.7,
           4.7,
           4.7,
           5.1,
           5.1,
           5.1,
           5.6,
           5.6,
           5.6,
           6.1,
           6.1,
           6.1,
           6.8,
           6.8,
           6.8,
           7.4,
           7.4,
           7.4,
           7.7,
           7.7,
           7.7,
           8.1,
           8.1,
           8.1,
           9,
           9,
           9,
           9.9,
           9.9,
           9.9,
           1.9,
           1.9,
           1.9,
           3,
           3,
           3,
           3.4,
           3.4,
           3.4,
           3.8,
           3.8,
           3.8,
           4,
           4,
           4,
           4.4,
           4.4,
           4.4,
           4.7,
           4.7,
           4.7,
           5.1,
           5.1,
           5.1,
           5.6,
           5.6,
           5.6,
           6.1,
           6.1,
           6.1,
           6.7,
           6.7,
           6.7,
           7.3,
           7.3,
           7.3,
           8,
           8,
           8,
           9.3,
           9.3,
           9.3,
           10,
           10,
           10,
           1.6,
           1.6,
           1.6,
           2.35,
           2.35,
           2.35,
           2.6,
           2.6,
           2.6,
           2.9,
           2.9,
           2.9,
           3.2,
           3.2,
           3.2,
           3.6,
           3.6,
           3.6,
           3.8,
           3.8,
           3.8,
           4.1,
           4.1,
           4.1,
           4.5,
           4.5,
           4.5,
           5.2,
           5.2,
           5.2,
           5.5,
           5.5,
           5.5,
           6.1,
           6.1,
           6.1,
           6.5,
           6.5,
           6.5,
           7.3,
           7.3,
           7.3,
           8.3,
           8.3,
           8.3,
           10,
           10,
           10)

Particle.Size=rep(c("5um","10um","15um","20um"),times=c(21,39,45,48))

mydata<-data.frame(Voltage,Flow.Rate, Particle.Size)
mydata$Particle.Size<-as.factor(mydata$Particle.Size)
head(mydata)

##data summary function to calculate mean and sd source: http://www.sthda.com/english/wiki/ggplot2-error-bars-quick-start-guide-r-software-and-data-visualization
##add sd column to data and takes means for each voltage replicate for each particle size
library(plyr)

data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}
mydata2<-data_summary(mydata, varname = "Flow.Rate", groupnames = c("Particle.Size","Voltage"))
mydata2$Particle.Size=as.factor(mydata2$Particle.Size)
head(mydata2)

######################PLOT USED#################### with greek symbol in tite
theplot<-ggplot(mydata2, aes(x=Voltage,y=Flow.Rate,group=Particle.Size, color=Particle.Size)) +
geom_errorbar(aes(ymin=Flow.Rate-sd,ymax=Flow.Rate+sd),color="grey3")+
  geom_line()+
    geom_point()+
      theme_bw()+
    ylab(expression("Flow rate ("*mu~"l/min)"))+
    xlab("Voltage (Vpp)")+
    theme(text=element_text(size=16))

print(theplot)

theplot2<-ggplot(mydata2, aes(x=Voltage,y=Flow.Rate,group=Particle.Size, color=Particle.Size)) +
  geom_errorbar(aes(ymin=Flow.Rate-sd,ymax=Flow.Rate+sd),color="grey3")+
  geom_point()+
  theme_bw()+
  ylab(expression("Flow rate ("* mu~"l/min)"))+
  xlab("Voltage (Vpp)")+
   theme(text=element_text(size=16))

print(theplot2)
######################PLOT USED#################### with greek symbol in tite


#########EDA mostly###########################
##plot of particle size vs flow rate 
p1<-ggplot(data, aes(Particle.Size,Flow.Rate))
p1_plot<-p1+geom_boxplot(aes(fill=factor(Particle.Size)),show.legend=F)+scale_x_discrete(limits=c("5um","10um","15um","20um"))

##particle size vs voltage
p2<-ggplot(data, aes(Particle.Size,Voltage))
p2_plot<-p2+geom_boxplot(aes(fill=factor(Particle.Size)),show.legend=F)+scale_x_discrete(limits=c("5um","10um","15um","20um"))

##flow rate vs voltage per particle
#5um
five<-subset(data, Particle.Size=="5um")
five_p<-ggplot(five,aes(factor(Voltage),Flow.Rate))
five_plot<-five_p+geom_boxplot(aes(fill=factor(Particle.Size)),show.legend = F)+ylab("Flow rate (ul/min)")+xlab("Voltage (Vpp)")+geom_jitter()+ggtitle("5 um particles")

#10um
ten<-subset(data, Particle.Size=="10um")
ten_p<-ggplot(ten,aes(factor(Voltage),Flow.Rate))
ten_plot<-ten_p+geom_boxplot(aes(fill=factor(Particle.Size)),show.legend = F)+ylab("Flow rate (ul/min)")+xlab("Voltage (Vpp)")+geom_jitter()+ ggtitle("10 um particles")

#15um
fif<-subset(data, Particle.Size=="15um")
fif_p<-ggplot(fif,aes(factor(Voltage),Flow.Rate))
fif_plot<-fif_p+geom_boxplot(aes(fill=factor(Particle.Size)),show.legend = F)+ylab("Flow rate (ul/min)")+xlab("Voltage (Vpp)")+geom_jitter()+ggtitle("15 um particles")

#20um
twenty<-subset(data, Particle.Size=="20um")
twenty_p<-ggplot(twenty,aes(factor(Voltage),Flow.Rate))
twenty_plot<-twenty_p+geom_boxplot(aes(fill=factor(Particle.Size)),show.legend = F)+ylab("Flow rate (ul/min)")+xlab("Voltage (Vpp)")+geom_jitter()+ggtitle("20 um particles")

##all particles 
##grouped scatterplot
sp<-ggplot(data, aes(x=Voltage,y=Flow.Rate))
sp+geom_point(aes(color=Particle.Size))

##grouped scatterplot
sp2<-ggscatter(data, x="Voltage",y="Flow.Rate",color = "Particle.Size", size = 3, alpha=0.6,  palette = "npg")

##calculating mean with dplyr
five.summary<- five %>% group_by(Voltage) %>% 
  summarize (ymin=min(Flow.Rate),
             ymax=max(Flow.Rate),
             ymean=mean(Flow.Rate))
plotfive<-ggplot(five.summary, aes(x=Voltage,y=ymean)) + geom_point(size=2)+geom_errorbar(aes(ymin=ymin,ymax=ymax))

ten

##scatterhist
ggscatterhist(data,x="Voltage", y= "Flow.Rate", color="Particle.Size", size=3, alpha=0.6, palette = "npg", margin.plot = "boxplot", ggtheme=theme_bw())

##all particle plots
p3<-ggplot(data, aes(factor(Voltage),Flow.Rate)) 
p3_pl<-p3+
p3_plot<-p3+geom_boxplot(aes(fill=factor(Particle.Size)),show.legend = F)+ylab("Flow rate (ul/min)")+xlab("Voltage (Vpp)")+geom_jitter()

##another graph
Library(cars)
Library(scales)
scatterplot(Flow.Rate~Voltage|Particle.Size, data=data, ylab="Flow rate (ul/min)", xlab="Voltage (Vpp)")


##outputting pdf
pdffile <- "plots.pdf"
pdf(paste(pdffile,sep=""), paper="letter", width=5, height=3)
grid.arrange(p1_plot,p2_plot,cols = 2)
dev.off()

pdffile <- "sizeplots.pdf"
pdf(paste(pdffile,sep=""), paper="letter", width=8, height=8)
grid.arrange(five_plot,ten_plot,fif_plot,twenty_plot, ncol=2)
dev.off()

####final file selected

pdffile <- "Arash_plot1.pdf"
pdf(paste(pdffile,sep=""), paper="letter", width=8, height=6)
theplot
theplot2
dev.off()




