##FlowRate vs Channel Height

library (ggplot2) #version 3.1.0
library(gridExtra)
library (dplyr)

## adding data in manually
FlowRate<-c(0.95,
             0.97,
             0.99,
             0.53,
             0.5,
             0.55,
             0.45,
             0.36,
             0.4,
             0.3,
             0.3,
             0.27,
             3,
             2.84,
             3.2,
             1.5,
             1.4,
             1.65,
             1.25,
             1.2,
             1.3,
             1.16,
             1.25,
             1.1,
             4.5,
             4.47,
             4.52,
             2.6,
             2.5,
             2.4,
             2.1,
             2.1,
             2,
             1.88,
             1.7,
             1.95,
             7.3,
             7.4,
             7.1,
             3.9,
             3.7,
             4,
             3.35,
             3.2,
             3.4,
             3,
             2.9,
             3.1)
Channel.Height<-c(25,
                  25,
                  25,
                  40,
                  40,
                  40,
                  60,
                  60,
                  60,
                  75,
                  75,
                  75,
                  25,
                  25,
                  25,
                  40,
                  40,
                  40,
                  60,
                  60,
                  60,
                  75,
                  75,
                  75,
                  25,
                  25,
                  25,
                  40,
                  40,
                  40,
                  60,
                  60,
                  60,
                  75,
                  75,
                  75,
                  25,
                  25,
                  25,
                  40,
                  40,
                  40,
                  60,
                  60,
                  60,
                  75,
                  75,
                  75)

ParticleSize=rep(c("5um","10um","15um","20um"),times=c(12,12,12,12))

channeldata<-data.frame(FlowRate,Channel.Height, ParticleSize)
channeldata$ParticleSize<-as.factor(channeldata$ParticleSize)
channeldata$Channel.Height<-as.factor(channeldata$Channel.Height)
print(levels(channeldata$ParticleSize))
##changed the order of particle size from 5 to 20um, which fixes the order of the legend
channeldata$ParticleSize=factor(channeldata$ParticleSize,levels(channeldata$ParticleSize)[c(4,1,2,3)])
print(levels(channeldata$ParticleSize))
head(channeldata)

####function####
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

####end of function####
##get mean and sd
channeldata2<-data_summary(channeldata, varname = "FlowRate", groupnames = c("ParticleSize","Channel.Height"))

##plots
###with line
channelplot<-ggplot(channeldata2, aes(x=Channel.Height,y=FlowRate,group=ParticleSize, color=ParticleSize)) +
  geom_errorbar(aes(ymin=FlowRate-sd,ymax=FlowRate+sd),color="grey3",width=0.1)+
  geom_line()+
  geom_point()+
  theme_bw()+
  ylab(expression("Flow rate ("*mu~"l/min)"))+
  xlab("Channel Height ("*mu~"m)")+
  theme(text=element_text(size=16))

print(channelplot)

###without line
channelplot2<-ggplot(channeldata2, aes(x=Channel.Height,y=FlowRate,group=ParticleSize, color=ParticleSize)) +
  geom_errorbar(aes(ymin=FlowRate-sd,ymax=FlowRate+sd),color="grey3",width=0.1)+
  geom_point()+
  theme_bw()+
  ylab(expression("Flow rate ("*mu~"l/min)"))+
  xlab("Channel Height ("*mu~"m)")+
  theme(text=element_text(size=16))

print(channelplot2)

##output plots
pdffile <- "Arash_plot2.pdf"
pdf(paste(pdffile,sep=""), paper="letter", width=8, height=6)
channelplot
channelplot2
dev.off()

pdffile <- "Arash_all.pdf"
pdf(paste(pdffile,sep=""), paper="letter", width=8, height=6)
theplot
theplot2
channelplot
channelplot2
dev.off()

