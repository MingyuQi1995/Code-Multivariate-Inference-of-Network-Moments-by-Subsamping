library(data.table)
library(Hmisc)
library(ggplot2)
library(tidyverse)
library(RColorBrewer)
load("/Users/mingyuqi/Desktop/netsimures/graphon1/b06/simu025/eval.Rdata")
setwd("/Users/mingyuqi/Desktop/netsimures/graphon1/b06/simu025")
load('eval.Rdata')
f1name<-paste0('n',c(2000,4000,8000,16000))
names(eval)<-f1name



lapply(names(eval),function(bb)
{
  eval[[bb]]->x
  
  lapply(names(x),function(y) {
    x[[y]]->zz
    #do.call(rbind,zz)->zz
    #data.table(b=bb,what=y,zz)->zz
    data.table(g=bb,zz)->zz
  })
  
})->resu


lapply(resu,`[[`,1) |> rbindlist()->uni

copy(uni)->d1

d1[,g:=factor(g,c('n2000','n4000','n8000','n16000'),labels=c('2000','4000','8000','16000'))]
d1<-melt(d1,id.vars='g')
d1<-d1[,as.list(smean.sd(value)),.(g,variable)]
d1[,ymin:=Mean-1.96*SD/sqrt(50)]
d1[,ymax:=Mean+1.96*SD/sqrt(50)]

d1 <- d1[1:12,]


ts = 32
bs = 35
colorManualunit<- c("#377EB8", "#FF7F00", "#984EA3")
colorManualbi<- c("#377EB8", "#FF7F00", "#984EA3")




p1<-ggplot(d1, aes(x=g, y=Mean, colour=variable, group=variable)) +
  geom_errorbar(aes(ymin=ymin, ymax=ymax), position=position_dodge(width=0.1), width=0.7) +
  geom_point(position=position_dodge(width=0.1), size=5) +
  geom_line(aes(group=variable), position=position_dodge(width=0.1), size=2, linetype= "solid") +
  theme_bw()+
  theme(panel.grid=element_blank(),
        legend.position='none', 
        axis.text.x = element_text(size = ts), 
        axis.text.y = element_text(size = ts),
        axis.title.x = element_text(size = bs), 
        axis.title.y = element_text(size = bs))+
  scale_x_discrete(expand=expansion(add=0.4))+
  scale_y_continuous(limits = c(0.13, 0.36),
                     breaks=c(0.14,0.21,0.28,0.35),
                     labels = c("0.15", "0.22", "0.29", "0.36"),
                     expand=expansion(add=0.02)) +
  #scale_y_continuous(breaks=c(0.155, 0.195, 0.235, 0.275),
   #                 labels=c("0.16", "0.21", "0.26", "0.31"), 
    #                expand=expansion(add=0.01)) +
  scale_color_manual(values =  colorManualunit)+
  labs(x='n', y = "CDF Approx. Err.")



source('legend.R')


p1+grid_panel(leg1)->p1


ggsave('uni.pdf',p1,width=7,height=6)



### log 

p3<-ggplot(d1, aes(x=g, y= log(Mean), colour=variable, group=variable)) +
  geom_errorbar(aes(ymin=log(ymin), ymax= log(ymax)), position=position_dodge(width=0.1), width=0.7) +
  geom_point(position=position_dodge(width=0.1), size=5) +
  geom_line(aes(group=variable), position=position_dodge(width=0.1), size=2, linetype= "solid") +
  theme_bw()+
  theme(panel.grid=element_blank(),
        legend.position='none', 
        axis.text.x = element_text(size = ts), 
        axis.text.y = element_text(size = ts),
        axis.title.x = element_text(size = bs), 
        axis.title.y = element_text(size = bs))+
  scale_x_discrete(expand=expansion(add=0.4))+
  scale_y_continuous(limits = c(-1.95, -1.01),
                     breaks= c(-1.09,-1.35,-1.61,-1.87),
                     labels = c("-1.02","-1.29","-1.56","-1.83"),
                     expand=expansion(add=0.07)) +
  scale_color_manual(values =  colorManualunit)+
  # scale_shape_manual(values = c(16, 17, 18)) + 
  labs(x='n', y = " (log) Approx. Err.")



source('/Users/mingyuqi/Desktop/netsimures/graphon1/b06/simu01/legend.R')


p3+grid_panel(leg1)->p3


ggsave('loguni.pdf',p3,width=7,height=6)



### six
lapply(resu,`[[`,2) |> rbindlist()->mul

copy(mul)->d1
d1[g!='n500']->d1
d1[g!='n1000']->d1
d1[,g:=factor(g,c('n2000','n4000','n8000','n16000'),labels=c('2000','4000','8000','16000'))]
d1<-melt(d1,id.vars='g')
d1<-d1[,as.list(smean.sd(value)),.(g,variable)]
d1[,ymin:=Mean-1.96*SD/sqrt(50)]
d1[,ymax:=Mean+1.96*SD/sqrt(50)]

d1 <- d1[c(1:8,13:16),]



p2<-ggplot(d1, aes(x=g, y=Mean, colour=variable, group=variable)) +
  geom_errorbar(aes(ymin=ymin, ymax=ymax), position=position_dodge(width=0.1), width=0.7) +
  geom_point(position=position_dodge(width=0.1), size=5) +
  geom_line(aes(group=variable), position=position_dodge(width=0.1), size=2, linetype= "solid") +
  theme_bw()+
  theme(panel.grid=element_blank(),
        legend.position='none', 
        axis.text.x = element_text(size = ts), 
        axis.text.y = element_text(size = ts),
        axis.title.x = element_text(size = bs), 
        axis.title.y = element_text(size = bs))+
  scale_x_discrete(expand=expansion(add=0.4))+
  scale_y_continuous(limits = c(0.13, 0.36),
                     breaks=c(0.14,0.21,0.28,0.35),
                     labels = c("0.15", "0.22", "0.29", "0.36"),
                     expand=expansion(add=0.01)) +
  scale_color_manual(values = colorManualbi)+
  labs(x='n', y = "CDF Approx. Err.")



p2+grid_panel(leg2)->p2

ggsave('bi.pdf',p2,width=7,height=6)



p4<-ggplot(d1, aes(x=g, y= log(Mean), colour=variable, group=variable)) +
  geom_errorbar(aes(ymin= log(ymin), ymax=log(ymax) ), position=position_dodge(width=0.1), width=0.7) +
  geom_point(position=position_dodge(width=0.1), size=5) +
  geom_line(aes(group=variable), position=position_dodge(width=0.1), size=2, linetype= "solid") +
  theme_bw()+
  theme(panel.grid=element_blank(),
        legend.position='none', 
        axis.text.x = element_text(size = ts), 
        axis.text.y = element_text(size = ts),
        axis.title.x = element_text(size = bs), 
        axis.title.y = element_text(size = bs))+
  scale_x_discrete(expand=expansion(add=0.4))+
  scale_y_continuous(limits = c(-1.841, -1.01),
                     breaks=c(-1.021,-1.291,-1.561,-1.831),
                     labels = c("-1.02","-1.29","-1.56","-1.83"),
                    # breaks=c(-1.02,-1.29,-1.56,-1.83),
                     #  labels = c("0.15", "0.22", "0.29", "0.36"),
                     expand=expansion(add=0.07)) +
  scale_color_manual(values = colorManualbi)+
  #scale_shape_manual(values = c(3, 15, 8)) + 
  labs(x='n', y = "(log) Approx. Err.")


p4+grid_panel(leg2)->p4

ggsave('logbi.pdf',p4,width=7,height=6)

