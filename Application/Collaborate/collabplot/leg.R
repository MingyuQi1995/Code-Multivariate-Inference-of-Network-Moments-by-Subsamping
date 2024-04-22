library(data.table)
library(grid)
library(gtable)
library(ggplot2)
library(RColorBrewer)


#####legends
dx<-1
tuvp<-viewport(h=0.8,w=0.8,default.units='snpc')

gTree(children=gList(
pointsGrob(x=c(0,0.5,1),y=c(1,0,1),default='npc',pch=20,gp=gpar(cex=dx)),
linesGrob(x=c(0,0.5,1),y=c(1,0,1),gp=gpar(lwd=2))
),vp=tuvp)->v1

gTree(children=gList(
pointsGrob(x=c(0,0.5,1),y=c(0,1,0),default='npc',pch=20,gp=gpar(cex=dx)),
linesGrob(x=c(0,0.5,1,0),y=c(0,1,0,0),gp=gpar(lwd=2))
),vp=tuvp)->v2


gTree(children=gList(
pointsGrob(x=c(0.5,0.5,0,1),y=c(0,0.6,1,1),default='npc',pch=20,gp=gpar(cex=dx)),
linesGrob(x=c(0.5,0.5,0),y=c(0,0.6,1),gp=gpar(lwd=2)),
linesGrob(x=c(0.5,1),y=c(0.6,1),gp=gpar(lwd=2))
),vp=tuvp)->v3

gTree(children=gList(
pointsGrob(x=c(0,1,1,0),y=c(0,0,1,1),default='npc',pch=20,gp=gpar(cex=dx)),
linesGrob(x=c(0,1,1,0,0),y=c(0,0,1,1,0),gp=gpar(lwd=2))
),vp=tuvp)->v4
