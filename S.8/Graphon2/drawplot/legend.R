library(scales)
library(grid)
library(gtable)
library(data.table)
library(grid)
library(gtable)
library(ggplot2)
library(RColorBrewer)
library(gggrid)

colorManualunit<- c("#377EB8", "#FF7F00", "#984EA3")
colorManualbi<- c("#377EB8", "#FF7F00", "#984EA3")


myYanse4<-setNames(colorManualunit,c('V1','V2','V3'))
myYanse6<-setNames(colorManualbi,c('V1','V2','V3'))


#####legends
dx<-0.6 ##可以调数字
tuvp<-viewport(h=0.8,w=0.8,default.units='snpc')

gTree(children=gList(
pointsGrob(x=c(0.2,0.5,0.8),y=c(0.8,0.2,0.8),default='npc',pch=20,gp=gpar(cex=dx)),
linesGrob(x=c(0.2,0.5,0.8),y=c(0.8,0.2,0.8))
),vp=tuvp)->v1


gTree(children=gList(
pointsGrob(x=c(0.2,0.5,0.8),y=c(0.2,0.8,0.2),default='npc',pch=20,gp=gpar(cex=dx)),
linesGrob(x=c(0.2,0.5,0.8,0.2),y=c(0.2,0.8,0.2,0.2))
),vp=tuvp)->v2

gTree(children=gList(
pointsGrob(x=c(0.5,0.5,0.2,0.8),y=c(0.2,0.6,0.8,0.8),default='npc',pch=20,gp=gpar(cex=dx)),
linesGrob(x=c(0.5,0.5,0.2),y=c(0.2,0.6,0.8)),
linesGrob(x=c(0.5,0.8),y=c(0.6,0.8))
),vp=tuvp)->v3


############
mkl<-function(x)
{
   gTree(children=gList(
   rectGrob(gp=gpar(fill='gray80',col='transparent')),
   pointsGrob(0.5,0.5,pch=20,gp=gpar(col=x)),
   linesGrob(x=c(0.1,0.9),y=c(0.5,0.5),gp=gpar(col=x))
   ),
   vp=viewport(h=unit(1,'lines'),w=0.9))
}

lapply(myYanse4,mkl)->l4
lapply(myYanse6,mkl)->l6


packGrob(packGrob(frameGrob(), v1),v2,side="right")->v12
packGrob(packGrob(frameGrob(), v1),v3,side="right")->v13
packGrob(packGrob(frameGrob(), v2),v3,side="right")->v23


frameGrob(name='a',layout=grid.layout(nrow=3,ncol=2,
                           heights=unit.c(unit(1.3,'cm'),unit(1.5,'cm'),unit(1.5,'cm')), ##可以调数字
                           widths=unit.c(unit(1.0,'cm'),unit(2,'cm')))) |>  ##可以调数字
placeGrob(v1,row=1,col=2) |>
placeGrob(v2,row=2,col=2) |>
placeGrob(v3,row=3,col=2) |>
placeGrob(l4[[1]],row=1,col=1) |>
placeGrob(l4[[2]],row=2,col=1) |>
placeGrob(l4[[3]],row=3,col=1)->leg1

leg1$vp<-viewport(y=unit(1,'npc')-unit(1,'mm'),x=unit(1,'npc')-unit(2,'mm'),just=c('right','top'),width=grobWidth(leg1),height=grobHeight(leg1))


#grid.draw(leg1)


frameGrob(name='a',layout=grid.layout(nrow=3,ncol=2,
                           heights=unit.c(unit(1.3,'cm'),unit(1.5,'cm'),unit(1.5,'cm')),  ##可以调数字
                           widths=unit.c(unit(1.2,'cm'),unit(2.4,'cm')))) |> ##可以调数字
  
placeGrob(v12,row=1,col=2) |>
placeGrob(v13,row=2,col=2) |>
placeGrob(v23,row=3,col=2) |>
placeGrob(l6[[1]],row=1,col=1) |>
placeGrob(l6[[2]],row=2,col=1) |>
placeGrob(l6[[3]],row=3,col=1)->leg2

leg2$vp<-viewport(y=unit(1,'npc')-unit(1,'mm'),x=unit(1,'npc')-unit(2,'mm'),just=c('right','top'),width=grobWidth(leg2),height=grobHeight(leg2))


#grid.draw(leg2)


