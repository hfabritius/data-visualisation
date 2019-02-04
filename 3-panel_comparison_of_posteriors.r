# ***************************************************************************************************************************************
# Code for visual comparison of posterior distributions of alternative models
# For an example, see Fig. 2 in Fabritius et al. (2015) Movement Ecology 3:4
# ***************************************************************************************************************************************
setwd(choose.dir())
library(graphics) # split.screen

# Read in posterior distributions that have been reduced to points of min, max 95 % Cr.I. and median
Pri<-read.csv2(file="Posteriors_Pri.csv", header=TRUE)
SIm<-read.csv2(file="Posteriors_SIm.csv", header=TRUE)
SIf<-read.csv2(file="Posteriors_SIf.csv", header=TRUE)
SOm<-read.csv2(file="Posteriors_SOm.csv", header=TRUE)
SOf<-read.csv2(file="Posteriors_SOf.csv", header=TRUE)
MEm<-read.csv2(file="Posteriors_MEm.csv", header=TRUE)
MEf<-read.csv2(file="Posteriors_MEf.csv", header=TRUE)
FIm<-read.csv2(file="Posteriors_FIm.csv", header=TRUE)
FIf<-read.csv2(file="Posteriors_FIf.csv", header=TRUE)

# SPLIT SCREEN TO PRODUCE DIFFERENTLY SIZED SCREENS FOR DIFFERENT PLOTS
# ----------------------------------------------------------------------------------------------------------------------------------------
split.screen(figs=c(2,1))
split.screen(figs=c(1,2), screen=2)

screen(1) # plot the prior distribution & posteriors of compared models, grouped for the first four parameters
par(mar=c(1.0, 5.0, 4.0, 1.5),mgp=c(4,0.8,0)) # set margins of the first screen
# distributions are drawn as lines, with a character to present the median value
# the at attribute gives the locations of each posterior distribution and enables grouping of posteriors into four groups
# distributions of the alternative models compared are displayed with different colours and line options
bxp(list(stats=as.matrix(cbind(Pri[,1:4],rep(0,5))),n=rep(10000,ncol(Pri[,1:4])+1)),boxwex=0,boxlty=1,boxlwd=2,boxcol="black", medpch="-",medcex=1.2,medcol="black", log="y",xaxt="n",ylim=c(1000,2000000),at=c(2,17,32,47,51),axes=F)
bxp(list(stats=as.matrix(SIm[,1:3]),n=rep(10000,ncol(SIm[,1:3]))),boxwex=0,boxlty=1,boxlwd=1.5,boxcol="blue",  medpch=15,medbg="white", medcol="blue",  log="y",xaxt="n",yaxt="n",add=TRUE,at=c(3,18,33),frame.plot=FALSE)
bxp(list(stats=as.matrix(SIf[,1:3]),n=rep(10000,ncol(SIf[,1:3]))),boxwex=0,boxlty=2,boxlwd=1.5,boxcol="blue",  medpch=19,medbg="red",medcol="blue",  log="y",xaxt="n",yaxt="n",add=TRUE,at=c(4,19,34),frame.plot=FALSE)
bxp(list(stats=as.matrix(SOm[,1:3]),n=rep(10000,ncol(SOm[,1:3]))),boxwex=0,boxlty=1,boxlwd=1.5,boxcol="purple",medpch=15,medbg="white",medcol="purple",log="y",xaxt="n",yaxt="n",add=TRUE,at=c(5,20,35),frame.plot=FALSE)
bxp(list(stats=as.matrix(SOf[,1:3]),n=rep(10000,ncol(SOf[,1:3]))),boxwex=0,boxlty=2,boxlwd=1.5,boxcol="purple",medpch=19,medbg="white",medcol="purple",log="y",xaxt="n",yaxt="n",add=TRUE,at=c(6,21,36),frame.plot=FALSE)
bxp(list(stats=as.matrix(MEm[,1:4]),n=rep(10000,ncol(MEm[,1:4]))),boxwex=0,boxlty=1,boxlwd=1.5,boxcol="green3", medpch=15,medbg="white",medcol="green3", log="y",xaxt="n",yaxt="n",add=TRUE,at=c(7,22,37,48),frame.plot=FALSE)
bxp(list(stats=as.matrix(MEf[,1:4]),n=rep(10000,ncol(MEf[,1:4]))),boxwex=0,boxlty=2,boxlwd=1.5,boxcol="green3", medpch=19,medbg="white",medcol="green3", log="y",xaxt="n",yaxt="n",add=TRUE,at=c(8,23,38,49),frame.plot=FALSE)
bxp(list(stats=as.matrix(FIm[,1:4]),n=rep(10000,ncol(FIm[,1:4]))),boxwex=0,boxlty=1,boxlwd=1.5,boxcol="brown",  medpch=15,medbg="white",medcol="brown",  log="y",xaxt="n",yaxt="n",add=TRUE,at=c(9,24,39,50),frame.plot=FALSE)
bxp(list(stats=as.matrix(FIf[,1:4]),n=rep(10000,ncol(FIf[,1:4]))),boxwex=0,boxlty=2,boxlwd=1.5,boxcol="brown",  medpch=19,medbg="white",medcol="brown",  log="y",xaxt="n",yaxt="n",add=TRUE,at=c(10,25,40,51),frame.plot=FALSE)
# Place text in the figure with superscript and formatting
mtext(c(expression(bolditalic("D"[BH])),expression(bolditalic("D"[HQM])),expression(bolditalic("D"[LQM])),expression(bolditalic("D"[R]))),side=1,line=0,at=c(6,21,36,49),font=2,cex=0.8)
mtext(c("Parameter value"),side=2,line=3.25,at=50000,font=2,cex=0.9) # Create the axis labels to a desired position
axis(2,pos=-0.5,at=c(1000,10000,100000,1000000),labels=c(expression("10"^3),expression("10"^4),expression("10"^5),expression("10"^6)),lty=1,tck=-0.02,las=1,cex.axis=0.8)
lines(x=c(-1,52),y=c(1000,1000))

# place legend texts in two rows, four columns
legend(xy.coords(-5.5,23000000,log="y"),legend=c("Prior","","REF1","REF1","REF2","REF2","RIPARIAN","RIPARIAN","GEN","GEN"), col=c("black","white","blue","blue","purple","purple","green3","green3","brown","brown"), lty=c(1,2,1,2,1,2,1,2,1,2),lwd=c(2,0,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5),pch=c(3,3,15,19,15,19,15,19,15,19),ncol=5,cex=0.65,box.lwd=0.5,box.col="grey",text.width=7.5,seg.len=3.5,merge=TRUE)
text(xy.coords(13.0,15000000,log="y"),"\\MA",vfont=c("serif symbol","plain"),cex=0.9)
text(xy.coords(24.55,15000000,log="y"),"\\MA",vfont=c("serif symbol","plain"),cex=0.9)
text(xy.coords(38.15,15000000,log="y"),"\\MA",vfont=c("serif symbol","plain"),cex=0.9)
text(xy.coords(47.6,15000000,log="y"),"\\MA",vfont=c("serif symbol","plain"),cex=0.9)
text(xy.coords(12.9,7500000,log="y"),"\\VE",vfont=c("serif symbol","plain"),cex=0.9)
text(xy.coords(24.45,7500000,log="y"),"\\VE",vfont=c("serif symbol","plain"),cex=0.9)
text(xy.coords(38.05,7500000,log="y"),"\\VE",vfont=c("serif symbol","plain"),cex=0.9)
text(xy.coords(47.5,7500000,log="y"),"\\VE",vfont=c("serif symbol","plain"),cex=0.9)

screen(3) # plot the next two parameters
par(mar=c(2.0, 5.0, 2.0, 2.0),mgp=c(4,0.8,0))
bxp(list(stats=as.matrix(cbind(rep(0,5),Pri[,8:9],rep(0,5))),n=rep(10000,ncol(Pri[,8:9])+2)),boxwex=0,boxlty=1,boxlwd=2,boxcol="black", medpch="-",medcex=1.2,medcol="black", log="y",xaxt="n",ylim=c(0.05,0.7),at=c(1,2,17,25),axes=F,font=2)
bxp(list(stats=as.matrix(SIm[,8:9]),n=rep(10000,ncol(SIm[,8:9]))),boxwex=0,boxlty=1,boxlwd=1.5,boxcol="blue",  medpch=15,medcol="blue",  log="y",xaxt="n",yaxt="n",add=TRUE,at=c(3,18),frame.plot=FALSE)
bxp(list(stats=as.matrix(SIf[,8:9]),n=rep(10000,ncol(SIf[,8:9]))),boxwex=0,boxlty=2,boxlwd=1.5,boxcol="blue",  medpch=19,medcol="blue",  log="y",xaxt="n",yaxt="n",add=TRUE,at=c(4,19),frame.plot=FALSE)
bxp(list(stats=as.matrix(SOm[,8:9]),n=rep(10000,ncol(SOm[,8:9]))),boxwex=0,boxlty=1,boxlwd=1.5,boxcol="purple",medpch=15,medcol="purple",log="y",xaxt="n",yaxt="n",add=TRUE,at=c(5,20),frame.plot=FALSE)
bxp(list(stats=as.matrix(SOf[,8:9]),n=rep(10000,ncol(SOf[,8:9]))),boxwex=0,boxlty=2,boxlwd=1.5,boxcol="purple",medpch=19,medcol="purple",log="y",xaxt="n",yaxt="n",add=TRUE,at=c(6,21),frame.plot=FALSE)
bxp(list(stats=as.matrix(MEm[,8:9]),n=rep(10000,ncol(MEm[,8:9]))),boxwex=0,boxlty=1,boxlwd=1.5,boxcol="green3", medpch=15,medcol="green3", log="y",xaxt="n",yaxt="n",add=TRUE,at=c(7,22),frame.plot=FALSE)
bxp(list(stats=as.matrix(MEf[,8:9]),n=rep(10000,ncol(MEf[,8:9]))),boxwex=0,boxlty=2,boxlwd=1.5,boxcol="green3", medpch=19,medcol="green3", log="y",xaxt="n",yaxt="n",add=TRUE,at=c(8,23),frame.plot=FALSE)
bxp(list(stats=as.matrix(FIm[,8:9]),n=rep(10000,ncol(FIm[,8:9]))),boxwex=0,boxlty=1,boxlwd=1.5,boxcol="brown",  medpch=15,medcol="brown",  log="y",xaxt="n",yaxt="n",add=TRUE,at=c(9,24),frame.plot=FALSE)
bxp(list(stats=as.matrix(FIf[,8:9]),n=rep(10000,ncol(FIf[,8:9]))),boxwex=0,boxlty=2,boxlwd=1.5,boxcol="brown",  medpch=19,medcol="brown",  log="y",xaxt="n",yaxt="n",add=TRUE,at=c(10,25),frame.plot=FALSE)
mtext(c(expression(bolditalic("m")),expression(bolditalic("p"))),side=1,line=0,at=c(6,21),font=2,cex=0.8)
mtext(c("Parameter value"),side=2,line=3.25,at=0.18,font=2,cex=0.9)
axis(2,pos=-0.5,at=c(0.05,0.1,0.2,0.35,0.7),lty=1,tck=-0.02,las=1,cex.axis=0.8)
lines(x=c(-1,26),y=c(0.05,0.05))

screen(4) # plot the last three parameters
par(mar=c(2.0, 2.0, 2.0, 1.5),mgp=c(4,0.8,0))
bxp(list(stats=as.matrix(cbind(rep(0,5),Pri[,5:7],rep(0,5))),n=rep(10000,ncol(Pri[,5:7])+2)),boxwex=0,boxlty=1,boxlwd=2,boxcol="black", medpch="-",medcex=1.2,medcol="black", log="y",xaxt="n",ylim=c(0.00001,1),at=c(1,2,17,32,36),xlab="Parameter",axes=F)
bxp(list(stats=as.matrix(SIm[,5:6]),n=rep(10000,ncol(SIm[,5:6]))),boxwex=0,boxlty=1,boxlwd=1.5,boxcol="blue",  medpch=15,medcol="blue",  log="y",xaxt="n",yaxt="n",add=TRUE,at=c(3,18),frame.plot=FALSE)
bxp(list(stats=as.matrix(SIf[,5:6]),n=rep(10000,ncol(SIf[,5:6]))),boxwex=0,boxlty=2,boxlwd=1.5,boxcol="blue",  medpch=19,medcol="blue",  log="y",xaxt="n",yaxt="n",add=TRUE,at=c(4,19),frame.plot=FALSE)
bxp(list(stats=as.matrix(SOm[,5:6]),n=rep(10000,ncol(SOm[,5:6]))),boxwex=0,boxlty=1,boxlwd=1.5,boxcol="purple",medpch=15,medcol="purple",log="y",xaxt="n",yaxt="n",add=TRUE,at=c(5,20),frame.plot=FALSE)
bxp(list(stats=as.matrix(SOf[,5:6]),n=rep(10000,ncol(SOf[,5:6]))),boxwex=0,boxlty=2,boxlwd=1.5,boxcol="purple",medpch=19,medcol="purple",log="y",xaxt="n",yaxt="n",add=TRUE,at=c(6,21),frame.plot=FALSE)
bxp(list(stats=as.matrix(MEm[,5:7]),n=rep(10000,ncol(MEm[,5:7]))),boxwex=0,boxlty=1,boxlwd=1.5,boxcol="green3", medpch=15,medcol="green3", log="y",xaxt="n",yaxt="n",add=TRUE,at=c(7,22,33),frame.plot=FALSE)
bxp(list(stats=as.matrix(MEf[,5:7]),n=rep(10000,ncol(MEf[,5:7]))),boxwex=0,boxlty=2,boxlwd=1.5,boxcol="green3", medpch=19,medcol="green3", log="y",xaxt="n",yaxt="n",add=TRUE,at=c(8,23,34),frame.plot=FALSE)
bxp(list(stats=as.matrix(FIm[,5:7]),n=rep(10000,ncol(FIm[,5:7]))),boxwex=0,boxlty=1,boxlwd=1.5,boxcol="brown",  medpch=15,medcol="brown",  log="y",xaxt="n",yaxt="n",add=TRUE,at=c(9,24,35),frame.plot=FALSE)
bxp(list(stats=as.matrix(FIf[,5:7]),n=rep(10000,ncol(FIf[,5:7]))),boxwex=0,boxlty=2,boxlwd=1.5,boxcol="brown",  medpch=19,medcol="brown",  log="y",xaxt="n",yaxt="n",add=TRUE,at=c(10,25,36),frame.plot=FALSE)
mtext(c(expression(bolditalic("k"[HQM])),expression(bolditalic("k"[LQM])),expression(bolditalic("k"[R]))),side=1,line=0,at=c(6,21,34),font=2,cex=0.8)
axis(2,pos=-0.5,at=c(0.00001,0.0001,0.001,0.01,0.1,1),labels=c(expression("10"^-5),expression("10"^-4),expression("10"^-3),expression("10"^-2),expression("10"^-1),expression("10"^0)),lty=1,tck=-0.02,las=1,cex.axis=0.8)
lines(x=c(-1,37),y=c(0.00001,0.00001))

close.screen(all=TRUE)
dev.copy2pdf(file="Figure_name.pdf",out.type="pdf") # save the graph as a pdf (vector image)
