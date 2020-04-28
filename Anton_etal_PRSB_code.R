
###################################
# 
# Global determinants of prey naiveté to exotic predators 
# Proceedings of the Royal Society B #
# Script authors: Andrea Anton and Nathan Geraldi


library("readxl", lib.loc="~/Library/R/3.3/library")
library("metafor", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
library("Rmisc", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
library("plotrix", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
library("tiff", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")

data <- read_excel("naivete_meta_data_7.xlsx",sheet=1)
data$paper_ID<-as.character(data$paper_ID)
data$experiment_ID_2<-paste(data$paper_ID,data$experiment_ID)
data$insularity_1<-paste(data$ecosystem,data$insularity)
data$insularity_2<-data$insularity
data$insularity_2[data$realm_invasive_range__1=="australian"]<-"island"
data$insularity_3<-paste(data$ecosystem,data$insularity_2)
data$exo_pred_group_3<-paste(data$exo_pred_group_2,data$ecosystem)

dataIN2<-data[data$ecosystem=="terrestrial",]# only terrestrial
data$study_site<-paste(data$Station_Name,data$Lat_1,data$Long_1)


data$exo_pred_group_2<-factor(data$exo_pred_group_2,levels=c("insect","seastar","crustacean","fish","herpetofauna","mammal"))
data$prey_group<-factor(data$prey_group,levels=c("insect","mollusc","crustacean","fish","herpetofauna","mammal"))
data$ecosystem<-factor(data$ecosystem,levels=c('marine','freshwater','terrestrial'))
data$dif_realm<-factor(data$dif_realm,levels=c("no","yes"))
data$EP_genus_present<-factor(data$EP_genus_present,levels=c("yes","no"))


# Models (including PREY GENERATION)

# Categorival predictors
x1a<-rma.mv(yi, vi, mods = ~ ecosystem-1 + prey_generations, random=~factor(paper_ID)|factor(experiment_ID_2),data=data)
x4a<-rma.mv(yi,vi,mods=~dif_realm-1 + prey_generations, random=~factor(paper_ID)|factor(experiment_ID_2),data=data,method="ML")
datax5<-subset(data,prey_group%in%c("crustacean","fish","herpetofauna","mammal"))
x5a<-rma.mv(yi,vi,mods=~prey_group-1 + prey_generations,random=~factor(paper_ID)|factor(experiment_ID_2),data=datax5,method="ML")  
x9a<-rma.mv(yi,vi,mods=~EP_genus_present-1 + prey_generations,random=~factor(paper_ID)|factor(experiment_ID_2),data=data,method="ML")
datax11<-subset(data,exo_pred_group_2%in%c("crustacean","fish","herpetofauna","mammal"))
x11b<-rma.mv(yi,vi,mods=~exo_pred_group_2-1 + prey_generations,random=~factor(paper_ID)|factor(experiment_ID_2),data=datax11,method="ML")  
x17b<-rma.mv(yi~insularity_3-1 + prey_generations,vi,random=~factor(paper_ID)|factor(experiment_ID_2),data=dataIN2,method="ML")

# Continuous predictors
x21<-rma.mv(yi~prey_generations,vi,random=~factor(paper_ID)|factor(experiment_ID_2),data=data,method="ML")
x20<-rma.mv(yi~latitude_introduction_1,vi,random=~factor(paper_ID)|factor(experiment_ID_2),data=data,method="ML")



error.x <- function(x, y, xupper, xlower=xupper, length=0.05,...){
  if(length(x) != length(y) | length(y) !=length(xlower) | length(xlower) != length(xupper))
    stop("vectors must be same length")
  arrows(x+xupper,y, x-xlower, y, angle=90, code=3, length=length, ...)
}


# FIGURE 1 

tiff("Fig_2.tiff", width = 4.5, height = 4, units = 'in', res = 300)

par(mfrow=c(3,2),mar=c(2.5,5.5,1.5,2),oma=c(1,1,0,0),cex=.85)

m<-x1a
n<-c(1,2,3)##row # of y levels - 1,2,3 for fresh, ter, marine
lab<-c("Marine","Freshwater","Terrestrial")
xl<-c(-2.5,2)
yl<-c(.5,4.1)
x<-m$b[n]
x<-x[-4]
y<-n
ci<-m$b[n]-m$ci.lb[n]
plot(x,y,xlab=" ",ylab=" ",xlim=xl,ylim=yl,axes=F,pch=19,cex=1.7)
text(0,3.6,"System type",cex=1.2)
text(2,3.6,"(a)",cex=1.2)
text(1.8,3,"(72)",cex=1)
text(1.8,2,"(117)*",cex=1)
text(1.8,1,"(25)***",cex=1)
axis(2,las=2,labels=lab,at=c(1,2,3))
axis(1, las=1)
error.x(x,y,ci)
ablineclip(v = 0, y1 = 0,y2 = 3.2,lty = 3)

m<-x17b
n<-c(1,2)
lab<-c("Island","Mainland")
xl<-c(-2.5,2)
yl<-c(.5,3.1)
x<-m$b[n]
y<-n
ci<-m$b[n]-m$ci.lb[n]
plot(x,y,xlab=" ",ylab=" ",xlim=xl,ylim=yl,axes=F,pch=19,cex=1.5)
text(0,2.7,"Insularity (terrestrial)",cex=1.2)
text(2,2.7,"(b)",cex=1.2)
text(1.8,2,"(54)",cex=1)
text(1.8,1,"(18)**",cex=1)
axis(2,las=2,labels=lab,at=c(1,2))
axis(1, las=1)
error.x(x,y,ci)
ablineclip(v = 0, y1 = 0,y2 = 2.2,lty = 3)

m<-x4a
n<-c(1,2)
lab<-c("No","Yes")
xl<-c(-2.5,2)
yl<-c(.5,3.1)
x<-m$b[n]
y<-n
x<-x[-3]
ci<-m$b[n]-m$ci.lb[n]
plot(x,y,xlab=" ",ylab=" ",xlim=xl,ylim=yl,axes=F,pch=19,cex=1.7)
text(0,2.7,"Different biogeographic realm",cex=1.2)
text(2,2.7,"(c)",cex=1.2)
text(1.8,2,"(175)***",cex=1)
text(1.8,1,"(39)",cex=1)
axis(2,las=2,labels=lab,at=c(1,2))
axis(1, las=1)
error.x(x,y,ci)
ablineclip(v = 0, y1 = 0,y2 = 2.2,lty = 3)


m<-x9a
n<-c(1,2)
lab<-c("No","Yes")
xl<-c(-2.5,2)
yl<-c(.5,3.1)
x<-m$b[n]
y<-n
ci<-m$b[n]-m$ci.lb[n]
plot(x,y,xlab=" ",ylab=" ",xlim=xl,ylim=yl,axes=F,pch=19,cex=1.5)
text(0,2.7,"Taxonomic distinctiveness",cex=1.2)
text(2,2.7,"(d)",cex=1.2)
text(1.8,2,"(190)***",cex=1)
text(1.8,1,"(24)",cex=1)
axis(2,las=2,labels=lab,at=c(1,2))
axis(1, las=1)
error.x(x,y,ci)
ablineclip(v = 0, y1 = 0,y2 = 2.2,lty = 3)

m<-x11b
n<-c(1,2,3,4)
lab<-c("Crustacean","Fish","Herpetofauna","Mammal")
xl<-c(-2.5,2)
yl<-c(.5,5.1)
x<-m$b[n]
x<-x[-7]
y<-n
ci<-m$b[n]-m$ci.lb[n]
plot(x,y,xlab=" ",ylab=" ",xlim=xl,ylim=yl,axes=F,pch=19,cex=1.7)
text(0,5,"Exotic predator taxa",cex=1.2)
text(2,5,"(e)",cex=1.2)
text(1.8,4,"(66)",cex=1)
text(1.8,3,"(24)**",cex=1)
text(1.8,2,"(78)***",cex=1)
text(1.8,1,"(39)",cex=1)
axis(2,las=2,labels=lab,at=c(1,2,3,4))
axis(1, las=1)
error.x(x,y,ci)
ablineclip(v = 0, y1 = 0,y2 = 4.2,lty = 3)

m<-x5a
n<-c(1,2,3,4,5,6)
lab<-c("Insect","Mollusc","Crustacean","Fish","Herpetofauna","Mammal")
xl<-c(-2.5,2)
yl<-c(.5,7.1)
x<-m$b[n]
x<-x[-7]
y<-n
ci<-m$b[n]-m$ci.lb[n]
plot(x,y,xlab=" ",ylab=" ",xlim=xl,ylim=yl,axes=F,pch=19,cex=1.7)
text(0,7,"Native prey taxa",cex=1.2)
text(2,7,"(f)",cex=1.2)
text(1.8,6,"(56)",cex=1)
text(1.8,5,"(78)*",cex=1)
text(1.8,4,"(38)***",cex=1)
text(1.8,3,"(16)",cex=1)
text(1.8,2,"(15)",cex=1)
text(1.8,1,"(11)",cex=1)
axis(2,las=2,labels=lab,at=c(1,2,3,4,5,6))
axis(1, las=1)
error.x(x,y,ci)
ablineclip(v = 0, y1 = 0,y2 = 6.2,lty = 3)


mtext(expression("Effect size"~italic('g')), side=1, font=1,outer=TRUE,line=0,cex=1.1)

dev.off()




###############################
### FIGURE 2

par(mfrow=c(2,1),mar=c(4,5,1,1),oma=c(1,0,2,0),cex=1.5)
plot(data$prey_generations,data$yi,log="x",xlab="Number of prey generations",ylab=expression(Effect~size~italic(g)))
preds <- predict(x21, newmods=c(0:700)) 
preds_test <- predict(x21, newmods=c(100:300)) # 
lines(0:700, preds$pred,lwd=2)
lines(0:700, preds$ci.lb, lty="dashed",lwd=1)
lines(0:700, preds$ci.ub, lty="dashed",lwd=1)
text(1.7,2.5,"(a)",cex=1.2)

plot(data$latitude_introduction_1,data$yi,ylab=expression(Effect~size~italic(g)),xlab="Absolute latitude")
preds <- predict(x20, newmods=c(0:60))
lines(0:60, preds$pred,lwd=2)
lines(0:60, preds$ci.lb, lty="dashed",lwd=1)
lines(0:60, preds$ci.ub, lty="dashed",lwd=1)
text(2,2.5,"(b)",cex=1.2)

