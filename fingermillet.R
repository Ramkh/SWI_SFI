setwd("E:/R- Analysis- march-22/Fingermillet")
read.csv("SFI.csv")->rhizoc
attach(rhizoc)
names(rhizoc)
as.factor(Treatments)->trt
as.factor(Rep)->rep
options(show.signif.stars=F,digits=5)
model <- lm(Grain.yield..mt.ha. ~ trt, data = rhizoc)
anova(model)

library(agricolae)
model2<-aov(Grain.yield..mt.ha.~rep+trt)
model2
comp4<-LSD.test(model2,"trt")
comp4


model<-aov(Spad.reading~trt+rep)
summary(model)
library(agricolae)
comp4<-LSD.test(model,"trt",console=TRUE)
comp4

par(mfrow=c(1,4))
op <-par(mar = c(7,2.5,0.8,0.001)+0.5)
boxplot(Root.Length..cm.~trt, data = rhizoc, col=c("grey27", "white","grey"), ylab = "Root lenght (cm)", main= " Root length (cm)", 
        horizontal = F, axis=1, las=3, cex.axis = 1.4)
boxplot(Leaf.area..cm.2.~trt, data = rhizoc, col=c("grey27", "white","grey"), ylab = "Leaf area (sq. cm)", main= " Leaf area (cm^2)", 
        horizontal = F, axis=1, las=3, cex.axis = 1.4)
boxplot(X1000.Kernel.Weight..gm.~trt, data = rhizoc, col=c("grey27", "white","grey"), ylab = " Test weight (gm)", main= "Test weight (gm)", 
        horizontal = F, axis=1, las=3, cex.axis = 1.4)
boxplot(X..Filled.Grains.Spike~trt, data = rhizoc, col=c("grey27", "white","grey"), ylab = " Filled grain per spike", main= "Grain per spike", 
        horizontal = F, axis=1, las=3, cex.axis = 1.4)

boxplot(Grain.Yield..kg.ha.~meth, data = rhizoc, col=c("black", "white","grey"), ylab = " Grain yield", main= "Grain Yield (mt/ha", 
        horizontal = F, axis=1, las=3, cex.axis = 0.9)

boxplot(X..Filled.Grains.Spike~meth, data = rhizoc, col=c("blue", "white","red"), ylab = "Filled grain per spike", main= "Filled grains per spike" 
        horizontal = F, axis=1, las=3, cex.axis = 1.2

par(mfrow=c(1,4))
op <-par(mar = c(10,2.5,0.8,0.001)+0.5)
boxplot(Grain.yield..mt.ha.~trt, data = rhizoc, col=c("grey27", "white","grey","gray36","gray87","gray27","gray", "gray56"), ylab = "SPAD reading", main= " Grain yield (mt/ha)", 
        horizontal = F, axis=1, las=3, cex.axis = 1.2)
boxplot(Spad.reading~trt, data = rhizoc, col=c("grey27", "white","grey","gray36","gray87","gray27","gray", "gray56"), ylab = "SPAD reading", main= " SPAD", 
        horizontal = F, axis=1, las=3, cex.axis = 1.2)

boxplot(Ear.Length..cm.~trt, data = rhizoc, col=c("grey27", "white","grey","gray36","gray87","gray27","gray", "gray56"), ylab = "Finger per plant", main= " Fingers per plant", 
        horizontal = F, axis=1, las=3, cex.axis = 1.2)

boxplot(Finger.plant~trt, data = rhizoc, col=c("grey27", "white","grey","gray36","gray87","gray27","gray", "gray56"), ylab = "Ear length (cm)", main= " Ear length (cm)", 
        horizontal = F, axis=1, las=3, cex.axis = 1.2)


op <-par(mar = c(2,9,2,1)+0.1)
plot(Grain.yield..mt.ha., Spad.reading,xlab="Grain Yield (mt/ha)", ylab="SPAD reading",main = " p-value = 0.0036", pch=16)
abline(lm(Spad.reading ~ Grain.yield..mt.ha.))

par(mfrow=c(2,2))
op <-par(mar = c(2,2,1,1)+0.01); on.exit(par(op))
plot(Grain.yield..mt.ha., Spad.reading, Xlab="Grain Yield",cex.axis = 0.9, main = " p-value = 0.0036", pch=10)
abline(lm(Spad.reading ~ Grain.yield..mt.ha.))
plot(Straw.Yield.mt.ha., Spad.reading, Xlab="Straw Yield",cex.axis = 0.9, main = " p-value = 0.0036", pch=10)
abline(lm(Spad.reading ~ Straw.Yield.mt.ha.))
plot(Straw.Yield.mt.ha., Spad.reading, ylab="Ear length",cex.axis = 0.9, main = " p-value = 0.0036", pch=10)
abline(lm(Spad.reading ~ Ear.Length..cm.))
plot(Finger.plant, Spad.reading, ylab="Finger lenght",cex.axis = 0.9, main = " p-value = 0.0036", pch=10)
abline(lm(Spad.reading ~ Finger.plant))

