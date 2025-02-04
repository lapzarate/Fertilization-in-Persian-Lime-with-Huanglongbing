#plot of the effect of CLas infection and fertilization treatments on the stem diameter

#Gráfica
library(ggplot2)
Plot_StemDiameter$Sanidad <- factor(Plot_StemDiameter$Sanidad, levels = c("HLB+", "HLB-")) #Sort health status data
DT <- ggplot(Plot_StemDiameter, aes(Factor, Prom,fill=Sanidad)) + geom_bar(stat="identity", position=position_dodge(width=0.9),colour = 'black',linewidth=1)+scale_fill_manual(values = c("black", "grey"))+geom_errorbar(aes(ymin =Prom, ymax = Prom+DE), width=0.4,linewidth=1, position=position_dodge(width=0.9))+ylab("Cumulative TD (mm)")+xlab("Macronutrient levels")+ theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x = element_text(size=18,vjust=.7),axis.title.y = element_text(size=20),legend.title = element_text(size=30),axis.text = element_text(size = 20),strip.text.x = element_text(size = 30))+facet_grid(. ~ Nivel)
DT

#Adjust Y axis

DT+coord_cartesian(ylim = c(5, 22))
#guardar en alta resolución

ggsave(filename="DTFinal.jpg",width=30, height=15, units="cm")

#Packages
library(openxlsx)
library(rstatix)
library(ggplot2)
library(ggpubr)
library(lme4)
library(emmeans)
library(multcomp)
library(car)
library(MASS)
library(nlme)
library(lsmeans)

#File location
setwd("C:/Users/Luis Alfredo/Documents/MEGAsync/Respaldo/Doctorado/TESIS/Capítulo II_Fertilización/Nuevo análisis/diámetro de tallo")

#Load file
DT_final <- within(read.xlsx(xlsxFile="Stem diameter_ANOVA_Tukey.xlsx"),{
  ID = as.factor(ID)
  Sanidad= as.factor(Sanidad)
  Factor = as.factor(Factor)
  Nivel = as.factor(Nivel)
  Rep = as.factor(Rep)
  DT = as.numeric(DT)
})

#Check file
str(DT_final)
#Normality
shapiro.test(DT_final$DT)
#Check outliers
boxplot(DT_final$DT, col="skyblue", frame.plot=F)
#Box-Cox transformation
boxcox = boxcox(DT ~ Sanidad*Factor*Nivel, data=DT_final,lambda = seq(-2, 2, length = 100))

DT1= lme(DT~ Sanidad*Factor*Nivel, random = ~ 1 | Rep / ID, data=DT_final)
#ANOVA
joint_tests(DT1)

#Normalidad
r = residuals(DT1,type="normalized", level=0)
hist(r,freq=F)
xfit<-seq(min(r),max(r),length=40)
yfit<-dnorm(xfit, mean=mean(r), sd=sd(r))
lines(xfit, yfit,col="red",lwd=2)
shapiro.test(r)
e1071::kurtosis(r) 

#Homogeneidad de varianza
plot(fitted(DT1, level=0), r, pch=16, ylab="Normalized residuals", xlab="Predicted values")

abline(h=0, lty=2)

#Tukey

a1 = emmeans(DT1, ~ Sanidad*Factor*Nivel, type="response"); a1
DTf <- cld(a1,Letters=letters)
tidy(DTf)
plot(DTf)

#Second-degree polynomial regression analysis

#Polynomial regression analysis ----------------

#HLB infected plants
#Macronutrients
#Fit the data to a polynomial model.
fitQ = lm(DT~poly(Factor,2,raw=TRUE), data=Poly_DT_macros_sick) 
#ANOVA 
anova(fitQ)
summary(fitQ) #R2, R2adj and p

#micronutrients
fitQ2 = lm(DT~poly(Factor,2,raw=TRUE), data=Poly_DT_micros_sick) 
anova(fitQ2)
summary(fitQ2)

#Healthy plants 
#Macronutrients
fitQ3 = lm(DT~poly(Factor,2,raw=TRUE), data=Poly_DT_macros_Healthy) 
anova(fitQ3)
summary(fitQ3)
#Micronutrients
fitQ4 = lm(DT~poly(Factor,2,raw=TRUE), data=Poly_DT_micros_healthy) 
anova(fitQ4)
summary(fitQ4)


#Plot polynomial regression of macronutrients sick vs healthy plants
PloT_DT_macros_poly$Sanidad <- factor(PloT_DT_macros_poly$Sanidad, levels = c("HLB+", "HLB-")) #Sort health status data
polDT <-  ggplot(data = PloT_DT_macros_poly, aes(x = Factor, y = DT, fill=Sanidad)) + geom_point(aes(shape=Sanidad,stroke = 2),size=3)+geom_smooth(method = "lm", formula = y ~ poly(x,2),color="black")+ scale_fill_manual(values = c("black","gray80")) + scale_x_continuous(breaks=0:2, labels=c("Ma-1", "Ma-2", "Ma-3"))+ylab("Cumulative TD (mm)")+xlab("Macronutrients")+theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x = element_text(size=30,vjust=.7),axis.title.y = element_text(size=25),axis.title.x = element_text(size=25),legend.title = element_text(size=30),axis.text = element_text(size = 30),strip.text.x = element_text(size = 30))
polDT+coord_cartesian(ylim = c(5, 23))+geom_smooth(method = "lm", se = FALSE)

#Plot polynomial regression of micronutrients sick vs healthy plants
Plot_DT_micros_poly$Sanidad <- factor(Plot_DT_micros_poly$Sanidad, levels = c("HLB+", "HLB-")) #Sort health status data
polDT2 <-  ggplot(data = Plot_DT_micros_poly, aes(x = Factor, y = DT, fill=Sanidad)) + geom_point(aes(shape=Sanidad,stroke = 2),size=3)+geom_smooth(method = "lm", formula = y ~ poly(x,2),color="black")+ scale_fill_manual(values = c("black","gray80")) + scale_x_continuous(breaks=0:2, labels=c("Mi-1", "Mi-2", "M-3"))+ylab("Cumulative TD (mm)")+xlab("Micronutrients")+theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x = element_text(size=30,vjust=.7),axis.title.y = element_text(size=25),axis.title.x = element_text(size=25),legend.title = element_text(size=30),axis.text = element_text(size = 30),strip.text.x = element_text(size = 30))
polDT2+coord_cartesian(ylim = c(5, 23))+geom_smooth(method = "lm", se = FALSE)

