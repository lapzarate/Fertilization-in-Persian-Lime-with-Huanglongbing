#Clear
rm(list=ls())

#plot of the effect of CLas infection and fertilization treatments on the content of sucrose

library(ggplot2)
Plot_sucrose$Sanidad <- factor(Plot_sucrose$Sanidad, levels = c("HLB+", "HLB-")) #Sort health status data
sac1 <- ggplot(Plot_sucrose, aes(Factor, Prom,fill=Sanidad)) + geom_bar(stat="identity", position=position_dodge(width=0.9),colour = 'black',linewidth=1)+scale_fill_manual(values = c("black","grey"))+geom_errorbar(aes(ymin =Prom, ymax = Prom+DE), width=0.4,linewidth=1, position=position_dodge(width=0.9))+ylab(expression("Sucrose (mg g"^-1* " FW)")) + 
  xlab("Macronutrient levels")+ theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x = element_text(size=18,vjust=.7),axis.title.y = element_text(size=20),legend.title = element_text(size=30),axis.text = element_text(size = 20),strip.text.x = element_text(size = 30))+facet_grid(. ~ Nivel)
sac1

#Adjust Y axis
sac1+coord_cartesian(ylim = c(0, 20))
#Save
ggsave(filename="sac1.jpg",width=30, height=15, units="cm")

#Statistical analysis according to Brochu et al. (2023)
#Packages
library(openxlsx)
library(MASS)
library(rstatix)
library(ggplot2)
library(ggpubr)
library(lme4)
library(emmeans)
library(multcomp)
library(carData)
library(car)
library(nlme)
library(lsmeans)

#File location
setwd("C:/Users/Luis Alfredo/Documents/MEGAsync/Respaldo/Doctorado/TESIS/Capítulo II_Fertilización/Nuevo análisis/Sacarosa")

#load file
Sac_final <- within(read.xlsx(xlsxFile="Sucrose_ANOVA_Tukey.xlsx"),{
  ID = as.factor(ID)
  Sanidad= as.factor(Sanidad)
  Factor = as.factor(Factor)
  Nivel = as.factor(Nivel)
  Rep = as.factor(Rep)
  Sac= as.numeric(Sac)
})

#check file
str(Sac_final)
#normality
shapiro.test(Sac_final$Sac)
shapiro.test(Clor_final$Clor)
#Check outliers
boxplot(Sac_final$Sac, col="skyblue", frame.plot=F)
#Box-Cox transformation
boxcox = boxcox(Sac~ Sanidad*Factor*Nivel, data=Sac_final,lambda = seq(-2, 2, length = 100))

Sac1= lme(log(Sac)~ Sanidad*Factor*Nivel, random = ~ 1 | Rep / ID, data=Sac_final)

joint_tests(Sac1) #ANOVA

#Normality
r = residuals(Sac1,type="normalized", level=0)
hist(r,freq=F)
xfit<-seq(min(r),max(r),length=40)
yfit<-dnorm(xfit, mean=mean(r), sd=sd(r))
lines(xfit, yfit,col="red",lwd=2)
shapiro.test(r)
e1071::kurtosis(r) 
qqPlot(r)
#Homogeneidad de varianza
plot(fitted(Sac1, level=0), r, pch=16, ylab="Normalized residuals", xlab="Predicted values")

abline(h=0, lty=2)

#Tukey

a1 = emmeans(Sac1, ~ Sanidad*Factor*Nivel, type="response"); a1
sacf <- cld(a1,Letters=letters)
tidy(sacf)
plot(sacf)

#Second-degree polynomial regression analysis
setwd("C:/Users/Luis Alfredo/Documents/MEGAsync/Respaldo/Doctorado/TESIS/Capítulo II_Fertilización/Nuevo análisis/Sacarosa/Regresión polinomial")

#Polynomial regression analysis ----------------

#HLB infected plants
#Macronutrients
#Fit the data to a polynomial model.
fitQ = lm(Sac~poly(Factor,2,raw=TRUE), data=poly_sucrose_macros_sick) 
#ANOVA 
anova(fitQ)
summary(fitQ) #R2, R2adj and p

#micronutrients
fitQ2 = lm(Sac~poly(Factor,2,raw=TRUE), data=poly_sucrose_micros_sick) 
anova(fitQ2)
summary(fitQ2)

#Healthy plants 
#Macronutrients
fitQ3 = lm(Sac~poly(Factor,2,raw=TRUE), data=poly_sucrose_macros_Healthy) 
anova(fitQ3)
summary(fitQ3)
#Micronutrients
fitQ4 = lm(Sac~poly(Factor,2,raw=TRUE), data=poly_sucrose_micros_Healthy) 
anova(fitQ4)
summary(fitQ4)
#Plot polynomial regression of macronutrients sick vs healthy plants
Plot_sucrose_macros$Sanidad <- factor(Plot_sucrose_macros$Sanidad, levels = c("HLB+", "HLB-")) #Sort health status data
polSacMa <-  ggplot(data = Plot_sucrose_macros, aes(x = Factor, y = Sac, fill=Sanidad)) + geom_point(aes(shape=Sanidad,stroke = 2),size=3)+geom_smooth(method = "lm", formula = y ~ poly(x,2),color="black")+ scale_fill_manual(values = c("black","gray80")) + scale_x_continuous(breaks=0:2, labels=c("Ma-1", "Ma-2", "Ma-3"))+ylab("ng g")+xlab("Niveles de macros")+theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x = element_text(size=30,vjust=.7),axis.title.y = element_text(size=25),axis.title.x = element_text(size=25),legend.title = element_text(size=30),axis.text = element_text(size = 30),strip.text.x = element_text(size = 30))
polSacMa+coord_cartesian(ylim = c(0,22))+geom_smooth(method = "lm", se = FALSE)

#Plot polynomial regression of micronutrients sick vs healthy plants

Plot_sucreose_micros$Sanidad <- factor(Plot_sucreose_micros$Sanidad, levels = c("HLB+", "HLB-")) #Sort health status data
polSacMi <-  ggplot(data = Plot_sucreose_micros, aes(x = Factor, y = Sac, fill=Sanidad)) + geom_point(aes(shape=Sanidad,stroke = 2),size=3)+geom_smooth(method = "lm", formula = y ~ poly(x,2),color="black")+ scale_fill_manual(values = c("black","gray80")) + scale_x_continuous(breaks=0:2, labels=c("Ma-1", "Ma-2", "Ma-3"))+ylab("ng g")+xlab("Niveles de macros")+theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x = element_text(size=30,vjust=.7),axis.title.y = element_text(size=25),axis.title.x = element_text(size=25),legend.title = element_text(size=30),axis.text = element_text(size = 30),strip.text.x = element_text(size = 30))
polSacMi+coord_cartesian(ylim = c(0,22))+geom_smooth(method = "lm", se = FALSE)
