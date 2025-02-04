#plot of the effect of CLas infection and fertilization treatments on the content of starch   

library(ggplot2)
Plot_Starch$Sanidad <- factor(Plot_Starch$Sanidad, levels = c("HLB+", "HLB-")) #Sort health status data

Alm1 <- ggplot(Plot_Starch, aes(Factor, Prom,fill=Sanidad)) + geom_bar(stat="identity", position=position_dodge(width=0.9),colour = 'black',linewidth=1)+scale_fill_manual(values = c("black","grey"))+geom_errorbar(aes(ymin =Prom, ymax = Prom+DE), width=0.4,linewidth=1, position=position_dodge(width=0.9))+ylab("ng g peso fresco")+xlab("")+ theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x = element_text(size=18,vjust=.7),axis.title.y = element_text(size=20),legend.title = element_text(size=30),axis.text = element_text(size = 20),strip.text.x = element_text(size = 30))+facet_grid(. ~ Nivel)
Alm1

#Adjust Y axis
Alm1+coord_cartesian(ylim = c(0, 1))
#Save
ggsave(filename="Alm1.jpg",width=30, height=15, units="cm")

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
setwd("C:/Users/Luis Alfredo/Documents/MEGAsync/Respaldo/Doctorado/TESIS/Capítulo II_Fertilización/Nuevo análisis/Almidón")

#Load file
Alm_final <- within(read.xlsx(xlsxFile="Starch_ANOVA_Tukey.xlsx"),{
  ID = as.factor(ID)
  Sanidad= as.factor(Sanidad)
  Factor = as.factor(Factor)
  Nivel = as.factor(Nivel)
  Rep = as.factor(Rep)
  Alm= as.numeric(Alm)
})

#Check file
str(Alm_final)
#Normality
shapiro.test(Alm_final$Alm)
#Box-Cox transformation
boxplot(Alm_final$Alm, col="skyblue", frame.plot=F)

boxcox = boxcox(Alm ~ Sanidad*Factor*Nivel, data=Alm_final,lambda = seq(-2, 2, length = 100))

Alm1= lme(log(Alm)~ Sanidad*Factor*Nivel, random = ~ 1 | Rep / ID, data=Alm_final)
#ANOVA
joint_tests(Alm1)

#Normality
r = residuals(Alm1,type="normalized", level=0)
hist(r,freq=F)
xfit<-seq(min(r),max(r),length=40)
yfit<-dnorm(xfit, mean=mean(r), sd=sd(r))
lines(xfit, yfit,col="red",lwd=2)
shapiro.test(r)
e1071::kurtosis(r) 
qqPlot(r)
#Homogeneidad de varianza
plot(fitted(Alm1, level=0), r, pch=16, ylab="Normalized residuals", xlab="Predicted values")

abline(h=0, lty=2)

#Tukey

a1 = emmeans(Alm1, ~ Sanidad*Factor*Nivel, type="response"); a1
almf <- cld(a1,Letters=letters)
tidy(almf)
plot(almf)

#Second-degree polynomial regression analysis
#Polynomial regression analysis ----------------
  
  #HLB infected plants
  #Macronutrients
#Fit the data to a polynomial model.
  fitQ = lm(Alm~poly(Factor,2,raw=TRUE), data=Poly_starch_macros_sick) 
#ANOVA 
anova(fitQ)
summary(fitQ) #R2, R2adj and p

#micronutrients
fitQ2 = lm(Alm~poly(Factor,2,raw=TRUE), data=Poly_starch_micros_sick) 
anova(fitQ2)
summary(fitQ2)

#Healthy plants 
#Macronutrients
fitQ3 = lm(Alm~poly(Factor,2,raw=TRUE), data=Poly_starch_macros_Healthy) 
anova(fitQ3)
summary(fitQ3)
#Micronutrients
fitQ4 = lm(Alm~poly(Factor,2,raw=TRUE), data=Poly_starch_micros_Healthy) 
anova(fitQ4)
summary(fitQ4)

#Plot polynomial regression of macronutrients sick vs healthy plants

library(ggplot2)
Plot_starch_macros_poly$Sanidad <- factor(Plot_starch_macros_poly$Sanidad, levels = c("HLB+", "HLB-")) #Sort health status data

plot_poly_starch_macros<-  ggplot(data = Plot_starch_macros_poly, aes(x = Factor, y = Alm, fill=Sanidad)) + geom_point(aes(shape=Sanidad,stroke = 2),size=3)+geom_smooth(method = "lm", formula = y ~ poly(x,2),color="black")+ scale_fill_manual(values = c("black","gray80")) + scale_x_continuous(breaks=0:2, labels=c("Ma-1", "Ma-2", "Ma-3"))+ylab(expression("Total chlorophyll (mg g"^-1* " FW)")) + 
  xlab("Macronutrient")+theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x = element_text(size=30,vjust=.7),axis.title.y = element_text(size=25),axis.title.x = element_text(size=25),legend.title = element_text(size=30),axis.text = element_text(size = 30),strip.text.x = element_text(size = 30))
plot_poly_starch_macros+coord_cartesian(ylim = c(0,1))+geom_smooth(method = "lm", se = FALSE)

#Plot polynomial regression of micronutrients sick vs healthy plants
Plot_starch_micros_poly$Sanidad <- factor(Plot_starch_micros_poly$Sanidad, levels = c("HLB+", "HLB-")) #Sort health status data
plot_poly_starch_micros <-  ggplot(data = Plot_starch_micros_poly, aes(x = Factor, y = Alm, fill=Sanidad)) + geom_point(aes(shape=Sanidad,stroke = 2),size=3)+geom_smooth(method = "lm", formula = y ~ poly(x,2),color="black")+ scale_fill_manual(values = c("black","gray80")) + scale_x_continuous(breaks=0:2, labels=c("Ma-1", "Ma-2", "Ma-3"))+ylab(expression("Total chlorophyll (mg g"^-1* " FW)")) + 
  xlab("Micronutrient")+theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x = element_text(size=30,vjust=.7),axis.title.y = element_text(size=25),axis.title.x = element_text(size=25),legend.title = element_text(size=30),axis.text = element_text(size = 30),strip.text.x = element_text(size = 30))
plot_poly_starch_micros +coord_cartesian(ylim = c(0,1))+geom_smooth(method = "lm", se = FALSE)
