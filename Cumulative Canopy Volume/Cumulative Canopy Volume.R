#plot of the effect of CLas infection and fertilization treatments on the Canopy Volume
library(ggplot2)
Canopy_Volume_Plot$Sanidad <- factor(Canopy_Volume_Plot$Sanidad, levels = c("HLB+", "HLB-")) #Sort health status data
VD1 <- ggplot(Canopy_Volume_Plot, aes(Factor, Prom,fill=Sanidad)) + geom_bar(stat="identity", position=position_dodge(width=0.9),colour = 'black',linewidth=1)+scale_fill_manual(values = c("black", "grey"))+geom_errorbar(aes(ymin =Prom, ymax = Prom+DE), width=0.4,linewidth=1, position=position_dodge(width=0.9))+ylab(expression("Cumulative CV (m"^3* " )")) + 
  xlab("Macronutrient levels")+ theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x = element_text(size=18,vjust=.7),axis.title.y = element_text(size=20),legend.title = element_text(size=30),axis.text = element_text(size = 20),strip.text.x = element_text(size = 30))+facet_grid(. ~ Nivel)
VD1

#Adjust Y axis
VD1+coord_cartesian(ylim = c(0.7, 4.3))
#guardar en alta resolución

ggsave(filename="VD.jpg",width=30, height=15, units="cm")

#Statistical analysis according to Brochu et al. (2023)

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

#File locationo
setwd("C:/Users/Luis Alfredo/Documents/MEGAsync/Respaldo/Doctorado/TESIS/Capítulo II_Fertilización/Nuevo análisis/Volumen de dosel")

#Load file
VD_final <- within(read.xlsx(xlsxFile="Canopy_Volume_ANOVA_Tukey.xlsx"),{
  ID = as.factor(ID)
  Sanidad= as.factor(Sanidad)
  Factor = as.factor(Factor)
  Nivel = as.factor(Nivel)
  Rep = as.factor(Rep)
  VD = as.numeric(VD)
})

#Check file
str(VD_final)
#Normality
shapiro.test(VD_final$VD)

#Check outliers
boxplot(VD_final$VD, col="skyblue", frame.plot=F)

#Box-Cox transformation
boxcox = boxcox(VD ~ Sanidad*Factor*Nivel, data=VD_final,lambda = seq(-2, 2, length = 100))

VD1= lme(VD~ Sanidad*Factor*Nivel, random = ~ 1 | Rep / ID, data=VD_final)
#ANOVA
joint_tests(VD1)

#Normalidad
r = residuals(VD1,type="normalized", level=0)
hist(r,freq=F)
xfit<-seq(min(r),max(r),length=40)
yfit<-dnorm(xfit, mean=mean(r), sd=sd(r))
lines(xfit, yfit,col="red",lwd=2)
shapiro.test(r)
e1071::kurtosis(r) 

#Homogeneidad de varianza
plot(fitted(VD1, level=0), r, pch=16, ylab="Normalized residuals", xlab="Predicted values")

abline(h=0, lty=2)

#Tukey

a1 = emmeans(VD1, ~ Sanidad*Factor*Nivel, type="response"); a1
altf <- cld(a1,Letters=letters)
tidy(altf)
plot(altf)

#Second-degree polynomial regression analysis
#Polynomial regression analysis ----------------

#HLB infected plants
#Macronutrients
#Fit the data to a polynomial model.
fitQ = lm(VD~poly(Factor,2,raw=TRUE), data=Poly_CV_macros_sick) 
#ANOVA 
anova(fitQ)
summary(fitQ) #R2, R2adj and p

#micronutrients
fitQ2 = lm(VD~poly(Factor,2,raw=TRUE), data=Poly_CV_micros_sick) 
anova(fitQ2)
summary(fitQ2)

#Healthy plants 
#Macronutrients
fitQ3 = lm(VD~poly(Factor,2,raw=TRUE), data=Poly_CV_macros_Healthy) 
anova(fitQ3)
summary(fitQ3)
#Micronutrients
fitQ4 = lm(VD~poly(Factor,2,raw=TRUE), data=Poly_CV_micros_Healthy) 
anova(fitQ4)
summary(fitQ4)

#Plot polynomial regression of macronutrients sick vs healthy plants
Plot_CV_macros_poly$Sanidad <- factor(Plot_CV_macros_poly$Sanidad, levels = c("HLB+", "HLB-")) #Sort health status data
polVDMa <-  ggplot(data = Plot_CV_macros_poly, aes(x = Factor, y = VD, fill=Sanidad)) + geom_point(aes(shape=Sanidad,stroke = 2),size=3)+geom_smooth(method = "lm", formula = y ~ poly(x,2),color="black")+ scale_fill_manual(values = c("black","gray80")) + scale_x_continuous(breaks=0:2, labels=c("Ma-1", "Ma-2", "Ma-3"))+ylab(expression("Cumulative CV (m"^3* " )")) + 
  xlab("Macronutrient levels")+theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x = element_text(size=30,vjust=.7),axis.title.y = element_text(size=25),axis.title.x = element_text(size=25),legend.title = element_text(size=30),axis.text = element_text(size = 30),strip.text.x = element_text(size = 30))
polVDMa+coord_cartesian(ylim = c(0.7,4.7))+geom_smooth(method = "lm", se = FALSE)

#Plot polynomial regression of micronutrients sick vs healthy plants
Plot_CV_micros_poly$Sanidad <- factor(Plot_CV_micros_poly$Sanidad, levels = c("HLB+", "HLB-")) #Sort health status data

polVDMi <-  ggplot(data = Plot_CV_micros_poly, aes(x = Factor, y = VD, fill=Sanidad)) + geom_point(aes(shape=Sanidad,stroke = 2),size=3)+geom_smooth(method = "lm", formula = y ~ poly(x,2),color="black")+ scale_fill_manual(values = c("black","gray80")) + scale_x_continuous(breaks=0:2, labels=c("Mi-1", "Mi-2", "Mi-3"))+ylab(expression("Cumulative CV (m"^3* " )")) + 
  xlab("Micronutrient levels")+theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x = element_text(size=30,vjust=.7),axis.title.y = element_text(size=25),axis.title.x = element_text(size=25),legend.title = element_text(size=30),axis.text = element_text(size = 30),strip.text.x = element_text(size = 30))
polVDMi+coord_cartesian(ylim = c(0.7,4.7))+geom_smooth(method = "lm", se = FALSE)
