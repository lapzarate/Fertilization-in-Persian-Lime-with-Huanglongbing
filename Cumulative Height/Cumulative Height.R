#plot of the effect of CLas infection and fertilization treatments on the cumulative height 

#Gráfica
library(ggplot2)

Plot_Cumulative_Height$Sanidad <- factor(Plot_Cumulative_Height$Sanidad, levels = c("HLB+", "HLB-")) #Sort health status data
Alt1 <- ggplot(Plot_Cumulative_Height, aes(Factor, Prom,fill=Sanidad)) + geom_bar(stat="identity", position=position_dodge(width=0.9),colour = 'black',linewidth=1)+scale_fill_manual(values = c("black", "grey"))+geom_errorbar(aes(ymin =Prom, ymax = Prom+DE), width=0.4,linewidth=1, position=position_dodge(width=0.9))+ylab("Cumulative growth (cm)")+xlab("Macronutrient levels")+ theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x = element_text(size=18,vjust=.7),axis.title.y = element_text(size=20),legend.title = element_text(size=30),axis.text = element_text(size = 20),strip.text.x = element_text(size = 30))+facet_grid(. ~ Nivel)

#Adjust Y axisy
Alt1+coord_cartesian(ylim = c(50, 250))

#Save
ggsave(filename="Alt1.jpg",width=30, height=15, units="cm")

#Paquetes
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
setwd("C:/Users/Luis Alfredo/Documents/MEGAsync/Respaldo/Doctorado/TESIS/Capítulo II_Fertilización/Nuevo análisis/Altura")

#Load file
Alt_final <- within(read.xlsx(xlsxFile="Cumulative height_ANOVA_Tukey.xlsx"),{
  ID = as.factor(ID)
  Sanidad= as.factor(Sanidad)
  Factor = as.factor(Factor)
  Nivel = as.factor(Nivel)
  Rep = as.factor(Rep)
  Alt = as.numeric(Alt)
})

#Check file
str(Alt_final)
#Normality
shapiro.test(Alt_final$Alt)
#Check outliers
boxplot(Alt_final$Alt, col="skyblue", frame.plot=F)
#Box-Cox transformation
boxcox = boxcox(Alt ~ Sanidad*Factor*Nivel, data=Alt_final,lambda = seq(-2, 2, length = 100))

Alt1= lme((Alt)^2~ Sanidad*Factor*Nivel, random = ~ 1 | Rep / ID, data=Alt_final)
#Anova
joint_tests(Alt1)

#Normalidad
r = residuals(Alt1,type="normalized", level=0)
hist(r,freq=F)
xfit<-seq(min(r),max(r),length=40)
yfit<-dnorm(xfit, mean=mean(r), sd=sd(r))
lines(xfit, yfit,col="red",lwd=2)
shapiro.test(r)
e1071::kurtosis(r) 

#Homogeneidad de varianza
plot(fitted(Alt1, level=0), r, pch=16, ylab="Normalized residuals", xlab="Predicted values")

abline(h=0, lty=2)

#Tukey

a1 = emmeans(Alt1, ~ Sanidad*Factor*Nivel, type="response"); a1
altf <- cld(a1,Letters=letters)
tidy(altf)
plot(altf)


#Second-degree polynomial regression analysis
#Polynomial regression analysis ----------------
  
  #HLB infected plants
  #Macronutrients
  #Fit the data to a polynomial model.
fitQ = lm(Alt~poly(Factor,2,raw=TRUE), data=Poly_height_macros_sick) 
#ANOVA 
anova(fitQ)
summary(fitQ) #R2, R2adj and p

#micronutrients
fitQ2 = lm(Alt~poly(Factor,2,raw=TRUE), data=Poly_height_micros_sick) 
anova(fitQ2)
summary(fitQ2)

#Healthy plants 
#Macronutrients
fitQ3 = lm(Alt~poly(Factor,2,raw=TRUE), data=Poly_height_macros_Healthy) 
anova(fitQ3)
summary(fitQ3)
#Micronutrients
fitQ4 = lm(Alt~poly(Factor,2,raw=TRUE), data=Poly_height_micros_Healthy) 
anova(fitQ4)
summary(fitQ4)

#Plot polynomial regression of macronutrients sick vs healthy plants

Plot_height_macros_poly$Sanidad <- factor(Plot_height_macros_poly$Sanidad, levels = c("HLB+", "HLB-")) #Sort health status data

poly_macronutrients <-  ggplot(data = Plot_height_macros_poly, aes(x = Factor, y = Alt, fill=Sanidad)) + geom_point(aes(shape=Sanidad,stroke = 2),size=3)+geom_smooth(method = "lm", formula = y ~ poly(x,2),color="black")+ scale_fill_manual(values = c("black","gray80")) + scale_x_continuous(breaks=0:2, labels=c("Ma-1", "Ma-2", "Ma-3"))+ylab("Cumulative growth (cm)")+xlab("Macronutrients")+theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x = element_text(size=30,vjust=.7),axis.title.y = element_text(size=25),axis.title.x = element_text(size=25),legend.title = element_text(size=30),axis.text = element_text(size = 30),strip.text.x = element_text(size = 30))
poly_macronutrients+coord_cartesian(ylim = c(50, 200))+geom_smooth(method = "lm", se = FALSE)

#Plot polynomial regression of micronutrients sick vs healthy plants
Plot_height_micros_poly$Sanidad <- factor(Plot_height_micros_poly$Sanidad, levels = c("HLB+", "HLB-")) #Sort health status data

poly_micronutrients <-  ggplot(data = Plot_height_micros_poly, aes(x = Factor, y = Alt, fill=Sanidad)) + geom_point(aes(shape=Sanidad,stroke = 2),size=3)+geom_smooth(method = "lm", formula = y ~ poly(x,2),color="black")+ scale_fill_manual(values = c("black","gray80")) + scale_x_continuous(breaks=0:2, labels=c("Ma-1", "Ma-2", "Ma-3"))+ylab("Cumulative growth (cm)")+xlab("Micronutrients")+theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x = element_text(size=30,vjust=.7),axis.title.y = element_text(size=25),axis.title.x = element_text(size=25),legend.title = element_text(size=30),axis.text = element_text(size = 30),strip.text.x = element_text(size = 30))
poly_micronutrients+coord_cartesian(ylim = c(50, 200))+geom_smooth(method = "lm", se = FALSE)
