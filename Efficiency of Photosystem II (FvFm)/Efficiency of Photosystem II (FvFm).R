#Photosynthetic efficiency graphs
#Summer
EFsummer <- ggplot(EfFot_summer, aes(Factor, Prom,fill=Sanidad)) + geom_bar(stat="identity", position=position_dodge(width=0.9),colour = 'black',linewidth=1)+scale_fill_manual(values = c("black", "grey"))+geom_errorbar(aes(ymin =Prom, ymax = Prom+DE), width=0.4,linewidth=1, position=position_dodge(width=0.9))+ylab("Fv/Fm")+xlab("")+theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x = element_text(size=15,vjust=.7),axis.title.y = element_text(size=18),legend.title = element_text(size=10),axis.text = element_text(size = 18),strip.text.x = element_text(size = 20))+facet_grid(. ~ Nivel)

EFsummer+coord_cartesian(ylim = c(0.7, 0.90))
ggsave(filename="Eficsummer.jpg",width=15, height=15, units="cm")

#winter
EFwinter <- ggplot(EfFot_winter, aes(Factor, Prom,fill=Sanidad)) + geom_bar(stat="identity", position=position_dodge(width=0.9),colour = 'black',linewidth=1)+scale_fill_manual(values = c("black", "grey"))+geom_errorbar(aes(ymin =Prom, ymax = Prom+DE), width=0.4,linewidth=1, position=position_dodge(width=0.9))+ylab("Fv/Fm")+xlab("")+theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x = element_text(size=15,vjust=.7),axis.title.y = element_text(size=18),legend.title = element_text(size=10),axis.text = element_text(size = 18),strip.text.x = element_text(size = 20))+facet_grid(. ~ Nivel)
EFwinter+coord_cartesian(ylim = c(0.7, 0.90))

#Fall
EFFallr <- ggplot(EfFot_fall, aes(Factor, Prom,fill=Sanidad)) + geom_bar(stat="identity", position=position_dodge(width=0.9),colour = 'black',linewidth=1)+scale_fill_manual(values = c("black", "grey"))+geom_errorbar(aes(ymin =Prom, ymax = Prom+DE), width=0.4,linewidth=1, position=position_dodge(width=0.9))+ylab("Fv/Fm")+xlab("")+theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x = element_text(size=15,vjust=.7),axis.title.y = element_text(size=18),legend.title = element_text(size=10),axis.text = element_text(size = 18),strip.text.x = element_text(size = 20))+facet_grid(. ~ Nivel)
EFFallr+coord_cartesian(ylim = c(0.7, 0.90))

#Statistical analisis for FV/FM (Repeated measurements ) acording to Brochu et al. (2023)
#clear
rm(list=ls())
#load packages
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

setwd("C:/Users/Luis Alfredo/Documents/MEGAsync/Respaldo/Doctorado/TESIS/Capítulo II_Fertilización/Nuevo análisis/EficienciaFoto")

#Load File
Efic_final <- within(read.xlsx(xlsxFile="FV_FM_ANOVA_Tukey.xlsx"),{
  ID = as.factor(ID)
  Sanidad= as.factor(Sanidad)
  Factor = as.factor(Factor) 
  Nivel = as.factor(Nivel)  
  Rep = as.factor(Rep) #Repetition
  Reptec=as.factor(Reptec)  #Technical repetition
  MDI = as.factor(MDI) #1= summer, 6= winter, 14= Fall
  Tiempo = as.numeric(Tiempo) #1= summer, 6= winter, 14= Fall
  Efic = as.numeric(Efic) #response
})

#Check File
str(Efic_final)

#Normality Test
shapiro.test(Efic_final$Efic)

#Check outliers
boxplot(Efic_final$Efic, col="skyblue", frame.plot=F)

#Check Correlation structures

fit.AR1 = lme(Efic~ Sanidad*Factor*Nivel*MDI, random = ~ 1 | Reptec/Rep / ID, correlation =  corCAR1(form=~ Tiempo | Reptec/Rep/ID), data=Efic_final)

fit.CS = lme(Efic~ Sanidad*Factor*Nivel*MDI, random = ~ 1 | Reptec/Rep / ID, correlation = corCompSymm(form=~ Tiempo | Reptec/Rep / ID), data=Efic_final)

fit.UN = lme(Efic~ Sanidad*Factor*Nivel*MDI, random = ~ 1 | Reptec/Rep / ID,correlation = corSymm(form=~ 1 | Reptec/Rep /ID), data=Efic_final)

#Check AIC criterion
AIC(fit.AR1, fit.CS, fit.UN)

joint_tests(fit.UN)


a1 = emmeans(fit.UN, ~ Sanidad*Factor*Nivel| MDI , type="response"); a1
cld(a1, Letters=letters)
# Normality
EF = residuals(fit.UN,type="normalized", level=0)
hist(EF,freq=F)
xfit<-seq(min(EF),max(EF),length=40)
yfit<-dnorm(xfit, mean=mean(EF), sd=sd(EF))
lines(xfit, yfit,col="red",lwd=2)

shapiro.test(EF)
qqPlot(EF)
e1071::kurtosis(EF)
# Equality of variances
plot(fitted(fit.AR1, level=0), EF, pch=16, ylab="Normalized residuals", xlab="Predicted values")
abline(h=0, lty=2)

