
#Load packages
library(ggplot2)
library(ggspectra)

#Spectrogram of the average of healthy vs. diseased plants
ggplot(AverageHvsS, aes(w.length,promedio)) +geom_line(aes(color = Sanidad),lwd=1,linetype=1)+ labs(x='Longitu de onda (nm)', y='Reflectancia (%)')+scale_colour_manual("",values = c("#a10707", "#38a33b"))+stat_wl_strip(ymin = -Inf, ymax = -0.025)+scale_fill_identity()+theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+theme(axis.text= element_text(size= 24),axis.title=element_text(size=24))+theme(legend.title = element_text(size=16), legend.text = element_text(size = 24))

#Guardar
ggsave(filename="ReflectpromS_E.jpg",width=20, height=15, units="cm")

#Graphs of averages by treatment
#540-560 nm

graf540_560$Sanidad <- factor(graf540_560$Sanidad, levels = c("HLB+", "HLB-")) #Sort health status data
ref550 <- ggplot(graf540_560, aes(w.length,prom)) +geom_line(aes(group=sub,color=Sanidad),lwd=1,linetype=1)+ labs(x='Longitu de onda (nm)', y='Reflectancia (%)')+scale_colour_manual("",values = c("#a10707", "#38a33b"))+stat_wl_strip(ymin = 5, ymax = 5.5)+scale_fill_identity()+theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x = element_text(size=14, angle=35,vjust=.7),axis.title.y = element_text(size=18),legend.title = element_text(size=20),axis.text = element_text(size = 20),strip.text.x = element_text(size = 20))+facet_grid(~Sanidad)
ref550+coord_cartesian(ylim = c(5,14))
ggsave(filename="R550.jpg",width=20, height=15, units="cm")

#780-800 nm
graf780_800$Sanidad <- factor(graf780_800$Sanidad, levels = c("HLB+", "HLB-")) #Sort health status data
ref790 <- ggplot(graf780_800, aes(w.length,prom)) +geom_line(aes(group=sub,color=Sanidad),lwd=1,linetype=1)+ labs(x='Longitu de onda (nm)', y='Reflectancia (%)')+scale_colour_manual("",values = c("#a10707","#38a33b"))+stat_wl_strip(ymin = 54, ymax = 55)+scale_fill_identity()+theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x = element_text(size=14, angle=35,vjust=.7),axis.title.y = element_text(size=18),legend.title = element_text(size=20),axis.text = element_text(size = 20),strip.text.x = element_text(size = 20))+facet_grid(~Sanidad)
ref790+coord_cartesian(ylim = c(54,70))
ggsave(filename="R790.jpg",width=20, height=15, units="cm")

#Graph for macro and micronutrient levels at sampling times at 550 nm
#summer
Reflec550_summer$Sanidad <- factor(Reflec550_summer$Sanidad, levels = c("HLB+", "HLB-")) #Sort health status data
Ref550summer <- ggplot(Reflec550_summer, aes(Factor, Prom,fill=Sanidad)) + geom_bar(stat="identity", position=position_dodge(width=0.9),colour = 'black',linewidth=1)+scale_fill_manual(values = c("black", "grey"))+geom_errorbar(aes(ymin =Prom, ymax = Prom+DE), width=0.4,linewidth=1, position=position_dodge(width=0.9))+ylab("Reflectance at 550 nm (%)")+xlab("Macronutrients")+theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x = element_text(size=12,vjust=.7),axis.title.y = element_text(size=18),legend.title = element_text(size=10),axis.text = element_text(size = 18),strip.text.x = element_text(size = 16))+facet_grid(. ~ Nivel)
Ref550summer+coord_cartesian(ylim = c(5, 17))
ggsave(filename="Reflect550summer.jpg",width=15, height=15, units="cm")

#winter
Reflec550_winter$Sanidad <- factor(Reflec550_winter$Sanidad, levels = c("HLB+", "HLB-")) #Sort health status data
Ref550winter <- ggplot(Reflec550_winter, aes(Factor, Prom,fill=Sanidad)) + geom_bar(stat="identity", position=position_dodge(width=0.9),colour = 'black',linewidth=1)+scale_fill_manual(values = c("black", "grey"))+geom_errorbar(aes(ymin =Prom, ymax = Prom+DE), width=0.4,linewidth=1, position=position_dodge(width=0.9))+ylab("Reflectance at 550 nm (%)")+xlab("Macronutrients")+theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x = element_text(size=12,vjust=.7),axis.title.y = element_text(size=18),legend.title = element_text(size=10),axis.text = element_text(size = 18),strip.text.x = element_text(size = 16))+facet_grid(. ~ Nivel)
Ref550winter+coord_cartesian(ylim = c(5, 17))
ggsave(filename="Reflect550winter.jpg",width=15, height=15, units="cm")

#Fall
Reflec550_fall$Sanidad <- factor(Reflec550_fall$Sanidad, levels = c("HLB+", "HLB-")) #Sort health status data
Ref550fall <- ggplot(Reflec550_fall, aes(Factor, Prom,fill=Sanidad)) + geom_bar(stat="identity", position=position_dodge(width=0.9),colour = 'black',linewidth=1)+scale_fill_manual(values = c("black", "grey"))+geom_errorbar(aes(ymin =Prom, ymax = Prom+DE), width=0.4,linewidth=1, position=position_dodge(width=0.9))+ylab("Reflectance at 550 nm (%)")+xlab("Macronutrients")+theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x = element_text(size=12,vjust=.7),axis.title.y = element_text(size=18),legend.title = element_text(size=10),axis.text = element_text(size = 18),strip.text.x = element_text(size = 16))+facet_grid(. ~ Nivel)
Ref550fall+coord_cartesian(ylim = c(5, 17))
ggsave(filename="Reflect550fall.jpg",width=15, height=15, units="cm")

#Graph for macro and micronutrient levels at sampling times at 790 nm

#summer
Reflec790_summer$Sanidad <- factor(Reflec790_summer$Sanidad, levels = c("HLB+", "HLB-")) #Sort health status data
Ref790summer <- ggplot(Reflec790_summer, aes(Factor, Prom,fill=Sanidad)) + geom_bar(stat="identity", position=position_dodge(width=0.9),colour = 'black',linewidth=1)+scale_fill_manual(values = c("black", "grey"))+geom_errorbar(aes(ymin =Prom, ymax = Prom+DE), width=0.4,linewidth=1, position=position_dodge(width=0.9))+ylab("Reflectance at 790 nm (%)")+xlab("Macronutrients")+theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x = element_text(size=12,vjust=.7),axis.title.y = element_text(size=18),legend.title = element_text(size=10),axis.text = element_text(size = 18),strip.text.x = element_text(size = 16))+facet_grid(. ~ Nivel)
Ref790summer+coord_cartesian(ylim = c(40, 75))
ggsave(filename="Reflect790summer.jpg",width=15, height=15, units="cm")

#winter
Reflec790_winter$Sanidad <- factor(Reflec790_winter$Sanidad, levels = c("HLB+", "HLB-")) #Sort health status data
Ref790winter <- ggplot(Reflec790_winter, aes(Factor, Prom,fill=Sanidad)) + geom_bar(stat="identity", position=position_dodge(width=0.9),colour = 'black',linewidth=1)+scale_fill_manual(values = c("black", "grey"))+geom_errorbar(aes(ymin =Prom, ymax = Prom+DE), width=0.4,linewidth=1, position=position_dodge(width=0.9))+ylab("Reflectance at 790 nm (%)")+xlab("Macronutrients")+theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x = element_text(size=12,vjust=.7),axis.title.y = element_text(size=18),legend.title = element_text(size=10),axis.text = element_text(size = 18),strip.text.x = element_text(size = 16))+facet_grid(. ~ Nivel)
Ref790winter+coord_cartesian(ylim = c(40, 75))
ggsave(filename="Reflect790winter.jpg",width=15, height=15, units="cm")

#fall
Reflec790_fall$Sanidad <- factor(Reflec790_fall$Sanidad, levels = c("HLB+", "HLB-")) #Sort health status data
Ref790fall <- ggplot(Reflec790_fall, aes(Factor, Prom,fill=Sanidad)) + geom_bar(stat="identity", position=position_dodge(width=0.9),colour = 'black',linewidth=1)+scale_fill_manual(values = c("black", "grey"))+geom_errorbar(aes(ymin =Prom, ymax = Prom+DE), width=0.4,linewidth=1, position=position_dodge(width=0.9))+ylab("Reflectance at 790 nm (%)")+xlab("Macronutrients")+theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x = element_text(size=12,vjust=.7),axis.title.y = element_text(size=18),legend.title = element_text(size=10),axis.text = element_text(size = 18),strip.text.x = element_text(size = 16))+facet_grid(. ~ Nivel)
Ref790fall+coord_cartesian(ylim = c(40, 75))

#Statistical analysis of repeated measurements at 550 nm, according to Brochu et al. (2023)
#For 550 nm
# Load packages
library(openxlsx)
library(lme4)
library(emmeans)
library(nlme)
library(lsmeans)
library(multcomp)
library(car)


setwd("C:/Users/Luis Alfredo/Documents/MEGAsync/Respaldo/Doctorado/TESIS/Capítulo II_Fertilización/Nuevo análisis/Reflect")

#Load the file and specify the variable types
Reflectancia550 <- within(read.xlsx(xlsxFile="Reflect550_Repeated measurements.xlsx"),{
  ID = as.factor(ID)
  Sanidad= as.factor(Sanidad) #E= HLB Infected, S= healthy plants
  Factor = as.factor(Factor)
  Nivel = as.factor(Nivel)
  Rep = as.factor(Rep) #Repetition
  Reptec=as.factor(Reptec) #Technical repetition
  MDI = as.factor(MDI) #1= summer, 6= winter, 14= Fall
  Tiempo = as.numeric(Tiempo) #1= summer, 6= winter, 14= Fall
  R550 = as.numeric(R550) #response
})

#Check file
str(Reflectancia550)

#Normality test
shapiro.test(Reflectancia550$R550)

#Check outliers
boxplot(Reflectancia550$R550, col="skyblue", frame.plot=F)

#box-cox test
boxcox = boxcox(R550 ~ Sanidad*Factor*Nivel*MDI, data=Reflectancia550,lambda = seq(-2, 2, length = 100))
#log transformation

##Correlation structures
fit.AR1 = lme(log(R550)~ Sanidad*Factor*Nivel*MDI, random = ~ 1 | Reptec/Rep / ID, correlation =  corCAR1(form=~ Tiempo | Reptec/Rep/ID), data=Reflectancia550)

fit.CS = lme(log(R550)~ Sanidad*Factor*Nivel*MDI, random = ~ 1 | Reptec/Rep / ID, correlation = corCompSymm(form=~ Tiempo | Reptec/Rep / ID), data=Reflectancia550)

fit.UN = lme(log(R550)~ Sanidad*Factor*Nivel*MDI, random = ~ 1 | Reptec/Rep / ID,correlation = corSymm(form=~ 1 | Reptec/Rep /ID), data=Reflectancia550)

#The Akaike information criterion (AIC)
AIC(fit.AR1, fit.CS, fit.UN)

#The best fit is fit.AR1
joint_tests(fit.AR1) #ANOVA
#Tukey
a1 = emmeans(fit.AR1, ~ Sanidad*Factor*Nivel| MDI , type="response"); a1
cld(a1, Letters=letters)

## Normality
EF = residuals(fit.AR1,type="normalized", level=0)
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

#Statistical analysis of repeated measurements at 790 nm, according to Brochu et al. (2023)

#Load the file and specify the variable types
Reflectance790 <- within(read.xlsx(xlsxFile="Reflect790_Repeated measurements.xlsx"),{
  ID = as.factor(ID)
  Sanidad= as.factor(Sanidad)
  Factor = as.factor(Factor)
  Nivel = as.factor(Nivel)
  Rep = as.factor(Rep) #Repetition
  Reptec=as.factor(Reptec) #Technical repetitio
  MDI = as.factor(MDI) #1= summer, 6= winter, 14= Fall
  Tiempo = as.numeric(Tiempo) #1= summer, 6= winter, 14= Fall
  R790 = as.numeric(R790) #response
})

#Check file
str(Reflectance790)

#normality test
shapiro.test(Reflectance790$R790)

#Check outliers
boxplot(Reflectance790$R790, col="skyblue", frame.plot=F)

#Box-Cox transformation
boxcox = boxcox(R790 ~ Sanidad*Factor*Nivel*MDI, data=Reflectance790,lambda = seq(-2, 2, length = 100))
#Transformation is not necessary


##Correlation structures
fit.AR1 = lme(R790~ Sanidad*Factor*Nivel*MDI, random = ~ 1 | Reptec/Rep / ID, correlation =  corCAR1(form=~ Tiempo | Reptec/Rep/ID), data=Reflectance790)

fit.CS = lme(R790~ Sanidad*Factor*Nivel*MDI, random = ~ 1 | Reptec/Rep / ID, correlation = corCompSymm(form=~ Tiempo | Reptec/Rep / ID), data=Reflectance790)

fit.UN = lme(R790~ Sanidad*Factor*Nivel*MDI, random = ~ 1 | Reptec/Rep / ID,correlation = corSymm(form=~ 1 | Reptec/Rep /ID), data=Reflectance790)

#The Akaike information criterion (AIC)
AIC(fit.AR1, fit.CS, fit.UN)
#ANOVA
joint_tests(fit.AR1)

#Tukey
a1 = emmeans(fit.AR1, ~ Sanidad*Factor*Nivel| MDI , type="response"); a1
cld(a1, Letters=letters)

# Normality
EF = residuals(fit.AR1,type="normalized", level=0)
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
