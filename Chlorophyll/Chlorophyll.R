#plot of the effect of CLas infection and fertilization treatments on the content of total chlorophyll
library(ggplot2)

ChloropGraf$Sanidad <- factor(ChloropGraf$Sanidad, levels = c("HLB+", "HLB-")) #Sort health status data
clor1 <- ggplot(ChloropGraf, aes(Factor, Prom, fill=Sanidad)) + 
  geom_bar(stat="identity", position=position_dodge(width=0.9), colour = 'black', linewidth=1) +
  scale_fill_manual(values = c("black", "grey")) +
  geom_errorbar(aes(ymin = Prom, ymax = Prom + DE), width=0.4, linewidth=1, position=position_dodge(width=0.9)) +
  ylab(expression("Total chlorophyll (mg g"^-1* " FW)")) + 
  xlab("Macronutrient levels") + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_text(size=18, vjust=.7), axis.title.y = element_text(size=20),
        legend.title = element_text(size=30), axis.text = element_text(size = 20),
        strip.text.x = element_text(size = 30)) +
  facet_grid(. ~ Nivel)

clor1
  
#Adjust Y axis

clor1+coord_cartesian(ylim = c(0.5, 2.7))
#save

ggsave(filename="clor1.jpg",width=30, height=15, units="cm")

#Statistical analysis according to Brochu et al. (2023)

#Paquetes
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

#ubicación del archivo
setwd("C:/Users/Luis Alfredo/Documents/MEGAsync/Respaldo/Doctorado/TESIS/Capítulo II_Fertilización/Special Issue Plants/Artículo_Plants/Repositorio/Chlorophyll")

#Cargar archivo
Clor_final <- within(read.xlsx(xlsxFile="Chlorop_ANOVA_Tukey.xlsx"),{
  ID = as.factor(ID)
  Sanidad= as.factor(Sanidad)
  Factor = as.factor(Factor)
  Nivel = as.factor(Nivel)
  Rep = as.factor(Rep) #Repetition
  Clor= as.numeric(Clor) #response
})

#Check file
str(Clor_final)
#normality test
shapiro.test(Clor_final$Clor)
#Check outliers
boxplot(Clor_final$Clor, col="skyblue", frame.plot=F)

#Box-Cox transformation
boxcox = boxcox(Clor~ Sanidad*Factor*Nivel, data=Clor_final,lambda = seq(-2, 2, length = 100))

clor1= lme(Clor~ Sanidad*Factor*Nivel, random = ~ 1 | Rep / ID, data=Clor_final)

#ANOVA
joint_tests(clor1)

# Normality
r = residuals(clor1,type="normalized", level=0)
hist(r,freq=F)
xfit<-seq(min(r),max(r),length=40)
yfit<-dnorm(xfit, mean=mean(r), sd=sd(r))
lines(xfit, yfit,col="red",lwd=2)
shapiro.test(r)
e1071::kurtosis(r) 
qqPlot(r)

# Equality of variances
plot(fitted(clor1, level=0), r, pch=16, ylab="Normalized residuals", xlab="Predicted values")

abline(h=0, lty=2)

#Tukey

a1 = emmeans(clor1, ~ Sanidad*Factor*Nivel, type="response"); a1
clor <- cld(a1,Letters=letters)
tidy(clor)
plot(clor)

#Second-degree polynomial regression analysis

setwd("C:/Users/Luis Alfredo/Documents/MEGAsync/Respaldo/Doctorado/TESIS/Capítulo II_Fertilización/Special Issue Plants/Artículo_Plants/Repositorio/Chlorophyll")

#Polynomial regression analysis ----------------

#HLB infected plants
#Macronutrients
#Fit the data to a polynomial model.
fitQ = lm(Clor~poly(Factor,2,raw=TRUE), data=Poly_MacrosSick) 
#ANOVA 
anova(fitQ)
summary(fitQ) #R2, R2adj and p

#micronutrients
fitQ2 = lm(Clor~poly(Factor,2,raw=TRUE), data=Poly_MicrosSick) 
anova(fitQ2)
summary(fitQ2)

#Healthy plants 
#Macronutrients
fitQ3 = lm(Clor~poly(Factor,2,raw=TRUE), data=Poly_MacrosHealthy) 
anova(fitQ3)
summary(fitQ3)
#Micronutrients
fitQ4 = lm(Clor~poly(Factor,2,raw=TRUE), data=Poly_MicrosHealthy) 
anova(fitQ4)
summary(fitQ4)

#Plot polynomial regression of macronutrients sick vs healthy plants

library(ggplot2)
Plot_macros_Poly$Sanidad <- factor(Plot_macros_Poly$Sanidad, levels = c("HLB+", "HLB-")) #Sort health status data

poly_macronutrients <-  ggplot(data = Plot_macros_Poly, aes(x = Factor, y = Clor, fill=Sanidad)) + geom_point(aes(shape=Sanidad,stroke = 2),size=3)+geom_smooth(method = "lm", formula = y ~ poly(x,2),color="black")+ scale_fill_manual(values = c("black","gray80")) + scale_x_continuous(breaks=0:2, labels=c("Ma-1", "Ma-2", "Ma-3"))+ylab(expression("Total chlorophyll (mg g"^-1* " FW)")) + 
  xlab("Macronutrient")+theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x = element_text(size=30,vjust=.7),axis.title.y = element_text(size=25),axis.title.x = element_text(size=25),legend.title = element_text(size=30),axis.text = element_text(size = 30),strip.text.x = element_text(size = 30))
poly_macronutrients+coord_cartesian(ylim = c(0.8,2.7))+geom_smooth(method = "lm", se = FALSE)

#Plot polynomial regression of micronutrients sick vs healthy plants
Plot_micros_Poly$Sanidad <- factor(Plot_micros_Poly$Sanidad, levels = c("HLB+", "HLB-")) #Sort health status data

poly_micronutrients <-  ggplot(data = Plot_micros_Poly, aes(x = Factor, y = Clor, fill=Sanidad)) + geom_point(aes(shape=Sanidad,stroke = 2),size=3)+geom_smooth(method = "lm", formula = y ~ poly(x,2),color="black")+ scale_fill_manual(values = c("black","gray80")) + scale_x_continuous(breaks=0:2, labels=c("Ma-1", "Ma-2", "Ma-3"))+ylab(expression("Total chlorophyll (mg g"^-1* " FW)")) + 
  xlab("Micronutrient")+theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x = element_text(size=30,vjust=.7),axis.title.y = element_text(size=25),axis.title.x = element_text(size=25),legend.title = element_text(size=30),axis.text = element_text(size = 30),strip.text.x = element_text(size = 30))
poly_micronutrients+coord_cartesian(ylim = c(0.8,2.7))+geom_smooth(method = "lm", se = FALSE)
