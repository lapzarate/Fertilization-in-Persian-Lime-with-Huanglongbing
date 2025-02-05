
#Graph
library(ggplot2)
Plot_BA$Sanidad <- factor(Plot_BA$Sanidad, levels = c("HLB+", "HLB-")) #Sort health status data
BA1 <- ggplot(Plot_BA, aes(Factor, Prom,fill=Sanidad)) + geom_bar(stat="identity", position=position_dodge(width=0.9),colour = 'black',linewidth=1)+scale_fill_manual(values = c("black","grey"))+geom_errorbar(aes(ymin =Prom, ymax = Prom+DE), width=0.4,linewidth=1, position=position_dodge(width=0.9))+ylab(expression("Benzoic acid (ng g"^-1* " FW)")) + 
  xlab("Macronutrient levels")+ theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x = element_text(size=18,vjust=.7),axis.title.y = element_text(size=20),legend.title = element_text(size=30),axis.text = element_text(size = 20),strip.text.x = element_text(size = 30))+facet_grid(. ~ Nivel)
BA1

#Adjust Y axis

BA1+coord_cartesian(ylim = c(25000, 125000))
#guardar en alta resolución

ggsave(filename="BA1.jpg",width=30, height=15, units="cm")

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
setwd("C:/Users/Luis Alfredo/Documents/MEGAsync/Respaldo/Doctorado/TESIS/Capítulo II_Fertilización/Nuevo análisis/Fitohormonas/BA")

#Load file
BA_final <- within(read.xlsx(xlsxFile="BA_ANOVA_Tukey.xlsx"),{
  ID = as.factor(ID)
  Sanidad= as.factor(Sanidad)
  Factor = as.factor(Factor)
  Nivel = as.factor(Nivel)
  Rep = as.factor(Rep)
  BA= as.numeric(BA)
})

#Check file
str(BA_final)
#Normality
shapiro.test(BA_final$BA)
#Check outliers
boxplot(BA_final$BA, col="skyblue", frame.plot=F)

#Box-Cox transformation
boxcox = boxcox(BA~ Sanidad*Factor*Nivel, data=BA_final,lambda = seq(-2, 2, length = 100))

BA1= lme(sqrt(BA)~ Sanidad*Factor*Nivel, random = ~ 1 | Rep / ID, data=BA_final)

#ANOVA
joint_tests(BA1)

#Normalidad
r = residuals(BA1,type="normalized", level=0)
hist(r,freq=F)
xfit<-seq(min(r),max(r),length=40)
yfit<-dnorm(xfit, mean=mean(r), sd=sd(r))
lines(xfit, yfit,col="red",lwd=2)
shapiro.test(r)
e1071::kurtosis(r) 
qqPlot(r)
#Homogeneidad de varianza
plot(fitted(BA1, level=0), r, pch=16, ylab="Normalized residuals", xlab="Predicted values")

abline(h=0, lty=2)

#Tukey

a1 = emmeans(BA1, ~ Sanidad*Factor*Nivel, type="response"); a1
BAf <- cld(a1,Letters=letters)
tidy(BAf)
plot(BAf)

#Second-degree polynomial regression analysis

#Polynomial regression analysis ----------------

#HLB infected plants
#Macronutrients
#Fit the data to a polynomial model.
fitQ = lm(BA~poly(Factor,2,raw=TRUE), data=Poly_BA_macros_sick) 
#ANOVA 
anova(fitQ)
summary(fitQ) #R2, R2adj and p

#micronutrients
fitQ2 = lm(BA~poly(Factor,2,raw=TRUE), data=Poly_BA_micros_sick) 
anova(fitQ2)
summary(fitQ2)

#Healthy plants 
#Macronutrients
fitQ3 = lm(BA~poly(Factor,2,raw=TRUE), data=Poly_BA_macros_Healthy) 
anova(fitQ3)
summary(fitQ3)
#Micronutrients
fitQ4 = lm(BA~poly(Factor,2,raw=TRUE), data=Poly_BA_micros_healthy) 
anova(fitQ4)
summary(fitQ4)


#Plot polynomial regression of macronutrients sick vs healthy plants

library(ggplot2)
Plot_BA_macros_poly$Sanidad <- factor(Plot_BA_macros_poly$Sanidad, levels = c("HLB+", "HLB-")) #Sort health status data

polGlucMa <-  ggplot(data = Plot_BA_macros_poly, aes(x = Factor, y = BA, fill=Sanidad)) + geom_point(aes(shape=Sanidad,stroke = 2),size=3)+geom_smooth(method = "lm", formula = y ~ poly(x,2),color="black")+ scale_fill_manual(values = c("black","gray80")) + scale_x_continuous(breaks=0:2, labels=c("Ma-1", "Ma-2", "Ma-3"))+ylab(expression("Benzoic acid (ng g"^-1* " FW)")) + 
  xlab("Macronutrient")+theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x = element_text(size=30,vjust=.7),axis.title.y = element_text(size=25),axis.title.x = element_text(size=25),legend.title = element_text(size=30),axis.text = element_text(size = 30),strip.text.x = element_text(size = 30))
polGlucMa+geom_smooth(method = "lm", se = FALSE)+coord_cartesian(ylim = c(20000,125000))


#Plot polynomial regression of micronutrients sick vs healthy plants
Plot_BA_micros_poly$Sanidad <- factor(Plot_BA_micros_poly$Sanidad, levels = c("HLB+", "HLB-")) #Sort health status data

polGlucmMi <-  ggplot(data = Plot_BA_micros_poly, aes(x = Factor, y = BA, fill=Sanidad)) + geom_point(aes(shape=Sanidad,stroke = 2),size=3)+geom_smooth(method = "lm", formula = y ~ poly(x,2),color="black")+ scale_fill_manual(values = c("black","gray80")) + scale_x_continuous(breaks=0:2, labels=c("Ma-1", "Ma-2", "Ma-3"))+ylab(expression("Benzoic acid (ng g"^-1* " FW)")) + 
  xlab("Macronutrient")+theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x = element_text(size=30,vjust=.7),axis.title.y = element_text(size=25),axis.title.x = element_text(size=25),legend.title = element_text(size=30),axis.text = element_text(size = 30),strip.text.x = element_text(size = 30))
polGlucmMi+geom_smooth(method = "lm", se = FALSE)+coord_cartesian(ylim = c(20000,125000))
