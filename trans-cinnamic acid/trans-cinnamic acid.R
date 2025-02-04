
#Graph
library(ggplot2)
Plot_TCA$Sanidad <- factor(Plot_TCA$Sanidad, levels = c("HLB+", "HLB-")) #Sort health status data
tca1 <- ggplot(Plot_TCA, aes(Factor, Prom,fill=Sanidad)) + geom_bar(stat="identity", position=position_dodge(width=0.9),colour = 'black',linewidth=1)+scale_fill_manual(values = c("black","grey"))+geom_errorbar(aes(ymin =Prom, ymax = Prom+DE), width=0.4,linewidth=1, position=position_dodge(width=0.9))+ylab(expression("t-Cinnamic acid (ng g"^-1* " FW)")) + 
  xlab("Macronutrient levels") + theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x = element_text(size=18,vjust=.7),axis.title.y = element_text(size=20),legend.title = element_text(size=30),axis.text = element_text(size = 20),strip.text.x = element_text(size = 30))+facet_grid(. ~ Nivel)
tca1

#Adjust Y axis

tca1+coord_cartesian(ylim = c(250, 2500))
#guardar en alta resolución

ggsave(filename="tca1.jpg",width=30, height=15, units="cm")

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
setwd("C:/Users/Luis Alfredo/Documents/MEGAsync/Respaldo/Doctorado/TESIS/Capítulo II_Fertilización/Nuevo análisis/Fitohormonas/t-CA")

#Load file
tca_final <- within(read.xlsx(xlsxFile="TCA_ANOVA_Tukey.xlsx"),{
  ID = as.factor(ID)
  Sanidad= as.factor(Sanidad)
  Factor = as.factor(Factor)
  Nivel = as.factor(Nivel)
  Rep = as.factor(Rep)
  tca= as.numeric(tca)
})

#Check file
str(tca_final)
#normality test
shapiro.test(tca_final$tca)
#Check outliers
boxplot(tca_final$tca, col="skyblue", frame.plot=F)

#Box-Cox transformation
boxcox = boxcox(tca~ Sanidad*Factor*Nivel, data=tca_final,lambda = seq(-2, 2, length = 100))
#Aunque la transformación box-cox indicó que las mejores transformaciones serían 1/sqrt(x) y 1/x en ningun caso hubo normalidad posttransformación. La mejor transformación fue log con lo cual hay normalidad. 
TCA1= lme(log(tca)~ Sanidad*Factor*Nivel, random = ~ 1 | Rep / ID, data=tca_final)

joint_tests(TCA1)

#Normalidad
r = residuals(TCA1,type="normalized", level=0)
hist(r,freq=F)
xfit<-seq(min(r),max(r),length=40)
yfit<-dnorm(xfit, mean=mean(r), sd=sd(r))
lines(xfit, yfit,col="red",lwd=2)
shapiro.test(r)
e1071::kurtosis(r) 
qqPlot(r)
#Homogeneidad de varianza
plot(fitted(TCA1, level=0), r, pch=16, ylab="Normalized residuals", xlab="Predicted values")

abline(h=0, lty=2)

#Tukey

a1 = emmeans(TCA1, ~ Sanidad*Factor*Nivel, type="response"); a1
tcaf <- cld(a1,Letters=letters)
tidy(tcaf)
plot(tcaf)

#Polynomial regression analysis ----------------

#HLB infected plants
#Macronutrients
#Fit the data to a polynomial model.
fitQ = lm(tca~poly(Factor,2,raw=TRUE), data=Poly_TCA_macros_sick) 
#ANOVA 
anova(fitQ)
summary(fitQ) #R2, R2adj and p

#micronutrients
fitQ2 = lm(tca~poly(Factor,2,raw=TRUE), data=Poly_TCA_micros_sick) 
anova(fitQ2)
summary(fitQ2)

#Healthy plants 
#Macronutrients
fitQ3 = lm(tca~poly(Factor,2,raw=TRUE), data=Poly_TCA_macros_Healthy) 
anova(fitQ3)
summary(fitQ3)
#Micronutrients
fitQ4 = lm(tca~poly(Factor,2,raw=TRUE), data=Poly_TCA_micros_Healthy) 
anova(fitQ4)
summary(fitQ4)

#Plot polynomial regression of macronutrients sick vs healthy plants
Plot_TCA_macros_poly$Sanidad <- factor(Plot_TCA_macros_poly$Sanidad, levels = c("HLB+", "HLB-")) #Sort health status data

poltcaMa <-  ggplot(data = Plot_TCA_macros_poly, aes(x = Factor, y = tca, fill=Sanidad)) + geom_point(aes(shape=Sanidad,stroke = 2),size=3)+geom_smooth(method = "lm", formula = y ~ poly(x,2),color="black")+ scale_fill_manual(values = c("black","gray80")) + scale_x_continuous(breaks=0:2, labels=c("Ma-1", "Ma-2", "Ma-3"))+ylab(expression("t-Cinnamic acid (ng g"^-1* " FW)")) + 
  xlab("Macronutrient")+theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x = element_text(size=30,vjust=.7),axis.title.y = element_text(size=25),axis.title.x = element_text(size=25),legend.title = element_text(size=30),axis.text = element_text(size = 30),strip.text.x = element_text(size = 30))
poltcaMa+geom_smooth(method = "lm", se = FALSE)+coord_cartesian(ylim = c(400,2800))

Plot_TCA_micros_poly$Sanidad <- factor(Plot_TCA_micros_poly$Sanidad, levels = c("HLB+", "HLB-")) #Sort health status data
poltcaMi <-  ggplot(data = Plot_TCA_micros_poly, aes(x = Factor, y = tca, fill=Sanidad)) + geom_point(aes(shape=Sanidad,stroke = 2),size=3)+geom_smooth(method = "lm", formula = y ~ poly(x,2),color="black")+ scale_fill_manual(values = c("black","gray80")) + scale_x_continuous(breaks=0:2, labels=c("Mi-1", "Mi-2", "Mi-3"))+ylab(expression("t-Cinnamic acid (ng g"^-1* " FW)")) + 
  xlab("Macronutrient")+theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x = element_text(size=30,vjust=.7),axis.title.y = element_text(size=25),axis.title.x = element_text(size=25),legend.title = element_text(size=30),axis.text = element_text(size = 30),strip.text.x = element_text(size = 30))
poltcaMi+geom_smooth(method = "lm", se = FALSE)+coord_cartesian(ylim = c(400,2800))
