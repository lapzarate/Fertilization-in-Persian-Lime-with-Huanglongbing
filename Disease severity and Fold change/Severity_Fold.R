#Dual graph: Fold change and severity

library(ggplot2)

ggplot() + 
  geom_bar(
    data = fold_final, 
    aes(x = Macros, y = Fold, fill = Repetición), 
    stat = "identity", 
    position = position_dodge(width = 0.8), 
    width = 0.7,color = "black"
  ) + 
  geom_errorbar(
    data = fold_final, 
    aes(x = Macros, ymin = Fold - SD, ymax = Fold + SD, group = Repetición), 
    position = position_dodge(width = 0.8), 
    width = 0.25, 
    color = "black"
  ) + 
  geom_point(
    data = Severidad2, 
    aes(x = Macros, y = Severidad * escala), 
    color = "red", 
    size = 3, 
    position = position_dodge(width = 0.9)
  ) + 
  geom_errorbar(
    data = Severidad2, 
    aes(x = Macros, ymin = (Severidad - SD2) * escala, ymax = (Severidad + SD2) * escala), 
    width = 0.4, 
    linewidth = 1, 
    position = position_dodge(width = 0.9), 
    color = "red"
  ) + 
  geom_line(
    data = Severidad2, 
    aes(x = Macros, y = Severidad * escala, group = 1), 
    color = "red", 
    linetype = "solid", 
    linewidth = 1, 
    position = position_dodge(width = 0.9)
  ) + 
  scale_fill_manual(
    values = c("gDNA pool 1" = "black", "gDNA pool 2" = "gray")
  ) + 
  scale_y_continuous(
    name = "Fold Change", 
    sec.axis = sec_axis(~ . / escala, name = "Disease severity")
  ) + 
  theme_minimal() +  
  labs(
    x = "", 
    fill = "Repetición"
  ) + 
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    axis.text.x = element_text(size = 18, vjust = 0.7, angle = 30, hjust = 1), 
    axis.title.y = element_text(size = 20), 
    legend.title = element_text(size =15, color= "black"), 
    axis.text = element_text(size = 20),
    strip.text.x = element_text(size = 30),
    axis.text.y.right = element_text(color = "red"), 
    legend.text = element_text(color = "black"), panel.border = element_rect(color = "black", fill = NA, size = 1)
  ) + 
  facet_grid(. ~ Micros)


ggsave(filename="Fold+Severity.jpg",width=30, height=15, units="cm")

#Statistical analysis: ANOVA and Tukey test for the severity variable

# Load packages:
library(openxlsx)
library(lme4)
library(emmeans)
library(multcomp)
library(car)
library(MASS)
library(nlme)

#Define working directory path
setwd("C:/Users/Luis Alfredo/Documents/MEGAsync/Doctorado/TESIS/Capítulo II_Fertilización/Nuevo análisis/Severidad")

#Import database and define variable types

Sev_Final <- within(read.xlsx(xlsxFile="Severity_ANOVA_Tukey.xlsx"),{
  ID = as.factor(ID)
  Factor = as.factor(Factor)
  Nivel = as.factor(Nivel)
  Rep = as.factor(Rep)
  Sev= as.numeric(Sev)
})

#Check database
str(Sev_Final)

#Normality test with the loaded data
shapiro.test(Sev_Final$Sev)


#Check for outliers
boxplot(Sev_Final$Sev, col="skyblue", frame.plot=F)

#ANOVA
sev1= lme(Sev~ Factor*Nivel, random = ~ 1 | Rep / ID, data=Sev_Final)

joint_tests(sev1) 

# Normality
EF = residuals(sev1,type="normalized", level=0)
hist(EF,freq=F)
xfit<-seq(min(EF),max(EF),length=40)
yfit<-dnorm(xfit, mean=mean(EF), sd=sd(EF))
lines(xfit, yfit,col="red",lwd=2)

shapiro.test(EF)

qqPlot(EF) #Check residual
e1071::kurtosis(EF) #Check kurtosis

#Homogeneity of variance
plot(fitted(sev1, level=0), EF, pch=16, ylab="Normalized residuals", xlab="Predicted values")
abline(h=0, lty=2)

#Tukey
a1 = emmeans(sev1, ~ Factor*Nivel, type="response"); a1 
cld(a1, Letters=letters)
