#PCA

library(ggplot2)
library(ggfortify)
pca_res <- prcomp(Datos_PCA, scale. = TRUE)

autoplot(pca_res)

autoplot(pca_res, data = DATA_PCA, colour = 'Sanidad')

p <- autoplot(pca_res, data = DATA_PCA, colour = 'Sanidad',
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.colour='blue',loadings.label.size = 5)+scale_color_manual(values = c("black","red"))+theme(aspect.ratio = 1)+theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x = element_text(size=20),axis.title.y = element_text(size=20),legend.title = element_text(size=20),axis.text = element_text(size = 20),strip.text.x = element_text(size = 20))
p+ geom_point(aes(colour = factor(Sanidad), size=3))

ggsave(filename="PCA_congreso.jpg",width=30, height=15, units="cm")

