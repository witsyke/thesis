library(ggplot2)
library(RColorBrewer)

colors = colorRampPalette(brewer.pal(12, "Paired"))(16)

umap_data <- read_csv("PATH TO UMAP OUTPUT")
ggplot(umap_data, aes(x=x, y=y, color=bundesland, shape=bundesland)) +
  geom_point() + 
  scale_shape_manual(values=seq(0,15)) +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=18,face="bold"),
        legend.key.size = unit(1, 'cm'),
        legend.text = element_text(size=16),
        legend.title = element_text(size=18)) 
