library(here)
library(tidyverse)

setwd(file.path("/Users/aymeric.hermann/GitHub/sem-eds-data-processing"))

getwd()


# load data
d <- read.csv("2025-06-12_MEB-EMAE/data/combined_output.csv", 
              header=TRUE, sep=",", stringsAsFactors=FALSE)
d_sum <- read.csv("2025-06-12_MEB-EMAE/data/combined_output_conv.csv", 
                header=TRUE, sep=",", stringsAsFactors=FALSE)

# 1- visualize raw data ----
my_theme <- theme_classic() + 
  theme(axis.line=element_blank()) +
  theme(plot.title = element_blank(),
        axis.title = element_text(size = 11), axis.text = element_text(size = 11),
        panel.border = element_rect(colour="black", fill = NA, linewidth = 1),
        legend.position="right"#, aspect.ratio=1
  )
library(viridis)

### check for potential outliers
# Compute variability (mean, sd, range) for each duplicated ID
var <- d %>%
  group_by(id) %>%
  summarise(
    mean_O = mean(O), sd_O = sd(O), range_O = max(O) - min(O),
    mean_Si = mean(Si), sd_Si = sd(Si), range_Si = max(Si) - min(Si),
    .groups = "drop"
  )
var %>% print(n=23)

# vizualise
var_O <- ggplot(d, aes(x = id, y = O, fill = id)) +
  geom_boxplot(outlier.shape = NA) +  # hide default outliers to avoid duplication
  geom_jitter(aes(color = id), width = 0.2, size = 1.5, alpha = 0.7) +  # adds points
  scale_fill_viridis_d(option = "D") +
  scale_color_viridis_d(option = "D") +  # match point color to fill
  labs(title = "O Variability", y = "O", x = "id") + my_theme + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5)) +
  theme(legend.position = "none")
var_O

var_Si <- ggplot(d, aes(x = id, y = Si, fill = id)) +
  geom_boxplot(outlier.shape = NA) +  # hide default outliers to avoid duplication
  geom_jitter(aes(color = id), width = 0.2, size = 1.5, alpha = 0.7) +  # adds points
  scale_fill_viridis_d(option = "D") +
  scale_color_viridis_d(option = "D") +  # match point color to fill
  labs(title = "Si Variability", y = "Si", x = "id") + my_theme + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5)) +
  theme(legend.position = "none")
var_Si

var_K <- ggplot(d, aes(x = id, y = K, fill = id)) +
  geom_boxplot(outlier.shape = NA) +  # hide default outliers to avoid duplication
  geom_jitter(aes(color = id), width = 0.2, size = 1.5, alpha = 0.7) +  # adds points
  scale_fill_viridis_d(option = "D") +
  scale_color_viridis_d(option = "D") +  # match point color to fill
  labs(title = "K Variability", y = "K", x = "id") + my_theme + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5)) +
  theme(legend.position = "none")
var_K

ggplot(data = d, aes(x = id, y = O/Si, group = id, color = id)) + 
  geom_point(size = 2) + scale_color_viridis_d(option = "D") + 
  my_theme + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5),
        legend.position = "none")

ggplot(data = var, aes(x = sd_O, y = sd_Si, group = id, color = id)) + 
  geom_point(size = 2) +
  scale_color_viridis_d(option = "D") +  # Use color scale, not fill
  labs(title = "O-Si variability correlation", y = "sd_Si", x = "sd_O") +
  geom_abline(size = .5, linetype = "dashed", col = "black") +
  scale_x_continuous(limits=c(0, 3.5)) + 
  scale_y_continuous(limits=c(0, 3.5)) +
  my_theme + theme(aspect.ratio=1, legend.position = "none")

#save
#setwd("~/GitHub/sem-eds-data-processing/2025-05-06_MEB-EMAE")
setwd("~/GitHub/sem-eds-data-processing/2025-06-12_MEB-EMAE")

require(patchwork)

pdf(("fig/var_raw.pdf"), width=8, height=12)
var_O /
  var_Si /
  var_K
dev.off()


# 2- visualize converted data ----
my_theme <- theme_classic() + 
  theme(axis.line=element_blank()) +
  theme(plot.title = element_blank(),
        axis.title = element_text(size = 11), axis.text = element_text(size = 11),
        panel.border = element_rect(colour="black", fill = NA, linewidth = 1),
        legend.position="right"#, aspect.ratio=1
  )
library(viridis)

## K2O_SiO2
df = data.frame(x = c(45,78), y = c(.3,6.3))
K2O_SiO2_bg <- ggplot(data=df, mapping=aes(x=x, y=y)) + geom_blank() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_y_continuous(limits=c(.3,6.3), breaks = c(1,2,3,4,5,6)) +
  scale_x_continuous(limits=c(45,78), breaks = c(45,52,56,63,70,78)) +
  labs(x=expression(SiO[2]*~ "(wt%)"), y=expression(K[2]*O*~ "(wt%)"))+
  annotate("segment", x=52, xend=52, y=.3, yend=6, linewidth=0.2, linetype="dashed", colour="gray70")+
  annotate("segment", x=56, xend=56, y=.3, yend=6, linewidth=0.2, linetype="dashed", colour="gray70")+
  annotate("segment", x=63, xend=63, y=.3, yend=6, linewidth=0.2, linetype="dashed", colour="gray70")+
  annotate("segment", x=70, xend=70, y=.3, yend=6, linewidth=0.2, linetype="dashed", colour="gray70")+
  annotate("segment", x=48, xend=52, y=0.3, yend=0.5, linewidth=0.2, colour="gray70")+
  annotate("segment", x=52, xend=56, y=0.5, yend=0.7, linewidth=0.2, colour="gray70")+
  annotate("segment", x=56, xend=63, y=0.7, yend=1, linewidth=0.2, colour="gray70")+
  annotate("segment", x=63, xend=70, y=1, yend=1.3, linewidth=0.2, colour="gray70")+
  annotate("segment", x=70, xend=78, y=1.3, yend=1.6, linewidth=0.2, colour="gray70")+
  annotate("segment", x=48, xend=52, y=1.2, yend=1.5, linewidth=0.2, colour="gray70")+
  annotate("segment", x=52, xend=56, y=1.5, yend=1.8, linewidth=0.2, colour="gray70")+
  annotate("segment", x=56, xend=63, y=1.8, yend=2.4, linewidth=0.2, colour="gray70")+
  annotate("segment", x=63, xend=70, y=2.4, yend=3, linewidth=0.2, colour="gray70")+
  annotate("segment", x=70, xend=78, y=3, yend=3.7, linewidth=0.2, colour="gray70")+
  annotate("segment", x=48, xend=52, y=1.7, yend=2.4, linewidth=0.2, colour="gray70")+
  annotate("segment", x=52, xend=56, y=2.4, yend=3.2, linewidth=0.2, colour="gray70")+
  annotate("segment", x=56, xend=63, y=3.2, yend=4, linewidth=0.2, colour="gray70")+
  annotate("segment", x=45, xend=48, y=1.4, yend=1.7, linewidth=0.2, colour="gray70")+
  annotate("segment", x=45, xend=48, y=0.9, yend=1.2, linewidth=0.2, colour="gray70")+
  annotate("segment", x=45, xend=48, y=0.2, yend=0.3, linewidth=0.2, colour="gray70")+
  annotate("text", label="Basalt", x=47.5, y=6.2, size=2.5, colour="black")+
  annotate("text", label="Basaltic\n andesite", x=54, y=6.2, size=2.5, colour="black")+
  annotate("text", label="Andesite", x=59.5, y=6.2, size=2.5, colour="black")+
  annotate("text", label="Dacite", x=66.5, y=6.2, size=2.5, colour="black")+
  annotate("text", label="Rhyolite", x=74, y=6.2, size=2.5, colour="black")+
  annotate("text", label="Alkaline series", x=53, y=4, size=2.5, colour="gray50", angle = 40)+
  annotate("text", label="High-K calc-alkaline series", x=59.5, y=2.8, size=2.5, colour="gray50", angle = 30)+
  annotate("text", label="Calc-alkaline series", x=63, y=1.7, size=2.5, colour="gray50", angle = 17)+
  annotate("text", label="Low-K series", x=65, y=0.6, size=2.5, colour="gray50", angle = 4)
#plot data
K2O_SiO2 <- K2O_SiO2_bg +
  geom_point(data = d_sum, 
             aes(x = SiO2, y = K2O, group = id, color = id),
             size = 2) +
  scale_color_viridis_d(option = "D") +  # Use color scale, not fill
  my_theme
K2O_SiO2

#save
#setwd("~/GitHub/sem-eds-data-processing/2025-05-06_MEB-EMAE")
setwd("~/GitHub/sem-eds-data-processing/2025-06-12_MEB-EMAE")

require(patchwork)
#save
pdf(("fig/K2O_SiO2.pdf"), width=8, height=4)
K2O_SiO2
dev.off()
