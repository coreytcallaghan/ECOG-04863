#### Make a figure 1 for the paper

# packages
library(ggplot2)
library(dplyr)
library(patchwork)

### First want to make three fake figures for the three different scenarios
### adapter
adapter <- ggplot(data = data.frame(x = c(-3, 3)), aes(x))+
  stat_function(fun = dnorm, n = 101, args = list(mean = 0, sd = 1), color="blue", size=1.2)+ 
  ylab("")+
  xlab("")+
  theme_bw()+
  theme(axis.ticks=element_blank())+
  theme(axis.text=element_blank())+
  theme()+
  theme(panel.grid.major=element_blank())+
  theme(panel.grid.minor=element_blank())+
  ggtitle("Adapter")+
  theme(plot.title = element_text(size = 10))



avoider <- ggplot(data = data.frame(x = c(0, 20)), aes(x))+
  stat_function(fun = dexp, n = 101, args = list(rate=0.3), color="blue", size=1.2)+ 
  ylab("")+
  xlab("")+
  theme_bw()+
  theme(axis.ticks=element_blank())+
  theme(axis.text=element_blank())+
  theme()+
  theme(panel.grid.major=element_blank())+
  theme(panel.grid.minor=element_blank())+
  ggtitle("Avoider")+
  theme(plot.title = element_text(size = 10))



exploiter <- ggplot(data = data.frame(x = c(0, 4)), aes(x))+
  stat_function(fun = exp, n = 101, color="blue", size=1.2)+ 
  ylab("")+
  xlab("Urbanization level")+
  theme_bw()+
  theme(axis.ticks=element_blank())+
  theme(axis.text=element_blank())+
  theme()+
  theme(panel.grid.major=element_blank())+
  theme(panel.grid.minor=element_blank())+
  ggtitle("Exploiter")+
  theme(plot.title = element_text(size = 10))


## Now make three figures based on real data that show somewhat similar trends
## read in the data first
# read in urbanness scores
# taken from Callaghan et al. 2019
urban_scores <- read_csv("Data/urbanness_scores.csv")

# read in all eBird data
# used to calculate the urban scores
# and subset to only the species potentially included
# in this analysis (the possible 94 species)
load('Data/species_urban.RData')

species_urban <- species_urban %>%
  dplyr::filter(COMMON_NAME %in% unique(urban_scores$Species))

ggplot(species_urban, aes(avg_rad))+
  geom_density(alpha=0.4, fill="red")+
  scale_x_log10()+
  facet_wrap(~COMMON_NAME)

example_species <- species_urban %>%
  dplyr::filter(COMMON_NAME %in% c("Rainbow Lorikeet", "Eastern Spinebill", "Wonga Pigeon"))

Rainbow_Lorikeet <- species_urban %>%
  dplyr::filter(COMMON_NAME == "Rainbow Lorikeet") %>%
  ggplot(., aes(avg_rad))+
  geom_density(alpha=0.4, fill="green")+
  scale_x_log10()+
  ylab("")+
  xlab("Urbanization (night-time lights)")+
  theme_bw()+
  theme(axis.ticks=element_blank())+
  theme(axis.text=element_blank())+
  theme()+
  theme(panel.grid.major=element_blank())+
  theme(panel.grid.minor=element_blank())+
  ggtitle("Rainbow Lorikeet")+
  theme(plot.title = element_text(size = 10))

Eastern_Spinebill <- species_urban %>%
  dplyr::filter(COMMON_NAME == "Eastern Spinebill") %>%
  ggplot(., aes(avg_rad))+
  geom_density(alpha=0.4, fill="green")+
  scale_x_log10()+
  ylab("")+
  xlab("")+
  theme_bw()+
  theme(axis.ticks=element_blank())+
  theme(axis.text=element_blank())+
  theme()+
  theme(panel.grid.major=element_blank())+
  theme(panel.grid.minor=element_blank())+
  ggtitle("Eastern Spinebill")+
  theme(plot.title = element_text(size = 10))

Wonga_Pigeon <- species_urban %>%
  dplyr::filter(COMMON_NAME == "Wonga Pigeon") %>%
  ggplot(., aes(avg_rad))+
  geom_density(alpha=0.4, fill="green")+
  scale_x_log10()+
  ylab("")+
  xlab("")+
  theme_bw()+
  theme(axis.ticks=element_blank())+
  theme(axis.text=element_blank())+
  theme()+
  theme(panel.grid.major=element_blank())+
  theme(panel.grid.minor=element_blank())+
  ggtitle("Wonga Pigeon")+
  theme(plot.title = element_text(size = 10))

avoider + Wonga_Pigeon + adapter + Eastern_Spinebill + exploiter + Rainbow_Lorikeet + plot_layout(ncol=2)

ggsave(filename="Figures/Figure_1.pdf")
ggsave(filename="Figures/Figure_1.png")
