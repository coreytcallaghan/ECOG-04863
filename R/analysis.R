### An analysis script
### The purpose is to test whether or not
### continental-scale urbanness predicts local-scale response to urbanization
### This relies on broad-scale urbanness measures
### and local-scale surveys in small-cities



# packages
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(purrr)
library(broom)
library(lme4)
library(patchwork)
library(outliers)
library(scales)
library(lubridate)

# read in data
# blue mountains data
data <- read_csv("Data/survey_data.csv")

# read in urbanization level data
urban_data <- read_csv("Data/Point_ID-Data.csv")

# read in exotic status
exotic_status <- read_csv("Data/exotic_status.csv")

# read in urbanness scores
# taken from Callaghan et al. 2019
urban_scores <- read_csv("Data/urbanness_scores.csv")


# make a plot of the histograms of the distances
# of the species detections (<250m)
# for the reviewer
data %>%
  dplyr::filter(Distance.m<250) %>%
  ggplot(., aes(x=Distance.m))+
  geom_histogram(bins=50, fill="blue", color="black")+
  theme_classic()+
  theme(axis.text=element_text(color="black"))+
  xlab("Distance of detection")+
  ylab("Count")

#ggsave("distance_detection_histogram.png")

# read in all eBird data
# used to calculate the urban scores
# and subset to only the species potentially included
# in this analysis (the possible 94 species)
load('Data/species_urban.RData')

species_urban <- species_urban %>%
  dplyr::filter(COMMON_NAME %in% unique(urban_scores$Species))

species_obs_continental <- species_urban %>%
  group_by(COMMON_NAME) %>%
  summarise(N_continental=n()) %>%
  rename(Species=COMMON_NAME)

# histogram of the number of continental observations
ggplot(species_obs_continental, aes(x=N_continental))+
  geom_histogram(bins=50, fill="blue", color="black")+
  theme_classic()+
  theme(axis.text=element_text(color="black"))+
  scale_x_log10(label=comma)+
  xlab("Number of continental observations")+
  ylab("Count")

#ggsave("number_continental_obs_histogram.png")


# make a plot showing the amount of data against year
species_urban %>%
  mutate(year=year(OBSERVATION_DATE)) %>%
  group_by(year) %>%
  summarise(Number_obs=n()) %>%
  ggplot(., aes(x=year, y=Number_obs))+
  geom_col()+
  theme_classic()+
  theme(axis.text=element_text(color="black"))+
  xlab("Year")+
  ylab("Number of continental observations")


#ggsave("data_per_year.png")

# first look at the number of observations for those species
# at the local scale
# lots of species observed only once, etc. Potentially a major problem
species_obs <- data %>% 
  group_by(Species) %>%
  summarise(N_local=n()) %>%
  right_join(., species_obs_continental, by="Species")

# first an example of how to calculate 
# local-level response to urbanization
# just for a single species to start out with...
species_example <- data %>%
  replace_na(list(Flyover="in_count")) %>%
  dplyr::filter(Flyover == "in_count") %>%
  dplyr::filter(Distance.m < 250) %>%
  dplyr::select(Date, Point, Transect, Observer, Point_ID, Species) %>%
  group_by(Date, Observer, Species, Transect, Point_ID, Point) %>%
  distinct(.) %>%
  inner_join(., urban_data, by="Point_ID") %>%
  group_by(Point, Transect, Species) %>%
  summarise(N=n(),
            urbanness_imperv=mean(Percent_impervious)) %>%
  replace_na(list(Species="none")) %>%
  dplyr::filter(Species != "none") %>%
  spread(Species, N, fill=0) %>%
  gather(., key="Species", value="N", 4:98) %>%
  dplyr::filter(Species=="Eastern Spinebill")

hist(species_example$N)


# Use a glmm to model these data
# for each species
test_model <- lme4::glmer(N ~ urbanness_imperv + (1|Transect), 
                          data=species_example, family=poisson(link="log"))

summary(test_model)

## try a negative binomial model instead
test_model_nb <- lme4::glmer(N ~ urbanness_imperv + (1|Transect), 
                          data=species_example, family=MASS::negative.binomial(theta=5))

summary(test_model_nb)

# now we need to extract this 'local-level'
# response to urbanization for each species
# so write a function and apply it to each species
# will use a nest model unnest approach here
# Apply a function to each species
# nest the data by species
species_nested <- data %>%
  replace_na(list(Flyover="in_count")) %>%
  dplyr::filter(Flyover == "in_count") %>%
  dplyr::filter(Distance.m < 250) %>%
  dplyr::select(Date, Point, Transect, Observer, Point_ID, Species) %>%
  group_by(Date, Observer, Species, Transect, Point_ID, Point) %>%
  distinct(.) %>%
  inner_join(., urban_data, by="Point_ID") %>%
  group_by(Point, Transect, Species) %>%
  summarise(N=n(),
            urbanness_imperv=mean(Percent_impervious)) %>%
  replace_na(list(Species="none")) %>%
  dplyr::filter(Species != "none") %>%
  spread(Species, N, fill=0) %>%
  gather(., key="Species", value="N", 4:98) %>%
  group_by(Species) %>%
  dplyr::filter(Species %in% filter(species_obs, N_local > 10)$Species) %>%
  nest()

# function to apply to each species
apply_glm <- function(df){
  lme4::glmer(N ~ urbanness_imperv + (1|Transect), 
              data=df, family=poisson(link="log"))      
}

# apply the function to each nested df
nested_models <- species_nested %>%
  mutate(glm = map(.f=apply_glm, .x=data))

# tidy the results
# and also calculate deviance for each model
nested_tidy <- nested_models %>%
  mutate(tidy = map(.f=tidy, .x=glm)) %>%
  mutate(deviance = map(.f=deviance, .x=glm))

# unnest the results and clean the dataframe up
# add a few things of potential interest as well
# also join in the urban scores and
# total species' observations to the dataframe
unnested_models <- nested_tidy %>%
  unnest(tidy) %>%
  #inner_join(., select(nested_tidy, Species, deviance) %>% unnest(deviance), by="Species") %>%
  dplyr::filter(term == "urbanness_imperv") %>%
  mutate(sign=ifelse(estimate > 0, 1, 0)) %>%
  mutate(significant=ifelse(p.value < 0.05, "significant", "not-significant")) %>%
  left_join(., urban_scores, by="Species") %>%
  left_join(., species_obs, by="Species")

# histogram of urban scores
ggplot(urban_scores, aes(x=log(urban_score)))+
  geom_histogram(fill="black", color="white")+
  theme_classic()+
  xlab("Urban scores (log)")+
  ylab("Count")+
  theme(axis.text=element_text(size=12, color="black"))+
  theme(axis.title=element_text(size=15, color="black"))+
  ggtitle(paste0("Number of species:", length(unique(urban_scores$Species))))

#############################################################
######## Now I want to test the relationship between ########
#### continental and local scale urbanization responses #####
#############################################################

# first try a linear model
ggplot(unnested_models, aes(y=estimate, x=log(urban_score)))+
  geom_point()+
  geom_smooth(method="lm")+
  theme_classic()+
  xlab("Continental-scale urbanness")+
  ylab("Local-scale urbanness")+
  theme(axis.text=element_text(size=12, color="black"))+
  theme(axis.title=element_text(size=15, color="black"))


## there appear to be two 'outliers' which were probably not fit well enough
# looks like a couple of outliers!!
ggplot(unnested_models, aes(x=std.error))+
  geom_histogram()

# remove this outlier manually
unnested_models$outlier <- scores(unnested_models$std.error, type="z", prob=0.95)

unnested_models <- unnested_models %>%
  dplyr::filter(outlier == "FALSE")

# look again
ggplot(unnested_models, aes(x=std.error))+
  geom_histogram()
# still looks like another outlier!

# repeat procedure above
unnested_models$outlier <- scores(unnested_models$std.error, type="z", prob=0.95)

unnested_models <- unnested_models %>%
  dplyr::filter(outlier == "FALSE")

# look again at histogram
# looks heaps better
ggplot(unnested_models, aes(x=std.error))+
  geom_histogram()

ggplot(unnested_models, aes(x=estimate, y=log(urban_score)))+
  geom_point()+
  geom_smooth(method="lm")+
  theme_classic()+
  ylab("Continental-scale urbanness")+
  xlab("Local-scale urbanness")+
  theme(axis.text=element_text(size=12, color="black"))+
  theme(axis.title=element_text(size=15, color="black"))+
  theme(panel.border=element_rect(fill=NA))

#write_csv(unnested_models, "table_S2.csv")

# linear model of that plot for results
mod <- lm(estimate ~ log(urban_score), data=unnested_models)

par(mfrow = c(2, 2))
plot(mod)

summary(mod)

mod_w_weights <- lm(estimate ~ log(urban_score), weights=std.error, data=unnested_models)

par(mfrow = c(2, 2))
plot(mod_w_weights)

summary(mod_w_weights)

ggplot(unnested_models, aes(x=estimate, y=log(urban_score)))+
  geom_point()+
  geom_errorbarh(aes(xmin=estimate-std.error, xmax=estimate+std.error))+
  geom_smooth(method="lm", se=FALSE)+
  theme_classic()+
  ylab("Continental-scale urbanness")+
  xlab("Local-scale urbanness")+
  theme(axis.text=element_text(size=12, color="black"))+
  theme(axis.title=element_text(size=15, color="black"))+
  theme(panel.border=element_rect(fill=NA))+
  geom_label(x=-0.05, y=3, label=paste0("r2 without weights = ", format(summary(mod)$r.squared, digits = 3)))+
  geom_label(x=-0.05, y=2.5, label=paste0("r2 with weights = ", format(summary(mod_w_weights)$r.squared, digits = 3)))

# before making plot final, it would be good to capture standard error
# in how the continental scale scores are calculated
# here I use some package to calculate the median for each with standard error
library(boot)
extract_se_median <- function(species) {
  
  df <- species_urban %>%
    dplyr::filter(COMMON_NAME == species)
  
  Mboot <- boot(df$avg_rad, function(x, i) median(x[i]), R=100)
  
  boot.ci(Mboot,
          conf=0.95,
          type=c("norm"))
  
  
  #original:
  Mboot$t0
  #bias:
  mean(Mboot$t)-Mboot$t0
  #se: 
  se <- data.frame(se=sd(Mboot$t),
                   Species=paste0(species))
  
  return(se)
}

# I'll do this only for the 49 species included in final model
list_of_species <- unique(unnested_models$Species)

# run the resample median function for
# each of the 49 species
results_list_se <- lapply(list_of_species, function(X) {extract_se_median(X)})

results_df_se <- bind_rows(results_list_se) %>%
  left_join(., select(unnested_models, Species, estimate, urban_score), by="Species")

# plot of sample size vs se
# doesn't show too much, surprisingly, but could include it in 
# supplementary information
results_df_se %>%
  left_join(., species_obs_continental, by="Species") %>%
  ggplot(., aes(x=log(N_continental), y=se))+
  geom_point()



### Maybe try sampling 10 observations for each of the 49 species
### and then refitting the model
### and so on for a number of observations for each species (10:1000) by 10
### to estimate how many observations of a species (on average)
### we might need to get a reliabile predicton of how the birds 
### respond at a local scale
resample_median_function <- function(X) {
  
  df <- species_urban %>%
    dplyr::filter(COMMON_NAME %in% list_of_species) %>%
    group_by(COMMON_NAME) %>%
    sample_n(X) %>%
    summarise(urban_score=median(avg_rad)) %>%
    ungroup() %>%
    rename(Species=COMMON_NAME) %>%
    left_join(., select(unnested_models, Species, estimate, std.error)) %>%
    mutate(sample_size=X)
  
  return(df)
}


sampling_vector <- seq.int(10, 1000, by=10)


list_of_results_by_sample_size <- lapply(sampling_vector, function(X) {resample_median_function(X)})

df_of_results_by_sample_size <- bind_rows(list_of_results_by_sample_size) 

# plot each one of the models using different sample sizes for each of the
# continental scale calculations
ggplot()+
  geom_point(data=unnested_models, aes(y=estimate, x=log(urban_score)))+
  geom_smooth(data=df_of_results_by_sample_size, aes(y=estimate, x=log(urban_score), group=sample_size), 
              method="lm", se=FALSE, color="gray")+
  geom_smooth(data=unnested_models, aes(y=estimate, x=log(urban_score)), method="lm", se=FALSE, color="red")+
  theme_classic()+
  xlab("Continental-scale urbanness")+
  ylab("Local-scale urbanness")+
  theme(axis.text=element_text(size=12, color="black"))+
  theme(axis.title=element_text(size=15, color="black"))+
  theme(panel.border=element_rect(fill=NA))


# apply a lm to each of the 100 potential models in
# the dataset created above
nested_sample_size <- df_of_results_by_sample_size %>%
  group_by(sample_size) %>%
  nest()

# function to apply to each species
apply_lm <- function(df){
  lm(estimate ~ log(urban_score), data=df)      
}

# apply the function to each nested df
nested_lms <- nested_sample_size %>%
  mutate(lm = map(.f=apply_lm, .x=data))

# tidy the results
# and also calculate deviance for each model?
tidy <- nested_lms %>%
  mutate(glance = map(.f=glance, .x=lm)) %>%
  unnest(glance)

# plot sample size against R2
ggplot(tidy, aes(x=sample_size, y=r.squared))+
  geom_point()+
  geom_smooth()+
  theme_classic()+
  xlab("Number of citizen science observations")+
  ylab("R2")+
  theme(axis.text=element_text(size=12, color="black"))+
  theme(axis.title=element_text(size=15, color="black"))+
  theme(panel.border=element_rect(fill=NA))+
  geom_hline(yintercept=0.507, color="green")



##################################################################################
##################################################################################
##################################################################################
################## MAKE FIGUREs BASED ON OBJECTS CALCULATED ABOVE ################
##################################################################################

# Figure 1
# see specific script used to make figure 1

# Figure 2
# Figure showing the final linear model for 49 species with
# continental scale scores regressed against
# their local-level response to urbanization
# also shows 100 lines for different models of varying sample size (10:1000)
# used to calculate the underlying continental urban scores
# shows the standard error calculated for the median (urban score)
# and standard error from the GLMs for each of the species
fig2a <- ggplot()+
  geom_smooth(data=df_of_results_by_sample_size, aes(y=estimate, x=urban_score, group=sample_size), 
              method="lm", se=FALSE, color="gray72", alpha=0.7)+
  geom_errorbar(data=unnested_models, aes(ymin=estimate-std.error, ymax=estimate+std.error, x=urban_score),
                 color="darkmagenta")+
  geom_errorbarh(data=results_df_se, aes(y=estimate, xmin=urban_score-se, xmax=urban_score+se),
                color="cadetblue3")+
  geom_point(data=unnested_models, aes(y=estimate, x=urban_score))+
  geom_smooth(data=unnested_models, aes(y=estimate, x=urban_score), method="lm", se=FALSE, size=1.9, color="red3")+
  theme_classic()+
  scale_x_log10(labels=comma)+
  xlab("Continental-scale urbanness")+
  ylab("Local-scale urbanness")+
  theme(axis.text=element_text(size=12, color="black"))+
  theme(axis.title=element_text(size=15, color="black"))+
  theme(panel.border=element_rect(fill=NA))+
  ggtitle("a)")


# Figure showing sample size of continental scale calculations
# and the R2 for a linear model fit when assessing it against
# the local-scale response to urbanization
fig2b <- ggplot(tidy, aes(x=sample_size, y=r.squared))+
  geom_hline(yintercept=0.507, color="red", size=1.4)+
  geom_point()+
  geom_smooth()+
  theme_classic()+
  xlab("Number of eBird observations")+
  ylab(bquote(''*  ~R^2*''))+
  theme(axis.text=element_text(size=12, color="black"))+
  theme(axis.title=element_text(size=15, color="black"))+
  theme(panel.border=element_rect(fill=NA))+
  ggtitle("b)")


fig2a + fig2b + plot_layout(ncol=1)

ggsave("Figures/Figure_2.pdf", width=4, height=7, units="in")
ggsave("Figures/Figure_2.png", width=4, height=7, units="in")


# Figure S1
# study site map

# Figure S2
# example of assigning local-scale urbanness figure

# Figure S3
library(forcats)

s3 <- ggplot(unnested_models, aes(x=estimate))+
  geom_histogram(fill="black", color="white")+
  theme_classic()+
  xlab("GLMM parameter estimate")+
  ylab("Count")+
  theme(axis.text=element_text(size=12, color="black"))+
  theme(axis.title=element_text(size=15, color="black"))+
  ggtitle(paste0("Number of species:", length(unique(unnested_models$Species))))

s3

ggsave(filename="Figures/Figure_S3.png", height=4, width=4.5, units="in", dpi=300)

# Figure S4
s4a <- ggplot(urban_scores, aes(x=urban_score))+
  geom_histogram(fill="black", color="white")+
  theme_classic()+
  xlab("Urban scores (log)")+
  ylab("Count")+
  scale_x_log10()+
  theme(axis.text=element_text(size=12, color="black"))+
  theme(axis.title=element_text(size=15, color="black"))+
  ggtitle(paste0("a)   Number of species:", length(unique(urban_scores$Species))))

s4b <- ggplot(unnested_models, aes(x=urban_score))+
  geom_histogram(fill="black", color="white")+
  theme_classic()+
  xlab("Urban scores (log)")+
  ylab("Count")+
  scale_x_log10()+
  theme(axis.text=element_text(size=12, color="black"))+
  theme(axis.title=element_text(size=15, color="black"))+
  ggtitle(paste0("b)   Number of species:", length(unique(unnested_models$Species))))

s4a + s4b + plot_layout(ncol=1)

ggsave(filename="Figures/Figure_S4.png", height=8, width=4.5, units="in")

# Figure S5
s5 <- unnested_models %>%
  arrange(desc(N_local)) %>%
  ungroup() %>%
  mutate(Species = fct_reorder(Species, estimate)) %>%
  ggplot(., aes(x=Species, y=estimate))+
  geom_segment(aes(x=Species, xend=Species, y=0, yend=estimate), color="gray45")+
  geom_point(aes(color=sign), size=2.5, alpha=0.8)+
  xlab("")+
  ylab("GLMM parameter estimate")+
  theme(axis.text=element_text(size=12, color="black"))+
  theme(axis.title=element_text(size=15, color="black"))+
  coord_flip()+
  theme_classic()+
  theme(axis.text=element_text(size=8, color="black"))+
  theme(axis.title=element_text(size=12, color="black"))+
  guides(color=FALSE)

ggsave(filename="Figures/Figure_S5.png", height=7, width=5.5, units="in", dpi=300)

# Figure S6
unnested_models %>%
  ungroup() %>%
  mutate(Species = fct_reorder(Species, urban_score)) %>%
  ggplot(., aes(x=Species, y=urban_score))+
  geom_segment(aes(x=Species, xend=Species, y=0, yend=urban_score), color="gray45")+
  geom_point(size=2.5, alpha=0.8)+
  xlab("")+
  ylab("Continental-scale urban score")+
  theme(axis.text=element_text(size=12, color="black"))+
  theme(axis.title=element_text(size=15, color="black"))+
  coord_flip()+
  theme_classic()+
  theme(axis.text=element_text(size=8, color="black"))+
  theme(axis.title=element_text(size=12, color="black"))+
  guides(color=FALSE)

ggsave(filename="Figures/Figure_S6.png", height=7, width=5.5, units="in", dpi=300)


## Make Appendix S1
## output species' obs from above
write_csv(species_obs, "AppendixS1.csv")






