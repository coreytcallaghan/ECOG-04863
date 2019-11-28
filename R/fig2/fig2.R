library(plotly)
library(htmlwidgets)

load("data_for_fig2a.RData")

fig2a <- ggplot()+
  # geom_smooth(data=df_of_results_by_sample_size, aes(y=estimate, x=urban_score, group=sample_size), 
  #             method="lm", se=FALSE, color="gray72", alpha=0.7)+
  # geom_errorbar(data=unnested_models, aes(ymin=estimate-std.error, ymax=estimate+std.error, x=urban_score),
  #               color="darkmagenta")+
  # geom_errorbarh(data=results_df_se, aes(y=estimate, xmin=urban_score-se, xmax=urban_score+se),
  #                color="cadetblue3")+
  geom_point(data=unnested_models, aes(y=estimate, x=urban_score, label=Species))+
  #geom_abline(slope = lobf$coefficients[2], intercept = lobf$coefficients[1], size=1.9, colour = "red3") +
  #geom_text(data=unnested_models, aes(y=estimate, x=urban_score, label=Species), alpha = 0) +
  geom_smooth(data=unnested_models, aes(y=estimate, x=urban_score), method="lm", se=FALSE, size=1.9, color="red3", n =100)+
  theme_classic()+
  scale_x_log10(breaks=c(0.1,1,10), labels=c(0,1,10))+
  xlab("Continental-scale urbanness")+
  ylab("Local-scale urbanness")+
  theme(axis.text=element_text(size=12, color="black"))+
  theme(axis.title=element_text(size=15, color="black"))+
  theme(panel.border=element_rect(fill=NA))+
  ggtitle("a)")

plot_bits <- ggplot_build(fig2a)$data

fig2a_p <- ggplotly(fig2a, width = 900, height = 700)

saveWidget(widget = fig2a_p, file = "local_cont_urbanness.html")
