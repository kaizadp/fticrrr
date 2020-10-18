# FTICRRR: fticr results in R
# Kaizad F. Patel
# October 2020

################################################## #

## `functions_vankrevelens.R`
## this script will load functions for plotting Van Krevelen diagrams
## source this file in the `fticr_drake_plan.R` file, do not run the script here.

################################################## #
################################################## #


theme_kp <- function() {  # this for all the elements common across plots
  theme_bw() %+replace%
    theme(legend.position = "top",
          legend.key=element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(size = 12),
          legend.key.size = unit(1.5, 'lines'),
          legend.background = element_rect(colour = NA),
          panel.border = element_rect(color="black",size=1.5, fill = NA),
          
          plot.title = element_text(hjust = 0.05, size = 14),
          axis.text = element_text(size = 14, color = "black"),
          axis.title = element_text(size = 14, face = "bold", color = "black"),
          
          # formatting for facets
          panel.background = element_blank(),
          strip.background = element_rect(colour="white", fill="white"), #facet formatting
          panel.spacing.x = unit(1.5, "lines"), #facet spacing for x axis
          panel.spacing.y = unit(1.5, "lines"), #facet spacing for x axis
          strip.text.x = element_text(size=12, face="bold"), #facet labels
          strip.text.y = element_text(size=12, face="bold", angle = 270) #facet labels
    )
}
gg_vankrev <- function(data,mapping){
  ggplot(data,mapping) +
    # plot points
    geom_point(size=2, alpha = 0.2) + # set size and transparency
    # axis labels
    ylab("H/C") +
    xlab("O/C") +
    # axis limits
    xlim(0,1.25) +
    ylim(0,2.5) +
    # add boundary lines for Van Krevelen regions
    geom_segment(x = 0.0, y = 1.5, xend = 1.2, yend = 1.5,color="black",linetype="longdash") +
    geom_segment(x = 0.0, y = 0.7, xend = 1.2, yend = 0.4,color="black",linetype="longdash") +
    geom_segment(x = 0.0, y = 1.06, xend = 1.2, yend = 0.51,color="black",linetype="longdash") +
    guides(colour = guide_legend(override.aes = list(alpha=1)))
}


# van krevelen plots ------------------------------------------------------
plot_vankrevelen_domains = function(fticr_meta){
  
  gg_vk_domains = 
    gg_vankrev(fticr_meta, aes(x = OC, y = HC, color = Class))+
    scale_color_manual(values = PNWColors::pnw_palette("Sunset2"))+
    theme_kp()
  
  gg_vk_domains_nosc = 
    gg_vankrev(fticr_meta, aes(x = OC, y = HC, color = as.numeric(NOSC)))+
    scale_color_gradientn(colors = PNWColors::pnw_palette("Bay"))+
    theme_kp()
  
  list(gg_vk_domains = gg_vk_domains,
       gg_vk_domains_nosc = gg_vk_domains_nosc)
}

plot_vankrevelens = function(fticr_data_longform, fticr_meta){
  
  fticr_hcoc = 
    fticr_data_longform %>% 
    left_join(dplyr::select(fticr_meta, formula, HC, OC), by = "formula")
  
  gg_vk1 = 
    gg_vankrev(fticr_hcoc, aes(x = OC, y = HC, color = sat_level))+
    facet_grid(.~sat_level)+
    theme_kp()
  
  gg_fm = gg_vankrev(fticr_hcoc, aes(x = OC, y = HC, color = sat_level))+
    stat_ellipse()+
    theme_kp()
  
  gg_vk2 = 
    ggExtra::ggMarginal(gg_fm,groupColour = TRUE,groupFill = TRUE)
  
  list(gg_vk1 = gg_vk1,
       gg_vk2 = gg_vk2)
}

