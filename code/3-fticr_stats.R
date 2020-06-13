# HYSTERESIS AND SOIL CARBON
# 3-fticr_initial processing
# Kaizad F. Patel
# June 2020

## this script will perform statistical analyses on processed FTICR-MS data
## use relative abundances for statistical analyses

source("code/0-packages.R")

# ------------------------------------------------------- ----


# I. Load files -----------------------------------------------------------
relabund_cores = read.csv(FTICR_RELABUND_CORES)
relabund_trt = read.csv(FTICR_RELABUND_TRT)

#
# II. MANOVA: overall treatment effect ---------------------------------------------------------------
# perform a Multivariate ANOVA with relative abundances of groups as the response variables and 
# moisture level as an explanatory variable.

# to perform MANOVA, each group must be a column

relabund_wide = 
  relabund_cores %>% 
  dplyr::select(CoreID, sat_level, Class, relabund) %>% 
  spread(Class, relabund) %>% 
  replace(is.na(.),0)

# create a matrix of all the group columns
relabund_wide$DV = as.matrix(relabund_wide[,3:11])

# since the relative abundances are not strictly independent and all add to 100 %,
# use the isometric log ratio transformation
# http://www.sthda.com/english/wiki/manova-test-in-r-multivariate-analysis-of-variance#import-your-data-into-r

library(compositions)

man = manova(ilr(clo(DV)) ~ sat_level, data = relabund_wide)
summary(man)

#
# III. PCA ---------------------------------------------------------------------


# IV. Pairwise statistics -------------------------------------------------
# testing the treatment effect for each group 

# fit an ANOVA+HSD function and then apply it to each FTICR group

fit_hsd <- function(dat) {
  a <-aov(relabund ~ sat_level, data = dat)
  h <-agricolae::HSD.test(a,"sat_level")
  #create a tibble with one column for each treatment
  #the hsd results are row1 = drought, row2 = saturation, row3 = time zero saturation, row4 = field moist. hsd letters are in column 2
  tibble(`100` = h$groups["100",2], 
         `50` = h$groups["50",2],
         `FM` = h$groups["FM",2])
}

fticr_hsd = 
  relabund_cores %>% 
  group_by(Class) %>% 
  do(fit_hsd(.)) %>% 
  pivot_longer(-Class,
               names_to = "sat_level",
               values_to = "hsd")

relabund_summary = 
  relabund_trt %>% 
  left_join(fticr_hsd, by = c("sat_level", "Class")) %>% 
  dplyr::mutate(relative_abundance = paste(relative_abundance, hsd))
  

    # for a simple ANOVA (no HSD), use this code instead:
    # fit_aov_ <- function(dat) {
    #   a <-aov(relabund ~ sat_level, data = dat)
    #   tibble(`p` = summary(a)[[1]][[1,"Pr(>F)"]])
    # } 

    # fticr_aov = 
    #   relabund_cores %>% 
    #   group_by(Class) %>% 
    #   do(fit_aov(.)) %>% 
    #   dplyr::mutate(p = round(p,4),
    #                 asterisk = if_else(p<0.05,"*",as.character(NA))) %>% 
    #   dplyr::select(-p)

#
# V. SHANNON DIVERSITY ----
# Shannon diversity, H = - sum [p*ln(p)], where n = no. of individuals per species/total number of individuals

shannon = 
  relabund %>% 
  dplyr::mutate(
    p = abund/total,
    log = log(p),
    p_logp = p*log) %>% 
  group_by(Core_assignment) %>% 
  dplyr::summarize(H1 = sum(p_logp),
                   H = round(-1*H1, 2)) %>% 
  dplyr::select(-H1)


#
# VI. OUTPUT ----
write.csv(shannon, "data/processed/fticr_shannon.csv", row.names = FALSE)
write.csv(relabund, "data/processed/fticr_relabund.csv", row.names=FALSE)

write.csv(relabund_summary, FTICR_RELABUND_SUMMARY, row.names = F)


