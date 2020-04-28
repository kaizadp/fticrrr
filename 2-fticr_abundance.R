# HYSTERESIS AND SOIL CARBON
# 
# Kaizad F. Patel
# April 2020


source("0-packages.R")

# ------------------------------------------------------- ----

# I. LOAD FILES ----

fticr_data = read.csv(FTICR_LONG)
fticr_meta = read.csv(FTICR_META)
meta_hcoc  = read.csv(FTICR_META_HCOC)


# II. RELATIVE ABUNDANCE ----
## IIa. calculate relative abundance of classes in each core 
relabund_cores = 
  fticr_data %>% 
# add the Class column to the data
  left_join(dplyr::select(fticr_meta, formula, Class), by = "formula") %>% 
# calculate abundance of each Class as the sum of all counts
  group_by(CoreID, treatment, sat_level, Class) %>%       #$$$$#
  dplyr::summarise(abund = sum(presence)) %>%
  ungroup %>% 
# create a new column for total counts per core assignment
# and then calculate relative abundance  
  group_by(CoreID, treatment, sat_level) %>%      #$$$$#
  dplyr::mutate(total = sum(abund),
                relabund  = round((abund/total)*100,2))

## IIb. calculate mean relative abundance across all replicates of the treatments
relabund_trt = 
  relabund_core %>% 
  group_by(treatment, sat_level, Class) %>%       #$$$$#
  dplyr::summarize(rel_abund = round(mean(relabund),2),
                se  = round((sd(relabund/sqrt(n()))),2),
                relative_abundance = paste(rel_abund, "\u00b1",se))

# IV. OUTPUT ----
write.csv(relabund_cores, FTICR_RELABUND_CORES, row.names=FALSE)
write.csv(relabund_trt, FTICR_RELABUND_TRT, row.names=FALSE)



#
# III. SHANNON DIVERSITY ----
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
# IV. OUTPUT ----
write.csv(shannon, "data/processed/fticr_shannon.csv", row.names = FALSE)
write.csv(relabund, "data/processed/fticr_relabund.csv", row.names=FALSE)



