# HYSTERESIS AND SOIL CARBON
# 3-fticr_initial processing
# Kaizad F. Patel
# March 2020

 ## this script will process the input data and metadata files and
 ## generate clean files that can be used for subsequent analysis.
 ## each dataset will generate longform files of (a) all cores, (b) summarized data for each treatment (i.e. cores combined) 

source("code/0-packages.R")

# ------------------------------------------------------- ----

## step 1: load the files ----
report = read.csv("data/Report.csv")
corekey = read.csv(COREKEY)

fticr_report = 
  report %>% 
# filter appropriate mass range
  filter(Mass>200 & Mass<900) %>% 
# remove isotopes
  filter(C13==0) %>% 
# remove peaks without C assignment
  filter(C>0)


# this has metadata as well as sample data
# split them

# 1a. create meta file ----
fticr_meta = 
  fticr_report %>% 
# select only the relevant columns for the formula assignments
  dplyr::select(Mass:Candidates) %>% 
# alternatively, use `starts_with()` if all your sample names start with the same prefix
# dplyr::select(-starts_with("FT")) %>% 
# select only necessary columns
  dplyr::select(Mass, C, H, O, N, S, P, El_comp, Class) %>% 
# create columns for indices
  dplyr::mutate(AImod = round((1+C-(0.5*O)-S-(0.5*(N+P+H)))/(C-(0.5*O)-S-N-P),4),
                NOSC =  round(4-(((4*C)+H-(3*N)-(2*O)-(2*S))/C),4),
                HC = round(H/C,2),
                OC = round(O/C,2)) %>% 
# create column/s for formula
# first, create columns for individual elements
# then, combine
  dplyr::mutate(formula_c = if_else(C>0,paste0("C",C),as.character(NA)),
                formula_h = if_else(H>0,paste0("H",H),as.character(NA)),
                formula_o = if_else(O>0,paste0("O",O),as.character(NA)),
                formula_n = if_else(N>0,paste0("N",N),as.character(NA)),
                formula_s = if_else(S>0,paste0("S",S),as.character(NA)),
                formula_p = if_else(P>0,paste0("P",P),as.character(NA)),
                formula = paste0(formula_c,formula_h, formula_o, formula_n, formula_s, formula_p),
                formula = str_replace_all(formula,"NA","")) %>% 
  dplyr::select(Mass, formula, El_comp, Class, HC, OC, AImod, NOSC, C:P)


# subset of meta for HC/OC only, for Van Krevelen diagrams
meta_hcoc = 
  fticr_meta %>% 
  dplyr::select(Mass, formula, HC, OC)

#  
# 1b. create data file ----

fticr_data = 
  fticr_report %>% 
# select only the relevant columns for the formula assignments
  dplyr::select(-c(C:Candidates)) %>% 
# alternatively, use `starts_with()` if all your sample names start with the same prefix
# dplyr::select(Mass,starts_with("FT")) %>% 
  melt(id = c("Mass"), value.name = "presence", variable.name = "CoreID") %>% 
# convert intensities to presence==1/absence==0  
  dplyr::mutate(presence = if_else(presence>0,1,0)) %>% 
# keep only peaks present
  filter(presence>0) %>% 
  left_join(dplyr::select(fticr_meta, Mass,formula), by = "Mass")  %>% 
  left_join(corekey, by = "CoreID") %>% 
# rearrange columns
  dplyr::select(-Mass,-formula, -presence,Mass,formula,presence) %>% 
# now we want only peaks that are in 3 of the 5 replicates
# group by the treatment levels  
  group_by(treatment, sat_level,formula) %>% 
  dplyr::mutate(n = n(),
                presence = mean(presence)) %>% 
  filter(n>2) 

## OUTPUTS
write.csv(fticr_data,FTICR_LONG, row.names = FALSE)
write.csv(fticr_meta,FTICR_META, row.names = FALSE)
write.csv(meta_hcoc,FTICR_META_HCOC, row.names = FALSE)



