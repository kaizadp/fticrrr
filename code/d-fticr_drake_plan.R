## FTICR-MS DRAKE PLAN
## USE THIS SCRIPT/PLAN TO PROCESS, ANALYZE, AND GRAPH FTICR-MS DATA
## KAIZAD F. PATEL
## OCT-06-2020

##############################
##############################

## SOURCE THE FUNCTION FILES FIRST, AND THEN RUN THE DRAKE PLAN
## DON'T RUN THE PROCESSING PLAN MULTIPLE TIMES. ONCE IS ENOUGH.

##############################
##############################


# 1. SET input file paths -------------------------------
COREKEY = "data/corekey.csv"
REPORT = "data/Report.csv"

## SET the treatment variables
TREATMENTS = quos(sat_level)
## this will work with multiple variables too. just add all the variable names in the parentheses.

# 2. load packages and source the functions --------------------------------------------------------
library(drake)
library(tidyverse)

source("code/a-functions_processing.R")
source("code/b-functions_relabund.R")
source("code/c-functions_vankrevelen.R")
#source("code/d-functions_statistics.R")

# 3. load drake plans -----------------------------------------------------
fticr_processing_plan = drake_plan(
  
  
  # a. PROCESSING
  datareport = read.csv(file_in(REPORT)),
  fticr_meta = make_fticr_meta(datareport)$meta2,
  fticr_data_longform = make_fticr_data(datareport, sat_level, treatment)$data_long_key_repfiltered,
  fticr_data_trt = make_fticr_data(datareport, sat_level, treatment)$data_long_trt,
  
  ## OUTPUT
  #  fticr_meta %>% write.csv(),
  #  fticr_data_trt %>% write.csv(),
  #  fticr_data_longform %>% write.csv() 
  
  # b. relative abundance
  relabund_cores = fticr_data_longform %>% 
    compute_relabund_cores(fticr_meta, sat_level, treatment),
  gg_relabund_bar = relabund_cores %>% plot_relabund(TREATMENTS)+
    scale_fill_manual(values = PNWColors::pnw_palette("Sailboat")),
  ## create relabund table
  ## OUTPUT ?
  
  # c. van krevelen plots
  gg_vankrevelen_domains = plot_vankrevelen_domains(fticr_meta),
  gg_vankrevelens = plot_vankrevelens(fticr_data_longform, fticr_meta),
  
  # d. stats
  ## PERMANOVA
  ## PCA
  gg_pca = compute_fticr_pca(relabund_cores), 
  
  
  # e. REPORT
  outputreport = rmarkdown::render(
    knitr_in("reports/fticrrr_report.Rmd"),
    output_format = rmarkdown::github_document())
)


# 4. make plans -------------------------------------------------------------------------
corekey = read.csv(file_in(COREKEY))
make(fticr_processing_plan)

