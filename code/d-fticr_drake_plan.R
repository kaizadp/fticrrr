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


# 2. load packages and source the functions --------------------------------------------------------
library(drake)
library(tidyverse)

source("code/a-functions_processing.R")
source("code/b-functions_computing.R")
source("code/c-functions_plotting.R")

# 3. load drake plans -----------------------------------------------------
## a. PROCESSING -------------------------------------------------------------------------
fticr_processing_plan = drake_plan(
  report = read.csv(file_in(REPORT)),

  fticr_meta = make_fticr_meta(report)$meta2,
  fticr_data_longform = make_fticr_data(report, sat_level, treatment)$data_long_key_repfiltered,
  fticr_data_trt = make_fticr_data(report, sat_level, treatment)$data_long_trt
  
  #  fticr_meta %>% write.csv(),
  #  fticr_data_trt %>% write.csv(),
  #  fticr_data_longform %>% write.csv() 
  
)


## b. COMPUTING -------------------------------------------------------------------------



# 4. make plans -------------------------------------------------------------------------
corekey = read.csv(file_in(COREKEY))
make(fticr_processing_plan)

