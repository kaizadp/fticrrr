mean_mpg = function(data, group_col) {
  data %>% group_by_(.dots = lazyeval::lazy(group_col)) %>% summarize(mean_mpg = mean(mpg))
}



relativeabundancebycores = function(dat, treatments){
  dat %>% 
    # add the Class column to the data
    left_join(dplyr::select(fticr_meta, formula, Class), by = "formula") %>% 
    # calculate abundance of each Class as the sum of all counts
    group_by(.dots = lazyeval::lazy(treatments)) %>%       #$$$$#
    dplyr::summarise(abund = sum(presence)) %>%
    ungroup %>% 
    # create a new column for total counts per core assignment
    # and then calculate relative abundance  
    group_by(treatments) %>%      #$$$$#
    dplyr::mutate(total = sum(abund),
                  relabund  = round((abund/total)*100,2))
  
}

treatments = "sat_level"
fticr_data %>% relativeabundancebycores(sat_level)


mean_mpg = function(data, group_col) {
  data %>% group_by(.dots = lazyeval::lazy(group_col)) %>% summarize(mean_mpg = mean(mpg))
}

mtcars %>% mean_mpg(cyl)
mtcars %>% mean_mpg(gear)



nopeaks = 
  fticr_data %>% group_by(CoreID) %>% 
  dplyr::summarise(count = n())


peakcount = function(dat, ...){
  dat %>% group_by(.dots = lazyeval::lazy_dots(...)) %>% 
    dplyr::summarise(count = n())
}

fticr_data %>% peakcount(sat_level, treatment)



mean_mpg = function(data, ...) {
  data %>% group_by_(.dots = lazyeval::lazy_dots(...)) %>% 
    summarize(mean_mpg = mean(mpg))
}
mtcars %>% mean_mpg(cyl, gear)






relativeabundancebycores = function(dat, ...){ 
  dat %>% 
  # add the Class column to the data
  left_join(dplyr::select(fticr_meta, formula, Class), by = "formula") %>% 
  # calculate abundance of each Class as the sum of all counts
  # group_by(CoreID, Class) %>%       #$$$$#
    group_by_(.dots = lazyeval::lazy_dots(CoreID, Class, ...)) %>% 
  dplyr::summarise(abund = sum(presence)) %>%
  ungroup %>% 
  # create a new column for total counts per core assignment
  # and then calculate relative abundance  
  # group_by(CoreID, sat_level) %>%      #$$$$#
    group_by_(.dots = lazyeval::lazy_dots(CoreID, ...)) %>% 
    dplyr::mutate(total = sum(abund),
                relabund  = round((abund/total)*100,2))
}


fticr_data %>% relativeabundancebycores(sat_level, treatment)
