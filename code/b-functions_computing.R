# relative abundance ------------------------------------------------------

compute_relabund_cores = function(fticr_data_longform, fticr_meta, ...){
  fticr_data_longform %>% 
    # add the Class column to the data
    left_join(dplyr::select(fticr_meta, formula, Class), by = "formula") %>% 
    # calculate abundance of each Class as the sum of all counts
    group_by(CoreID, Class, ...) %>%
    dplyr::summarise(abund = sum(presence)) %>%
    ungroup %>% 
    # create a new column for total counts per core assignment
    # and then calculate relative abundance  
    group_by(CoreID, ...) %>% 
    dplyr::mutate(total = sum(abund),
                  relabund  = round((abund/total)*100,2))
}


# pca functions -----------------------------------------------------------
library(ggbiplot)

compute_fticr_pca = function(relabund_cores){
  relabund_pca=
    relabund_cores %>% 
    ungroup %>% 
    dplyr::select(-c(abund, total)) %>% 
    spread(Class, relabund) %>% 
    replace(.,is.na(.),0)  %>% 
    dplyr::select(-1)
  
  relabund_pca_num = 
    relabund_pca %>% 
    dplyr::select(c(aliphatic, aromatic, `condensed aromatic`, `unsaturated/lignin`))
  
  relabund_pca_grp = 
    relabund_pca %>% 
    dplyr::select(-c(aliphatic, aromatic, `condensed aromatic`, `unsaturated/lignin`)) %>% 
    dplyr::mutate(row = row_number())
  
  pca = prcomp(relabund_pca_num, scale. = T)
  summary(pca)
  
  ggbiplot(pca, obs.scale = 1, var.scale = 1, 
           groups = relabund_pca_grp$treatment, ellipse = TRUE, circle = F,
           var.axes = TRUE)+
    geom_point(size=2,stroke=2, aes(color = groups, shape = as.factor(relabund_pca_grp$sat_level)))
}


