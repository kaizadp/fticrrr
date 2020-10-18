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


compute_permanova = function(relabund_cores, indepvar){
  relabund_wide = 
    relabund_cores %>% 
    ungroup() %>% 
    mutate(Class = factor(Class, 
                          levels = c("aliphatic", "unsaturated/lignin", 
                                     "aromatic", "condensed aromatic"))) %>% 
    dplyr::select(-c(abund, total)) %>% 
    spread(Class, relabund) %>% 
    replace(is.na(.), 0)
  
  permanova_fticr_all = 
    adonis(relabund_wide %>% dplyr::select(aliphatic:`condensed aromatic`) ~ indepvar, 
           data = relabund_wide)
  
  broom::tidy(permanova_fticr_all$aov.tab)
}

variables = c("sat_level", "treatment")
indepvar = paste(variables, collapse = " + ")

compute_permanova(indepvar)
