

# 1. PROCESSING FUNCTIONS -------------------------------------------------
## LEVEL I FUNCTIONS -------------------------------------------------------
## for metadata file
apply_filter_report = function(report){
  report %>% 
    # filter appropriate mass range
    filter(Mass>200 & Mass<900) %>% 
    # remove isotopes
    filter(C13==0) %>% 
    # remove peaks without C assignment
    filter(C>0)
}
compute_indices = function(dat){
  dat %>% 
    dplyr::select(Mass, C:P) %>% 
    dplyr::mutate(AImod = round((1+C-(0.5*O)-S-(0.5*(N+P+H)))/(C-(0.5*O)-S-N-P),4),
                  NOSC =  round(4-(((4*C)+H-(3*N)-(2*O)-(2*S))/C),4),
                  HC = round(H/C,2),
                  OC = round(O/C,2),
                  DBE_AI = 1+C-O-S-0.5*(N+P+H),
                  DBE =  1 + ((2*C-H + N + P))/2,
                  DBE_C = round(DBE_AI/C,4)) %>% 
    dplyr::select(-c(C:P))
}
compute_mol_formula = function(dat){
  dat %>% 
    dplyr::select(Mass, C:P) %>% 
    dplyr::mutate(formula_c = if_else(C>0,paste0("C",C),as.character(NA)),
                  formula_h = if_else(H>0,paste0("H",H),as.character(NA)),
                  formula_o = if_else(O>0,paste0("O",O),as.character(NA)),
                  formula_n = if_else(N>0,paste0("N",N),as.character(NA)),
                  formula_s = if_else(S>0,paste0("S",S),as.character(NA)),
                  formula_p = if_else(P>0,paste0("P",P),as.character(NA)),
                  formula = paste0(formula_c,formula_h, formula_o, formula_n, formula_s, formula_p),
                  formula = str_replace_all(formula,"NA","")) %>% 
    dplyr::select(Mass, formula)
}
assign_class_seidel = function(meta_clean, meta_indices){
  meta_clean %>%
    left_join(meta_indices, by = "Mass") %>% 
    mutate(Class = case_when(AImod>0.66 ~ "condensed aromatic",
                             AImod<=0.66 & AImod > 0.50 ~ "aromatic",
                             AImod <= 0.50 & HC < 1.5 ~ "unsaturated/lignin",
                             HC >= 1.5 ~ "aliphatic"),
           Class = replace_na(Class, "other"),
           Class_detailed = case_when(AImod>0.66 ~ "condensed aromatic",
                                      AImod<=0.66 & AImod > 0.50 ~ "aromatic",
                                      AImod <= 0.50 & HC < 1.5 ~ "unsaturated/lignin",
                                      HC >= 2.0 & OC >= 0.9 ~ "carbohydrate",
                                      HC >= 2.0 & OC < 0.9 ~ "lipid",
                                      HC < 2.0 & HC >= 1.5 & N==0 ~ "aliphatic",
                                      HC < 2.0 & HC >= 1.5 & N > 0 ~ "aliphatic+N")) %>% 
    dplyr::select(Mass, EMSL_class, Class, Class_detailed)
}

## for data file
compute_presence = function(dat){
  dat %>% 
    pivot_longer(-("Mass"), values_to = "presence", names_to = "CoreID") %>% 
    # convert intensities to presence==1/absence==0  
    dplyr::mutate(presence = if_else(presence>0,1,0)) %>% 
    # keep only peaks present
    filter(presence>0)
}
apply_replication_filter = function(data_long_key, ...){
  max_replicates = 
    data_long_key %>% 
    ungroup() %>% 
    group_by(...) %>% 
    distinct(CoreID) %>% 
    dplyr::summarise(reps = n())
  
  
  # second, join the `reps` file to the long_key file
  # and then use the replication filter  
  data_long_key %>% 
    group_by(formula, ...) %>% 
    dplyr::mutate(n = n()) %>% 
    left_join(max_replicates) %>% 
    ungroup() %>% 
    mutate(keep = n >= (2/3)*reps) %>% 
    filter(keep) %>% 
    dplyr::select(-keep, -reps)
  
}

## LEVEL II FUNCTIONS ------------------------------------------------------
make_fticr_meta = function(report){
  fticr_report = (apply_filter_report(report))
  
  meta_clean = 
    fticr_report %>% 
    # select only the relevant columns for the formula assignments
    dplyr::select(Mass, C, H, O, N, S, P, El_comp, Class) %>% 
    rename(EMSL_class = Class)
  
  meta_indices = compute_indices(meta_clean)
  meta_formula = compute_mol_formula(meta_clean)
  meta_class = assign_class_seidel(meta_clean, meta_indices)
  
  # output
  meta2 = meta_formula %>% 
    left_join(meta_class, by = "Mass") %>% 
    left_join(meta_indices, by = "Mass") %>% dplyr::select(-Mass) %>% distinct(.)
  
  
  list(meta2 = meta2,
       meta_formula = meta_formula)
}
make_fticr_data = function(report, ...){
  fticr_report = (apply_filter_report(report))
  mass_to_formula = make_fticr_meta(report)$meta_formula
  
  data_columns = fticr_report %>% dplyr::select(-c(C:Candidates))
  
  data_presence = compute_presence(data_columns) %>% 
    left_join(mass_to_formula, by = "Mass") %>% 
    dplyr::select(formula, CoreID, presence)
  
  data_long_key = data_presence %>% left_join(corekey, by = "CoreID")
  
  data_long_key_repfiltered = apply_replication_filter(data_long_key, ...)
  
  data_long_trt = data_long_key_repfiltered %>% 
    distinct(formula, ...)
  
  list(data_long_trt = data_long_trt,
       data_long_key_repfiltered = data_long_key_repfiltered)
  
}








