library(dplyr)
library(tidyverse)

t_test_per_group <- function(data,
                             comp,
                             left_,
                             right_,
                             value = "f",
                             group = measurement,
                             paired=T
    ){
  
  if (paired){
    invalid_groups <- data %>%
      group_by(!!as.name(group), !!as.name(comp)) %>%
      count() %>%
      pivot_wider(id_cols=group, names_from = comp, values_from = "n") %>%
      filter(!!as.name(left_) != !!as.name(right_)) %>%
      .[,group , T]
  } else { invalid_groups=NULL }
  
  left_n <- str_c("n(", left_, ")")
  right_n <- str_c("n(", right_, ")")
  
  data_selected <- data %>%
    filter(!!as.name(comp) %in% c(left_, right_)) %>%
    filter(!(!!as.name(group) %in% invalid_groups)) #%>%
    # mutate(!!as.name(group) = as.character(!!as.name(group)))
  
  data_selected %>%
    group_by(!!as.name(group), !!as.name(comp)) %>%
    nest() %>%
    spread(key = comp, value = data)  %>%
    mutate(
      t_test = map2(!!as.name(left_),
                    !!as.name(right_),
                    ~{t.test( .y[value], .x[value], paired=paired) %>% tidy()}),
      !!left_ := map(!!as.name(left_), nrow),
      !!right_ := map(!!as.name(right_), nrow)
    ) %>%
    unnest(all_of(c(left_, right_, "t_test"))) %>%
    ungroup() %>%
    rename(!!left_n := !!left_, !!right_n := !!right_)
}



t_test_per_group_safe <- function(data, form,
                                  left_ = "CNN",
                                  right_ = "hybrid",
                                  fold="fold",
                                  label = "label",
                                  paired=F
                                  ){
  if (length(form[[3]])==1){
    names <-  form[[3]]
  }else{names <- form[[3]][[2]]}
  
  data %>% pivot_wider(id_cols = c(label, fold),
                       names_from = names,   values_from =form[[2]]) %>% 
    filter_at(vars(!!as.name(left_), !!as.name(right_)), 
                    dplyr::all_vars(!is.na(.))
                  ) %>% 
    arrange(label, !!as.name(fold)) %>% 
    filter(!is.na(!!as.name(left_)) & !is.na(!!as.name(right_))) %>% 
    group_by(label) %>% filter(n()>1) %>% 
    summarise(t.test(CNN, hybrid, paired=paired) %>% tidy())
}

