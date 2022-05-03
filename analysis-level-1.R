#setwd("/Users/dlituiev/repos/infocommons/social-determinants-of-health-cocoa")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# setwd(getSrcDirectory()[1])
getwd()
rm(list=ls())

library(ggplot2)
library(tidyverse)
library(vroom)
library(latex2exp)
library(ggsignif)
# install.packages("ggpubr")
library(ggpubr)
library(tools)
library(glue)
library(broom)
library(dplyr)
library(kableExtra)

theme_set(theme_bw())

dir.create("fig")
#' ## Todo
#' scoring methods
#' - level-1 level
#' - lowest level
#' - weighted

method_mappings <- as_tibble(as.data.frame(list(new=c(
                                  `entailment-RoBERTa`="entailment-RoBERTa",
                                  `entailment-RoBERTa-mean`="entailment-RoBERTa-mean",
                                   bow="NER-hybrid",
                                   spacy="NER-CNN",
                                   en_core_web_md="NER-CNN", 
                                   roberta_lr_5e_5_bs_16 = "NER-RoBERTa",
                                   `CNN`="CNN-en_core_web_md", 
                                   cocoa2 = "cTAKES-SDoH", 
                                   infoc="cTAKES-InfoCommons", 
                                   snorx="cTAKES-default"
                                   ))),
          rownames = "old") %>% 
  mutate(new = factor(new, levels = unique(new)))


#' - level-1 level
#' 
# data_spacy_classification_snippet <- vroom("results/classification-snippet-metrics.csv")
# data_spacy_classification_snippet[,"method"] <- "spacy+GBM"
# data_spacy_classification_snippet <- data_spacy_classification_snippet %>% rename(f=f1, p=precision,r=recall)
# 
# data_spacy <- vroom("results/ents_per_type_fold-lvl1.csv")
# data_spacy[,"method"] <- "spacy+GBM"
# data_spacy[,"level"] <- "level-1" # native
# 
# data_spacy_classification_snippet <- vroom("results/classification-snippet-metrics-with-scorer.csv")
# data_spacy_classification_snippet[,"method"] <- "spacy+GBM"
# data_spacy_classification_snippet[,"level"] <- "level-2"
# data_spacy_classification_snippet <- data_spacy_classification_snippet %>% rename(label=label_level_1)
# 
# data_spacy_weighted <- vroom("results/spacy-gbm-weighted-metrics.csv")
# data_spacy_weighted[,"level"] <- "weighted"
# data_spacy_weighted[,"method"] <- "spacy+GBM"

#' Spacy (deep)
#' 

# data_spacy_lvl2 <- vroom("results/ents_per_type_fold-native-lvl2-aggregated.csv") %>% 
#   rename(label=label_level_1)
# data_spacy_lvl2[,"method"] <- "spacy"
# data_spacy_lvl2[,"level"] <- "level-2"
# 
# data_spacy_lvl2_high <- vroom("results/ents_per_type_fold-native-lvl2-scored-at-lvl1-aggregated.csv")
# data_spacy_lvl2_high[,"method"] <- "spacy"
# data_spacy_lvl2_high[,"level"] <- "level-1"

# data_spacy_lvl2_weighted <- vroom("results/metrics-spacy-level2-native-weighted.csv")
# data_spacy_lvl2_weighted[,"method"] <- "spacy"
# data_spacy_lvl2_weighted[,"level"] = "weighted"
group_level_1 <- function(data, ..., vars=c("f1-score","precision", "recall")){
  # arguments <- list(...)
  
  data  %>% 
    mutate(label_level_1 = str_split_fixed(label, ": ", 2)[,1]) %>% 
    filter(!(label_level_1 %in% c("macro avg", "weighted avg", 
                                  "non-specific", "null", "accuracy", ""))) %>% 
    group_by(label_level_1, fold, ...) %>% 
    summarise(across(vars, ~  sum(.x * support)/sum(support)),
              support = sum(support)) %>%
    ungroup() %>% rename(label=label_level_1) 
}


library(stringi)

read_tables <- function(pattern, values=c()){
  var_name <- stri_match(pattern, regex="\\{([a-z_A-Z0-9]+)\\}")[,2]
  if (length(values)==0){
    reg_ <- glue(pattern, .envir = setNames(list("([a-z_A-z0-9]+)"), var_name))
    files <- Sys.glob(glue(pattern, .envir = setNames(list("*"), var_name)))
    values <-  stri_match(files, regex=reg_)[,2]
  }
  print(values)
  data <- list()
  for (value in values){
    fn <- glue(pattern, .envir = setNames(list(value), var_name))
    tmp <- vroom(fn)
    tmp[, var_name] <- value
    # tmp[,"level"] <- "level-1"
    data <- append(data, list(tmp))
  }
  data <- bind_rows(data) 
  data
}

# group_level_1
data_spacy_lvl2 <- 
  read_tables("results/metrics-spacy-{method}-level2-sklearn-test.csv") %>% 
  group_level_1(method) %>% 
  rename(f=`f1-score`, p=precision, r=recall) %>% 
  mutate(level="level-2")
# 
# data_spacy_lvl2 <- vroom("results/metrics-spacy-en_core_web_md-level2-sklearn-grouped.csv") %>% 
#   rename(f=`f1-score`, p=precision, r=recall) 
# data_spacy_lvl2[,"method"] <- "spacy"
# data_spacy_lvl2[,"level"] <- "level-2"

# data_spacy_lvl2

data_spacy_lvl1 <- 
  read_tables("results/metrics-spacy-{method}-level1-sklearn-test.csv") %>%
  rename(f=`f1-score`, p=precision, r=recall)

data_spacy_lvl1[,"level"] <- "level-1"

#' #' CTAKES
#' data_ctakes_weighted <- vroom("results/metrics-ctakes-method-fold-weighted.csv")
#' data_ctakes_weighted[,"level"] <- "weighted"


# pattern <- "results/metrics-ctakes-with-sklearn-{method}-fold-level-1-native.csv"
pattern <- "results/metrics-ctakes-with-sklearn-report-{method}-fold-level1.csv"
data_ctakes_exact_level1 <- read_tables(pattern) %>%
  rename(f=`f1-score`,
          # f = 2*p*r / (p + r),
         p=precision, r=recall,
         ) %>%
  mutate(level="level-1",
         f=ifelse(support>0, f, NA),)


# data_ctakes_exact_level1 %>% 
#   group_by(label, method, level) %>%
#   summarise_all(median) %>% 
#   pivot_wider(id_cols = c("label", "level"),
#               names_from = method,
#               values_from = f) %>% 
#   mutate_all(~function(x){100*x})

# pattern <- "results/metrics-ctakes-with-sklearn-{method}-fold-exact-match-agg.csv"
pattern <- "results/metrics-ctakes-with-sklearn-report-{method}-fold-level2.csv"
data_ctakes_exact_level2 <- read_tables(pattern) %>%
  # rename(f=`f1-score`, p=precision, r=recall) %>% 
  group_by(label_level_1, fold, method) %>% 
  summarise(f = sum(`f1-score`*support)/sum(support),
            r = sum(recall*support)/sum(support),
            p = sum(precision*support)/sum(support),
            # f = 2*p*r / (p + r),
            support = sum(support),
  ) %>% 
  ungroup() %>% rename(label=label_level_1) %>% 
  mutate(level="level-2")

# data_bow_deepest <- vroom("results/metrics_per_ref_level2_folds_Bow-sklearn-grouped-211220a.csv")
data_bow_deepest <- vroom("results/metrics-bow-with-sklearn-report-fold-level2-grouped.csv") %>% 
  mutate(level="level-2")

# data_bow_highest <- vroom("results/metrics_per_pred_level1_BoW-class_report_folds-211220a.csv")
data_bow_highest <- vroom("results/metrics-bow-with-sklearn-report-fold-level1.csv")
data_bow_highest <- data_bow_highest %>% rename(f=`f1-score`, p=precision, r=recall) %>% 
  mutate(level="level-1", method="bow")


data_spacy_lvl2_en_core_web_md <- vroom("results/metrics-spacy-en_core_web_md-native-lvl2-aggregated.csv")

data_spacy_lvl2_en_core_web_md <- data_spacy_lvl2_en_core_web_md %>% 
  rename(label=label_level_1) %>% 
  mutate(level="level-2", method="CNN-en_core_web_md")

data_spacy_lvl1_en_core_web_md <- vroom("results/metrics-native-spacy-en_core_web_md-lvl2-scored-at-lvl1-aggregated.csv")
data_spacy_lvl1_en_core_web_md <- data_spacy_lvl1_en_core_web_md %>% 
  mutate(level="level-1", method="CNN-en_core_web_md")

#' Entailment
#' 

metrics_entailment_agg <- vroom("results/metrics-entailment-RoBERTa-level-2-agg.csv") %>%
  mutate(method="entailment-RoBERTa", level="level-2") %>% rename(f=`f1-score`, p=precision, r=recall) %>%
  rename(label=label_level_1) %>% 
  mutate(method=ifelse(aggregation=="mean", "entailment-RoBERTa-mean", method))

metrics_entailment_level1 <- vroom("results/metrics-entailment-RoBERTa-level-1.csv") %>%
  mutate(method="entailment-RoBERTa", level="level-1") %>% rename(f=`f1-score`, p=precision, r=recall) %>% 
  mutate(method=ifelse(aggregation=="mean", "entailment-RoBERTa-mean", method))


# data_spacy %>% ggplot(aes(x=label, y=f)) + 
#   geom_boxplot(fill="grey") + coord_flip() + 
#   scale_x_discrete(limits = rev(levels(data$label))) +
#   ylab("F1 score") + xlab("")

data_ctakes_exact_level1 %>% 
  filter(label!="Misc") %>% 
  group_by(label) %>% 
  # group_modify(~ lm(f ~ method-1, data=.x) %>% tidy())
  group_modify(~ aov(f ~ method, data=.x) %>% tidy()) %>% 
  filter(term!="Residuals") %>% 
  select(label, term, p.value)
  
data_ctakes_exact_level1 %>%
  filter(label!="Misc") %>%
  group_by(label) %>%
  group_modify(~ lm(f ~ method-1, data=.x) %>% tidy()) %>%
  #filter(term!="Residuals") %>%
  select(label, term, estimate, p.value) #%>% view()
  
data_ctakes_exact_level1 %>%
  filter(method=="snorx") %>% 
  group_by(label) %>% 
  summarise_at(vars(c("p", "r", "f")), median) %>% 
  ggplot(aes(x=p, y=r)) +
  geom_point()
#   geom_boxplot() + coord_flip() + 
#   scale_x_discrete(limits = rev(levels(data$label))) +
#   ylab("F1 score") + xlab("")



data <- bind_rows(
                  # data_ctakes_weighted,
                  data_ctakes_exact_level1,
                  data_ctakes_exact_level2,
                  #data_spacy, 
                  #data_spacy_classification_snippet,
                  #data_spacy_weighted,
                  data_spacy_lvl2,
                  data_spacy_lvl1,
                  # data_spacy_lvl2_weighted,
                  # data_spacy_lvl1_en_core_web_md,
                  # data_spacy_lvl2_en_core_web_md,
                  data_bow_deepest,
                  data_bow_highest,
                  metrics_entailment_agg, #%>% filter(aggregation=="max"),
                  metrics_entailment_level1 #%>% filter(aggregation=="max") 
                  )


data %>% count(level, method)

data %>% count(label)

data <- data %>% inner_join(method_mappings, by = c(method="old")) %>% 
  select(-method) %>% rename(method=new)

# data <- data %>% transform(method = factor(method, levels=rev(c("snorx", "infoc","cocoa", "cocoa2",
#                                                                 "spacy", "spacy+GBM", "bow")))) %>% as_tibble()
data <- data %>% filter(!is.na(label))


# data %>% count(method, fold) %>% print(n = Inf)
data %>% count(method, fold, level) %>% print(n = Inf)

data %>% count(method)

data %>% count(label)

# data %>% count(method, label) %>% view()

data %>% count(method, level)# %>% view()


labels = c('Food',#'Misc',
           'Financial_strain',
           'Insurance_status',
           'Transportation', 
           'Social_isolation', 'Housing',  'Pain_Scores',
           'Marital_or_partnership_status', 'Depression', 'Anxiety')

data %>%
  mutate(label = ifelse(label!="pain_and_disability", label, "Pain_Scores")) %>% 
  filter(!(label %in% labels)) %>% select(label) %>% unique()

data <- data %>% 
  mutate(label = ifelse(label!="pain_and_disability", label, "Pain_Scores")) %>% 
  mutate(label = factor(label, levels=rev(labels))) %>% filter(!is.na(label))

data %>% group_by(label, method, level) %>% 
  mutate(level=gsub("level-(\\1)", "\\1", level)) %>% 
  summarise(f = round(100*median(f),2)) %>%
  pivot_wider(id_cols = c("label", "level"), names_from = "method", values_from = f) %>%
  arrange(level, label) %>% kable() %>% kable_styling()

# 
# data %>% group_by(label, method, level) %>% 
#   summarise(f = round(100*median(p), 2)) %>%
#   pivot_wider(id_cols = c("label", "level"), names_from = "method", values_from = f) %>%
#   arrange(level, label) %>% kable() %>% kable_styling()

#' ==============================

all_metrics_per_level_fold <- 
  data %>% 
  filter(label != "Misc") %>% 
  filter(level!="weighted") %>%
  filter(method!="cocoa") %>%
  rename(precision="p", recall="r", `f1-score` = "f") %>% 
  pivot_longer(c("precision", "recall", "f1-score"), names_to = "metric") %>% 
  # mutate(level)
  # inner_join(label_counts) %>% 
  mutate(vv = replace(value, is.na(value), 0)) %>% 
  mutate(vw = vv*support) %>% group_by(method, level, metric, fold) %>% 
  summarise(value = sum(vw)/ sum(support)) %>% ungroup()# %>% 
# mutate(value = round(100*value,2)) 

all_metric_median_per_level <- 
  all_metrics_per_level_fold %>% 
  group_by(method, level, metric) %>% 
  summarise(value=median(value))
# %>% 
# pivot_wider(id_cols = c("method",),
#             names_from = c("metric", "level"), values_from = vw,
#             names_sep=": ") %>%
# kable() %>% kable_styling()

all_metric_median_per_level %>% 
  mutate(value = round(100*value,2)) %>% 
  pivot_wider(id_cols = c("method",),
              names_from = c("metric", "level"), values_from = value,
              names_sep=": ") %>%
  kable() %>% kable_styling()


t.test(
  all_metrics_per_level_fold %>% arrange(fold) %>% 
    filter(level=="level-1", metric=="f1-score", method=="NER-hybrid") %>% .$value,
  all_metrics_per_level_fold %>% arrange(fold) %>%  
    filter(level=="level-1", metric=="f1-score", method=="NER-CNN") %>% .$value,
  paired = T
)

label_counts <- vroom("results/label_counts.csv")


# label_counts <- data_ctakes_exact_level1 %>% 
#   filter(method=="cocoa2") %>%
#   select(fold, label, support) %>% group_by(label) %>% summarise(n=sum(support))


# data_ctakes_exact_level1 %>% group_by(label) %>% 
#   filter(!(label %in% c("weighted avg", NA, "macro avg"))) %>% 
#   mutate(label = factor(label, levels=(labels))) %>% 
#   summarise(support = sum(support)) %>% 
#   filter(!is.na(label)) %>% 
label_counts <- label_counts %>% 
  mutate(label = ifelse(label!="pain_and_disability", label, "Pain_Scores")) %>% 
  mutate(label = factor(label, levels=labels))

label_counts %>% 
  ggplot(aes(x=label, y=support)) +
  geom_bar(stat = "identity") +
  coord_flip()

# data %>% mutate(label = gsub("_", " ", ifelse(label!="pain_and_disability", label, "Pain_and_Disability")))

#' ## Supplementary Table 7. ANOVA of factors contributing to F1 model performance
data %>% 
  filter(label != "Misc") %>% 
  filter(level=="level-1") %>%
  filter(method %in% c("cTAKES-SDoH", "hybrid", "CNN", "entailment-RoBERTa")) %>% 
  aov(f~label + fold + method, data=.) %>%  tidy() %>% 
  dplyr::mutate_if(is.numeric, funs(as.character(signif(., 3)))) %>%
  kable() %>% kable_styling()


methods_ <- c("cTAKES-SDoH", "NER-hybrid", "NER-RoBERTa", "entailment-RoBERTa" #"CNN-en_core_web_md"
              )


pairwise_correlation <- list()
for (n1 in seq(1,length(methods_))){
  for (n2 in seq(n1+1, length(methods_))){
  tmp <-  data %>% 
  filter(label != "Misc") %>% 
  filter(level=="level-1") %>%
  pivot_wider(id_cols = c(label, fold),
              names_from = method,
              values_from = f) %>% 
  with(cor.test( eval(as.name(methods_[n1])), eval(as.name(methods_[n2])), 
                 use ="complete.obs", method="spearman")) %>% 
  tidy()
  tmp[,"m1"] <-  methods_[n1]
  tmp[,"m2"] <-  methods_[n2]
  pairwise_correlation <- append(pairwise_correlation, list(tmp))
}
}

pairwise_correlation <- bind_rows(pairwise_correlation)

pairwise_correlation %>% select("m1" ,"m2", "estimate","p.value") %>% 
  dplyr::mutate_if(is.numeric, funs(as.character(signif(., 3)))) %>%
  kableExtra::kable() %>%
  kable_styling()

data %>% 
  filter(label != "Misc") %>% 
  filter(level!="weighted") %>% 
  filter(method!="cocoa") %>%
  # mutate(method=ifelse(method=="cocoa2","cocoa", method )) %>%
  # filter(method %in% c("cocoa", "snorx", "infoc")) %>% 
  # arrange(label) %>%   # mutate(label=gsub("_", " ", as.character(label))) %>% 
  ggplot(aes(x= label, y=f, color=method)) + 
  geom_boxplot(outlier.size = 0) + 
  coord_flip() +
  scale_x_discrete(limits = rev(levels(data$label))) +
  scale_y_continuous(breaks=seq(0,1,0.1)) +
  facet_grid(.~level) +
  # ylim(0,1) +
  ylab("F1 score") + xlab("") +
  theme(#text = element_text(size=20),
        axis.text.y = element_text(size=18)
        ) 
#+ facet_grid(~label, , space = "free")

ggsave("fig/boxplots-level1-2g-all.png")
ggsave("fig/boxplots-level1-2g-all.eps")


data %>% 
  # filter(label != "Misc") %>% 
  filter(level!="weighted") %>% 
  filter(method %in% c("entailment-RoBERTa", "NER-hybrid", "NER-RoBERTa", "cTAKES-SDoH")) %>%
  arrange(method) %>% 
  mutate(label=as.character(label)) %>% 
  mutate(label = ifelse(label=="Marital_or_partnership_status", "Marital/Partnership",label)) %>%
  mutate(method=gsub("\\-","-\n", method),
         method = factor(method, unique(method))) %>% 
  arrange(-support) %>% 
  mutate(label = as.factor(label)) %>% 
  # inner_join(label_counts)
  ggplot(aes(x= label, y=f*100, color=method)) + 
  geom_boxplot(outlier.size = 0) + 
  coord_flip() +
  scale_x_discrete(#limits = rev(levels(data$label))
                   labels = (label_counts %>% mutate(label=as.character(label)) %>% 
                               mutate(label = ifelse(label=="Marital_or_partnership_status", "Marital/Partnership",label)) %>%
                               arrange(label) %>% mutate(lab = glue("{label} ({support})")) %>% .$lab)
                   ) +
  scale_y_continuous(breaks=100*seq(0,1,0.2)) +
  facet_grid(.~level) +
  # ylim(0,1) +
  # ylab("F1 score, %") + xlab("") +
  labs(colour="method",
       x="", y="F1 score, %") +
  theme(#text = element_text(size=20),
    strip.text.x = element_text(size = 16),
    axis.text.x = element_text(size=12),
    axis.text.y = element_text(size=16),
    legend.text = element_text(size=12),
    legend.title = element_text(size=16),
    legend.key.height=unit(2, "cm")
  )
#+ facet_grid(~label, , space = "free")

ggsave("fig/boxplots-all-ctakes-sdoh.png")
ggsave("fig/boxplots-all-ctakes-sdoh.eps")



fun_mean <- function(x){
  return(data.frame(y=mean(x),
                    label=paste0(round(mean(x,na.rm=T), 2), collapse="")))}

fun_median <- function(x){
  return(data.frame(y=mean(x),
                    label=paste0(round(median(x,na.rm=T), 2), collapse="")))}

addline_format <- function(x,...){
  gsub('[-\\s]','\n',x)
}

#' %% all metrics ====================


all_metrics_per_level_fold %>%
  mutate(value = 100*value) %>%
  mutate(method=ordered(method, rev(method_mappings$new %>% unique()))) %>% 
  ggplot(aes(x=method, color=level, y=value)) +
  geom_boxplot() +
  scale_y_continuous(breaks=100*seq(0,1,0.1)) +
  coord_cartesian(ylim=c(0,110)) +
  scale_x_discrete(labels = addline_format) +
  stat_summary(fun.data = fun_median, geom="text", vjust=-2.,
               position = position_dodge(width = 0.75)) +
  theme(text = element_text(size=11)) +
  facet_grid(metric~.) +
  xlab("") + ylab("value, %") 

ggsave("fig/aggregated-metrics-all.eps", height = 9, width = 7)
ggsave("fig/aggregated-metrics-all.png", height = 9, width = 7)

#' ======= 

# data %>% 
#   filter(label != "Misc") %>% 
#   filter(level!="weighted") %>%
#   filter(method!="cocoa") %>% 
#   # mutate(level)
#   # group_by(method, label, level) %>% summarize(vv=median(f)) %>%
#   rename(vv= f) %>% 
#   inner_join(label_counts) %>% 
#   mutate(vv = replace(vv, is.na(vv), 0)) %>% 
#   mutate(vw = vv*n) %>% group_by(method, level, fold) %>% 
#   summarise(vw = sum(vw)/ sum(n)) %>% 
#   pivot_wider(id_cols = "method", names_from = level, values_from = vw) %>% 
#   select(`level-1`, `level-2`) 

#' Table -- weighted averages


# 
# metric_median_per_level <- 
#   data %>% 
#   filter(label != "Misc") %>% 
#   filter(level!="weighted") %>%
#   filter(method!="cocoa") %>% 
#   # mutate(level)
#   group_by(method, label, level) %>% summarize(vv=median(f)) %>%
#   inner_join(count_per_level_1) %>% 
#   mutate(vv = replace(vv, is.na(vv), 0)) %>% 
#   mutate(vw = vv*n) %>% group_by(method, level) %>% 
#   summarise(vw = sum(vw)/ sum(n)) %>% 
#   pivot_wider(id_cols = "method", names_from = level, values_from = vw) %>% 
#   select(`level-1`, `level-2`) 
# 
# metric_median_per_level %>% 
#   mutate_all(~scales::percent(., accuracy = 0.01)) %>% 
#   kableExtra::kable() %>%
#   kable_styling()




#'  pairwise T-tests

source("t_test_per_group.R")
library(dplyr)
library(tidyverse)
# library(data.table)
library(rstatix)

t_test_per_group_safe <- function(data, form,
                                  left_ = "CNN",
                                  right_ = "hybrid",
                                  fold="fold",
                                  grouping = c("label", "level"),
                                  paired=F
){
  if (length(form[[3]])==1){
    names <-  form[[3]]
  }else{names <- form[[3]][[2]]}
  print(glue("left: {left_}"))
  print(glue("right: {right_}"))
  tmp <- data %>% pivot_wider(id_cols = c(grouping, fold),
                       names_from = names,   values_from =form[[2]]) %>% 
    filter_at(vars(!!as.name(left_), !!as.name(right_)), 
              dplyr::all_vars(!is.na(.))
    ) 
    # group_by_at(vars(one_of(grouping)))
  # browser()
  tmp %>% 
    arrange(!!! rlang::syms(grouping), !!as.name(fold)) %>% 
    filter(!is.na(!!as.name(left_)) & !is.na(!!as.name(right_))) %>% 
    group_by_(.dots = all_of(grouping)) %>%
    filter(n()>1 #&& 
                 #var(!!as.name(left_))>0.0 && var(!!as.name(right_))>0.0
                 ) %>% #& all(var(across(vars(left_, right_)))>0.0)
    summarise(t.test(!!as.name(left_), !!as.name(right_), paired=paired) %>% tidy())
}


#' -----------------------
methods <- data %>% count(method) %>% .$method
model_pairs <- crossing(m1=methods, m2=methods) %>% filter(ordered(m1, methods)> ordered(m2, methods)) 

t_test <- list()

for (i in 1:nrow(model_pairs)) {
  print(model_pairs[i,])
  left_ <- as.character(model_pairs[i,"m1", drop=T])
  right_ <- as.character(model_pairs[i,"m2", drop=T])
  
  t_test_ <- data %>% t_test_per_group_safe(f~method, 
                                            left_=left_, 
                                            right_=right_) %>% 
    select(label, level, estimate, p.value) %>%
    mutate(m1=left_, m2 = right_)
  
  t_test <- c(t_test, list(t_test_))
}

t_test <- t_test %>% bind_rows()

t_test_display <- 
  t_test %>% 
  mutate(level = gsub("level-", "", level)) %>% 
  mutate(m1=ordered(m1, rev(methods)),
         m2=ordered(m2, rev(methods)),
         estimate = - round(estimate* 100),
  ) %>% 
  select(m1, m2, level, label, estimate, p.value) %>% 
  arrange(m1,m2,label, level) %>% 
  rename(`model 1` = m1, `model 2` = m2, 
         `difference, % points`=estimate, `p-value`=p.value)


t_test_display %>%
  dplyr::mutate_if(is.numeric, funs(as.character(signif(., 1)))) %>% 
  knitr::kable() %>% kable_styling()
#######

# 
# t_test <- bind_rows(t_test)
# t_test %>% count(level)

t_test %>% select(comparison, label, level,
                  # estimate1, estimate2,
                  estimate, p.value) %>% 
  mutate(estimate=-100*round(estimate,2),
         # estimate1=100*round(estimate1,2),
         # estimate2=100*round(estimate2,2),
         level=gsub("level-(\\1)","\\1", level)) %>% 
  rename(`difference, % points` = estimate) %>% 
  dplyr::mutate_if(is.numeric, funs(as.character(signif(., 1)))) %>% 
  knitr::kable() %>% kable_styling()
# install.packages("docxtools")
# library("docxtools")
# 
# t_test %>% 
#   pivot_wider(id_cols=c("label", "comparison"),
#               names_from = level, values_from = p.value) %>% 
#   knitr::kable() %>% kable_styling()
# 
# t_test %>% 
#   pivot_wider(id_cols=c("label", "comparison"),
#               names_from = level, values_from = p.value) %>% 
#   # format_engr() %>%
#   dplyr::mutate_if(is.numeric, funs(as.character(signif(., 1)))) %>% 
#   knitr::kable() %>% kable_styling()
  # kableExtra::kable() %>%
  # kable_styling()

t_test %>%  pivot_wider(id_cols=c("label", "comparison"),
                        names_from = level, values_from = estimate)

t_test_display %>% write.csv(file="results/t-test-level-1-2g-f1.csv") 


t_test_l1_l2g <- list()

for (i in 1:nrow(model_pairs)) {
  print(model_pairs[i,])
  left_ <- as.character(model_pairs[i,"m1", drop=T])
  right_ <- as.character(model_pairs[i,"m2", drop=T])
  
  t_test_l1_l2g_ <- all_metrics_per_level_fold %>% 
    t_test_per_group_safe(value~method, 
                         left_=left_, 
                         right_=right_,
                         fold="fold",
                         grouping = c("metric", "level"),
                         ) %>% 
    select(metric, level, estimate, p.value) %>%
    mutate(m1=left_, m2 = right_)
  
  t_test_l1_l2g <- c(t_test_l1_l2g, list(t_test_l1_l2g_))
}

t_test_l1_l2g <- t_test_l1_l2g %>% bind_rows()


t_test_l1_l2g %>% 
  select(m1, m2, metric, level, estimate, `p.value`) %>%
  mutate(estimate=-100*round(estimate,2),
         # estimate1=100*round(estimate1,2),
         # estimate2=100*round(estimate2,2),
         level=gsub("level-(\\1)","\\1", level)) %>% 
  rename(`difference, % points` = estimate) %>% 
  dplyr::mutate_if(is.numeric, funs(as.character(signif(., 1)))) %>% 
  knitr::kable() %>% kable_styling()

all_metrics_per_level_fold %>% group_by(across(c(-fold, -value))) %>%
  mutate(method=ordered(method, levels(methods))) %>% 
  summarise(value=median(value)) %>% knitr::kable() %>% kable_styling()

# t_test_level_1 %>% 
#   arrange(p.value) %>% 
#   kableExtra::kable() %>%
#   kable_styling()

