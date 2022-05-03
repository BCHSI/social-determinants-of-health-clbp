#setwd("/Users/dlituiev/repos/infocommons/social-determinants-of-health-cocoa")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# setwd(getSrcDirectory()[1])
getwd()

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
library(tidyverse)
library(kableExtra)
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
#'  pairwise T-tests
source("t_test_per_group.R")

theme_set(theme_bw())
dir.create("fig")


method_mappings <- as_tibble(as.data.frame(list(new=c(
  `entailment-RoBERTa`="entailment-RoBERTa",
  `entailment-RoBERTa-mean`="entailment-RoBERTa-mean",
  bow="NER-hybrid",
  # spacy="CNN", 
  `en_core_web_md`="NER-CNN-en_core_web_md", 
  `roberta_lr_5e_5_bs_16`="NER-RoBERTa", 
  cocoa2 = "cTAKES-SDoH"
  # infoc="cTAKES-InfoCommons", 
  # snorx="cTAKES-default"
))),
rownames = "old") %>% 
  mutate(new = factor(new, levels = new))


fn_interrater_agreement <-"results/inter-rater-agreement-f1-kappa.csv"
interrater_agreement <- vroom(fn_interrater_agreement)


fn_bow <- "results/metrics-bow-with-sklearn-report-fold-level2.csv"
data_bow <- vroom(fn_bow)
data_bow <- data_bow %>% rename(f=`f1-score`, p=precision, r=recall)
data_bow[, "method"] <- "bow"

fn_spacy <- "results/metrics-spacy-en_core_web_md-level2-sklearn.csv"
data_spacy <- vroom(fn_spacy) %>% filter(!is.na(label))

pattern <- "results/metrics-spacy-{method}-level2-sklearn-test.csv"
# "metrics-spacy-roberta_lr_5e_5_bs_16-native-level2-test.csv"
data_spacy <- read_tables(pattern)

data_spacy <- data_spacy %>% rowwise() %>%
  # mutate(label = str_c(str_split(label, ":", simplify = T)[,1:2], collapse = ":")) %>%
  rename(f=`f1-score`,
         # f = 2*p*r / (p + r),
         p=precision, r=recall,
  )

# data_spacy[, "method"] <- "spacy"


# pattern <- "results/metrics-ctakes-with-sklearn-{method}-fold-level-1-native.csv"
pattern <- "results/metrics-ctakes-with-sklearn-{method}-fold-exact-match.csv"
data_ctakes_exact_level2 <- read_tables(pattern) %>%
  rename(f=`f1`,
         # f = 2*p*r / (p + r),
         p=precision, r=recall,
         support=n,
  ) %>%
  mutate(level="level-1",
         f=ifelse(support>0, f, NA),)

fn_entailment <- "results/metrics-entailment-roberta-level-2.csv"
data_entailment <- vroom(fn_entailment) %>% filter(!is.na(support)) %>% 
  mutate(method="entailment-RoBERTa") %>% rename(f=`f1-score`, p=precision, r=recall) %>%
  mutate(method=ifelse(aggregation=="mean", "entailment-RoBERTa-mean", method)) %>% 
  mutate(label=ifelse(label=="Depression: Family hx", "Depression: Family hx: depression", label))

vroom(fn_entailment) %>% filter(label=="Depression: Family hx: Depression")

data_entailment %>% count(label)

#' Concatenate data
data <- bind_rows(data_spacy, data_bow, data_ctakes_exact_level2, data_entailment) %>% 
  mutate(f = ifelse(is.na(f) & p==0 & r==0, 0, f))


data <- data %>% inner_join(method_mappings, by=c(method="old")) %>% 
  select(-method) %>% rename(method="new")

data %>% count(label, method) %>% pivot_wider(id_cols=c("label"), names_from = "method", values_from = "n") %>% view()

#' QC
data %>% count(label, method) %>%
  pivot_wider(id_cols = label, names_from = method, values_from = n) %>% 
  rowwise() %>% filter(mean(c_across(where(is.numeric)))>0.8)

# data %>% count(label, method) %>%
#   pivot_wider(id_cols = label, names_from = method, values_from = n) %>% view()

nonzero_sum <- function(x)  sum(!is.na(x))

support <- data %>% group_by(method, label) %>% summarise(support = sum(support)) %>% 
  pivot_wider("label", names_from="method", values_from = "support") %>% select(label, `NER-CNN-en_core_web_md`) %>%
  rename(support=`NER-CNN-en_core_web_md`)  #%>% 
  # mutate(label_str = glue("{label} ({support})"))


valid_labels <- 
  data %>% count(label, method) %>%
    pivot_wider(id_cols = label, names_from = method, values_from = n) %>% 
  rowwise() %>% 
  filter(nonzero_sum(c_across(where(is.numeric)))>2) %>% 
  select(label) %>% inner_join(support)
  # .$label


data %>% group_by(method, label) %>% summarise(support = sum(support)) %>% 
  pivot_wider("label", names_from="method", values_from = "support")

inner_join(interrater_agreement, data) %>% 
  aov(f ~ `span overlap F1, %` , data=.) %>% tidy()


res_anova_interrater <- inner_join(interrater_agreement, data) %>% with(anova(lm(f ~ `span overlap F1, %`))) %>% tidy()

res_anova_interrater %>% mutate(sumsq / sum(sumsq))

# afss <- af$"Sum Sq"

inner_join(interrater_agreement, data) %>% 
  aov(f ~ `label` , data=.) %>% tidy()


inner_join(interrater_agreement, data) %>% with(anova(lm(f ~ `span overlap F1, %`),
                                                      lm(f ~ `span overlap F1, %`+`label`))) %>% tidy()


inner_join(interrater_agreement, data) %>% with(BIC(lm(f ~ `span overlap F1, %`),
                                                      lm(f ~ `label`)))# %>% tidy()

#' ========
#' 
label_sets = list(
  c("Anxiety", "Depression", "Pain", "Food"),
  c("Housing", "Transportation", "Financial", "Insurance"),
  c("Marital", "Social")
  
)

labels <- vroom("results/label-order.txt", delim="|") #[,"label", drop=T])

labels <- labels %>% mutate(label=factor(label, label)) %>% 
  mutate(label_level_1 = toTitleCase(str_split_fixed(str_split_fixed(label, ": ", 2)[,1], "_", 2)[,1]),
         label_level_2 = str_split_fixed(label, ": ", 2)[,2],
         label_level_2 = as.factor(label_level_2),
  ) %>% 
  inner_join(valid_labels, by="label") %>% 
  mutate(label=factor(label, label)) %>% 
  # arrange(label) %>% 
  rev() %>% 
  rowwise() %>% 
  mutate(label_str = glue("{label_level_2} ({support})"),
         label_str = factor(label_str))

#####################

data %>% pivot_wider(id_cols = c(label, fold),
                     names_from = "method",   values_from = "f") %>% 
  filter(label %in% c("weighted avg", "macro avg")) %>% arrange(label, fold)



data %>%
  filter((!grepl("non-specific", label)) &
           (!grepl("null", label)) &
           (!grepl("avg", label)) &
           (!grepl("accuracy", label))
         ) %>% 
  mutate(label_level_1 = str_split_fixed(label, ": ", 2)[,1]) %>% 
#  filter(label_level_1=="")
  group_by(fold, method, label_level_1) %>% 
  summarise(
    f_w = sum(f*support)/sum(support),
    # support = sum(support)
  ) %>% 
  pivot_wider(id_cols = c(label_level_1, fold),
              names_from = "method",   values_from = "f_w") %>% 
  group_by(label_level_1) %>%
  summarise_all(median) %>% ungroup()
#%>% 
 # filter(label_level_1!="Food") #%>% 
  # summarise_if(is.numeric, median)



data %>% 
  filter((!grepl("non-specific", label)) &
           (!grepl("null", label)) &
           (!grepl("avg", label))
  ) %>% 
  group_by(fold, method) %>% 
  summarise(
    f_w = sum(f*support)/sum(support)
  ) %>% 
  pivot_wider(id_cols = c(fold),
              names_from = "method",   values_from = "f_w") %>% 
  ungroup() %>% 
  summarise_all(median)

#%>% filter(method ==)

######################


page <- 1

data %>% 
  select(-support, -label_level_1) %>% 
  # filter(label %in% valid_labels$label) %>% 
  # filter(method)
  filter(label!="Anxiety: Level of anxiety") %>%  # this class was only annotated by Sam
  inner_join(labels) %>% 
  # count(label_level_1)
  filter(label_level_2!="") %>% 
  rowwise() %>% 
  filter(label_level_1 %in% label_sets[[page]] ) %>% 
  # inner_join(valid_labels, by="label") %>% 
  # arrange(label) %>% 
  # rev() %>% 
  # mutate(label_str = glue("{label_level_2} ({support})")) %>% 
  arrange(method) %>% 
  mutate(method=gsub("\\-","-\n", method),
         method = factor(method, unique(method))) %>% 
  rename(F1 = f, precision=p, recall=r) %>% 
  pivot_longer(c("F1","precision", "recall"), names_to = "metric", values_to = "value") %>% 
  ggplot(aes(x=label_str, y=value*100, color=method)) + 
  geom_boxplot(outlier.size = 0, position = position_dodge(preserve = "single")) + 
  coord_flip() +
  scale_x_discrete(
    #limits = rev(levels(data$label))
    ) +
  scale_y_continuous(breaks=100*seq(0,1,0.2)) +
  # ylim(0,1) +
  ylab("metric, %") + xlab("") +
  theme(#text = element_text(size=20),
    axis.text.y = element_text(size=12),
    axis.text.x = element_text(size=12),
    strip.text.y = element_text(size = 16),
    strip.text.x = element_text(size = 16),
    legend.key.height=unit(2, "cm"),
  ) + 
  facet_grid(label_level_1~metric, space = "free",
             scales = "free_y")

ggsave(glue("fig/metrics-level2-detailed-page-{page}.png"))
ggsave(glue("fig/metrics-level2-detailed-page-{page}.eps"))


source("t_test_per_group.R")
library(rstatix)

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
    group_by(label) %>% filter(n()>1 && var(!!as.name(left_))>0.0 && var(!!as.name(right_))>0.0) %>% #& all(var(across(vars(left_, right_)))>0.0)
    summarise(t.test(!!as.name(left_), !!as.name(right_), paired=paired) %>% tidy())
}


# data %>%

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
    select(label, estimate, p.value) %>%
    mutate(m1=left_, m2 = right_)
  
  t_test <- c(t_test, list(t_test_))
  }
  
t_test <- t_test %>% bind_rows()

t_test_display <- 
  t_test %>% 
  mutate(m1=ordered(m1, rev(methods)),
         m2=ordered(m2, rev(methods)),
         estimate = - round(estimate* 100),
         ) %>% 
  select(m1, m2, label, estimate, p.value) %>% 
  arrange(m1,m2,label) %>% 
  rename(`model 1` = m1, `model 2` = m2, 
         `difference, % points`=estimate, `p-value`=p.value)

t_test_display %>% 
  write_csv("results/metric-comparison-level2-detailed-all.csv")

t_test_display %>% 
  # dplyr::mutate_if(is.numeric, funs(as.character(signif(., 1)))) %>% 
  dplyr::mutate(`p-value` = as.character(signif(`p-value`, 1))) %>% 
  knitr::kable() %>% kable_styling()


results_t_test %>% 
  arrange(p.value) %>% 
  kableExtra::kable() %>%
  kable_styling()

