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
library(stringr)
library(glue)

theme_set(theme_bw())

dir.create("fig")

metadata <- vroom("data/skippy-2020-02-12-data-splits-2022-01-11.csv", col_types=cols(note_id=col_integer()))
sentence_num <- vroom("results/skippy-entities-with-sentence-num.csv", 
                      col_types=cols(sentence_id=col_integer(), note_id=col_integer(),
                                     start=col_integer(), end=col_integer(),
                                     start_sent=col_integer()
                                     )) %>% 
  mutate(label_ref=ifelse(label_ref=="Depression: Family hx", "Depression: Family hx: Depression", label_ref))
#' Entailment
#' 
# fn <- "results/skippy-entailment-2022-01-13-pad-before-1.csv"
# fn <- "results/skippy-entailment-2022-01-18-pad-before-0.csv"
fn <- "results/entailment-roberta-all-combinations-2022-01-24.csv"
fn <- "results/entailment-roberta-all-combinations-2022-01-26.csv"

padding <- as.integer(str_match(fn, "before-(?<padding>[0-1]{1})\\.csv")[,"padding"])
padding <- ifelse(is.na(padding), 0, padding)

# entailment <- entailment %>% mutate(label_level_1 = str_split_fixed(label, ": ", 2)[,1]) 
#%>% select(label, label_level_1)

"
Sent Prem Hyp label_probe label_ref pred
0    ...  0   A           A        ent
0    ...  0   B           A        x
0    ...  0   A           B        x
0    ...  0   B           B        ent
"

entailment <- vroom(fn, col_types=cols(note_id=col_integer(), 
                                       prediction=col_factor(),
                                       # note_type=col_factor(),
                                       # label_probe=col_factor(),
                                       premise_len = col_integer(),
                                       )
                    ) %>% rename(idx =`...1`, label_probe = label) %>% 
  mutate(label_probe=ifelse(label_probe=="Depression: Family hx", "Depression: Family hx: Depression", label_probe),
         # label_ref=ifelse(label_ref=="Depression: Family hx", "Depression: Family hx: depression", label_ref)
         )




entailment <- entailment %>% filter(!grepl("unsheltered", hypothesis))
         
entailment %>% count(label_probe)             


entailment %>% nrow()
colnames(entailment)

entailment <- inner_join(entailment, 
                         metadata %>% select(-annotator, -filename, -n_entities, -datetime) %>% unique(),
                         by = c("note_id"))
entailment %>% nrow()

# entailment <- entailment %>% select(-sentence, -label)

# entailment <- entailment %>% filter(label_probe %in% label_ref)

entailment <- entailment %>% left_join(sentence_num %>% select(-text, -start, -end, -id, -annotator) %>% unique(),
                                        by = c("note_id", `premise`="text_sent"),
                                        suffix=c("_probe", "_ref") ) %>% 
                mutate(label_lvl1_probe = str_split_fixed(label_probe, ": ", 2)[,1],
                       label_lvl1_ref = str_split_fixed(label_ref, ": ", 2)[,1],
                )

entailment %>% nrow()


#' for 0-padding
label_probe_rating <- entailment %>% filter(!is.na(sentence_id)) %>% 
  group_by(note_id, sentence_id, premise, label_probe) %>% 
  summarize(label_probe_holds =label_probe %in% label_ref) %>% 
  unique() %>% arrange(note_id, sentence_id)

label_probe_rating_level_1 <- entailment %>% filter(!is.na(sentence_id)) %>% 
  group_by(note_id, sentence_id, premise, label_lvl1_probe) %>% 
  summarize(label_lvl1_probe_holds =label_lvl1_probe %in% label_lvl1_ref) %>% 
  unique() %>% arrange(note_id, sentence_id)


#' for 1-padding
#' Merge ground truths for preceding sentence to the probes of current sentence

if (padding>0){
  
  label_probe_rating_shifted <- 
    entailment %>%  mutate(sentence = sentence - 1) %>%
    select(note_id, sentence, label_ref, premise, label_level_1) %>% unique() %>% inner_join(entailment %>% select(label_probe, note_id, sentence) %>% unique()) %>% 
    group_by(note_id, sentence, premise, label_probe, label_level_1) %>% 
    summarize(label_probe_holds =label_probe %in% label_ref) %>% 
    unique() %>% ungroup() %>% 
    group_by(note_id, sentence, label_level_1) %>% 
    mutate(any_holds = any(label_probe_holds)) %>% 
    arrange(note_id, sentence) %>% filter(any_holds) %>% select(-any_holds)
  
  
  label_probe_rating <- bind_rows(label_probe_rating, label_probe_rating_shifted) %>% 
      bind_rows(label_probe_rating, label_probe_rating_shifted) %>% 
      unique() %>% arrange(note_id, sentence) 
  
  }

entailment_simple <- entailment %>% unique() %>% 
  inner_join(label_probe_rating %>% unique() )

entailment_simple %>% nrow()


entailment_simple <- entailment_simple %>% 
  mutate(label_level_1 = str_split_fixed(label_ref, ": ", 2)[,1]) %>% 
  mutate(label_level_1 = ifelse(label_level_1!="pain_and_disability", label_level_1, "Pain_and_Disability"))

entailment_simple %>% nrow()


entailment_simple %>% filter(label_ref=="Food: Fruit and vegetable intake",
                             label_ref==label_probe
                             )

entailment_simple %>% filter(label_ref=="Depression: Family hx: Depression",
                             # prediction=="entailment",
                             label_ref==label_probe
)

#'============
entailment_simple_lvl1 <- entailment %>% unique() %>%
  inner_join(label_probe_rating_level_1 %>% unique() )

#'============
prediction_max <- entailment_simple %>% 
  group_by(note_id, premise, label_ref, sentence_id, label_probe, fold, label_level_1, label_probe_holds) %>% 
  summarise(prediction = "entailment" %in% prediction) %>% ungroup()  %>% unique()


recall_max <- prediction_max %>% 
  filter(label_ref==label_probe) %>% 
  mutate(label=label_ref) %>% 
  group_by(label, fold, label_level_1) %>% 
  summarise(recall = mean(prediction), support=n()) %>% 
  arrange(recall) %>% 
  mutate(label = factor(label, levels = unique(label))) %>% ungroup()

recall_max %>% summarise(mean(recall))

recall_max %>% summarise(recall=sum(recall*support) / sum(support))



recall_raw <- entailment_simple %>% 
  filter(label_ref==label_probe) %>% 
  group_by(label_ref) %>% 
  summarise(recall = mean(prediction=="entailment"), support=n()) %>% 
  arrange(recall)

recall_raw %>% 
  mutate(label = factor(label_ref, levels = label_ref)) %>% 
  ggplot(aes(x=label, y=recall)) + 
  geom_bar(stat="identity") +
  coord_flip()

recall_raw %>% summarise(mean(recall))
recall_raw %>% summarise(recall=sum(recall*support) / sum(support))

entailment_simple %>% 
  filter(label_ref==label_probe) %>% 
  summarise(recall = mean(prediction=="entailment")) 



# entailment %>% 
#   filter(label_ref==label_probe) %>% 
  # filter(prediction=="contradiction") %>% view()

recall_mean <- entailment_simple %>% 
  filter(label_ref==label_probe) %>% 
  mutate(label=label_ref) %>% 
  group_by(note_id, hypothesis, premise, label, fold, label_level_1) %>% 
  summarise(entailment = mean(entailment),
            neutral = mean(neutral),
            contradiction = mean(contradiction)) %>% 
  ungroup() %>% rowwise() %>% 
  mutate(prediction=c("entailment", "neutral", "contradiction")[which.max(c(entailment, neutral, contradiction))]) %>% 
  group_by(label, fold, label_level_1) %>% 
  summarise(recall = mean(prediction=="entailment"), support=n()) %>% 
  arrange(recall) %>% 
  mutate(label = factor(label, levels = unique(label)))


entailment_simple %>% 
  filter(prediction == "entailment") %>% 
  group_by(note_id, premise, label_ref, label_probe) %>%
  summarise(support=n()) %>% 
  filter(grepl("Marital", label_ref)) %>% 
  mutate(label_probe_lvl2 = str_split_fixed(label_probe, ": ", 2)[,2],
         label_ref_lvl2 = str_split_fixed(label_ref, ": ", 2)[,2],) %>% 
  pivot_wider(id_cols = label_ref_lvl2,
              names_from = label_probe_lvl2,
              values_from = support,
              values_fn=sum,
              values_fill = 0) %>% 
  kable() %>% kable_styling()
  

entailment_simple %>%
  filter(prediction == "entailment") %>% 
  mutate(label_probe_lvl2 = str_split_fixed(label_probe, ": ", 2)[,2],
                      label_ref_lvl2 = str_split_fixed(label_ref, ": ", 2)[,2],) %>% 
  filter(label_ref_lvl2=="Married",
         label_probe_lvl2=="Widowed",
         label_probe_holds) %>% 
  select(note_id, premise, hypothesis, label_probe_holds)# %>% view()

  

label_order <- recall_max %>% group_by(label) %>% 
  summarise(recall = mean(recall)) %>% 
  arrange(recall) %>% 
  .$label

bind_rows(
  recall_max %>% mutate(aggregation="max"),
  recall_mean %>% mutate(aggregation="mean")
) %>% 
  mutate(label = factor(label, levels = label_order)) %>% 
  ggplot(aes(x=label, y=recall, colour=aggregation)) + 
  geom_boxplot()+
  # geom_bar(position=position_dodge(), stat="identity") +
  coord_flip()


#' Recall on level-1


#' Precision on level-2
precision_max <- 
  prediction_max %>% 
  filter(!grepl(": NA", label_probe),
         !grepl(": NA", label_ref)) %>%  # no NA labels
  filter(prediction) %>% 
  mutate(#true_positive = label_ref==label_probe,
         label = label_probe) %>%
  group_by(label, fold) %>% 
  summarise(precision = mean(label_probe_holds),
            support = n())

precision_mean <- 
  entailment_simple %>% 
  filter(prediction == "entailment") %>% 
  filter(!grepl(": NA", label_probe),
         !grepl(": NA", label_ref)) %>%  # no NA labels
  mutate(true_positive = label_ref==label_probe,
         label = label_probe) %>% 
  group_by(label, fold, label_level_1) %>% 
  summarise(precision = mean(true_positive),
            support = n())

label_order <- precision_max %>% group_by(label) %>% 
  summarise(precision = mean(precision)) %>% 
  arrange(precision) %>% 
  .$label

#' # Precision
bind_rows(precision_max %>% mutate(aggregation="max"),
           precision_mean %>% mutate(aggregation="mean") 
           ) %>% 
  mutate(label = factor(label, levels = label_order)) %>% 
  ggplot(aes(x=label, y=precision, colour=aggregation)) + 
  geom_boxplot()+
  # geom_bar(position=position_dodge(), stat="identity") +
  coord_flip()


all_metrics <- 
  bind_rows(
    full_join(precision_max %>% select(-support), recall_max) %>% mutate(aggregation="max"),
    full_join(precision_mean  %>% select(-support), recall_mean) %>% mutate(aggregation="mean") 
            ) %>% 
  rowwise() %>% 
  mutate(`f1-score` = ifelse(!is.na(precision) & !is.na(recall),
                             2*precision*recall/(precision + recall),
                             max(precision, recall, na.rm=T))
         )

# entailment %>% filter(label_probe == "Anxiety: Signs and symptoms of anxiety",label_probe==label_ref) %>% view()
all_metrics %>% filter(label == "Food: Provision of food")
all_metrics %>% filter(label=="Transportation: Has access to public transport vehicle")

all_metrics %>% filter(label=="Depression: Family hx: Depression")

label_order <- all_metrics %>% group_by(label) %>% 
  summarise(`f1-score` = mean(`f1-score`, na.rm =T)) %>% 
  arrange(`f1-score`) %>% 
  .$label



label_order <- all_metrics %>% group_by(label) %>% 
  filter(aggregation=="max") %>% 
  summarise(f1 = median(`f1-score`, na.rm=T)) %>% 
  mutate(f1= ifelse(is.na(f1),0, f1)) %>% 
  arrange(f1) %>% 
  .$label


all_metrics %>% 
  mutate(label = factor(label, levels = label_order)) %>% 
  pivot_longer(c("f1-score", "precision", "recall")) %>%  
  # mutate(label = factor(label, levels = label_order)) %>% 
  ggplot(aes(x=label, y=value, colour=aggregation
             )) + 
  geom_boxplot()+
  coord_flip() +
  facet_grid(.~name)


ggsave(glue("fig/entailment-prf-level2-pad-before-{padding}.png"))
ggsave(glue("fig/entailment-prf-level2-pad-before-{padding}.eps"))

#' # Level-1

all_metrics_agg <- all_metrics %>%
  group_by(label_level_1, fold, aggregation) %>% 
  summarise(precision = weighted.mean(precision, ifelse(!is.na(support), support, 0), na.rm=T),
            `f1-score` = weighted.mean(`f1-score`, ifelse(!is.na(support), support, 0),  na.rm=T),
            recall = weighted.mean(recall, ifelse(!is.na(support), support, 0),  na.rm=T),
            # recall_sum = sum(recall* support,  na.rm=T)/ sum(support, na.rm=T),
            support = sum(support,  na.rm=T),
  )

all_metrics_agg %>%
  filter(!is.na(label_level_1)) %>% 
  rename(label = label_level_1) %>% 
  # mutate(label = factor(label, levels = label_order)) %>% 
  ggplot(aes(x=label, y=`f1-score`, colour=aggregation)) + 
  geom_boxplot() +
  # coord_cartesian(xlim=c(0,1)) +
  ylim(0,1.0) +
  # geom_bar(position=position_dodge(), stat="identity") +
  coord_flip()

ggsave(glue("fig/entailment-f1-level2-agg-pad-before-{padding}.png"))
ggsave(glue("fig/entailment-f1-level2-agg-pad-before-{padding}.eps"))


all_metrics %>% ungroup() %>% 
  mutate(support = ifelse(!is.na(support), support, 0)) %>% 
  summarise(
    `f1-score` = weighted.mean(`f1-score`, support, na.rm=T),
    `precision` = weighted.mean(precision, support, na.rm=T),
    `recall` = weighted.mean(recall, support, na.rm=T),
            )


all_metrics_agg %>%
  filter(!is.na(label_level_1)) %>% 
  rename(label = label_level_1) %>% 
  pivot_longer(c("f1-score", "precision", "recall")) %>% 
  # mutate(label = factor(label, levels = label_order)) %>% 
  ggplot(aes(x=label, y=value, colour=aggregation)) + 
  geom_boxplot() +
  # coord_cartesian(xlim=c(0,1)) +
  ylim(0,1.0) +
  # geom_bar(position=position_dodge(), stat="identity") +
  coord_flip() +
  facet_grid(~name)

ggsave(glue("fig/entailment-prf-agg-pad-before-{padding}.png"), width = 8, height = 6)
ggsave(glue("fig/entailment-prf-agg-pad-before-{padding}.eps"), width = 8, height = 6)


#' # Level-1 analysis

prediction_level1_max <- entailment_simple_lvl1 %>% 
  group_by(note_id, premise, label_lvl1_ref, sentence_id, label_lvl1_probe, fold,
           label_lvl1_probe_holds) %>% 
  summarise(prediction = "entailment" %in% prediction) %>% ungroup()  %>% unique()


recall_level1_max <- prediction_level1_max %>% 
  filter(label_lvl1_ref==label_lvl1_probe) %>% 
  mutate(label=label_lvl1_probe) %>% 
  group_by(label, fold) %>% 
  summarise(recall = mean(prediction), support=n()) %>% 
  arrange(recall) %>% 
  mutate(label = factor(label, levels = unique(label))) %>% ungroup()

precision_level1_max <- 
  prediction_level1_max %>% 
  filter(!grepl(": NA", label_lvl1_probe),
         !grepl(": NA", label_lvl1_ref)) %>%  # no NA labels
  filter(prediction) %>% 
  mutate(#true_positive = label_ref==label_probe,
    label = label_lvl1_probe) %>%
  group_by(label, fold) %>% 
  summarise(precision = mean(label_lvl1_probe_holds),
            support = n())


recall_level1_mean <- entailment_simple_lvl1 %>% 
  filter(label_lvl1_ref==label_lvl1_probe) %>% 
  mutate(label=label_lvl1_ref) %>% 
  group_by(note_id, hypothesis, premise, label, fold) %>% 
  summarise(entailment = mean(entailment),
            neutral = mean(neutral),
            contradiction = mean(contradiction)) %>% 
  ungroup() %>% rowwise() %>% 
  mutate(prediction=c("entailment", "neutral", "contradiction")[which.max(c(entailment, neutral, contradiction))]) %>% 
  group_by(label, fold) %>% 
  summarise(recall = mean(prediction=="entailment"), support=n()) %>% 
  arrange(recall) %>% 
  mutate(label = factor(label, levels = unique(label)))


precision_level1_mean <- 
  entailment_simple_lvl1 %>% 
  filter(prediction == "entailment") %>% 
  filter(!grepl(": NA", label_lvl1_probe),
         !grepl(": NA", label_lvl1_ref)) %>%  # no NA labels
  mutate(positive = label_lvl1_ref==label_lvl1_probe,
         label = label_lvl1_probe) %>% 
  group_by(label, fold) %>% 
  summarise(precision = mean(positive),
            support = n())

all_metrics_level1 <- 
  bind_rows(
    full_join(precision_level1_mean %>% select(-support), recall_level1_mean) %>% mutate(aggregation="mean"),
    full_join(precision_level1_max %>% select(-support), recall_level1_max) %>% mutate(aggregation="max")
  ) %>% 
    rowwise() %>% 
    mutate(`f1-score` = ifelse(!is.na(precision) & !is.na(recall),
                               2*precision*recall/(precision + recall),
                               max(precision, recall, na.rm=T))
    ) 

all_metrics_level1 %>% 
  pivot_longer(c("f1-score", "precision", "recall")) %>% 
  # mutate(label = factor(label, levels = label_order)) %>% 
  ggplot(aes(x=label, y=value, colour=aggregation)) + 
  geom_boxplot() +
  # coord_cartesian(xlim=c(0,1)) +
  ylim(0,1.0) +
  # geom_bar(position=position_dodge(), stat="identity") +
  coord_flip() +
  facet_grid(~name)


all_metrics %>% 
  write_csv("results/metrics-entailment-roberta-level-2.csv")

all_metrics_agg %>% 
  write_csv("results/metrics-entailment-roberta-level-2-agg.csv")

all_metrics_level1 %>% 
  write_csv("results/metrics-entailment-roberta-level-1.csv")

#' ====================
#' # Spot-checking

# False positives
entailment %>%
  filter(prediction == "entailment") %>%
  # filter(!grepl(": NA", label_ref)) %>%  # no NA labels
  mutate(true_positive = label_ref==label_probe,
         label = label_probe) %>%
  filter(!true_positive) %>%
  filter(label=="Financial_strain: Unable to afford medication") %>% view()

# False negatives
entailment %>%
  filter(prediction != "entailment") %>%
  # filter(!grepl(": NA", label_ref)) %>%  # no NA labels
  mutate(positive = label_ref==label_probe,
         label = label_probe) %>%
  filter(positive) %>%
  filter(label=="Financial_strain: Unable to afford medication") %>% view()

entailment %>%
  filter(prediction == "entailment") %>%
  # filter(!grepl(": NA", label_ref)) %>%  # no NA labels
  mutate(true_positive = label_ref==label_probe,
         label = label_probe) %>%
  filter(!true_positive) %>%
  filter(label=="Marital_or_partnership_status: Marital status of parents") %>% view()


entailment %>%
  filter(label_probe == "Depression: Family hx")

entailment %>%
  filter(prediction == "entailment") %>%
  # filter(!grepl(": NA", label_ref)) %>%  # no NA labels
  mutate(positive = label_ref==label_probe,
         label = label_probe) %>%
  filter(!positive) %>%
  filter(label=="Financial_strain: Unable to afford medication") %>% view()


precision_mean %>% arrange(precision) %>%  kable() %>% kable_styling()
# 
# group_by(note_id, premise, text, label) %>% 
#   summarise(prediction = "entailment" %in% prediction) %>% 
#   group_by(label) %>% 
#   summarise(recall = mean(prediction), support=n()) %>% 
#   arrange(recall) %>% 
#   mutate(label = factor(label, levels = label)) 

#' Spot-checking low recall
#' 
#' entailment %>% filter(label=="Housing: Homeless") %>% view()
#' 
#' entailment %>% filter(label=="Housing: Homeless") %>% view()
#' 
#' entailment %>% filter(label=="Housing: Marginally housed") %>% view()
#' 
#' entailment %>%
#'   filter(label_ref==label_probe) %>% 
#'   mutate(label=label_ref) %>% 
#'   filter(prediction!="entailment") %>% 
#'   filter(label=="Financial_strain: Financially secure") %>% view()
#'   # filter(label=="Housing: Homeless") %>% view()
#' # filter(label=="Transportation: Has access to a car") %>% view()
#'   # filter(label=="Social_isolation: Has social support") %>% view()
#' 
#' entailment %>% filter(label=="Anxiety: Anxiety") %>% view()
#' 
#' #' Spot-checking low precision
#' entailment %>%
#'   filter(label_ref!=label_probe) %>%
#'   filter(prediction=="entailment") %>%
#'   filter(label_probe=="Social_isolation: Personal relationship breakdown")%>% 
#'   view()

'
# Methods
To evaluate performance of entailment model, hypothesis sentences were constructed based on NER labels,
e.g. "Has access to car" was phrased as "Patient has access to a car", "Patient drives a car",
and "Patient drives a vehicle".  Each premise sentences (i.e. original text containing a label) was fed together with the each of the hypothesis sentences.
For computation expediency, hypotheses alternative to the ground truth included only other level-2 categories
from the same level-1 parent of the ground truth label, and thus results are not directly comparable to NER.

The results were aggregated per premise sentence (i.e. original text) and label combination either
(1) by taking an average rate of entailment prediction over alternative hypotheses ("mean")
or (2) by scoring a match if at least one of the hypotheses was deemed to be entailed ("max").
The sentences that contained more than one entity 
(either due to presence of multiple entities consecutively or due to inter-rater disagreement) 
were excluded from precision calculation for simplicity of analysis.
Classes with final node labelled as "NA" were phrased as "This describes patient\'s (level-1 topic)", 
and were excluded from precision analysis as more specific categories would fall under that premise.
For classes or folds where positive prediction were missing and thus precision was undefined,
F1 was assigned the value of recall.

# Results


'

"TODO: 
- [] precision on cases with more than one label
  - 
- [] how come F1 for food is that high with low recall?
- [x] remove hypothesis terms that are not in the ground truth corpus

"

entailment %>% group_by(premise, label_ref, label_level_1) %>% count() %>% 
  ungroup() %>%  group_by(premise, label_level_1) %>% count() #%>% filter(n>1)

