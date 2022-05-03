setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# setwd(getSrcDirectory()[1])
getwd()
library(vroom)
library(broom)
library(dplyr)
library(ggplot2)
library(stringr)
library(glue)

# library(extrafont)
# fonts()
# font_import(prompt = TRUE)
# fonts()

note_stats <- vroom("results/note_stats.csv")

note_stats %>% 
  ggplot(aes(x = note_len,
             y = n_entities,
  )) +
  geom_point(alpha=0.5, size=2.5) + 
  theme(text=element_text(size=18)) +
  xlim(0, 30000)+
  ylab("number of entities") + xlab("note length, characters")

note_stats %>% 
  ggplot(aes(x = note_len,
             y = n_entities,
             )) +
  geom_point() +
  facet_grid(~note_type)


counts_per_note_type_no_empty <- vroom("results/counts-per-note-type-no-empty.csv") %>% 
  mutate(label_level_1 = str_split_fixed(label, ": ", 2)[,1]) %>% 
  mutate(label_level_1 = ifelse(label_level_1!="pain_and_disability", label_level_1, "Pain_and_Disability")) %>% 
  pivot_longer(-c("label", "label_level_1"),
               names_to="note_type", values_to = "n_ents")

mapping_note_type <- vroom("data/note_type_short.csv")

counts_per_note_type_no_empty <- counts_per_note_type_no_empty %>% 
  inner_join(mapping_note_type, by=c("note_type"="long")) %>% 
  select(-note_type) %>% 
  rename(note_type="short")


count_note_type <- counts_per_note_type_no_empty %>% group_by(note_type) %>%
  summarise(n_ann_total = sum(n_ents)) %>% 
  inner_join(number_of_uniq_notes) %>% 
  arrange(n_ann_total) %>% 
  mutate(name_count = glue("{str_pad(note_type, 12, side='right')} ({str_pad(round(n_ann_total,1), 5, 'right')} ann / {n_notes} notes)"),
         ann_per_note=n_ann_total/n_notes) %>% 
  mutate(name_count=factor(name_count, levels=rev(name_count)))

count_note_type %>% kable() %>% kable_styling()

count_note_type_string <- count_note_type %>% .$name_count

count_label <- counts_per_note_type_no_empty %>% group_by(label_level_1) %>%
  group_by(label_level_1) %>% summarize(n_labels=sum(n_ents)) %>% arrange(n_labels) %>% 
  mutate(name_count = glue("{str_pad(label_level_1, 0, side='right')} ({n_labels})"))

counts_per_note_type_no_empty %>% 
  group_by(label_level_1, note_type) %>%
  summarise(n_ents = sum(n_ents)) %>%
  filter(label_level_1!="Misc") %>% 
  # group_by(label_level_1, note_type) %>% 
  # count() %>%  
  mutate(label_level_1=factor(label_level_1, levels=order_label),
         note_type=factor(note_type, rev(order_note_type))) %>%
  inner_join(count_label) %>% arrange(n_labels) %>%
  rename(label=name_count) %>%
  mutate(label=factor(label, levels = count_label$name_count)) %>%
  inner_join(count_note_type %>% select(note_type, name_count) %>% 
               rename(note_type_str=name_count)) %>% 
  ggplot(aes(x=label , y=n_ents, fill=note_type_str)) +
  geom_bar(stat="identity", position = position_dodge()) +
  coord_flip() + 
  scale_fill_manual(values=cbPalette,
                    name="Note Type") +
  xlab("") + ylab("number of entities") +
  theme(legend.text=element_text(family = "Monaco", size=12),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12))

ggsave("fig/n_ent_per_type_label.eps")
ggsave("fig/n_ent_per_type_label.png")
  

#'================
#'
data <- vroom("data/annotated-entities.csv") %>% 
  mutate(label_level_1 = str_split_fixed(label, ": ", 2)[,1]) %>% 
  mutate(label_level_1 = ifelse(label_level_1!="pain_and_disability", label_level_1, "Pain_and_Disability"))

data %>% group_by(note_type) %>% 
  count()

mean_n_per_note_per_type_per_label <- 
  data %>% group_by(note_type, note_id, annotator, label, label_level_1) %>% 
  count() %>% group_by(note_type, note_id, label, label_level_1) %>%
  summarize(n=mean(n)) 


data %>% group_by(note_type, note_id, annotator) %>% 
  count() %>% group_by(note_type, note_id) %>% summarize(n=mean(n)) %>% 
  lm(n~note_type-1, data=.) %>% summary()


data_counts = data %>% 
  group_by(note_type, note_id, annotator) %>% 
  count()

data_counts %>% group_by(note_type, note_id) %>% summarize(n=mean(n)) %>% 
  ungroup() %>% 
  aov(n~ note_type, data=.) %>% summary()


data_counts %>% 
  ungroup() %>% 
  aov(n~ note_type + annotator, data=.) %>% tidy()

data %>% count(note_type) 

data %>% group_by(note_type, note_id) %>%
  count() %>% group_by(note_type) %>% 
  summarize(n = mean(n))

#' ## ANOVA: + Label
#' TODO: how to account for notes with no annotation for a given label?
data %>% 
  group_by(note_type, note_id, annotator, label) %>% 
  count() %>% 
  ungroup() %>% 
  aov(n~ note_type + annotator+ label, data=.) %>% tidy()

mapping_note_type <- vroom("data/note_type_short.csv")

data <- data %>% 
  inner_join(mapping_note_type, by=c("note_type"="long")) %>% 
  select(-note_type) %>% 
  rename(note_type="short")



data <- data %>% mutate(note_type=ordered(note_type,
    data %>% group_by(note_type) %>% count() %>% 
      arrange(-n) %>% .$note_type
  ))

#' Boxplot of Labels per Note type
data %>% 
  filter(label_level_1!="Misc") %>% 
  group_by(label_level_1, note_type, note_id) %>% 
  count() %>%  
  ggplot(aes(y= n, color=label_level_1)) +
  geom_boxplot() +
  coord_flip() +
  facet_grid(note_type~.) + 
  theme(#axis.title.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())



# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# The palette with black:
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


order_label <- data %>% group_by(label_level_1) %>% count() %>% arrange(n) %>% .$label_level_1
order_note_type <- data %>% group_by(note_type) %>% count() %>% arrange(n) %>% .$note_type

number_of_uniq_notes <- data %>% select(note_id, note_type) %>% unique() %>% group_by(note_type, note_id) %>% filter(row_number()==1) %>% ungroup()  %>% group_by(note_type) %>% summarize(n_notes=n())


count_note_type_raw <- 
  data %>% 
  group_by(note_type, annotator, note_id) %>% 
  summarise(n_ann = n()) %>% ungroup() %>% 
  group_by(note_type, note_id) %>% 
  summarize(n_ann = mean(n_ann)) %>% ungroup() %>% 
  group_by(note_type) %>% summarise(n_ann_total = sum(n_ann))

data %>% filter(note_type == "Instructions") %>% 
  group_by(note_type, annotator, note_id) %>% 
  summarise(n_ann = n()) %>% ungroup() %>% 
  group_by(note_type, note_id) %>% 
  summarize(n_ann = mean(n_ann)) %>% ungroup() %>% 
  group_by(note_type) %>% summarise(n_ann_total = sum(n_ann))


count_note_type <- count_note_type_raw %>% 
  inner_join(number_of_uniq_notes) %>% 
  mutate(name_count = glue("{str_pad(note_type, 12, side='right')} ({str_pad(n_ann_total, 4, 'right')} ann / {n_notes} notes)"),
         ann_per_note=n_ann_total/n_notes) %>% .$name_count

mean_n_per_note_per_type_per_label

count_label <- mean_n_per_note_per_type_per_label %>% 
  select(note_id, label_level_1) %>% unique() %>% 
  group_by(label_level_1) %>% summarize(n_labels=n()) %>% arrange(n_labels) %>% 
  mutate(name_count = glue("{str_pad(label_level_1, 0, side='right')} ({n_labels})"))

#' Barplot of Labels per Note type
mean_n_per_note_per_type_per_label %>% 
  filter(label_level_1!="Misc") %>% 
  group_by(label_level_1, note_type) %>% 
  count() %>%  
  mutate(label_level_1=factor(label_level_1, levels=order_label),
         note_type=factor(note_type, rev(order_note_type))) %>% 
  inner_join(count_label) %>% rename(label=name_count) %>% 
  arrange(n_labels) %>% 
  mutate(label=factor(label, levels = count_label$name_count)) %>% 
  ggplot(aes(x=label , y=n, fill=note_type)) +
  geom_bar(stat="identity", position = position_dodge()) +
  coord_flip() + 
  scale_fill_manual(values=cbPalette,
                    labels=rev(count_note_type),
                    name="Note Type") +
  xlab("") + ylab("number of entities") +
  theme(legend.text=element_text(family = "Monaco", size=12),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12))




#' ANOVA
data %>% 
  filter(label_level_1!="Misc") %>% 
  group_by(label_level_1, note_type, note_id, annotator) %>% 
  count() %>% 
  ungroup() %>% 
  group_by(label_level_1) %>% 
  group_modify(~ aov(n~ note_type + annotator, data=.x) %>% tidy()) %>% 
  filter(term!="Residuals") %>% 
  select(label_level_1, term, p.value) %>% 
  ggplot(aes(x=label_level_1, y= -log10(p.value), fill=term)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip()
#  pivot_wider(id_cols=label, names_from=term, values_from = p.value)

  


