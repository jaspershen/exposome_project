##avoid source
no_function()

##load data
setwd(r4projects::get_project_wd())
library(tidyverse)
rm(list = ls())

####load metabolomics data
setwd(r4projects::get_project_wd())
load("data_20200511/metabolome/expression_data")
met_expression_data <- expression_data
load("data_20200511/metabolome/sample_info")
met_sample_info <- sample_info
load("data_20200511/metabolome/variable_info")
met_variable_info <- variable_info

met_annotation_info <- readr::read_csv("data_20200511/metabolome/annotation_table.csv")

met_annotation_info %>% dim()

variable_info <-
  variable_info %>% 
  left_join(met_annotation_info, by = c("peak_name" = "name"))
  
load("data_20200511/exposome/expression_data")
exp_expression_data <- expression_data
load("data_20200511/exposome/sample_info")
exp_sample_info <- sample_info
load("data_20200511/exposome/variable_info")
exp_variable_info <- variable_info

setwd("data_analysis/exposomeChemical_metabolome/")

dim(exp_variable_info)

dim(met_variable_info)

temp1 <- 
exp_sample_info$start_date %>% 
  as.Date() %>% 
  data.frame(date = ., 
             exp = 1,
             stringsAsFactors = FALSE)

temp2 <- 
met_sample_info$CollectionDate %>% 
  as.Date() %>% 
  data.frame(date = ., 
             met = 1,
             stringsAsFactors = FALSE)
temp <- 
temp1 %>% 
  dplyr::full_join(temp2, by = "date") %>% 
  arrange(date)

temp <- 
temp %>% 
  tidyr::pivot_longer(cols = -date, names_to = "class", values_to = "value")

temp <- temp %>% 
  dplyr::filter(!is.na(value))

diff <- 
lapply(met_sample_info$CollectionDate %>% as.Date(), function(x){
  x <- x - as.Date(exp_sample_info$start_date) 
  as.Date(exp_sample_info$start_date) [which(x >=0 & x <= 2)]
}) 

names(diff) <- met_sample_info$CollectionDate %>% as.character()

diff <- 
purrr::map2(diff, names(diff), .f = function(x,y){
  if(length(x) == 0){
    return(NULL)
  }
  z <- data.frame(y, x, stringsAsFactors = FALSE)
  colnames(z) <- c("met", "exp")
  z
}) 

diff <- diff[lapply(diff, is.null) %>% unlist() %>% `!`]

diff[[2]] <- diff[[2]][-c(1),]

# diff[[3]] <- diff[[3]][-c(1),]

diff <- purrr::map2(.x = diff, .y = 1:length(diff), .f = function(x,y){
  data.frame(x, group = y, stringsAsFactors = FALSE)
}) %>% 
  do.call(rbind, .)

rownames(diff) <- NULL

diff <- 
diff %>% 
  mutate(exp = as.character(exp), 
         met = as.character(met)) %>% 
  tidyr::pivot_longer(cols = -group,
                      names_to = "class", 
                      values_to = "date")

temp <-
  temp %>% 
  mutate(date = as.character(date)) %>% 
  left_join(diff, by = c("date","class"))

temp$group[is.na(temp$group)] <- "No"

diff

library(plyr)

diff <-
  diff %>% 
  plyr::dlply(.variables = .(group)) %>%
  lapply(function(x){
    x %>% 
      plyr::dlply(.variables = .(class)) %>% 
      do.call(cbind, .)
  }) %>% 
  do.call(rbind, .)
  
diff$exp.date <- as.Date(diff$exp.date)

diff$met.date <- as.Date(diff$met.date)

diff$exp.group <- as.character(diff$exp.group)

plot <- 
temp %>% 
  mutate(date = as.Date(date)) %>% 
  dplyr::mutate(class = factor(class, levels = c("met", "exp"))) %>% 
  ggplot(aes(date, class)) +
  geom_point(aes(fill = group),
             color = "black",
             alpha = 0.6, size = 4,
             shape = 21, show.legend = FALSE) +
  geom_segment(aes(x = exp.date, 
                   xend = met.date, 
                   y = exp.class, 
                   yend = met.class,
                   color = exp.group), data = diff, show.legend = FALSE) +
  scale_fill_manual(values = c("No" = "grey", 
                                "1" = ggsci::pal_aaas()(8)[1],
                                "2" = ggsci::pal_aaas()(8)[2],
                                "3" = ggsci::pal_aaas()(8)[3],
                                "4" = ggsci::pal_aaas()(8)[4],
                                "5" = ggsci::pal_aaas()(8)[5])) +
  scale_x_continuous(trans = "date",
                     breaks = c(as.Date(temp$date)),
                     labels = as.character(temp$date)
  ) +
  # scale_y_continuous(breaks = c("met", "exp"), labels = c("Met", "Exp")) +
  scale_y_discrete(breaks = c("met", "exp"), labels = c("Metabolome", "Exposome")) +
  labs(x = "", y = "") +
  # ggrepel::geom_label_repel(aes(label = as.character(date))) +
  theme_bw() +
  theme(axis.title = element_text(size = 13),
        axis.text.x = element_text(size = 10, 
                                   angle = 45,
                                   vjust = 1, hjust = 1),
        axis.text = element_text(size = 10),
        panel.grid.minor = element_blank())

plot

ggsave(plot, filename = "exp_met_match.pdf", width = 10, height = 7)
ggsave(plot, filename = "exp_met_match.png", width = 10, height = 7)
ggsave(plot, filename = "exp_met_match2.png", width = 14, height = 7)

#-------------------------------------------------------------------------------
###prepare data
dim(met_expression_data)
dim(met_sample_info)

met_sample_info <-
met_sample_info %>% 
  dplyr::filter(CollectionDate %in% unique(diff$met.date))

met_expression_data <- 
  met_expression_data %>% 
  dplyr::select(one_of(met_sample_info$sample_id))

met_sample_info <- 
met_sample_info %>% 
  dplyr::left_join(diff[,c("met.date", "met.group")] %>% dplyr::distinct(met.date, met.group), 
                   by = c("CollectionDate" = "met.date")) %>% 
  dplyr::rename(group = met.group)

exp_sample_info <-
  exp_sample_info %>% 
  dplyr::filter(as.Date(start_date) %in% diff$exp.date)

exp_expression_data <-
  exp_expression_data %>%
  select(-contains("Blank")) %>% 
  t()

exp_expression_data <-
  exp_expression_data %>%
  as.data.frame() %>%
  rownames_to_column(var = "Sample_ID") %>%
  mutate(Sample_ID = stringr::str_replace(Sample_ID, "_[0-9]{1,2}", "")) %>%
  plyr::dlply(.variables = "Sample_ID") %>%
  lapply(function(x){
    apply(x[,-1], 2, mean)
  }) %>%
  do.call(rbind, .) %>% 
  t() %>% 
  as.data.frame()

exp_expression_data <- 
  exp_expression_data %>% 
  dplyr::select(one_of(exp_sample_info$sample_id))

exp_sample_info <- 
  exp_sample_info %>% 
  mutate(start_date = as.Date(start_date)) %>% 
  dplyr::left_join(diff[,c("exp.date", "exp.group")], by = c("start_date" = "exp.date")) %>% 
  dplyr::rename(group = exp.group)

dim(met_sample_info)
dim(exp_sample_info)

##combine data
colnames(exp_expression_data) == exp_sample_info$sample_id

data <-
  t(exp_expression_data) %>% 
  data.frame(., group = exp_sample_info$group, stringsAsFactors = FALSE) %>% 
  plyr::dlply(.variables = .(group)) %>% 
  lapply(function(x){
    x <- 
      x %>% dplyr::select(-group)
    purrr::map(x, .f = function(x){mean(x)}) %>% unlist()
  }) %>% 
  do.call(rbind, .)

rownames(data) == exp_sample_info$group

exp_expression_data <- t(data) %>% as.data.frame()

exp_sample_info <-
  exp_sample_info %>% 
  plyr::dlply(.variables = .(group)) %>% 
    lapply(function(x){
      x[which.max(x$start_date),]
    }) %>% 
    do.call(rbind, .)

dim(exp_expression_data)

dim(met_expression_data)

save(exp_expression_data, file = "exp_expression_data")
save(met_expression_data, file = "met_expression_data")

save(exp_sample_info, file = "exp_sample_info")
save(met_sample_info, file = "met_sample_info")


