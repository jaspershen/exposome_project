##avoid source
no_function()

##load data
setwd(r4projects::get_project_wd())
library(tidyverse)
rm(list = ls())

####load proteome data
setwd(r4projects::get_project_wd())
load("data_20200511/proteome/expression_data")
pro_expression_data <- expression_data
load("data_20200511/proteome/sample_info")
pro_sample_info <- sample_info
load("data_20200511/proteome/variable_info")
pro_variable_info <- variable_info


###microbiome biological
load("data_20200511/microbiome/dna_expression_data")
microbiome_expression_data <- dna_expression_data
load("data_20200511/microbiome/dna_sample_info")
microbiome_sample_info <- dna_sample_info
load("data_20200511/microbiome/dna_variable_info")
microbiome_variable_info <- dna_variable_info

setwd("data_analysis/exposomeBiological_proteome/")

dim(microbiome_variable_info)

dim(pro_variable_info)

temp1 <- 
  microbiome_sample_info$date.start %>% 
  as.Date() %>% 
  data.frame(date = ., 
             exp = 1,
             stringsAsFactors = FALSE)

temp2 <- 
  pro_sample_info$CollectionDate %>% 
  as.Date() %>% 
  data.frame(date = ., 
             pro = 1,
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
  lapply(pro_sample_info$CollectionDate %>% as.Date(), function(x){
    x <- x - as.Date(microbiome_sample_info$date.start) 
    as.Date(microbiome_sample_info$date.start) [which(x >=0 & x <= 2)]
  }) 

names(diff) <- pro_sample_info$CollectionDate %>% as.character()

diff <- 
  purrr::map2(diff, names(diff), .f = function(x,y){
    if(length(x) == 0){
      return(NULL)
    }
    z <- data.frame(y, x, stringsAsFactors = FALSE)
    colnames(z) <- c("pro", "exp")
    z
  }) 

diff <- diff[lapply(diff, is.null) %>% unlist() %>% `!`]

diff[[2]] <- diff[[2]][-c(1),]

diff <- purrr::map2(.x = diff, .y = 1:length(diff), .f = function(x,y){
  data.frame(x, group = y, stringsAsFactors = FALSE)
}) %>% 
  do.call(rbind, .)

rownames(diff) <- NULL

diff <- 
  diff %>% 
  mutate(exp = as.character(exp), 
         pro = as.character(pro)) %>% 
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

diff$pro.date <- as.Date(diff$pro.date)

diff$exp.group <- as.character(diff$exp.group)

plot <- 
  temp %>% 
  mutate(date = as.Date(date)) %>% 
  dplyr::mutate(class = factor(class, levels = c("pro", "exp"))) %>% 
  ggplot(aes(date, class)) +
  geom_point(aes(fill = group),
             color = "black",
             alpha = 0.6, size = 4,
             shape = 21, show.legend = FALSE) +
  geom_segment(aes(x = exp.date, 
                   xend = pro.date, 
                   y = exp.class, 
                   yend = pro.class,
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
  # scale_y_continuous(breaks = c("pro", "exp"), labels = c("pro", "Exp")) +
  scale_y_discrete(breaks = c("pro", "exp"), labels = c("Proteome", "Exposome")) +
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

ggsave(plot, filename = "microbiome_pro_match.pdf", width = 10, height = 7)
ggsave(plot, filename = "microbiome_pro_match2.pdf", width = 14, height = 7)


#-------------------------------------------------------------------------------
###prepare data
dim(pro_expression_data)
dim(pro_sample_info)

pro_sample_info <-
  pro_sample_info %>% 
  dplyr::filter(CollectionDate %in% unique(diff$pro.date))

pro_expression_data <- 
  pro_expression_data %>% 
  dplyr::select(one_of(pro_sample_info$sample_id))

pro_sample_info <- 
  pro_sample_info %>% 
  dplyr::left_join(diff[,c("pro.date", "pro.group")] %>% 
                     dplyr::distinct(pro.date, pro.group), 
                   by = c("CollectionDate" = "pro.date")) %>% 
  dplyr::rename(group = pro.group)

microbiome_sample_info <-
  microbiome_sample_info %>% 
  dplyr::filter(as.Date(date.start) %in% diff$exp.date)

microbiome_expression_data <-
  microbiome_expression_data %>%
  select(-contains("Blank")) %>% 
  t()

microbiome_expression_data <-
  microbiome_expression_data %>%
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

microbiome_expression_data <- 
  microbiome_expression_data %>% 
  dplyr::select(one_of(microbiome_sample_info$sample_id))

microbiome_sample_info <- 
  microbiome_sample_info %>% 
  mutate(date.start = as.Date(date.start)) %>% 
  dplyr::left_join(diff[,c("exp.date", "exp.group")], by = c("date.start" = "exp.date")) %>% 
  dplyr::rename(group = exp.group)

dim(pro_sample_info)
dim(microbiome_sample_info)

##combine data
colnames(microbiome_expression_data) == microbiome_sample_info$sample_id

data <-
  t(microbiome_expression_data) %>% 
  data.frame(., group = microbiome_sample_info$group, stringsAsFactors = FALSE) %>% 
  plyr::dlply(.variables = .(group)) %>% 
  lapply(function(x){
    x <- 
      x %>% dplyr::select(-group)
    purrr::map(x, .f = function(x){mean(x)}) %>% unlist()
  }) %>% 
  do.call(rbind, .)

rownames(data) == microbiome_sample_info$group

microbiome_expression_data <- t(data) %>% as.data.frame()

microbiome_sample_info <-
  microbiome_sample_info %>% 
  plyr::dlply(.variables = .(group)) %>% 
  lapply(function(x){
    x[which.max(x$date.start),]
  }) %>% 
  do.call(rbind, .)

dim(microbiome_expression_data)

dim(pro_expression_data)

save(microbiome_expression_data, file = "microbiome_expression_data")
save(pro_expression_data, file = "pro_expression_data")

save(microbiome_sample_info, file = "microbiome_sample_info")
save(pro_sample_info, file = "pro_sample_info")


