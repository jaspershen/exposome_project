##avoid source
no_function()

##load data
setwd(r4projects::get_project_wd())
library(tidyverse)
rm(list = ls())

###load data
###environment
load("data_20200511/environment/variable_info")
environment_variable_info <- variable_info

load("data_20200511/metabolome/variable_info")
metabolome_variable_info <- variable_info

setwd("data_analysis/environment_metabolome/")

##environment
load("environment_expression_data")
load("environment_sample_info")

##metabolome
load("metabolome_expression_data")
load("metabolome_sample_info")

environment_sample_info$sample_id == colnames(environment_expression_data)
colnames(environment_expression_data) <- as.character(environment_sample_info$date.start)
colnames(metabolome_expression_data) <- colnames(environment_expression_data)

#######correlation analysis
metabolome_expression_data <-
  log(metabolome_expression_data + 1, 2)

dim(metabolome_expression_data)
dim(environment_expression_data)

###correct fiber for metabolomics
metabolome_expression_data1 <- 
  purrr::map(as.data.frame(t(metabolome_expression_data)), .f = function(x){
    temp_data <-
      data.frame(fiber = c(0,
                           0,
                           0,
                           1,
                           1),
                 x,
                 stringsAsFactors = FALSE)
    lm_result <- lm(formula = x ~ fiber, data = temp_data)
    lm_result$residuals
  }) %>% 
  do.call(rbind, .) %>% 
  as.data.frame()

temp_data <-
  apply(environment_expression_data, 1, function(x) {
    (x - mean(x)) / sd(x)
  }) %>%
  t() 

colnames(temp_data) <- 
colnames(environment_expression_data) <-
  colnames(environment_expression_data)

##heatmap of environment
library(circlize)

col_fun = colorRamp2(
  breaks = seq(min(temp_data), max(temp_data), length.out = 90),
  colors =
    viridis::magma(n = 100)[-c(1:10)],
  transparency = 0
)

plot <- 
temp_data %>% 
  ComplexHeatmap::Heatmap(cluster_columns = FALSE,
                          show_column_names = TRUE,
                          show_row_names = FALSE,
                          clustering_method_rows = "ward.D",
                          clustering_method_columns = "ward.D",
                          clustering_distance_columns = "euclidean",
                          clustering_distance_rows = "euclidean",
                          col = col_fun,
                          km = 2, border = TRUE, 
                          row_dend_reorder = TRUE, 
                          column_dend_reorder = TRUE,
                          column_names_rot = 45, 
                          name = "Z-score")

plot <- ggplotify::as.ggplot(plot)

plot

ggsave(plot, filename = "environment_plot/environment_heatmap.pdf", width = 7, height = 7)

###remove some metabolome variables
remain_idx <-
  apply(metabolome_expression_data, 1, function(x) {
    length(unique(x)) != 1
  }) %>%
  which()

metabolome_expression_data1 <- 
  metabolome_expression_data1[remain_idx,]

metabolome_variable_info <- 
  metabolome_variable_info[remain_idx,]

###heatmap of metabolome
plot <- 
  metabolome_expression_data1 %>% 
  apply(1, function(x){
    (x - mean(x))/sd(x)
  }) %>% 
  t() %>% 
  ComplexHeatmap::Heatmap(cluster_columns = FALSE,
                          show_column_names = TRUE,
                          show_row_names = FALSE,
                          clustering_method_rows = "ward.D",
                          clustering_method_columns = "ward.D",
                          clustering_distance_columns = "euclidean",
                          clustering_distance_rows = "euclidean",
                          col = col_fun,
                          km = 2, border = TRUE, 
                          row_dend_reorder = TRUE, 
                          column_dend_reorder = TRUE,
                          column_names_rot = 45, 
                          name = "Z-score")

plot <- ggplotify::as.ggplot(plot)

plot

ggsave(plot, filename = "metabolome_plot/metabolome_heatmap.pdf", width = 7, height = 7)

####calculate correlation between environment and exposome
cor_value <-
  cor(x = t(as.matrix(metabolome_expression_data1)),
      y = t(as.matrix(environment_expression_data)),
      method = "spearman")

cor_value <-
  cor_value %>%
  as.data.frame() %>%
  tibble::rownames_to_column(var = "from") %>%
  tidyr::pivot_longer(-from, names_to = "to", values_to = "cor")

library(plyr)

p_value <-
  purrr::map(
    as.data.frame(t(cor_value)),
    .f = function(x) {
      value1 <- as.numeric(metabolome_expression_data[x[1], ])
      value2 <- as.numeric(environment_expression_data[x[2], ])
      cor.test(value1, value2, method = "spearman")$p.value
    }
  ) %>%
  unlist()

cor_value <-
  data.frame(cor_value, p_value, stringsAsFactors = FALSE)

plot(density(cor_value$p_value))

library(plyr)
cor_value <-
  cor_value %>%
  plyr::dlply(.variables = .(from)) %>%
  purrr::map(
    .f = function(x) {
      x <- x %>%
        dplyr::filter(abs(cor) > 0.9)
      fdr <- p.adjust(x$p_value, method = "fdr")
      x <-
        data.frame(x, fdr, stringsAsFactors = FALSE)
      x
    }
  )

cor_value <-
  cor_value %>%
  do.call(rbind, .) %>%
  as.data.frame()

cor_value <-
  cor_value %>%
  dplyr::filter(abs(cor) > 0.9 & cor != 1)

dim(cor_value)

save(cor_value, file = "cor_value")
load('cor_value')

cor_value1 <-
cor_value %>%
  dplyr::filter(abs(cor) > 0.9 & fdr < 0.05)

dim(cor_value1)
dim(cor_value)

setwd(r4projects::get_project_wd())
setwd("data_analysis/metabolome_environment/metabolome_environment_plot")
# for(idx in 1:nrow(cor_value1)){
#   cat(idx, " ")
#   path1 <- file.path(cor_value1$from[idx])
#   dir.create(path1, showWarnings = FALSE)
#   temp_data <-
#     data.frame(date = as.character(environment_sample_info$date.start),
#                exp = as.numeric(metabolome_expression_data[cor_value1$from[idx],]),
#                met = as.numeric(environment_expression_data[cor_value1$to[idx],]),
#                stringsAsFactors = FALSE)
#   plot <-
#   temp_data %>%
#     ggplot(aes(exp, met)) +
#     geom_point() +
#     geom_smooth(method = "lm", color = "skyblue") +
#     ggrepel::geom_label_repel(aes(x = exp, met, label = date)) +
#     labs(x = paste("Exposome (Biological): ", cor_value1$metabolome_id[idx], sep = ""),
#          y = paste("environment: " ,cor_value1$environment_id[idx]), sep = "") +
#     theme_bw() +
#     theme(axis.title = element_text(size = 13),
#           axis.text = element_text(size = 12),
#           plot.background = element_rect(fill = "transparent", color = NA),
#           panel.background = element_rect(fill = "transparent", color = NA)) +
#     annotate(geom = "text",
#              x = -Inf, y = Inf,
#              label = paste("Correlation: ",round(cor_value1$cor[idx], 2),
#                            "\nFDR adjusted P value: ",
#                            round(cor_value1$fdr[idx], 3), sep = ""),
#              vjust = 2, hjust = -1)
# 
#   name <- paste(cor_value1$from[idx], "_",
#                 cor_value1$to[idx], ".pdf", sep = "")
# 
#   ggsave(plot, filename = file.path(path1, name),
#          width = 7, height = 7, bg = "transparent")
# 
# }

setwd(r4projects::get_project_wd())  
setwd("data_analysis/metabolome_environment")

cor_value1$from %>% unique()

###correlation network for Metabolome and environment
cor_value1$from %>% unique() %>% length()
cor_value1$to %>% unique() %>% length()

library(igraph)
library(ggraph)
library(tidygraph)

###network for all the Metabolome and environment
edge_data <-  
  cor_value1 %>% 
  # dplyr::filter(from %in% cluster1) %>%
  dplyr::rename(from = from, 
                to = to, 
                Correlation = cor) %>% 
  dplyr::mutate(fdr = -log(fdr, 10))

node_data <- 
  cor_value1 %>% 
  # dplyr::filter(from %in% cluster1) %>%
  dplyr::rename(from = from, to = to) %>% 
  dplyr::select(from, to) %>% 
  tidyr::pivot_longer(cols = c(from, to), 
                      names_to = "class", values_to = "node") %>% 
  dplyr::mutate(class1 = case_when(
    stringr::str_detect(class, "from") ~ "Metabolome",
    TRUE ~ "Enviroment"
  )) %>% 
  dplyr::select(node, class1) %>% 
  dplyr::rename(Class = class1) %>%
  dplyr::distinct(node, .keep_all = TRUE)

node_data <- 
  node_data %>% 
  dplyr::arrange(Class)

temp_data <- 
  tidygraph::tbl_graph(nodes = node_data, 
                       edges = edge_data,
                       directed = TRUE) %>% 
  dplyr::mutate(Degree = centrality_degree(mode = 'all'))

pal <-
  wesanderson::wes_palette(name = "Zissou1", n = 100, type = "continuous")

plot1 <-
  ggraph(temp_data,
         layout = 'linear',
         circular = TRUE) +
  geom_edge_arc(aes(color = Correlation
                    # width = fdr
                    ),
                show.legend = TRUE) +
  geom_node_point(aes(fill = Class,
                      size = Degree),
                  shape = 21,
                  show.legend = TRUE) +
  scale_fill_manual(values = c(
    "Metabolome" = ggsci::pal_d3()(10)[3],
    "Enviroment" = ggsci::pal_d3()(10)[1]
  )) +
  scale_color_manual(values = c(
    "Metabolome" = ggsci::pal_d3()(10)[3],
    "Enviroment" = ggsci::pal_d3()(10)[1]
  )) +
  geom_node_text(
    aes(
      x = x * 1.05,
      y = y * 1.05,
      label = node,
      hjust = 'outward',
      angle = -((-node_angle(x, y) + 90) %% 180) + 90,
      size = 3,
      colour = Class
    ),
    size = 3,
    alpha = 1,
    show.legend = FALSE
  ) +
  guides(edge_width = guide_legend(title = "-log10(FDR adjusted P value)", 
                                   override.aes = list(shape = NA)),
         edge_color = ggraph::guide_edge_colorbar(title = "Spearman correlation"),
         fill = guide_legend(title = "Class", 
                             override.aes = list(size = 4, linetype = "blank")),
         size = guide_legend(title = "Degree", override.aes = list(linetype = 0))) +
  ggraph::scale_edge_color_gradientn(colours = pal) +
  ggraph::scale_edge_width(range = c(0.1, 1)) +
  scale_size_continuous(range = c(2, 8)) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA)
  )

plot1

ggsave(
  plot1,
  filename = "metabolome_environment_correlation_network.pdf",
  width = 8.5,
  height = 7,
  bg = "transparent"
)

ggsave(
  plot1,
  filename = "metabolome_environment_correlation_network.png",
  width = 8.5,
  height = 7,
  bg = "transparent"
)




