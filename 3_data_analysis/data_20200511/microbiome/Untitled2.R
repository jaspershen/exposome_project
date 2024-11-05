#to avoind source
no_exist_function()

setwd(r4projects::get_project_wd())
rm(list = ls())
source("R/2020_02_13/tools.R")
library(tidyverse)


##load data (use the adjusted data)
load("data_analysis_2020_02_13/transcriptome_data_preparation/phenotype_info")
load("data_analysis_2020_02_13/transcriptome_data_preparation/expression_data")
load("data_analysis_2020_02_13/transcriptome_data_preparation/expression_data_combat")
load("data_analysis_2020_02_13/transcriptome_data_preparation/sample_info")
load("data_analysis_2020_02_13/transcriptome_data_preparation/variable_info")

load("data_analysis_2020_02_13/phenotype_data_preparation/phenotype_info")

idx <- c("subject_id",
         setdiff(colnames(phenotype_info),colnames(sample_info)))

phenotype_info <- 
  phenotype_info[,idx]

ls()


sample_info <- 
  sample_info %>% 
  dplyr::filter(!subject_id %in% c("DK004", "DK006", "DK008")) %>% 
  dplyr::filter(stringr::str_detect(subject_id, "DK"))

expression_data <- 
  expression_data %>% 
  dplyr::select(one_of(sample_info$sample_id))

rownames(expression_data) == variable_info$gene_id

colnames(expression_data) == sample_info$sample_id

subject_data <- 
  expression_data %>% 
  dplyr::select(one_of(sample_info$sample_id))

##log transformation
subject_data <-
  log(subject_data + 1, 2)

load("data_analysis_2020_02_13/transcriptome_analysis/transcriptome_DEG_analysis/importance_up_gene")
load("data_analysis_2020_02_13/transcriptome_analysis/transcriptome_DEG_analysis/importance_down_gene")

subject_data <-
  subject_data[c(importance_up_gene$gene_id, importance_down_gene$gene_id), ]

setwd("data_analysis_2020_02_13/transcriptome_analysis/consensus_clustering/")

temp_subject_data <- subject_data
temp_sample_info <- sample_info

#here we use the K-means consensus clustering
library(CancerSubtypes)

##scale
temp_subject_data <-
  temp_subject_data %>%
  apply(1, function(x) {
    (x - mean(x)) / sd(x)
  }) %>%
  t()

result <-
  ExecuteCC(
    clusterNum = 2,
    d = as.matrix(temp_subject_data),
    maxK = 6,
    reps = 1000,
    pItem = 0.8,
    pFeature = 0.8,
    title = "k_means_consensus",
    clusterAlg = "km",
    distance = "euclidean",
    plot = "png",
    writeTable = TRUE
  )
save(result, file = "result")
load("result")

idx <- 3
sil = silhouette_SimilarityMatrix(result$originalResult[[idx]]$consensusClass,
                                  result$originalResult[[idx]]$consensusMatrix)
# sil=silhouette_SimilarityMatrix(result$group, result$distanceMatrix)

sil_plot <-
  plot_silhouette(sil)

sil_plot

ggsave(sil_plot,
       file = "k_means_consensus/sil_plot.pdf",
       width = 7,
       height = 7)

plot(result$originalResult[[idx]]$consensusTree)

name2 <-
  colnames(temp_subject_data)[result$originalResult[[idx]]$consensusTree$order]

temp_subject_data2 <- temp_subject_data[, name2]
cluster <- result$originalResult[[idx]]$consensusClass

temp_sample_info2 <-
  temp_sample_info[match(colnames(temp_subject_data2), temp_sample_info$sample_id),]

cluster <-
  cluster[match(temp_sample_info2$sample_id, names(cluster))]

names(cluster) == colnames(temp_subject_data2)

ga <-
  temp_sample_info2 %>%
  dplyr::select(sample_id, g_stage) %>%
  dplyr::mutate(ga = ggplot2::cut_width(g_stage, width = 2)) %>%
  dplyr::mutate(ga = stringr::str_replace(ga, "\\[", "(")) %>%
  pull(ga)

library(pheatmap)
ga_level <- unique(ga) %>% stringr::str_sort(numeric = TRUE)

annotation_col = data.frame(ga = ga,
                            cluster) %>%
  dplyr::mutate(ga = factor(ga, levels = ga_level)) %>%
  dplyr::mutate(cluster = factor(cluster, levels = sort(unique(cluster))))

rownames(annotation_col) = temp_sample_info2$sample_id

# Specify colors
##ga
temp1 <-
  colorRampPalette(colors = c(RColorBrewer::brewer.pal(n = 12, name = "Spectral")))(length(ga_level)) %>%
  rev()
names(temp1) <- ga_level

##cluster
temp9 <- ggsci::pal_d3()(10)[1:length(unique(cluster))]
names(temp9) <- sort(unique(sort(cluster)))

ann_colors = list(ga = temp1,
                  cluster = temp9)

temp_data <- temp_subject_data2

range(temp_data)
# temp_data[temp_data > 4.63] <- 4.63
temp_data[temp_data > 3] <- 3
temp_data[temp_data < -3] <- -3

hm_palette = colorRampPalette(c("#BF0080", "#CE6EAE", "#dddddd", "#6EAE6E", "#008000"))(n = 100) %>%
  rev()

plot <-
  pheatmap(
    temp_data,
    show_colnames = FALSE,
    show_rownames = FALSE,
    cluster_cols = FALSE,
    border_color = NA,
    color = hm_palette,
    annotation_col = annotation_col,
    annotation_colors = ann_colors,
    clustering_method = "ward.D",
    scale = "none"
  )

##reorder cluster
cluster2 <- cluster[cluster == 2]
cluster3 <- cluster[cluster == 3]
cluster1 <- cluster[cluster == 1]
# cluster <- c(cluster3, cluster1, cluster2)
# # cluster <- rev(cluster)
# temp_sample_info2 <-
#   temp_sample_info2[match(names(cluster), temp_sample_info2$sample_id),]
#
# temp_subject_data2 <-
#   temp_subject_data2[,names(cluster)]
#
# names(cluster) == temp_sample_info2$sample_id
# names(cluster) == colnames(temp_subject_data2)

###complext heatamp
ga <-
  temp_sample_info2 %>%
  dplyr::select(sample_id, g_stage) %>%
  dplyr::mutate(ga = ggplot2::cut_width(g_stage, width = 2)) %>%
  dplyr::mutate(ga = stringr::str_replace(ga, "\\[", "(")) %>%
  pull(ga)

library(pheatmap)
ga_level <- unique(ga) %>% stringr::str_sort(numeric = TRUE)
subject_id <- temp_sample_info2$subject_id

##other information
age <-
  temp_sample_info2 %>%
  dplyr::left_join(phenotype_info, by = "subject_id") %>%
  pull(mothers_age)

sum(is.na(age))

bmi <-
  temp_sample_info2 %>%
  dplyr::left_join(phenotype_info, by = "subject_id") %>%
  pull(bmi)

sum(is.na(bmi))

parity <-
  temp_sample_info2 %>%
  dplyr::left_join(phenotype_info, by = "subject_id") %>%
  pull(parity)

sum(is.na(parity))

childs_sex <-
  temp_sample_info2 %>%
  dplyr::left_join(phenotype_info, by = "subject_id") %>%
  pull(childs_sex)

sum(is.na(childs_sex))

birth_weight_g <-
  temp_sample_info2 %>%
  dplyr::left_join(phenotype_info, by = "subject_id") %>%
  pull(birth_weight_g)

sum(is.na(birth_weight_g))

birth_length_cm <-
  temp_sample_info2 %>%
  dplyr::left_join(phenotype_info, by = "subject_id") %>%
  pull(birth_length_cm)

sum(is.na(birth_length_cm))


library(ComplexHeatmap)
range(temp_data)
library(circlize)
col_fun = colorRamp2(c(-3, 0, 3), c("#4292C6", "white", "red"))
col_fun(seq(-3, 3))
cluster_col_fun = colorRamp2(c(0, 2, 3), c("green", "white", "red"))

##ga color
temp1 <-
  colorRampPalette(colors = c(RColorBrewer::brewer.pal(n = 12, name = "Spectral")))(length(ga_level)) %>%
  rev()

names(temp1) <- ga_level

ha1 = HeatmapAnnotation(
  age = age,
  bmi = bmi,
  parity = factor(parity, level = sort(unique(parity))),
  childs_sex = as.character(childs_sex),
  birth_weight_g = as.numeric(birth_weight_g),
  birth_length_cm = as.numeric(birth_length_cm),
  # ga = factor(ga, levels = ga_level),
  # cluster = factor(cluster, levels = as.character(c(3,1,2))),
  col = list(
    childs_sex = c(
      "F" = alpha(ggsci::pal_aaas()(10)[1], 0.8),
      "M" = alpha(ggsci::pal_aaas()(10)[2], 0.8)
    ),
    parity = c(
      "1" = alpha(ggsci::pal_aaas()(10)[1], 0.2),
      "2" = alpha(ggsci::pal_aaas()(10)[1], 0.6),
      "3" = alpha(ggsci::pal_aaas()(10)[1], 1)
    )
    # age = circlize::colorRamp2(breaks = c(min(age), max(age)),
    #                            colors = c(
    #                              alpha(ggsci::pal_aaas()(10)[1], 1),
    #                              alpha(ggsci::pal_aaas()(10)[2], 1)
    #                            ))
  ),
  annotation_name_side = c("left")
)

ha2 = HeatmapAnnotation(
  cluster = factor(cluster, levels = as.character(c(3, 1, 2))),
  ga = factor(ga, levels = ga_level),
  col = list(ga = temp1, cluster = temp9),
  "GA"  = anno_points(
    temp_sample_info2$g_stage[match(colnames(temp_subject_data2), temp_sample_info2$sample_id)],
    # ylim = c(0, 1),
    pch = 16,
    gp = gpar(col = c(
      rep(ggsci::pal_d3()(10)[3], length(cluster3)),
      rep(ggsci::pal_d3()(10)[1], length(cluster1)),
      rep(ggsci::pal_d3()(10)[2], length(cluster2))
    )),
    size = unit(2, "mm"),
    height = unit(4, "cm"),
    show_legend = c(TRUE, FALSE),
    axis_param = list(side = "left"
                      # at = c(0, 0.5, 1),
                      # labels = c("zero", "half", "one"))
    ),
    annotation_name_side = c("left")
  )
  
  temp_data <- temp_subject_data2
  range((temp_data))
  
  temp_data[temp_data > 3] <- 3
  temp_data[temp_data < -3] <- -3
  
  plot <-
    Heatmap(
      temp_data,
      cluster_columns = FALSE,
      cluster_rows = TRUE,
      show_row_names = FALSE,
      show_column_names = FALSE,
      border = FALSE,
      col = col_fun,
      name = "Int",
      clustering_method_rows = "ward.D",
      row_km = 2,
      top_annotation = ha1,
      bottom_annotation = ha2
    )
  plot
  library(ggplotify)
  plot <- as.ggplot(plot)
  
  ggsave(plot,
         filename = "heatmap.pdf",
         width = 12,
         height = 7)
  ggsave(plot,
         filename = "heatmap.png",
         width = 12,
         height = 7)
  
