##avoid source
no_function()

setwd(r4projects::get_project_wd())
library(tidyverse)
rm(list = ls())

##load data
###exposome
load("data_20200511/exposome/variable_info")
exp_variable_info <- variable_info

###proteome
load("data_20200511/proteome/variable_info")
pro_variable_info <- variable_info

setwd("data_analysis/exposomeChemical_proteome/")

load("pro_expression_data")
load("exp_expression_data")
load("pro_sample_info")
load("exp_sample_info")

pro_sample_info$sample_id == colnames(pro_expression_data)
colnames(pro_expression_data) <- as.character(pro_sample_info$CollectionDate)
colnames(exp_expression_data) <- colnames(pro_expression_data)

#######correlation analysis
exp_expression_data <- log(exp_expression_data + 1, 2)

dim(exp_expression_data)
dim(pro_expression_data)

###correct fiber for metabolomics
pro_expression_data1 <- 
  purrr::map(as.data.frame(t(pro_expression_data)), .f = function(x){
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
  pro_expression_data1
  
colnames(temp_data) <- 
colnames(pro_expression_data1) <-
  colnames(pro_expression_data)

##heatmap of proteome
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

# ggsave(plot, filename = "proteome_plot/proteome_heatmap.pdf", width = 7, height = 7)

###heatmap of exposome
temp_data <-
  exp_expression_data %>%
  apply(1, function(x){
    (x - mean(x))/sd(x)
  }) %>%
  t()

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

# ggsave(plot, filename = "exposome_plot/exposome_heatmap.pdf", width = 7, height = 7)

# ####calculate correlation between proteome and exposome
# cor_value <-
#   cor(x = t(as.matrix(exp_expression_data)),
#       y = t(as.matrix(pro_expression_data1)),
#       method = "spearman")
#
# cor_value <-
#   cor_value %>%
#   as.data.frame() %>%
#   tibble::rownames_to_column(var = "from") %>%
#   tidyr::pivot_longer(-from, names_to = "to", values_to = "cor")
#
# library(plyr)
#
# p_value <-
#   purrr::map(as.data.frame(t(cor_value)), .f = function(x){
#     value1 <- as.numeric(exp_expression_data[x[1],])
#     value2 <- as.numeric(pro_expression_data1[x[2],])
#     cor.test(value1, value2, method = "spearman")$p.value
#   }) %>%
#   unlist()
#
# cor_value <-
#   data.frame(cor_value, p_value, stringsAsFactors = FALSE)
#
# plot(density(cor_value$p_value))
#
# library(plyr)
# cor_value <-
# cor_value %>%
#   plyr::dlply(.variables = .(from)) %>%
#   purrr::map(.f = function(x){
#     x <- x %>%
#       dplyr::filter(abs(cor) > 0.9)
#     fdr <- p.adjust(x$p_value, method = "fdr")
#     x <-
#       data.frame(x, fdr, stringsAsFactors = FALSE)
#     x
#   })
#
# cor_value <-
# cor_value %>%
#   do.call(rbind, .) %>%
#   as.data.frame()
#
# cor_value <-
#   cor_value %>%
#   dplyr::filter(abs(cor) > 0.9 & cor != 1)
#
# dim(cor_value)
#
# save(cor_value, file = "cor_value")
load('cor_value')

cor_value1 <- 
cor_value %>% 
  dplyr::filter(abs(cor) > 0.9 & fdr < 0.05)

dim(cor_value)
dim(cor_value1)

cor_value1 <- 
  cor_value1 %>% 
  dplyr::left_join(exp_variable_info[,c(1:2)], by = c("from" = "peak_ID")) %>% 
  dplyr::rename(exp_id = MetabID) %>% 
  dplyr::mutate(pro_id = to) %>% 
  dplyr::filter(!is.na(pro_id))

setwd(r4projects::get_project_wd())
setwd("data_analysis/exposomeChemical_proteome/exposomeChemical_proteome_plot")

# for(idx in 1:nrow(cor_value1)) {
#   cat(idx, " ")
#   path1 <- file.path(cor_value1$from[idx])
#   dir.create(path1, showWarnings = FALSE)
#   temp_data <-
#     data.frame(
#       date = as.character(pro_sample_info$CollectionDate),
#       exp = as.numeric(exp_expression_data[cor_value1$from[idx], ]),
#       pro = as.numeric(pro_expression_data1[cor_value1$to[idx], ]),
#       stringsAsFactors = FALSE
#     )
#   plot <-
#     temp_data %>%
#     ggplot(aes(exp, pro)) +
#     geom_point() +
#     geom_smooth(method = "lm", color = "skyblue") +
#     ggrepel::geom_label_repel(aes(x = exp, pro, label = date)) +
#     labs(
#       x = paste("Exposome (chemical): ", cor_value1$exp_id[idx], sep = ""),
#       y = paste("Proteome: " , cor_value1$pro_id[idx]),
#       sep = ""
#     ) +
#     theme_bw() +
#     theme(
#       axis.title = element_text(size = 13),
#       axis.text = element_text(size = 12),
#       plot.background = element_rect(fill = "transparent", color = NA),
#       panel.background = element_rect(fill = "transparent", color = NA)
#     ) +
#     annotate(
#       geom = "text",
#       x = -Inf,
#       y = Inf,
#       label = paste(
#         "Correlation: ",
#         round(cor_value1$cor[idx], 2),
#         "\nFDR adjusted P value: ",
#         round(cor_value1$fdr[idx], 3),
#         sep = ""
#       ),
#       vjust = 2,
#       hjust = -1
#     )
#   
#   name <- paste(cor_value1$from[idx], "_",
#                 cor_value1$to[idx], ".pdf", sep = "")
#   
#   ggsave(
#     plot,
#     filename = file.path(path1, name),
#     width = 7,
#     height = 7,
#     bg = "transparent"
#   )
#   
# }

setwd(r4projects::get_project_wd())  
setwd("data_analysis/exposomeChemical_proteome/")

cor_value1$from %>% unique()

###correlation network for pro and exp
cor_value1$from %>% unique() %>% length()
cor_value1$to %>% unique() %>% length()

library(igraph)
library(ggraph)
library(tidygraph)

###network for all the exposome and proteome
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
    stringr::str_detect(class, "from") ~ "Exposome",
    TRUE ~ "Proteome"
  )) %>% 
  dplyr::select(node, class1) %>% 
  dplyr::rename(Class = class1) %>%
  dplyr::distinct(node, .keep_all = TRUE)

node_data <- 
  node_data %>% 
  dplyr::arrange(desc(Class))

node_data <- 
node_data %>% 
  dplyr::left_join(exp_variable_info[,c("peak_ID", "MetabID")], 
                   by = c("node" = "peak_ID")) %>% 
  dplyr::mutate(compound.name = case_when(
    !is.na(MetabID) ~ MetabID,
    TRUE ~ node
  )) %>% 
  dplyr::select(node, Class, compound.name)

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
  geom_edge_arc(aes(color = Correlation),
                show.legend = TRUE) +
  geom_node_point(aes(fill = Class,
                      size = Degree),
                  shape = 21,
                  show.legend = TRUE) +
  scale_fill_manual(values = c(
    "Exposome" = ggsci::pal_d3()(10)[2],
    "Proteome" = ggsci::pal_d3()(10)[4]
  )) +
  scale_color_manual(values = c(
    "Exposome" = ggsci::pal_d3()(10)[2],
    "Proteome" = ggsci::pal_d3()(10)[4]
  )) +
  geom_node_text(
    aes(
      x = x * 1.05,
      y = y * 1.05,
      label = compound.name,
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
  ggraph::scale_edge_width(range = c(0.2, 2)) +
  scale_size_continuous(range = c(1, 8)) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA)
  )

plot1

# ggsave(
#   plot1,
#   filename = "exposome_proteome_correlation_network.pdf",
#   width = 8.5,
#   height = 7,
#   bg = "transparent"
# )

###pathway enrichment for proteome
library(clusterProfiler)

protein_list <-
  unique(c(cor_value1$to, cor_value1$from)) %>% 
  data.frame(symbol = ., stringsAsFactors = FALSE)
  
protein_list <-
  clusterProfiler::bitr(
    geneID = protein_list$symbol,
    toType = "ENTREZID",
    OrgDb = "org.Hs.eg.db",
    fromType = "SYMBOL",
    drop = TRUE
  ) %>%
  dplyr::left_join(protein_list, by = c("SYMBOL" = "symbol")) %>%
  dplyr::distinct(ENTREZID, .keep_all = TRUE)

protein_list <-
  clusterProfiler::bitr(
    geneID = protein_list$SYMBOL,
    toType = "UNIPROT",
    OrgDb = "org.Hs.eg.db",
    fromType = "SYMBOL",
    drop = TRUE
  ) %>%
  dplyr::left_join(protein_list, by = c("SYMBOL")) %>%
  dplyr::distinct(ENTREZID, .keep_all = TRUE)

###KEGG pathway
# enrich_kegg <-
#   clusterProfiler::enrichKEGG(
#     gene = protein_list$UNIPROT,
#     keyType = "uniprot",
#     organism = "hsa",
#     qvalueCutoff = 0.05,
#     pAdjustMethod = "fdr"
#   )
# 
# 
# kegg_class <-
#   purrr::map(
#     .x = enrich_kegg@result$ID,
#     .f = function(x) {
#       temp <- KEGGREST::keggGet(dbentries = x)
#       class <- temp[[1]]$CLASS
#       if (is.null(class)) {
#         class <- "no"
#       }
#       class
#     }
#   )  %>%
#   unlist()
# 
# enrich_kegg@result$kegg_class <- kegg_class
# save(enrich_kegg, file = "pathway_enrichment/enrich_kegg")

load("pathway_enrichment/enrich_kegg")

enrich_kegg@result$Description

enrich_kegg@result$p.adjust

clusterProfiler::dotplot(object = enrich_kegg)

##GO pathway
library(org.Hs.eg.db)
# enrich_go <-
#   clusterProfiler::enrichGO(
#     gene = protein_list$ENTREZID,
#     OrgDb = org.Hs.eg.db,
#     keyType = "ENTREZID",
#     ont = "ALL",
#     pvalueCutoff = 0.05,
#     pAdjustMethod = "fdr",
#     qvalueCutoff = 0.05
#   )
# 
# save(enrich_go, file = "pathway_enrichment/enrich_go")

load("pathway_enrichment/enrich_go")


##reactome pathway
library(ReactomePA)
# enrich_reactome <-
#   ReactomePA::enrichPathway(
#     gene = protein_list$ENTREZID,
#     organism = "human",
#     pvalueCutoff = 0.05,
#     pAdjustMethod = "fdr",
#     qvalueCutoff = 0.05
#   )
# 
# save(enrich_reactome, file = "pathway_enrichment/enrich_reactome")

load("pathway_enrichment/enrich_reactome")

clusterProfiler::dotplot(enrich_go)
clusterProfiler::cnetplot(enrich_go)
clusterProfiler::emapplot(enrich_go)
clusterProfiler::heatplot(enrich_go)

####output pathway enrichment result
library(openxlsx)
# wb <- createWorkbook()
# modifyBaseFont(wb, fontSize = 12, fontName = "Arial Narrow")
# addWorksheet(wb, sheetName = "GO", gridLines = FALSE)
# addWorksheet(wb, sheetName = "KEGG", gridLines = FALSE)
# addWorksheet(wb, sheetName = "Reactome", gridLines = FALSE)
# freezePane(wb, sheet = 1, firstRow = TRUE, firstCol = TRUE) 
# writeDataTable(
#   wb,
#   sheet = 1,
#   x = enrich_go@result %>% dplyr::filter(p.adjust < 0.05),
#   colNames = TRUE,
#   rowNames = TRUE,
#   tableStyle = "TableStyleLight9"
# )
# 
# freezePane(wb, sheet = 2, firstRow = TRUE, firstCol = TRUE) 
# writeDataTable(
#   wb,
#   sheet = 2,
#   x = enrich_kegg@result %>% dplyr::filter(p.adjust < 0.05),
#   colNames = TRUE,
#   rowNames = TRUE,
#   tableStyle = "TableStyleLight9"
# )
# 
# freezePane(wb, sheet = 3, firstRow = TRUE, firstCol = TRUE) 
# writeDataTable(
#   wb,
#   sheet = 3,
#   x = enrich_reactome@result %>% dplyr::filter(p.adjust < 0.05),
#   colNames = TRUE,
#   rowNames = TRUE,
#   tableStyle = "TableStyleLight9"
# )
# 
# saveWorkbook(wb, "enriched_pathway.xlsx", overwrite = TRUE) 


temp_data <- 
  rbind(
    data.frame(id = head(enrich_go@result$Description[which(enrich_go@result$p.adjust < 0.05)], 10),
               count = head(enrich_go@result$Count[which(enrich_go@result$p.adjust < 0.05)], 10),
               fdr = head(enrich_go@result$p.adjust[which(enrich_go@result$p.adjust < 0.05)], 10),
               class = "GO"),
    data.frame(id = head(enrich_kegg@result$Description[which(enrich_kegg@result$p.adjust < 0.05)], 10),
               count = head(enrich_kegg@result$Count[which(enrich_kegg@result$p.adjust < 0.05)], 10),
               fdr = head(enrich_kegg@result$p.adjust[which(enrich_kegg@result$p.adjust < 0.05)], 10),
               class = "KEGG"),
    data.frame(id = head(enrich_reactome@result$Description[which(enrich_reactome@result$p.adjust < 0.05)], 10),
               count = head(enrich_reactome@result$Count[which(enrich_reactome@result$p.adjust < 0.05)], 10),
               fdr = head(enrich_reactome@result$p.adjust[which(enrich_reactome@result$p.adjust < 0.05)], 10),
               class = "Rectome")    
  )

plot <- 
temp_data %>% 
  dplyr::arrange(class, fdr) %>% 
  dplyr::mutate(id = factor(id, levels = id)) %>% 
  ggplot(aes(id, -log(fdr, 10))) +
  geom_segment(aes(x = id, xend = id, y = 0, 
                   yend = -log(fdr, 10),
                   color = class), show.legend = FALSE) +
  geom_point(aes(fill = class, size = count), shape = 21) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  guides(fill = guide_legend(title = "", 
                             override.aes = list(size = 4))) +
  theme_bw() +
  labs(x = "", y = "-log10(FDR adjusted P value)") +
  ggsci::scale_color_d3() +
  ggsci::scale_fill_d3() +
  theme(axis.text.x = element_text(angle = 45, size = 10, hjust = 1, 
                                   vjust = 1),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.ticks.x = element_blank())

plot

# ggsave(plot, filename = "pathway_enrichment.pdf", width = 7, height = 8)

##for each pathway, it is positive or negative correlation with exposome chemical
result_go <-
  enrich_go@result[enrich_go@result$p.adjust < 0.05, ] %>% 
  head(10)

result_kegg <-
  enrich_kegg@result[enrich_kegg@result$p.adjust < 0.05, ] %>% 
  head(10)

result_reactome <-
  enrich_reactome@result[enrich_reactome@result$p.adjust < 0.05, ] %>% 
  head(10)

result_go$Description

####GO
gene_id_go <-
  result_go$geneID %>%
  purrr::map(function(x) {
    x <- stringr::str_split(x, "\\/")[[1]] %>%
      unique()
    x <-
      data.frame(x) %>%
      dplyr::left_join(protein_list, by = c("x" = "ENTREZID")) %>%
      dplyr::pull(SYMBOL) %>%
      unique() %>%
      data.frame(x = .) %>%
      dplyr::left_join(cor_value1[, c("from", "pro_id", "cor")], by = c("x" = "pro_id")) %>% 
      dplyr::arrange(cor)
    colnames(x) <- c("protein_id", "chemical_id", "correlation")
    x
  })

gene_id_go <-
  purrr::map2(
    .x = gene_id_go,
    .y = result_go$Description,
    .f = function(x, y) {
      data.frame(x, pathway = y) %>%
        dplyr::select(chemical_id, protein_id, pathway, correlation)
    }
  )

gene_id_go <-
  gene_id_go %>%
  do.call(rbind, .) %>%
  dplyr::distinct(.keep_all = TRUE) %>%
  dplyr::left_join(node_data,
                   c("node", "compound.name"),
                   by = c("chemical_id" = "node"))


gene_id_go <- 
  data.frame(gene_id_go, pathway_class = "GO")

###KEGG
gene_id_kegg <-
  result_kegg$geneID %>%
  purrr::map(function(x) {
    x <- stringr::str_split(x, "\\/")[[1]] %>%
      unique()
    x <-
      data.frame(x) %>%
      dplyr::left_join(protein_list, by = c("x" = "UNIPROT")) %>%
      dplyr::pull(SYMBOL) %>%
      unique() %>%
      data.frame(x = .) %>%
      dplyr::left_join(cor_value1[, c("from", "pro_id", "cor")], by = c("x" = "pro_id")) %>% 
      dplyr::arrange(cor)
    colnames(x) <- c("protein_id", "chemical_id", "correlation")
    x
  })

gene_id_kegg <-
  purrr::map2(
    .x = gene_id_kegg,
    .y = result_kegg$Description,
    .f = function(x, y) {
      data.frame(x, pathway = y) %>%
        dplyr::select(chemical_id, protein_id, pathway, correlation)
    }
  )

gene_id_kegg <-
  gene_id_kegg %>%
  do.call(rbind, .) %>%
  dplyr::distinct(.keep_all = TRUE) %>%
  dplyr::left_join(node_data,
                   c("node", "compound.name"),
                   by = c("chemical_id" = "node"))


gene_id_kegg <- 
  data.frame(gene_id_kegg, pathway_class = "KEGG")

###Reactome
gene_id_reactome <-
  result_reactome$geneID %>%
  purrr::map(function(x) {
    x <- stringr::str_split(x, "\\/")[[1]] %>%
      unique()
    x <-
      data.frame(x) %>%
      dplyr::left_join(protein_list, by = c("x" = "ENTREZID")) %>%
      dplyr::pull(SYMBOL) %>%
      unique() %>%
      data.frame(x = .) %>%
      dplyr::left_join(cor_value1[, c("from", "pro_id", "cor")], by = c("x" = "pro_id")) %>% 
      dplyr::arrange(cor)
    colnames(x) <- c("protein_id", "chemical_id", "correlation")
    x
  })

gene_id_reactome <-
  purrr::map2(
    .x = gene_id_reactome,
    .y = result_reactome$Description,
    .f = function(x, y) {
      data.frame(x, pathway = y) %>%
        dplyr::select(chemical_id, protein_id, pathway, correlation)
    }
  )

gene_id_reactome <-
  gene_id_reactome %>%
  do.call(rbind, .) %>%
  dplyr::distinct(.keep_all = TRUE) %>%
  dplyr::left_join(node_data,
                   c("node", "compound.name"),
                   by = c("chemical_id" = "node"))

gene_id_reactome <- 
  data.frame(gene_id_reactome, pathway_class = "Reactome")

gene_id_data <-
  rbind(gene_id_go,
        gene_id_kegg,
        gene_id_reactome)

###only remain immune system
gene_id_data2 <-
  gene_id_data %>%
  dplyr::filter(
    pathway_class == "GO" &
      pathway %in% c(
        "acute inflammatory response",
        "humoral immune response",
        "complement activation",
        "regulation of humoral immune response"
      ) |
      pathway_class == "KEGG" &
      pathway %in% c("Complement and coagulation cascades") |
      pathway_class == "Reactome" &
      pathway %in% c(
        "Complement cascade",
        "Regulation of Complement cascade",
        "Platelet activation, signaling and aggregation"
      )
  )

temp_data1 <- 
  gene_id_data2 %>%
  dplyr::select(pathway, compound.name, correlation) %>%
  dplyr::mutate(affect = case_when(correlation > 0 ~ "pos",
                                   correlation < 0 ~ "neg")) %>%
  dplyr::select(-c(correlation)) %>%
  dplyr::distinct() %>%
  dplyr::select(compound.name, affect) %>%
  dplyr::mutate(compound.name = factor(compound.name, levels = names(sort(
    table(compound.name),
    decreasing =
      FALSE
  )))) 

plot1 <- 
  temp_data1 %>% 
  ggplot(aes(y = compound.name)) +
  geom_bar(aes(fill = affect), show.legend = FALSE,
           color = "black") +
  scale_x_reverse(expand = expansion(mult = c(0.1,0))) +
  scale_fill_manual(values = c("pos" = pal[100],
                               "neg" = pal[1])) +
  theme_bw() +
  labs(y = "", x= "Pathway number") +
  theme(panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        axis.ticks.y = element_blank())

plot1

# ggsave(plot1, filename = "plot1.pdf", width = 7, height = 7)

temp_data2 <- 
  gene_id_data2 %>%
  dplyr::select(pathway, compound.name, correlation) %>%
  dplyr::mutate(affect = case_when(correlation > 0 ~ "pos",
                                   correlation < 0 ~ "neg")) %>%
  dplyr::select(-c(correlation)) %>%
  dplyr::distinct() %>%
  dplyr::select(pathway, affect) %>%
  dplyr::mutate(pathway = factor(pathway, levels = names(sort(
    table(pathway),
    decreasing =
      FALSE
  )))) 

plot2 <- 
  temp_data2 %>% 
  ggplot(aes(y = pathway)) +
  geom_bar(aes(fill = affect), show.legend = FALSE,
           color = "black") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
  scale_fill_manual(values = c("pos" = pal[100],
                               "neg" = pal[1])) +
  theme_bw() +
  labs(y = "", x= "Compound number") +
  theme(panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        axis.ticks.y = element_blank())

plot2

# ggsave(plot2, filename = "plot2.pdf", width = 7, height = 7)

edge_data <-
  rbind(
    gene_id_data2[, c("compound.name", "protein_id", "correlation")] %>%
      dplyr::distinct(compound.name, protein_id, .keep_all = TRUE) %>%
      dplyr::rename(from = compound.name,
                    to = protein_id,
                    cor = correlation),
    gene_id_data2[, c("protein_id", "pathway", "correlation")] %>%
      dplyr::distinct(pathway, protein_id, .keep_all = TRUE) %>%
      dplyr::rename(from = protein_id,
                    to = pathway,
                    cor = correlation) %>%
      dplyr::mutate(cor = NA)
  )

node_data <-
  rbind(
    gene_id_data2[, "compound.name", drop = FALSE] %>%
      dplyr::distinct() %>%
      dplyr::mutate(class = "chemical") %>%
      dplyr::rename(node = compound.name),
    gene_id_data2[, "protein_id", drop = FALSE] %>%
      dplyr::distinct() %>%
      dplyr::mutate(class = "protein") %>%
      dplyr::rename(node = protein_id),
    gene_id_data2[, "pathway", drop = FALSE] %>%
      dplyr::distinct() %>%
      dplyr::mutate(class = "pathway") %>%
      dplyr::rename(node = pathway)
  )

node_data[node_data$class == "chemical",] <- 
  node_data[node_data$class == "chemical",][match(levels(temp_data1$compound.name),
                                                  node_data[node_data$class == "chemical",]$node),]

node_data[node_data$class == "pathway",] <- 
  node_data[node_data$class == "pathway",][match(levels(temp_data2$pathway),
                                                  node_data[node_data$class == "pathway",]$node),]

node_data <- 
  node_data %>% 
  dplyr::mutate(node = factor(node, levels = node))

temp_data <- 
  tidygraph::tbl_graph(nodes = node_data, 
                       edges = edge_data,
                       directed = TRUE) %>% 
  dplyr::mutate(Degree = centrality_degree(mode = 'all'))

pal <-
  wesanderson::wes_palette(name = "Zissou1", n = 100, type = "continuous")


my_layout <- create_layout(temp_data, 
                           layout = 'linear')

my_layout$y[my_layout$class == "protein"] <- 5

my_layout$y[my_layout$class == "pathway"] <- 10

my_layout1 <-
  my_layout

my_layout1$x <-
  my_layout$y

my_layout1$y <-
  my_layout$x

# my_layout1[my_layout1$class == "chemical",] <-
#   dplyr::left_join(data.frame(node = levels(temp_data1$compound.name)),
#                    my_layout1[my_layout1$class == "chemical", ], by = "node") %>%
#   dplyr::select(x, y, node, everything())
# 
# my_layout1[my_layout1$class == "pathway", ] <-
#   dplyr::left_join(data.frame(node = levels(temp_data2$pathway)),
#                    my_layout1[my_layout1$class == "pathway",], by = "node") %>%
#   dplyr::select(x, y, node, everything())

my_layout1$y[my_layout1$class == "chemical"] <-
  seq(from = 1, to = 100, length.out = sum(my_layout1$class == "chemical"))
  
my_layout1$y[my_layout1$class == "protein"] <-
  my_layout1$y[my_layout1$class == "protein"] <-
  seq(from = 1, to = 100, length.out = sum(my_layout1$class == "protein"))

my_layout1$y[my_layout1$class == "pathway"] <-
  my_layout1$y[my_layout1$class == "pathway"] <-
  seq(from = 1, to = 100, length.out = sum(my_layout1$class == "pathway"))

plot <-
  ggraph(my_layout1) +
  geom_edge_link(aes(color = cor),
                show.legend = FALSE) +
  geom_node_point(aes(fill = class,
                      size = Degree,
                      shape = class),
                  show.legend = FALSE) +
  scale_shape_manual(values = c(
    "chemical" = 21,
    "protein" = 21,
    "pathway" = 22
  )) +
  scale_fill_manual(
    values = c(
      "chemical" = ggsci::pal_d3()(10)[2],
      "protein" = ggsci::pal_d3()(10)[4],
      "pathway" = "black"
    )
  ) +
  scale_color_manual(
    values = c(
      "chemical" = ggsci::pal_d3()(10)[2],
      "protein" = ggsci::pal_d3()(10)[4],
      "pathway" = "black"
    )
  ) +
  geom_node_text(
    aes(
      x = x * 1.05,
      y = y * 1,
      label = node,
      hjust = ifelse(class == "chemical", 1, 0),
      # angle = -((-node_angle(x, y) + 90) %% 180) + 90,
      size = 3,
      colour = class
    ),
    size = 3,
    alpha = 1, 
    show.legend = FALSE
  ) +
  guides(edge_color = ggraph::guide_edge_colorbar(title = "Spearman correlation"),
         fill = guide_legend(title = "Class", 
                             override.aes = list(size = 4, linetype = "blank")),
         size = guide_legend(title = "Degree", override.aes = list(linetype = 0))) +
  ggraph::scale_edge_color_gradientn(colours = pal) +
  ggraph::scale_edge_width(range = c(0.2, 2)) +
  scale_size_continuous(range = c(3, 10)) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA)
    # legend.position = c(1,0), legend.justification = c(1,0)
  )

plot

# ggsave(plot, filename = "chemical_protein_pathway.pdf", width = 15, height = 7)



plot <- 
gene_id_data %>% 
  dplyr::select(compound.name, pathway) %>% 
  dplyr::distinct() %>% 
  dplyr::group_by(compound.name) %>% 
  dplyr::summarise(n = n()) %>% 
  dplyr::ungroup() %>% 
  dplyr::arrange(n) %>% 
  dplyr::mutate(compound.name = factor(compound.name, levels = compound.name)) %>% 
  ggplot(aes(y = compound.name, x = n)) +
  labs(x = "Pathway number", y = "") +
  geom_bar(stat = "identity", fill = "black") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        axis.ticks.y = element_blank())

 plot
 
# ggsave(plot, filename = "chemical_pathway_number.pdf", width = 7, height = 7)
 
 plot <- 
 gene_id_data %>% 
  dplyr::select(compound.name, pathway) %>% 
  dplyr::distinct() %>% 
  dplyr::group_by(pathway) %>% 
  dplyr::summarise(n = n()) %>% 
  dplyr::ungroup() %>% 
   dplyr::arrange(n) %>% 
   dplyr::mutate(pathway = factor(pathway, levels = pathway)) %>% 
   ggplot(aes(y = pathway, x = n)) +
   labs(x = "Chemical number", y = "") +
   geom_bar(stat = "identity", fill = "black") +
   scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
   theme_bw() +
   theme(panel.grid.minor = element_blank(),
         plot.background = element_rect(fill = "transparent", color = NA),
         panel.background = element_rect(fill = "transparent", color = NA),
         axis.ticks.y = element_blank())
  
plot
 # ggsave(plot, filename = "pathway_chemical_number.pdf", width = 14, height = 7)


###for each pathway
unique_pathway =
  c(
    "acute inflammatory response",
    "humoral immune response",
    "complement activation",
    "regulation of humoral immune response",
    "Complement and coagulation cascades",
    "Complement cascade",
    "Regulation of Complement cascade",
    "Platelet activation, signaling and aggregation"
  )


for(i in unique_pathway) {
  ###only remain immune system
  temp_gene_id_data <-
    gene_id_data %>%
    dplyr::filter(
      pathway_class == "GO" &
        pathway %in% i |
        pathway_class == "KEGG" &
        pathway %in% i |
        pathway_class == "Reactome" &
        pathway %in% i
    )
  
  temp_edge_data <-
    rbind(
      temp_gene_id_data[, c("compound.name", "protein_id", "correlation")] %>%
        dplyr::distinct(compound.name, protein_id, .keep_all = TRUE) %>%
        dplyr::rename(from = compound.name,
                      to = protein_id,
                      cor = correlation),
      temp_gene_id_data[, c("protein_id", "pathway", "correlation")] %>%
        dplyr::distinct(pathway, protein_id, .keep_all = TRUE) %>%
        dplyr::rename(from = protein_id,
                      to = pathway,
                      cor = correlation) %>%
        dplyr::mutate(cor = NA)
    )
  
  temp_node_data <-
    rbind(
      temp_gene_id_data[, "compound.name", drop = FALSE] %>%
        dplyr::distinct() %>%
        dplyr::mutate(class = "chemical") %>%
        dplyr::rename(node = compound.name),
      temp_gene_id_data[, "protein_id", drop = FALSE] %>%
        dplyr::distinct() %>%
        dplyr::mutate(class = "protein") %>%
        dplyr::rename(node = protein_id),
      temp_gene_id_data[, "pathway", drop = FALSE] %>%
        dplyr::distinct() %>%
        dplyr::mutate(class = "pathway") %>%
        dplyr::rename(node = pathway)
    )
  
  temp_node_data <-
    temp_node_data %>%
    dplyr::mutate(node = factor(node, levels = node))
  
  temp_data <-
    tidygraph::tbl_graph(nodes = temp_node_data,
                         edges = temp_edge_data,
                         directed = TRUE) %>%
    dplyr::mutate(Degree = centrality_degree(mode = 'all'))
  
  plot =
    ggraph(temp_data) +
    geom_edge_link(aes(color = cor),
                   show.legend = FALSE) +
    geom_node_point(aes(
      fill = class,
      size = Degree,
      shape = class
    ),
    show.legend = FALSE) +
    scale_shape_manual(values = c(
      "chemical" = 21,
      "protein" = 21,
      "pathway" = 22
    )) +
    scale_fill_manual(
      values = c(
        "chemical" = ggsci::pal_d3()(10)[2],
        "protein" = ggsci::pal_d3()(10)[4],
        "pathway" = "black"
      )
    ) +
    scale_color_manual(
      values = c(
        "chemical" = ggsci::pal_d3()(10)[2],
        "protein" = ggsci::pal_d3()(10)[4],
        "pathway" = "black"
      )
    ) +
    geom_node_text(
      aes(
        x = x * 1,
        y = y * 1,
        label = node,
        hjust = ifelse(class == "chemical", 0, 1),
        angle = 0,
        # angle = -((-node_angle(x, y) + 90) %% 180) + 90,
        size = 5,
        colour = class
      ),
      size = 3,
      alpha = 1,
      show.legend = FALSE
    ) +
    guides(
      edge_color = ggraph::guide_edge_colorbar(title = "Spearman correlation"),
      fill = guide_legend(
        title = "Class",
        override.aes = list(size = 4, linetype = "blank")
      ),
      size = guide_legend(title = "Degree", override.aes = list(linetype = 0))
    ) +
    ggraph::scale_edge_color_gradientn(colours = pal) +
    ggraph::scale_edge_width(range = c(0.2, 2)) +
    scale_size_continuous(range = c(8, 15)) +
    theme_void() +
    theme(
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.background = element_rect(fill = "transparent", color = NA)
      # legend.position = c(1,0), legend.justification = c(1,0)
    ) +
    coord_flip()
  dir.create("pathway_enrichment_network")
  ggsave(
    plot,
    filename = file.path("pathway_enrichment_network", paste(i, ".pdf", sep = "")),
    width = 7,
    height = 7
  )
}








