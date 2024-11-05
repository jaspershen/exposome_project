#' @title Show the duplicated samples of peak tables from Thermo QE instrument
#' @description Show the duplicated samples of peak tables from Thermo QE instrument.
#' @author Xiaotao Shen
#' \email{shenxt1990@@163.com}
#' @param peak_table A peak_table from XCMS or other software.
#' @return A list of duplicated samples.
#' @export
#' @importFrom magrittr %>%

setGeneric(
  name = "show_duplicated_samples2",
  def = function(peak_table) {
    idx <-
      which(stringr::str_detect(colnames(peak_table), "_[0-9]{10,16}"))
    if (length(idx) == 0) {
      return(cat(crayon::red("No duplicated samples.\n")))
    }
    duplicated_name <- colnames(peak_table)[idx]
    duplicated_name <-
      duplicated_name %>%
      stringr::str_replace("_[0-9]{10,16}", "") %>%
      unique()
    
    result <-
      lapply(duplicated_name, function(x) {
        temp_idx1 <- match(x, colnames(peak_table))
        temp_idx2 <- stringr::str_detect(colnames(peak_table),
                                         paste(x, "_", sep = "")) %>%
          which()
        temp_idx <- c(temp_idx1, temp_idx2)
        temp_idx <- temp_idx[!is.na(temp_idx)]
        x <-
          colnames(peak_table)[temp_idx]
        x
      })
    names(result) <-
      duplicated_name
    return(result)
  }
)

#' @title Remove the duplicated samples of peak tables from Thermo QE instrument
#' @description Remove the duplicated samples of peak tables from Thermo QE instrument.
#' @author Xiaotao Shen
#' \email{shenxt1990@@163.com}
#' @param peak_table A peak_table from XCMS or other software.
#' @return A new peak table without duplicated samples.
#' @export
#' @importFrom magrittr %>%
setGeneric(
  name = "remove_duplicated_name2",
  def = function(peak_table) {
    idx <-
      which(stringr::str_detect(colnames(peak_table), "_[0-9]{10,16}"))
    if (length(idx) == 0) {
      cat(crayon::red("No duplicated samples.\n"))
      return(peak_table)
    }
    duplicated_name <- colnames(peak_table)[idx]
    duplicated_name <-
      duplicated_name %>%
      stringr::str_replace("_[0-9]{10,16}", "") %>%
      unique()
    remove_name <- NULL
    for (x in duplicated_name) {
      temp_idx1 <- match(x, colnames(peak_table))
      temp_idx2 <- stringr::str_detect(colnames(peak_table),
                                       paste(x, "_", sep = "")) %>%
        which()
      temp_idx <- c(temp_idx1, temp_idx2)
      temp_idx <- temp_idx[!is.na(temp_idx)]
      
      cat(crayon::yellow(rep("-", 13)), "\n")
      cat(crayon::yellow("--->"), crayon::green(x), "\n")
      if (length(temp_idx) == 1) {
        cat(crayon::red("Only one sample"), "\n")
        # colnames(peak_table)[temp_idx] <-
        #   colnames(peak_table)[temp_idx] %>%
        #   stringr::str_replace("_[0-9]{10,16}", "")
      } else{
        na_number <-
          apply(peak_table[, temp_idx], 2, function(x)
            sum(is.na(x)))
        info <- data.frame(
          sample = names(na_number),
          NA_number = unname(na_number),
          stringsAsFactors = FALSE
        )
        print(info)
        cat("\n")
        remove_idx <- temp_idx[-which.min(na_number)]
        remove_name <-
          c(remove_name, colnames(peak_table)[remove_idx])
        # remain_idx <- temp_idx[which.min(na_number)]
        cat(crayon::green(paste(colnames(peak_table)[remove_idx], collapse = ";"),
                          "are removed.\n"))
      }
    }
    
    peak_table <-
      peak_table %>%
      dplyr::select(-c(remove_name))
    
    colnames(peak_table) <-
      colnames(peak_table) %>%
      stringr::str_replace("_[0-9]{10,16}", "")
    peak_table
  }
)


theme_sxtlight <- function(...) {
  require(grid)
  ggplot2::theme_bw(...) +
    ggplot2::theme(
      axis.title.x = element_text(size = 13),
      axis.text.x = element_text(size = 12),
      axis.title.y = element_text(size = 13),
      axis.text.y = element_text(size = 12),
      rect = element_rect(fill = "white")
      # plot.margin = unit(rep(0.5,4), 'lines'),
      # panel.background = element_rect(fill = 'transparent', color =
      #                                   'transparent'),
      # panel.border = element_rect(fill = 'transparent', color = 'transparent'),
      # panel.grid = element_blank(),
      # axis.title = element_text(color = 'black', vjust = 0.1),
      # axis.ticks.length = unit(-0.3, "lines"),
      # axis.ticks = element_line(colour = "grey20"),
      # legend.title = element_blank(),
      # legend.key = element_rect(fill = 'transparent', color = 'transparent')
    )
}


# MAT <- matrix(rnorm(10000 * 400), nrow = 400)
# system.time(res <- bigcor(MAT, nblocks = 400))

# test <- ff(vmode = "single", dim = c(10000, 10))
#
#
# bigcor <- function(x,
#                    nblocks = 10,
#                    verbose = TRUE,
#                    ...)
# {
#   library(ff, quietly = TRUE)
#   NCOL <- ncol(x)
#
#   ## test if ncol(x) %% nblocks gives remainder 0
#   if (NCOL %% nblocks != 0)
#     stop("Choose different 'nblocks' so that ncol(x) %% nblocks = 0!")
#
#   ## preallocate square matrix of dimension
#   ## ncol(x) in 'ff' single format
#   corMAT <- ff(vmode = "single", dim = c(NCOL, NCOL))
#
#   ## split column numbers into 'nblocks' groups
#   SPLIT <- split(1:NCOL, rep(1:nblocks, each = NCOL / nblocks))
#
#   ## create all unique combinations of blocks
#   COMBS <- expand.grid(1:length(SPLIT), 1:length(SPLIT))
#   COMBS <- t(apply(COMBS, 1, sort))
#   COMBS <- unique(COMBS)
#
#   ## iterate through each block combination, calculate correlation matrix
#   ## between blocks and store them in the preallocated matrix on both
#   ## symmetric sides of the diagonal
#   for (i in 1:nrow(COMBS)) {
#     COMB <- COMBS[i,]
#     G1 <- SPLIT[[COMB[1]]]
#     G2 <- SPLIT[[COMB[2]]]
#     if (verbose)
#       cat("Block", COMB[1], "with Block", COMB[2], "\n")
#     flush.console()
#     COR <- cor(MAT[, G1], MAT[, G2], ...)
#     corMAT[G1, G2] <- COR
#     corMAT[G2, G1] <- t(COR)
#     COR <- NULL
#   }
#
#   gc()
#   return(corMAT)
# }
















plotLambdaVSdeviation <-
  function(object,
           xlab = "Log lambda",
           ylab = "Deviation ratio (%)") {
    data.frame(
      lambda = object$lambda,
      dev.ratio = object$dev.ratio,
      stringsAsFactors = FALSE
    ) %>%
      ggplot(aes(log(lambda), dev.ratio * 100)) +
      labs(x = xlab, y = ylab) +
      geom_point(size = 2) +
      geom_line() +
      theme_bw() +
      theme(
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 13)
      )
  }


plotLambdaVScoefficients <-
  function(object,
           xlab = "Log lambda",
           ylab = "Coefficients") {
    beta <-
      object$beta %>%
      as.matrix() %>%
      t() %>%
      as_tibble() %>%
      mutate(lambda = object$lambda) %>%
      tidyr::gather(., key = "feature", value = "coef", -lambda)
    
    label_index <- seq(range(log(beta$lambda))[1],
                       range(log(beta$lambda))[2],
                       by = 1)
    label <-
      lapply(label_index, function(x) {
        c(log(object$lambda) - x) %>%
          abs() %>%
          which.min() %>%
          `[`(object$df + 1, .)
      }) %>%
      unlist()
    
    
    beta %>%
      ggplot(., aes(log(lambda), coef)) +
      geom_line(aes(colour = feature), show.legend = FALSE) +
      scale_x_continuous(
        position = "bottom",
        sec.axis = sec_axis(
          ~ .,
          name = "",
          breaks = label_index,
          labels = label
        )
      ) +
      scale_colour_manual(values = colorRampPalette(ggsci::pal_uchicago()(5))(600)) +
      labs(x = xlab, y = ylab) +
      theme_bw() +
      theme(axis.title = element_text(size = 15),
            axis.text = element_text(size = 13))
  }


plotLambdaVSerror <- function(object,
                              xlab = "Log lambda",
                              ylab = "Mean absolute error") {
  cvm <-
    data.frame(
      lambda = object$lambda,
      df = object$glmnet.fit$df,
      cvm = object$cvm,
      cvup = object$cvup,
      cvlo = object$cvlo,
      stringsAsFactors = FALSE
    )
  
  cvm %>%
    ggplot(., aes(log(lambda), cvm)) +
    geom_vline(xintercept = log(c(object$lambda.min,
                                  object$lambda.1se)),
               linetype = 2) +
    geom_errorbar(aes(ymin = cvlo, ymax = cvup), colour = "#155F83FF") +
    geom_point(size = 2, colour = "#FFA319FF") +
    scale_x_continuous(
      position = "bottom",
      sec.axis = sec_axis(
        trans = ~ .,
        breaks = log(cvm$lambda)[seq(1, 100, by = 7)],
        labels = cvm$df[seq(1, 100, by = 7)],
        name = ""
      )
    ) +
    labs(x = xlab, y = ylab) +
    theme_bw() +
    theme(axis.title = element_text(size = 15),
          axis.text = element_text(size = 13)
          # plot.margin = margin(5.5, 5.5, 5.5, 5.5, "pt"))
    )
}


# x <- c(8, 8, 1,1,2,3,4,3,5,1,7)

sxt_rank <- function(x){
  data.frame(rank = c(1:length(x)), order = order(x), stringsAsFactors = FALSE) %>% 
    dplyr::arrange(order) %>% 
    data.frame(x, ., stringsAsFactors = FALSE) %>% 
    pull(rank)
}




volcano_plot <- function(fc,p_value,
                         p.cutoff = 0.05,
                         fc.cutoff = 2,
                         theme = c("light", "dark")
){
  theme <- match.arg(theme)
  temp_data <- data.frame(fc = log(fc,2),
                          p_value = -log(p_value, 10),
                          stringsAsFactors = FALSE)
  temp_data <- 
    temp_data %>% 
    dplyr::mutate(class = case_when(
      p_value > -log(p.cutoff, 10) & fc > log(fc.cutoff, 2) ~ "Yes",
      p_value > -log(p.cutoff, 10) & fc < log(1/fc.cutoff, 2) ~ "Yes",
      TRUE ~ "No"
    ))
  
  plot <-
    temp_data %>% 
    ggplot(aes(fc, p_value)) +
    # geom_vline(xintercept = 0, color = "black", linetype = 2) + 
    # geom_vline(xintercept = log(fc.cutoff,2), color = "black", linetype = 2) + 
    # geom_vline(xintercept = log(1/fc.cutoff,2), color = "black", linetype = 2) + 
    geom_hline(yintercept = -log(p.cutoff,10), color = "#FB8072", linetype = 2) + 
    geom_point(shape = 16, aes(color = class), show.legend = FALSE, alpha = 1) +
    scale_color_manual(values = c("Yes" = "#FB8072", "No" = "#D9D9D9")) +
    theme(
      axis.title = element_text(size = 13),
      axis.text = element_text(size = 12),
      legend.title = element_text(size = 13),
      legend.text = element_text(size = 12),
      legend.position = c(0, 1),
      legend.justification = c(0, 1),
      panel.background = element_rect(fill = "transparent", color = NA),
      plot.background = element_rect(fill = "transparent", color = NA),
      legend.background = element_rect(fill = "transparent", color = NA),
      strip.background = element_rect(fill = "#0099B47F"),
      strip.text = element_text(color = "white", size = 13)
    ) +
    labs(
      x = "log2(Fold change)",
      y = "-log10(p value, FDR)"
    )
  if(theme == "light"){
    plot <- plot + theme_classic()  +
      theme(
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 12),
        legend.position = c(0, 1),
        legend.justification = c(0, 1),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        strip.background = element_rect(fill = "#0099B47F"),
        strip.text = element_text(color = "white", size = 13)
      ) 
  }else{
    plot <- plot + ggdark::dark_theme_classic() +
      theme(
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 12),
        legend.position = c(0, 1),
        legend.justification = c(0, 1),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        strip.background = element_rect(fill = "#0099B47F"),
        strip.text = element_text(color = "white", size = 13)
      ) 
  }
  
  
}



get_stable_network <- function(network_list1, 
                               network_list2,
                               network_list3,
                               network_list4){
  
  ##1 vs 2
  similarity1 <- 
    purrr::map(.x = network_list1, .f = function(x){
      purrr::map(.x = network_list2, .f = function(y){
        c(cal_network_similarity(network1 = x, network2 = y))
      }) %>% 
        do.call(rbind, .)
    }) %>% 
    do.call(rbind, .)
  
  name1 <- 
    purrr::map(.x = names(network_list1), .f = function(x){
      purrr::map(.x = names(network_list2), .f = function(y){
        c(x, y)
      }) %>% 
        do.call(rbind, .)
    }) %>% 
    do.call(rbind, .)
  
  similarity1 <- 
    data.frame(name1, similarity1, stringsAsFactors = FALSE)
  
  
  ##1 vs 3
  similarity2 <- 
    purrr::map(.x = network_list1, .f = function(x){
      purrr::map(.x = network_list3, .f = function(y){
        c(cal_network_similarity(network1 = x, network2 = y))
      }) %>% 
        do.call(rbind, .)
    }) %>% 
    do.call(rbind, .)
  
  name2 <- 
    purrr::map(.x = names(network_list1), .f = function(x){
      purrr::map(.x = names(network_list3), .f = function(y){
        c(x, y)
      }) %>% 
        do.call(rbind, .)
    }) %>% 
    do.call(rbind, .)
  
  similarity2 <- 
    data.frame(name2, similarity2, stringsAsFactors = FALSE)
  
  
  ##1 vs 4
  similarity3 <- 
    purrr::map(.x = network_list1, .f = function(x){
      purrr::map(.x = network_list4, .f = function(y){
        c(cal_network_similarity(network1 = x, network2 = y))
      }) %>% 
        do.call(rbind, .)
    }) %>% 
    do.call(rbind, .)
  
  name3 <- 
    purrr::map(.x = names(network_list1), .f = function(x){
      purrr::map(.x = names(network_list4), .f = function(y){
        c(x, y)
      }) %>% 
        do.call(rbind, .)
    }) %>% 
    do.call(rbind, .)
  
  similarity3 <- 
    data.frame(name3, similarity3, stringsAsFactors = FALSE)
  
  
  ##2 vs 3
  similarity4 <- 
    purrr::map(.x = network_list2, .f = function(x){
      purrr::map(.x = network_list3, .f = function(y){
        c(cal_network_similarity(network1 = x, network2 = y))
      }) %>% 
        do.call(rbind, .)
    }) %>% 
    do.call(rbind, .)
  
  name4 <- 
    purrr::map(.x = names(network_list2), .f = function(x){
      purrr::map(.x = names(network_list3), .f = function(y){
        c(x, y)
      }) %>% 
        do.call(rbind, .)
    }) %>% 
    do.call(rbind, .)
  
  similarity4 <- 
    data.frame(name4, similarity4, stringsAsFactors = FALSE)
  
  
  ##2 vs 4
  similarity5 <- 
    purrr::map(.x = network_list2, .f = function(x){
      purrr::map(.x = network_list4, .f = function(y){
        c(cal_network_similarity(network1 = x, network2 = y))
      }) %>% 
        do.call(rbind, .)
    }) %>% 
    do.call(rbind, .)
  
  name5 <- 
    purrr::map(.x = names(network_list2), .f = function(x){
      purrr::map(.x = names(network_list4), .f = function(y){
        c(x, y)
      }) %>% 
        do.call(rbind, .)
    }) %>% 
    do.call(rbind, .)
  
  similarity5 <- 
    data.frame(name5, similarity5, stringsAsFactors = FALSE)
  
  
  ##3 vs 4
  similarity6 <- 
    purrr::map(.x = network_list3, .f = function(x){
      purrr::map(.x = network_list4, .f = function(y){
        c(cal_network_similarity(network1 = x, network2 = y))
      }) %>% 
        do.call(rbind, .)
    }) %>% 
    do.call(rbind, .)
  
  name6 <- 
    purrr::map(.x = names(network_list3), .f = function(x){
      purrr::map(.x = names(network_list4), .f = function(y){
        c(x, y)
      }) %>% 
        do.call(rbind, .)
    }) %>% 
    do.call(rbind, .)
  
  similarity6 <- 
    data.frame(name6, similarity6, stringsAsFactors = FALSE)
  
  
  similarity <- rbind(similarity1, similarity2,
                      similarity3, similarity4,
                      similarity5, similarity6)
  
  rownames(similarity) <- NULL
  colnames(similarity)[1:2] <- c("network1", "network2")
  similarity
}


cal_network_similarity <- function(network1, network2){
  node1 <- igraph::vertex_attr(network1, name = "node")
  node2 <- igraph::vertex_attr(network2, name = "node")
  
  ###node jaccard index
  node_jaccard_index <- length(intersect(node1, node2))/length(union(node1, node2))
  
  ##edge jaccard index
  edge1 <- igraph::as_data_frame(network1)
  edge2 <- igraph::as_data_frame(network2)
  
  edge1$from <- node1[edge1$from]
  edge1$to <- node1[edge1$to]
  
  edge2$from <- node2[edge2$from]
  edge2$to <- node2[edge2$to]
  
  edge1 <- edge1 %>% 
    dplyr::arrange(from) %>% 
    dplyr::select(from, to)
  
  edge2 <- edge2 %>% 
    dplyr::arrange(from) %>% 
    dplyr::select(from, to)
  
  edge1 <- apply(edge1, 1, function(x){
    sort(x)
  }) %>% 
    t() %>% 
    as.data.frame()
  
  edge2 <- apply(edge2, 1, function(x){
    sort(x)
  }) %>% 
    t() %>% 
    as.data.frame()
  
  colnames(edge1) <- colnames(edge2) <- c("from", "to")
  
  edge1 <- paste(edge1$from, edge1$to, sep = "_")
  edge2 <- paste(edge2$from, edge2$to, sep = "_")
  
  ###edvge jaccard index
  edge_jaccard_index <- length(intersect(edge1, edge2))/length(union(edge1, edge2))
  
  result <- c(node_jaccard_index, edge_jaccard_index)
  names(result) <- c("node", "edge")  
  return(result)
}




library(parmigene)
cal_mi_p <- function(data1, data2, intersect_name, times = 10000){
  x <- data1[intersect_name,]
  pc1 <- 
    prcomp(x = x, scale. = TRUE)
  y <- data2[intersect_name,]
  pc2 <- 
    prcomp(x = y, scale. = TRUE)
  
  temp_data <- data.frame(pc1 = pc1$x[,1], pc2 = pc2$x[,1],stringsAsFactors = FALSE)
  mi <- knnmi(x = temp_data[,1], y = temp_data[,2])
  
  ##permutation
  library(future)
  plan(future::multiprocess)
  
  null_mi <- 
    furrr::future_map(.x = 1:times, .f = function(i){
      x2 <- 
        x %>% 
        apply(2, function(z){
          sample(z, length(z))
        }) %>% 
        as.data.frame()
      
      y2 <- 
        y %>% 
        apply(2, function(z){
          sample(z, length(z))
        }) %>% 
        as.data.frame()
      
      pc1 <- 
        prcomp(x = x2, scale. = TRUE)
      
      pc2 <- 
        prcomp(x = y2, scale. = TRUE)
      
      knnmi(x = pc1$x[,1], y = pc2$x[,1])
    }) %>% 
    unlist()  
  
  p <- 1 - sum(mi > null_mi)/times
  c(mi, p)
}






cal_two_networks_p <- function(network1,
                               network2,
                               sample_info1,
                               sample_info2,
                               same.time = TRUE){
  
  all_p <- NULL
  for(i in 1:length(network1)){
    cat(i, "\n")
    for(j in 1:length(network2)){
      cat(j, "\n")
      data1 <- 
        network1[[i]] 
      
      if(same.time){
        ga1 <- 
          sample_info1$g_stage %>% 
          ggplot2::cut_width(width = 1,center = 1) %>% 
          as.character() %>% 
          stringr::str_replace("\\[", "\\(") 
        
        data1 <-
          t(data1) %>% 
          as.data.frame() %>% 
          data.frame(ga = ga1, stringsAsFactors = FALSE) %>% 
          plyr::dlply(.variables = .(ga)) %>% 
          purrr::map(.f = function(x){
            x %>% 
              dplyr::select(-ga) %>% 
              colMeans()
          }) %>% 
          do.call(rbind, .) %>% 
          as.data.frame()
      }else{
        subject_id1 <- 
          sample_info1$subject_id
        
        data1 <-
          t(data1) %>% 
          as.data.frame() %>% 
          data.frame(subject_id = subject_id1, stringsAsFactors = FALSE) %>% 
          plyr::dlply(.variables = .(subject_id)) %>% 
          purrr::map(.f = function(x){
            x %>% 
              dplyr::select(-subject_id) %>% 
              colMeans()
          }) %>% 
          do.call(rbind, .) %>% 
          as.data.frame() 
      }
      
      data2 <- 
        network2[[j]]
      
      if(same.time){
        ga2 <- 
          sample_info2$g_stage %>% 
          ggplot2::cut_width(width = 1,center = 1) %>% 
          as.character() %>% 
          stringr::str_replace("\\[", "\\(") 
        
        data2 <-
          t(data2) %>% 
          as.data.frame() %>% 
          data.frame(ga = ga2, stringsAsFactors = FALSE) %>% 
          plyr::dlply(.variables = .(ga)) %>% 
          purrr::map(.f = function(x){
            x %>% 
              dplyr::select(-ga) %>% 
              colMeans()
          }) %>% 
          do.call(rbind, .) %>% 
          as.data.frame()
      }else{
        subject_id2 <- 
          sample_info2$subject_id
        
        data2 <-
          t(data2) %>% 
          as.data.frame() %>% 
          data.frame(subject_id = subject_id2, stringsAsFactors = FALSE) %>% 
          plyr::dlply(.variables = .(subject_id)) %>% 
          purrr::map(.f = function(x){
            x %>% 
              dplyr::select(-subject_id) %>% 
              colMeans()
          }) %>% 
          do.call(rbind, .) %>% 
          as.data.frame()  
      }
      
      intersect_name <- intersect(rownames(data1), rownames(data2))
      
      p <- 
        try(expr =       cal_mi_p(data1 = data1, 
                                  data2 = data2, 
                                  intersect_name = intersect_name,
                                  times = 10000) , silent = TRUE)
      if(class(p) == "try-error"){
        all_p <- c(all_p, 1)
      }else{
        all_p <- c(all_p, p[2]) 
      }
    }
  }
  
  return(all_p)
  
}