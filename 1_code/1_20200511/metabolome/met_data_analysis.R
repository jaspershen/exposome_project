####load metabolomics data
setwd(r4projects::get_project_wd())
load("3_data_analysis/data_20200511/metabolome/expression_data")
met_expression_data <- expression_data
load("3_data_analysis/data_20200511/metabolome/sample_info")
met_sample_info <- sample_info
load("3_data_analysis/data_20200511/metabolome/variable_info")
met_variable_info <- variable_info


##load exposome data
load("3_data_analysis/data_20200511/exposome/expression_data")
load("3_data_analysis/data_20200511/exposome/sample_info")
load("3_data_analysis/data_20200511/exposome/variable_info")

exp_expression_data <- expression_data

exp_sample_info <- sample_info

exp_variable_info <- variable_info


setwd("3_data_analysis/data_20200511/met_exp/")

