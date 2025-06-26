# ==== Module 1: 年龄分析 Server ====

# 数据提取函数
extract_age_data <- function(gene_id) {
  if(gene_id %in% rownames(Protemics_list[[2]]$Matrix)) {
    values <- as.numeric(Protemics_list[[2]]$Matrix[gene_id, ])
    age_group <- Protemics_list[[2]]$Clinical$Age[match(colnames(Protemics_list[[2]]$Matrix), 
                                                       Protemics_list[[2]]$Clinical$Sample.ID)]
    
    data <- data.frame(
      Dataset = Protemics_list[[2]]$ID,
      Sample = colnames(Protemics_list[[2]]$Matrix),
      Value = values,
      Age_Group = age_group,
      stringsAsFactors = FALSE
    )
    
    return(na.omit(data))
  } else {
    return(NULL)
  }
}

# 创建分析服务器
createAnalysisServer(
  id = "age",
  analysis_function = dbGIST_Proteomics_boxplot_Age,
  extract_data_function = extract_age_data,
  global_state = global_state
)