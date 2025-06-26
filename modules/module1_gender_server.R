# ==== Module 1: 性别分析 Server ====

# 数据提取函数
extract_gender_data <- function(gene_id) {
  data_list <- list()
  
  # 数据集2
  if(gene_id %in% rownames(Protemics_list[[2]]$Matrix)) {
    values <- as.numeric(Protemics_list[[2]]$Matrix[gene_id, ])
    gender <- Protemics_list[[2]]$Clinical$Gender[match(colnames(Protemics_list[[2]]$Matrix), 
                                                        Protemics_list[[2]]$Clinical$Sample.ID)]
    data_list[[1]] <- data.frame(
      Dataset = Protemics_list[[2]]$ID,
      Sample = colnames(Protemics_list[[2]]$Matrix),
      Value = values,
      Gender = gender,
      stringsAsFactors = FALSE
    )
  }
  
  # 数据集3
  if(gene_id %in% rownames(Protemics_list[[3]]$Matrix)) {
    values <- as.numeric(Protemics_list[[3]]$Matrix[gene_id, ])
    gender <- Protemics_list[[3]]$Clinical$Gender[match(colnames(Protemics_list[[3]]$Matrix), 
                                                        Protemics_list[[3]]$Clinical$Sample)]
    data_list[[2]] <- data.frame(
      Dataset = Protemics_list[[3]]$ID,
      Sample = colnames(Protemics_list[[3]]$Matrix),
      Value = values,
      Gender = gender,
      stringsAsFactors = FALSE
    )
  }
  
  # 合并数据并去除NA
  if(length(data_list) > 0) {
    combined_data <- do.call(rbind, data_list)
    return(na.omit(combined_data))
  } else {
    return(NULL)
  }
}

# 创建分析服务器
createAnalysisServer(
  id = "gender",
  analysis_function = dbGIST_Proteomics_boxplot_Gender,
  extract_data_function = extract_gender_data,
  global_state = global_state
)