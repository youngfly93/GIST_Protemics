# ==== Module 1: 风险等级 Server ====

# 数据提取函数
extract_risk_data <- function(gene_id) {
  data_list <- list()
  
  # 数据集1
  if(gene_id %in% rownames(Protemics_list[[1]]$Matrix)) {
    values <- as.numeric(Protemics_list[[1]]$Matrix[gene_id, ])
    risk_levels <- Protemics_list[[1]]$Clinical$NIH[match(colnames(Protemics_list[[1]]$Matrix), 
                                                          Protemics_list[[1]]$Clinical$ID)]
    data_list[[1]] <- data.frame(
      Dataset = Protemics_list[[1]]$ID,
      Sample = colnames(Protemics_list[[1]]$Matrix),
      Value = values,
      Risk_Level = risk_levels,
      stringsAsFactors = FALSE
    )
  }
  
  # 数据集2
  if(gene_id %in% rownames(Protemics_list[[2]]$Matrix)) {
    values <- as.numeric(Protemics_list[[2]]$Matrix[gene_id, ])
    risk_levels <- Protemics_list[[2]]$Clinical$NIH[match(colnames(Protemics_list[[2]]$Matrix), 
                                                          Protemics_list[[2]]$Clinical$Sample.ID)]
    data_list[[2]] <- data.frame(
      Dataset = Protemics_list[[2]]$ID,
      Sample = colnames(Protemics_list[[2]]$Matrix),
      Value = values,
      Risk_Level = risk_levels,
      stringsAsFactors = FALSE
    )
  }
  
  # 数据集3
  if(gene_id %in% rownames(Protemics_list[[3]]$Matrix)) {
    values <- as.numeric(Protemics_list[[3]]$Matrix[gene_id, ])
    risk_levels <- Protemics_list[[3]]$Clinical$NIH[match(colnames(Protemics_list[[3]]$Matrix), 
                                                          Protemics_list[[3]]$Clinical$Sample)]
    data_list[[3]] <- data.frame(
      Dataset = Protemics_list[[3]]$ID,
      Sample = colnames(Protemics_list[[3]]$Matrix),
      Value = values,
      Risk_Level = risk_levels,
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
  id = "risk",
  analysis_function = dbGIST_Proteomics_boxplot_Risk,
  extract_data_function = extract_risk_data,
  global_state = global_state
)