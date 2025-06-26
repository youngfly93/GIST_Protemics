# ==== Module 4: 伊马替尼耐药分析 Server ====

# 数据提取函数
extract_drug_resistance_data <- function(gene_id) {
  if(gene_id %in% rownames(Protemics_list[[2]]$Matrix)) {
    values <- as.numeric(Protemics_list[[2]]$Matrix[gene_id, ])
    response <- Protemics_list[[2]]$Clinical$IM.Response[match(colnames(Protemics_list[[2]]$Matrix), 
                                                               Protemics_list[[2]]$Clinical$Sample.ID)]
    
    data <- data.frame(
      Dataset = Protemics_list[[2]]$ID,
      Sample = colnames(Protemics_list[[2]]$Matrix),
      Protein_Value = values,
      IM_Response = response,
      stringsAsFactors = FALSE
    )
    
    return(na.omit(data))
  } else {
    return(NULL)
  }
}

# 创建分析服务器
createAnalysisServer(
  id = "drug_resistance",
  analysis_function = dbGIST_Proteomics_boxplot_IM.Response,
  extract_data_function = extract_drug_resistance_data,
  global_state = global_state
)