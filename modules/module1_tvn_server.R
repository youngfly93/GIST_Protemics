# ==== Module 1: 肿瘤vs正常 Server ====

# 数据提取函数
extract_tvn_data <- function(gene_id) {
  # 尝试从不同数据集提取数据
  data_list <- list()
  
  # 数据集1
  if(gene_id %in% rownames(Protemics_list[[1]]$Matrix)) {
    values <- as.numeric(Protemics_list[[1]]$Matrix[gene_id, ])
    data_list[[1]] <- data.frame(
      Dataset = Protemics_list[[1]]$ID,
      Sample = colnames(Protemics_list[[1]]$Matrix),
      Value = values,
      Group = "Tumor",
      stringsAsFactors = FALSE
    )
  }
  
  # 数据集2和4
  if(gene_id %in% rownames(Protemics_list[[2]]$Matrix) && 
     gene_id %in% rownames(Protemics_list[[4]]$Matrix)) {
    tumor_values <- as.numeric(Protemics_list[[2]]$Matrix[gene_id, ])
    normal_values <- as.numeric(Protemics_list[[4]]$Matrix[gene_id, ])
    
    data_list[[2]] <- rbind(
      data.frame(
        Dataset = Protemics_list[[2]]$ID,
        Sample = colnames(Protemics_list[[2]]$Matrix),
        Value = tumor_values,
        Group = "Tumor",
        stringsAsFactors = FALSE
      ),
      data.frame(
        Dataset = Protemics_list[[2]]$ID,
        Sample = colnames(Protemics_list[[4]]$Matrix),
        Value = normal_values,
        Group = "Normal",
        stringsAsFactors = FALSE
      )
    )
  }
  
  # 合并所有数据
  if(length(data_list) > 0) {
    return(do.call(rbind, data_list))
  } else {
    return(NULL)
  }
}

# 创建分析服务器
createAnalysisServer(
  id = "tvn",
  analysis_function = dbGIST_Proteomics_boxplot_TvsN,
  extract_data_function = extract_tvn_data,
  global_state = global_state
)