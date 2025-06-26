# 最简单的内存优化版本 - 直接替换原有的pathway.R

# 不再一次性加载所有数据
# 而是根据需要读取特定数据集

dbGIST_Proteomics_ID_Pathway <- function(Dataset = "Sun's Study",
                                        ID = ID,
                                        Number = 100,
                                        Proteomics_ID_Pathway_list = NULL) {
  
  # 如果没有传入数据，尝试从拆分的文件加载
  if (is.null(Proteomics_ID_Pathway_list)) {
    # 定义数据文件路径
    data_file <- switch(Dataset,
                        "Sun's Study" = "correlation_data/Proteomics_Sun_s_Study.RDS",
                        "ZZU In-depth Proteomics" = "correlation_data/Proteomics_ZZU_In_depth_Proteomics.RDS",
                        {
                          # 如果找不到拆分文件，从原始文件加载特定数据集
                          cat("Loading dataset from original file...\n")
                          temp_list <- readRDS("Proteomics_ID_Pathway_list.RDS")
                          cor_matrix <- temp_list[[Dataset]]
                          rm(temp_list)
                          gc()
                          return(cor_matrix)
                        })
    
    if (file.exists(data_file)) {
      cor_matrix <- readRDS(data_file)
    } else {
      # 回退到原始文件
      cat("Split file not found, loading from original file...\n")
      temp_list <- readRDS("Proteomics_ID_Pathway_list.RDS")
      cor_matrix <- temp_list[[Dataset]]
      rm(temp_list)
      gc()
    }
  } else {
    # 使用传入的数据
    cor_matrix <- Proteomics_ID_Pathway_list[[Dataset]]
  }
  
  # 原有逻辑保持不变
  if("try-error" %in% class(try(cor_matrix[, ID]))) {
    return(NULL)
  } else {
    target_cor <- cor_matrix[, ID]
    
    # 如果只需要50个正相关和50个负相关，可以在这里优化
    if (Number == 50) {
      # 分别获取正负相关
      positive_cor <- target_cor[target_cor > 0]
      negative_cor <- target_cor[target_cor < 0]
      
      # 获取top 50
      top_positive <- names(positive_cor[order(positive_cor, decreasing = TRUE)][1:min(50, length(positive_cor))])
      top_negative <- names(negative_cor[order(negative_cor)][1:min(50, length(negative_cor))])
      
      # 清理内存
      rm(cor_matrix)
      gc()
      
      # 返回结构化结果，方便富集分析使用
      return(list(
        all = c(top_positive, top_negative),  # 兼容原有代码
        positive = top_positive,
        negative = top_negative
      ))
    } else {
      # 原有逻辑
      result <- names(target_cor[order(target_cor, decreasing = TRUE)][1:Number])
      
      # 清理内存
      rm(cor_matrix)
      gc()
      
      return(result)
    }
  }
}

# 新增：专门用于获取正负相关基因的函数
dbGIST_Proteomics_ID_Pathway_PosNeg <- function(Dataset = "Sun's Study",
                                                ID = ID,
                                                Number = 50) {
  
  result <- dbGIST_Proteomics_ID_Pathway(Dataset = Dataset, 
                                         ID = ID, 
                                         Number = Number)
  
  if (is.list(result) && !is.null(result$positive)) {
    return(result)
  } else {
    # 如果返回的是向量，需要重新处理
    # 这种情况不应该发生，但为了兼容性还是处理一下
    return(list(
      all = result,
      positive = result[1:min(Number, length(result))],
      negative = NULL
    ))
  }
}

# 一次性拆分数据的辅助函数（只需运行一次）
split_proteomics_data <- function() {
  if (!dir.exists("correlation_data")) {
    dir.create("correlation_data")
  }
  
  cat("Splitting Proteomics_ID_Pathway_list.RDS...\n")
  data_list <- readRDS("Proteomics_ID_Pathway_list.RDS")
  
  for (dataset_name in names(data_list)) {
    safe_name <- gsub("[^A-Za-z0-9]", "_", dataset_name)
    output_file <- paste0("correlation_data/Proteomics_", safe_name, ".RDS")
    saveRDS(data_list[[dataset_name]], output_file)
    cat("Saved:", dataset_name, "to", output_file, "\n")
  }
  
  cat("Data splitting completed!\n")
}

# 使用示例：
# 1. 首次使用时运行拆分（只需运行一次）
split_proteomics_data()

# 2. 获取相关基因
result <- dbGIST_Proteomics_ID_Pathway_PosNeg(
  Dataset = "Sun's Study",
  ID = "P4HA1",
  Number = 50
)
 
positive_genes <- result$positive  # 正相关的50个基因
negative_genes <- result$negative  # 负相关的50个基因