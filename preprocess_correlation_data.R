# 数据预处理脚本 - 将大型RDS文件拆分为小文件
# 只需运行一次

# 方法1：拆分数据集
split_correlation_data <- function(input_file = "Proteomics_ID_Pathway_list.RDS",
                                  output_dir = "correlation_data/") {
  
  # 创建输出目录
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # 加载原始数据
  cat("Loading original data...\n")
  data_list <- readRDS(input_file)
  
  # 为每个数据集创建单独的文件
  for (dataset_name in names(data_list)) {
    cat("Processing dataset:", dataset_name, "\n")
    
    # 创建安全的文件名
    safe_name <- gsub("[^A-Za-z0-9]", "_", dataset_name)
    output_file <- file.path(output_dir, paste0("Proteomics_", safe_name, ".RDS"))
    
    # 保存单个数据集
    saveRDS(data_list[[dataset_name]], output_file)
    
    cat("  Saved to:", output_file, "\n")
  }
  
  cat("Data splitting completed!\n")
  
  # 返回文件映射
  return(list(
    "Sun's Study" = file.path(output_dir, "Proteomics_Sun_s_Study.RDS"),
    "ZZU In-depth Proteomics" = file.path(output_dir, "Proteomics_ZZU_In_depth_Proteomics.RDS")
  ))
}

# 方法2：创建索引文件（只保存top相关的基因）
create_top_correlations_index <- function(input_file = "Proteomics_ID_Pathway_list.RDS",
                                         output_file = "top_correlations_index.RDS",
                                         top_n = 100) {
  
  cat("Creating top correlations index...\n")
  
  # 加载数据
  data_list <- readRDS(input_file)
  
  # 创建索引
  index_list <- list()
  
  for (dataset_name in names(data_list)) {
    cat("Processing dataset:", dataset_name, "\n")
    
    cor_matrix <- data_list[[dataset_name]]
    index_list[[dataset_name]] <- list()
    
    # 为每个蛋白质保存top相关的
    for (protein in colnames(cor_matrix)) {
      correlations <- cor_matrix[, protein]
      
      # 获取正相关top N
      positive_cor <- correlations[correlations > 0 & names(correlations) != protein]
      top_positive <- positive_cor[order(positive_cor, decreasing = TRUE)][1:min(top_n, length(positive_cor))]
      
      # 获取负相关top N
      negative_cor <- correlations[correlations < 0]
      top_negative <- negative_cor[order(negative_cor)][1:min(top_n, length(negative_cor))]
      
      # 保存结果
      index_list[[dataset_name]][[protein]] <- list(
        positive = data.frame(
          protein = names(top_positive),
          correlation = as.numeric(top_positive),
          stringsAsFactors = FALSE
        ),
        negative = data.frame(
          protein = names(top_negative),
          correlation = as.numeric(top_negative),
          stringsAsFactors = FALSE
        )
      )
    }
  }
  
  # 保存索引
  saveRDS(index_list, output_file)
  cat("Index created successfully!\n")
  
  return(index_list)
}

# 使用索引查询的函数
query_from_index <- function(index_file = "top_correlations_index.RDS",
                            Dataset = "Sun's Study",
                            ID = "P4HA1",
                            Number = 50) {
  
  # 加载索引
  index_list <- readRDS(index_file)
  
  # 检查数据集和蛋白质是否存在
  if (!Dataset %in% names(index_list)) {
    stop("Dataset not found in index")
  }
  
  if (!ID %in% names(index_list[[Dataset]])) {
    return(NULL)
  }
  
  # 获取结果
  protein_data <- index_list[[Dataset]][[ID]]
  
  # 返回指定数量的结果
  result <- list(
    positive = head(protein_data$positive$protein, Number),
    negative = head(protein_data$negative$protein, Number)
  )
  
  return(result)
}

# 执行预处理（根据需要选择一种方法）
# 方法1：拆分文件
# file_mapping <- split_correlation_data()

# 方法2：创建索引
# index <- create_top_correlations_index()

# 方法3：使用之前的SQLite或HDF5方法（见pathway_optimized.R）