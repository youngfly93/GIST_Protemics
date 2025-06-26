# GIST Proteomics Pathway 分析 - 最终优化版本
# 功能：获取目标基因的正相关和负相关基因
# 优化：内存使用减少，计算速度提升，支持缓存

# 主函数 - 获取相关基因
dbGIST_Proteomics_ID_Pathway <- function(Dataset = Dataset,
                                        ID = ID,
                                        Number = 100,
                                        Proteomics_ID_Pathway_list = NULL) {
  
  # 如果提供了数据，使用提供的数据
  if (!is.null(Proteomics_ID_Pathway_list)) {
    cor_matrix <- Proteomics_ID_Pathway_list[[Dataset]]
    
    if("try-error" %in% class(try(cor_matrix[, ID]))){
      return(NULL)
    } else {
      target_cor <- cor_matrix[, ID]
      return(names(target_cor[order(target_cor, decreasing = TRUE)][1:Number]))
    }
  }
  
  # 否则使用优化版本
  if (Number == 100) {
    # 获取50个正相关和50个负相关
    result <- dbGIST_Proteomics_ID_Pathway_Optimized(
      Dataset = Dataset,
      ID = ID,
      top_positive = 50,
      top_negative = 50
    )
    return(result$all)
  } else {
    # 获取前N个最相关的基因
    result <- dbGIST_Proteomics_ID_Pathway_TopN(
      Dataset = Dataset,
      ID = ID,
      Number = Number
    )
    return(result)
  }
}

# 优化版本 - 获取正负相关基因
dbGIST_Proteomics_ID_Pathway_Optimized <- function(Dataset = "Sun's Study",
                                                   ID = ID,
                                                   top_positive = 50,
                                                   top_negative = 50,
                                                   use_cache = TRUE) {
  
  # 缓存管理
  if (use_cache) {
    cache_dir <- "correlation_cache"
    if (!dir.exists(cache_dir)) {
      dir.create(cache_dir, showWarnings = FALSE)
    }
    
    cache_key <- paste0(gsub("[^A-Za-z0-9]", "_", Dataset), "_",
                        gsub("[^A-Za-z0-9]", "_", ID), "_",
                        top_positive, "_", top_negative, ".RDS")
    cache_file <- file.path(cache_dir, cache_key)
    
    # 检查缓存
    if (file.exists(cache_file)) {
      cache_time <- file.info(cache_file)$mtime
      if (Sys.time() - cache_time < 3600) {
        return(readRDS(cache_file))
      }
    }
  }
  
  # 加载数据
  cor_matrix <- load_correlation_matrix(Dataset)
  
  # 检查ID
  if (!ID %in% colnames(cor_matrix)) {
    warning(paste("ID not found:", ID))
    return(NULL)
  }
  
  # 获取相关性值
  target_cor <- cor_matrix[, ID]
  
  # 确保有名称
  if (is.null(names(target_cor))) {
    names(target_cor) <- rownames(cor_matrix)
  }
  
  # 清理内存
  rm(cor_matrix)
  gc(verbose = FALSE)
  
  # 获取结果
  result <- get_top_correlations_safe(target_cor, top_positive, top_negative)
  
  # 缓存结果
  if (use_cache && !is.null(result)) {
    saveRDS(result, cache_file)
  }
  
  return(result)
}

# 获取前N个最相关的基因
dbGIST_Proteomics_ID_Pathway_TopN <- function(Dataset = "Sun's Study",
                                              ID = ID,
                                              Number = 100) {
  
  # 加载数据
  cor_matrix <- load_correlation_matrix(Dataset)
  
  # 检查ID
  if (!ID %in% colnames(cor_matrix)) {
    warning(paste("ID not found:", ID))
    return(NULL)
  }
  
  # 获取相关性值
  target_cor <- cor_matrix[, ID]
  
  # 清理内存
  rm(cor_matrix)
  gc(verbose = FALSE)
  
  # 返回前N个
  result <- names(target_cor[order(target_cor, decreasing = TRUE)][1:min(Number, length(target_cor))])
  return(result)
}

# 加载相关性矩阵
load_correlation_matrix <- function(Dataset) {
  
  # 尝试从拆分文件加载
  split_file <- file.path("correlation_data", 
                         paste0("Proteomics_", gsub("[^A-Za-z0-9]", "_", Dataset), ".RDS"))
  
  if (file.exists(split_file)) {
    return(readRDS(split_file))
  }
  
  # 从原始文件加载
  if (file.exists("Proteomics_ID_Pathway_list.RDS")) {
    all_data <- readRDS("Proteomics_ID_Pathway_list.RDS")
    
    if (!Dataset %in% names(all_data)) {
      stop(paste("Dataset not found:", Dataset))
    }
    
    cor_matrix <- all_data[[Dataset]]
    rm(all_data)
    gc(verbose = FALSE)
    
    return(cor_matrix)
  }
  
  stop("No data file found")
}

# 安全的获取top相关基因
get_top_correlations_safe <- function(correlations, n_positive = 50, n_negative = 50) {
  
  # 移除NA值
  valid_idx <- !is.na(correlations)
  correlations <- correlations[valid_idx]
  
  if (length(correlations) == 0) {
    return(list(
      positive = character(0),
      negative = character(0),
      all = character(0),
      n_positive = 0,
      n_negative = 0
    ))
  }
  
  # 分离正负相关
  pos_idx <- which(correlations > 0)
  neg_idx <- which(correlations < 0)
  
  # 初始化结果
  result <- list(
    positive = character(0),
    negative = character(0),
    all = character(0),
    n_positive = 0,
    n_negative = 0
  )
  
  # 处理正相关
  if (length(pos_idx) > 0 && n_positive > 0) {
    n_pos <- min(n_positive, length(pos_idx))
    pos_values <- correlations[pos_idx]
    pos_order <- order(pos_values, decreasing = TRUE)
    top_pos_idx <- pos_idx[pos_order[1:n_pos]]
    result$positive <- names(correlations)[top_pos_idx]
    result$n_positive <- n_pos
  }
  
  # 处理负相关
  if (length(neg_idx) > 0 && n_negative > 0) {
    n_neg <- min(n_negative, length(neg_idx))
    neg_values <- correlations[neg_idx]
    neg_order <- order(neg_values, decreasing = FALSE)
    top_neg_idx <- neg_idx[neg_order[1:n_neg]]
    result$negative <- names(correlations)[top_neg_idx]
    result$n_negative <- n_neg
  }
  
  result$all <- c(result$positive, result$negative)
  return(result)
}

# 批量处理多个基因
dbGIST_Proteomics_ID_Pathway_Batch <- function(Dataset = "Sun's Study",
                                               IDs = character(),
                                               top_positive = 50,
                                               top_negative = 50) {
  
  if (length(IDs) == 0) {
    stop("No IDs provided")
  }
  
  # 批量处理
  results <- lapply(IDs, function(id) {
    tryCatch({
      dbGIST_Proteomics_ID_Pathway_Optimized(
        Dataset = Dataset,
        ID = id,
        top_positive = top_positive,
        top_negative = top_negative,
        use_cache = TRUE
      )
    }, error = function(e) {
      warning(paste("Error processing", id, ":", e$message))
      return(NULL)
    })
  })
  
  names(results) <- IDs
  return(results)
}

# 清理缓存
clear_correlation_cache <- function(older_than_days = 7) {
  cache_dir <- "correlation_cache"
  if (!dir.exists(cache_dir)) return(invisible())
  
  cache_files <- list.files(cache_dir, pattern = "\\.RDS$", full.names = TRUE)
  if (length(cache_files) == 0) return(invisible())
  
  file_info <- file.info(cache_files)
  cutoff_time <- Sys.time() - older_than_days * 24 * 60 * 60
  
  old_files <- cache_files[file_info$mtime < cutoff_time]
  if (length(old_files) > 0) {
    unlink(old_files)
    cat("Removed", length(old_files), "old cache files\n")
  }
}

# 数据预处理 - 拆分大文件
split_proteomics_data <- function(input_file = "Proteomics_ID_Pathway_list.RDS") {
  
  if (!file.exists(input_file)) {
    stop("Input file not found")
  }
  
  if (!dir.exists("correlation_data")) {
    dir.create("correlation_data")
  }
  
  cat("Loading and splitting data...\n")
  data_list <- readRDS(input_file)
  
  for (dataset_name in names(data_list)) {
    safe_name <- gsub("[^A-Za-z0-9]", "_", dataset_name)
    output_file <- file.path("correlation_data", paste0("Proteomics_", safe_name, ".RDS"))
    
    if (!file.exists(output_file)) {
      saveRDS(data_list[[dataset_name]], output_file)
      cat("Created:", output_file, "\n")
    }
  }
  
  cat("Data splitting completed!\n")
}

# 获取正负相关基因（便捷函数）
dbGIST_Proteomics_ID_Pathway_PosNeg <- function(Dataset = "Sun's Study",
                                                ID = ID,
                                                Number = 50) {
  
  result <- dbGIST_Proteomics_ID_Pathway_Optimized(
    Dataset = Dataset,
    ID = ID,
    top_positive = Number,
    top_negative = Number
  )
  
  return(result)
}

cat("Pathway analysis functions loaded successfully!\n")
cat("Main function: dbGIST_Proteomics_ID_Pathway()\n")
cat("For first time use, run: split_proteomics_data()\n")