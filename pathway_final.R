# GIST Proteomics Pathway 分析 - 最终优化版本
# 功能：获取目标基因的正相关和负相关基因
# 优化：内存使用减少，计算速度提升，支持缓存


library(org.Hs.eg.db)

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

# ============ 富集分析功能 ============

# 加载富集分析所需的包
load_enrichment_packages <- function() {
  required_packages <- c("clusterProfiler", "ReactomePA", "enrichplot", "ggplot2", "patchwork")
  
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      cat(paste("Installing", pkg, "...\n"))
      BiocManager::install(pkg, quiet = TRUE)
    }
    library(pkg, character.only = TRUE)
  }
}

# ============ GSEA 分析功能 ============

# 读取GMT文件
read_gmt_file <- function(gmt_file) {
  if (!file.exists(gmt_file)) {
    stop(paste("GMT file not found:", gmt_file))
  }
  
  lines <- readLines(gmt_file, warn = FALSE)
  gene_sets <- list()
  
  for (line in lines) {
    if (nchar(line) == 0) next
    
    fields <- strsplit(line, "\t")[[1]]
    if (length(fields) < 3) next
    
    pathway_name <- fields[1]
    genes <- fields[3:length(fields)]
    genes <- genes[genes != "" & !is.na(genes)]
    
    if (length(genes) > 0) {
      gene_sets[[pathway_name]] <- genes
    }
  }
  
  return(gene_sets)
}

# 计算GSEA富集得分
calculate_gsea_es <- function(gene_list, gene_set, p = 1) {
  # gene_list: 排序后的基因列表（名称向量，按相关系数排序）
  # gene_set: 基因集（字符向量）
  # p: 权重参数，默认为1
  
  N <- length(gene_list)
  gene_set <- intersect(gene_set, names(gene_list))
  Nh <- length(gene_set)
  
  if (Nh == 0) {
    return(list(ES = 0, running_ES = rep(0, N), hit_indices = integer(0)))
  }
  
  # 计算每个基因的权重
  hit_indicator <- names(gene_list) %in% gene_set
  
  # 获取在基因集中的基因的相关系数
  correlations <- gene_list[hit_indicator]
  
  # 计算权重和
  Nr <- sum(abs(correlations)^p)
  
  running_sum <- numeric(N)
  running_ES <- numeric(N)
  hit_indices <- which(hit_indicator)
  
  current_sum <- 0
  
  for (i in 1:N) {
    if (hit_indicator[i]) {
      # 基因在基因集中
      current_sum <- current_sum + (abs(gene_list[i])^p) / Nr
    } else {
      # 基因不在基因集中
      current_sum <- current_sum - 1 / (N - Nh)
    }
    running_ES[i] <- current_sum
  }
  
  # 找到最大偏离0的点作为ES
  max_pos <- max(running_ES)
  min_neg <- min(running_ES)
  
  ES <- ifelse(abs(max_pos) > abs(min_neg), max_pos, min_neg)
  
  return(list(
    ES = ES,
    running_ES = running_ES,
    hit_indices = hit_indices,
    Nh = Nh,
    N = N
  ))
}

# 计算GSEA的标准化富集得分和p值
calculate_gsea_significance <- function(gene_list, gene_set, nperm = 1000, p = 1) {
  # 计算观察到的ES
  observed_result <- calculate_gsea_es(gene_list, gene_set, p)
  observed_ES <- observed_result$ES
  
  if (observed_result$Nh == 0) {
    return(list(
      ES = 0,
      NES = 0,
      pval = 1,
      padj = 1,
      running_ES = observed_result$running_ES,
      hit_indices = observed_result$hit_indices,
      leading_edge = character(0)
    ))
  }
  
  # 随机排列计算null分布
  null_ES <- numeric(nperm)
  gene_names <- names(gene_list)
  
  for (i in 1:nperm) {
    # 随机打乱基因顺序
    permuted_order <- sample(length(gene_list))
    permuted_gene_list <- gene_list[permuted_order]
    names(permuted_gene_list) <- gene_names[permuted_order]
    
    # 计算随机ES
    perm_result <- calculate_gsea_es(permuted_gene_list, gene_set, p)
    null_ES[i] <- perm_result$ES
  }
  
  # 计算标准化富集得分（NES）
  if (observed_ES >= 0) {
    positive_null <- null_ES[null_ES >= 0]
    NES <- ifelse(length(positive_null) > 0, 
                 observed_ES / mean(positive_null), 
                 observed_ES)
  } else {
    negative_null <- null_ES[null_ES < 0]
    NES <- ifelse(length(negative_null) > 0, 
                 -observed_ES / mean(abs(negative_null)), 
                 observed_ES)
  }
  
  # 计算p值
  if (observed_ES >= 0) {
    pval <- sum(null_ES >= observed_ES) / nperm
  } else {
    pval <- sum(null_ES <= observed_ES) / nperm
  }
  
  # 计算leading edge基因
  running_ES <- observed_result$running_ES
  if (observed_ES >= 0) {
    max_idx <- which.max(running_ES)
    leading_edge_indices <- observed_result$hit_indices[observed_result$hit_indices <= max_idx]
  } else {
    min_idx <- which.min(running_ES)
    leading_edge_indices <- observed_result$hit_indices[observed_result$hit_indices <= min_idx]
  }
  
  leading_edge_genes <- gene_names[leading_edge_indices]
  
  return(list(
    ES = observed_ES,
    NES = NES,
    pval = max(pval, 1/nperm),  # 避免p值为0
    running_ES = running_ES,
    hit_indices = observed_result$hit_indices,
    leading_edge = leading_edge_genes,
    gene_set_size = observed_result$Nh
  ))
}

# 批量GSEA分析
perform_gsea_analysis <- function(gene_list, gene_sets, nperm = 1000, min_size = 15, max_size = 500) {
  # 过滤基因集大小
  gene_sets <- gene_sets[sapply(gene_sets, function(x) {
    overlap <- length(intersect(x, names(gene_list)))
    overlap >= min_size && overlap <= max_size
  })]
  
  if (length(gene_sets) == 0) {
    warning("No gene sets meet the size criteria")
    return(NULL)
  }
  
  cat(paste("Running GSEA on", length(gene_sets), "gene sets...\n"))
  
  # 进行GSEA分析
  gsea_results <- lapply(names(gene_sets), function(pathway_name) {
    if ((match(pathway_name, names(gene_sets)) %% 50) == 0) {
      cat(paste("Processing pathway", match(pathway_name, names(gene_sets)), "/", length(gene_sets), "\n"))
    }
    
    result <- calculate_gsea_significance(gene_list, gene_sets[[pathway_name]], nperm = nperm)
    result$pathway <- pathway_name
    result$size <- result$gene_set_size
    return(result)
  })
  
  names(gsea_results) <- names(gene_sets)
  
  # 整理结果为数据框
  results_df <- data.frame(
    pathway = sapply(gsea_results, function(x) x$pathway),
    ES = sapply(gsea_results, function(x) x$ES),
    NES = sapply(gsea_results, function(x) x$NES),
    pval = sapply(gsea_results, function(x) x$pval),
    size = sapply(gsea_results, function(x) x$size),
    stringsAsFactors = FALSE
  )
  
  # FDR校正
  results_df$padj <- p.adjust(results_df$pval, method = "BH")
  
  # 按NES排序
  results_df <- results_df[order(abs(results_df$NES), decreasing = TRUE), ]
  
  # 添加详细结果
  for (i in 1:nrow(results_df)) {
    pathway_name <- results_df$pathway[i]
    results_df$leading_edge[i] <- paste(gsea_results[[pathway_name]]$leading_edge, collapse = ";")
  }
  
  return(list(
    results = results_df,
    detailed_results = gsea_results
  ))
}

# GSEA可视化函数
plot_gsea_enrichment <- function(gsea_result, gene_list, pathway_name, gene_set) {
  library(ggplot2)
  
  N <- length(gene_list)
  running_ES <- gsea_result$running_ES
  hit_indices <- gsea_result$hit_indices
  ES <- gsea_result$ES
  NES <- gsea_result$NES
  pval <- gsea_result$pval
  
  # 创建数据框
  df <- data.frame(
    position = 1:N,
    running_ES = running_ES,
    hit = 1:N %in% hit_indices
  )
  
  # 主图：running enrichment score
  p1 <- ggplot(df, aes(x = position, y = running_ES)) +
    geom_line(color = "green", size = 1.2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    geom_hline(yintercept = ES, linetype = "dashed", color = "red") +
    labs(title = paste("GSEA:", pathway_name),
         subtitle = paste("NES =", round(NES, 3), ", p-value =", format(pval, scientific = TRUE)),
         x = "Rank in Gene List",
         y = "Running Enrichment Score") +
    theme_minimal() +
    theme(plot.title = element_text(size = 12, face = "bold"))
  
  # 中间图：基因集中基因的位置
  hit_df <- data.frame(
    position = hit_indices,
    y = 1
  )
  
  p2 <- ggplot(hit_df, aes(x = position, y = y)) +
    geom_point(color = "black", alpha = 0.8, size = 0.5) +
    scale_y_continuous(limits = c(0.5, 1.5)) +
    labs(x = "", y = "") +
    theme_minimal() +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          panel.grid = element_blank()) +
    ggtitle("Gene Set Hits")
  
  # 底部图：基因排序的度量（相关系数）
  metric_df <- data.frame(
    position = 1:N,
    metric = gene_list
  )
  
  p3 <- ggplot(metric_df, aes(x = position, y = metric)) +
    geom_area(fill = "lightblue", alpha = 0.7) +
    labs(x = "Rank in Gene List",
         y = "Correlation") +
    theme_minimal()
  
  # 组合图形
  library(patchwork)
  combined_plot <- p1 / p2 / p3 + plot_layout(heights = c(3, 0.5, 1))
  
  return(combined_plot)
}

# 创建GSEA结果汇总图
plot_gsea_summary <- function(gsea_results, top_n = 20, title = "GSEA Results") {
  library(ggplot2)
  library(dplyr)
  
  if (is.null(gsea_results) || nrow(gsea_results$results) == 0) {
    return(NULL)
  }
  
  # 选择top结果
  top_results <- gsea_results$results %>%
    filter(padj < 0.05) %>%
    arrange(desc(abs(NES))) %>%
    head(top_n)
  
  if (nrow(top_results) == 0) {
    return(NULL)
  }
  
  # 清理pathway名称（去掉前缀）
  top_results$pathway_clean <- gsub("^HALLMARK_|^KEGG_", "", top_results$pathway)
  top_results$pathway_clean <- gsub("_", " ", top_results$pathway_clean)
  
  # 按NES排序
  top_results$pathway_clean <- factor(top_results$pathway_clean, 
                                     levels = top_results$pathway_clean[order(top_results$NES)])
  
  # 添加显著性标记
  top_results$significance <- ifelse(top_results$padj < 0.001, "***", 
                                   ifelse(top_results$padj < 0.01, "**", 
                                        ifelse(top_results$padj < 0.05, "*", "")))
  
  # 创建图形
  p <- ggplot(top_results, aes(x = pathway_clean, y = NES, fill = NES > 0)) +
    geom_col(alpha = 0.8) +
    geom_text(aes(label = significance, y = NES + sign(NES) * 0.1), 
              hjust = 0.5, size = 4) +
    scale_fill_manual(values = c("TRUE" = "#E31A1C", "FALSE" = "#1F78B4"),
                      name = "Direction", labels = c("Negative", "Positive")) +
    coord_flip() +
    labs(title = title,
         x = "Pathway",
         y = "Normalized Enrichment Score (NES)",
         caption = "* p<0.05, ** p<0.01, *** p<0.001") +
    theme_minimal() +
    theme(plot.title = element_text(size = 14, face = "bold"),
          axis.text.y = element_text(size = 10),
          legend.position = "bottom")
  
  return(p)
}

# 获取基因的排序列表（基于相关系数）
get_ranked_gene_list <- function(Dataset = "Sun's Study", ID = ID) {
  # 加载相关性矩阵
  cor_matrix <- load_correlation_matrix(Dataset)
  
  # 检查ID
  if (!ID %in% colnames(cor_matrix)) {
    stop(paste("ID not found:", ID))
  }
  
  # 获取相关性值
  target_cor <- cor_matrix[, ID]
  
  # 清理内存
  rm(cor_matrix)
  gc(verbose = FALSE)
  
  # 移除NA值并按相关系数排序（从高到低）
  target_cor <- target_cor[!is.na(target_cor)]
  target_cor <- sort(target_cor, decreasing = TRUE)
  
  return(target_cor)
}

# GSEA主分析函数
dbGIST_Proteomics_GSEA <- function(Dataset = "Sun's Study",
                                   ID = ID,
                                   gmt_files = c("GSEA_KEGG.gmt", "GSEA_hallmark.gmt"),
                                   nperm = 1000,
                                   min_size = 15,
                                   max_size = 500,
                                   plot_top = 10) {
  
  cat(paste("Step 1: Getting ranked gene list for", ID, "...\n"))
  
  # 获取排序的基因列表
  ranked_gene_list <- get_ranked_gene_list(Dataset, ID)
  
  cat(paste("Found", length(ranked_gene_list), "genes with correlation data\n"))
  
  # 初始化结果列表
  all_results <- list()
  all_plots <- list()
  
  for (gmt_file in gmt_files) {
    if (!file.exists(gmt_file)) {
      warning(paste("GMT file not found:", gmt_file))
      next
    }
    
    cat(paste("\nStep 2: Loading gene sets from", gmt_file, "...\n"))
    
    # 读取基因集
    gene_sets <- read_gmt_file(gmt_file)
    
    cat(paste("Loaded", length(gene_sets), "gene sets\n"))
    
    # 进行GSEA分析
    cat(paste("Step 3: Running GSEA analysis...\n"))
    gsea_results <- perform_gsea_analysis(ranked_gene_list, gene_sets, 
                                        nperm = nperm, min_size = min_size, max_size = max_size)
    
    if (is.null(gsea_results)) {
      warning(paste("No results from", gmt_file))
      next
    }
    
    # 生成汇总图
    database_name <- gsub("\\.gmt$", "", basename(gmt_file))
    summary_plot <- plot_gsea_summary(gsea_results, top_n = plot_top, 
                                    title = paste("GSEA Results:", database_name, "for", ID))
    
    # 为显著结果生成详细图
    significant_results <- gsea_results$results[gsea_results$results$padj < 0.05, ]
    detailed_plots <- list()
    
    if (nrow(significant_results) > 0) {
      cat(paste("Generating detailed plots for top", min(5, nrow(significant_results)), "pathways...\n"))
      
      for (i in 1:min(5, nrow(significant_results))) {
        pathway_name <- significant_results$pathway[i]
        gene_set <- gene_sets[[pathway_name]]
        gsea_detail <- gsea_results$detailed_results[[pathway_name]]
        
        detailed_plot <- plot_gsea_enrichment(gsea_detail, ranked_gene_list, 
                                            pathway_name, gene_set)
        detailed_plots[[pathway_name]] <- detailed_plot
      }
    }
    
    # 保存结果
    all_results[[database_name]] <- gsea_results
    all_plots[[database_name]] <- list(
      summary = summary_plot,
      detailed = detailed_plots
    )
  }
  
  cat("\nGSEA analysis completed!\n")
  
  return(list(
    results = all_results,
    plots = all_plots,
    ranked_genes = ranked_gene_list
  ))
}

# 富集分析函数（改编自enrich_func.R）
enrichment_Multi <- function(proteins_data, OrgDb = "org.Hs.eg.db", orgKegg = "hsa", organismReactome = "human") {
  library(clusterProfiler)
  library(ReactomePA)
  
  # 加载对应的OrgDb
  if (is.character(OrgDb)) {
    if (!requireNamespace(OrgDb, quietly = TRUE)) {
      BiocManager::install(OrgDb)
    }
    OrgDb <- get(OrgDb)
  }
  
  gene <- unique(proteins_data)
  
  ID_trans <- function(enrich_result, from, to = "SYMBOL", OrgDb) {
    library(clusterProfiler)
    trans_result <- apply(enrich_result, 1, from = from, to = to, OrgDb2 = OrgDb, function(x, from, to, OrgDb2) {
      genelist <- unlist(strsplit(x["geneID"], split = '/'))
      genelist <- try(bitr(genelist, fromType = from, toType = to, OrgDb = OrgDb2), silent = TRUE)
      if ('try-error' %in% class(genelist)) {
        return("")
      } else {
        result <- paste0(genelist[[to]], collapse = "/")
        return(result)
      }
    })
    enrich_result$geneID2 <- trans_result
    return(enrich_result)
  }
  
  # GO富集分析
  GO_results <- tryCatch({
    res <- enrichGO(gene = gene, OrgDb = OrgDb, keyType = "SYMBOL", ont = "ALL", pvalueCutoff = 0.05)
    res
  }, error = function(e) {
    # 如果SYMBOL失败，尝试其他keyType
    tryCatch({
      enrichGO(gene = gene, OrgDb = OrgDb, keyType = "UNIPROT", ont = "ALL", pvalueCutoff = 0.05)
    }, error = function(e2) NULL)
  })
  
  # KEGG富集分析
  KEGG_results <- tryCatch({
    # 先尝试转换为ENTREZID
    entrez_ids <- bitr(gene, fromType = "SYMBOL", toType = "ENTREZID", OrgDb = OrgDb)
    enrichKEGG(gene = entrez_ids$ENTREZID, organism = orgKegg, pvalueCutoff = 0.05)
  }, error = function(e) NULL)
  
  # Reactome富集分析
  Reactome_results <- tryCatch({
    entrez_ids <- bitr(gene, fromType = "SYMBOL", toType = "ENTREZID", OrgDb = OrgDb)
    enrichPathway(gene = entrez_ids$ENTREZID, organism = organismReactome, pvalueCutoff = 0.05)
  }, error = function(e) NULL)
  
  enrichment_list <- list()
  enrichment_list[["go"]] <- GO_results
  enrichment_list[["kegg"]] <- KEGG_results
  enrichment_list[["reactome"]] <- Reactome_results
  return(enrichment_list)
}

# Pathway分析 + 富集分析的整合函数（包含GSEA）
dbGIST_Proteomics_Pathway_Enrichment <- function(Dataset = "Sun's Study",
                                                 ID = ID,
                                                 top_positive = 50,
                                                 top_negative = 50,
                                                 perform_enrichment = TRUE,
                                                 perform_gsea = TRUE,
                                                 gmt_files = c("GSEA_KEGG.gmt", "GSEA_hallmark.gmt"),
                                                 gsea_nperm = 1000,
                                                 gsea_min_size = 15,
                                                 gsea_max_size = 500,
                                                 plot_results = TRUE,
                                                 OrgDb = "org.Hs.eg.db",
                                                 orgKegg = "hsa",
                                                 organismReactome = "human") {
  
  # 1. 获取相关基因
  cat("Step 1: Getting correlated proteins...\n")
  pathway_result <- dbGIST_Proteomics_ID_Pathway_Optimized(
    Dataset = Dataset,
    ID = ID,
    top_positive = top_positive,
    top_negative = top_negative
  )
  
  if (is.null(pathway_result)) {
    stop("Failed to get pathway results")
  }
  
  cat(sprintf("Found %d positive and %d negative correlated proteins\n", 
              length(pathway_result$positive), 
              length(pathway_result$negative)))
  
  # 2. 进行富集分析
  enrichment_results <- NULL
  plots <- list()
  
  if (perform_enrichment) {
    cat("\nStep 2: Performing enrichment analysis...\n")
    load_enrichment_packages()
    
    # 对不同组进行富集分析
    enrichment_results <- list()
    
    # 正相关基因富集
    if (length(pathway_result$positive) > 5) {
      cat("- Analyzing positive correlated proteins...\n")
      enrichment_results$positive <- enrichment_Multi(
        pathway_result$positive, 
        OrgDb = OrgDb, 
        orgKegg = orgKegg, 
        organismReactome = organismReactome
      )
    }
    
    # 负相关基因富集
    if (length(pathway_result$negative) > 5) {
      cat("- Analyzing negative correlated proteins...\n")
      enrichment_results$negative <- enrichment_Multi(
        pathway_result$negative, 
        OrgDb = OrgDb, 
        orgKegg = orgKegg, 
        organismReactome = organismReactome
      )
    }
    
    # 全部基因富集
    if (length(pathway_result$all) > 10) {
      cat("- Analyzing all correlated proteins...\n")
      enrichment_results$all <- enrichment_Multi(
        pathway_result$all, 
        OrgDb = OrgDb, 
        orgKegg = orgKegg, 
        organismReactome = organismReactome
      )
    }
    
    # 3. 生成可视化
    if (plot_results) {
      cat("\nStep 3: Generating visualizations...\n")
      plots <- generate_enrichment_plots(enrichment_results, ID)
    }
  }
  
  # 4. 进行GSEA分析
  gsea_results <- NULL
  gsea_plots <- list()
  
  if (perform_gsea) {
    cat("\nStep 4: Performing GSEA analysis...\n")
    
    tryCatch({
      gsea_analysis <- dbGIST_Proteomics_GSEA(
        Dataset = Dataset,
        ID = ID,
        gmt_files = gmt_files,
        nperm = gsea_nperm,
        min_size = gsea_min_size,
        max_size = gsea_max_size,
        plot_top = 10
      )
      
      gsea_results <- gsea_analysis$results
      gsea_plots <- gsea_analysis$plots
      
      cat(paste("GSEA completed! Found results for", length(gsea_results), "databases\n"))
      
    }, error = function(e) {
      warning(paste("GSEA analysis failed:", e$message))
    })
  }
  
  # 返回结果
  result <- list(
    pathway = pathway_result,
    enrichment = enrichment_results,
    gsea = gsea_results,
    plots = plots,
    gsea_plots = gsea_plots
  )
  
  cat("\nAnalysis completed!\n")
  return(result)
}

# 生成富集分析可视化
generate_enrichment_plots <- function(enrichment_results, gene_id) {
  library(enrichplot)
  library(ggplot2)
  library(patchwork)
  
  plots <- list()
  
  # 为每个组（positive, negative, all）生成图
  for (group in names(enrichment_results)) {
    group_plots <- list()
    
    # GO图
    if (!is.null(enrichment_results[[group]]$go) && nrow(enrichment_results[[group]]$go@result) > 0) {
      tryCatch({
        # 点图
        group_plots$go_dot <- dotplot(enrichment_results[[group]]$go, 
                                      showCategory = 10, 
                                      title = paste(gene_id, "-", group, "- GO Enrichment"))
        
        # 条形图
        group_plots$go_bar <- barplot(enrichment_results[[group]]$go, 
                                      showCategory = 10, 
                                      title = paste(gene_id, "-", group, "- GO Terms"))
        
        # 如果有足够的terms，生成enrichment map
        if (nrow(enrichment_results[[group]]$go@result) > 5) {
          group_plots$go_emap <- emapplot(pairwise_termsim(enrichment_results[[group]]$go), 
                                          showCategory = 30)
        }
      }, error = function(e) {
        cat(paste("Warning: Could not generate GO plots for", group, "\n"))
      })
    }
    
    # KEGG图
    if (!is.null(enrichment_results[[group]]$kegg) && nrow(enrichment_results[[group]]$kegg@result) > 0) {
      tryCatch({
        group_plots$kegg_dot <- dotplot(enrichment_results[[group]]$kegg, 
                                        showCategory = 10, 
                                        title = paste(gene_id, "-", group, "- KEGG Pathways"))
        
        group_plots$kegg_bar <- barplot(enrichment_results[[group]]$kegg, 
                                        showCategory = 10, 
                                        title = paste(gene_id, "-", group, "- KEGG Pathways"))
      }, error = function(e) {
        cat(paste("Warning: Could not generate KEGG plots for", group, "\n"))
      })
    }
    
    # Reactome图
    if (!is.null(enrichment_results[[group]]$reactome) && nrow(enrichment_results[[group]]$reactome@result) > 0) {
      tryCatch({
        group_plots$reactome_dot <- dotplot(enrichment_results[[group]]$reactome, 
                                           showCategory = 10, 
                                           title = paste(gene_id, "-", group, "- Reactome Pathways"))
      }, error = function(e) {
        cat(paste("Warning: Could not generate Reactome plots for", group, "\n"))
      })
    }
    
    plots[[group]] <- group_plots
  }
  
  # 创建综合图
  if (length(plots) > 0) {
    plots$summary <- create_summary_plot(enrichment_results, gene_id)
  }
  
  return(plots)
}

# 创建汇总图
create_summary_plot <- function(enrichment_results, gene_id) {
  library(ggplot2)
  library(dplyr)
  
  # 收集所有富集结果的top terms
  summary_data <- data.frame()
  
  for (group in names(enrichment_results)) {
    for (db in c("go", "kegg", "reactome")) {
      if (!is.null(enrichment_results[[group]][[db]]) && 
          nrow(enrichment_results[[group]][[db]]@result) > 0) {
        
        top_terms <- enrichment_results[[group]][[db]]@result %>%
          arrange(p.adjust) %>%
          head(5) %>%
          mutate(Group = group, Database = db)
        
        summary_data <- rbind(summary_data, top_terms[c("Description", "p.adjust", "Count", "Group", "Database")])
      }
    }
  }
  
  if (nrow(summary_data) == 0) return(NULL)
  
  # 创建汇总图
  p <- ggplot(summary_data, aes(x = reorder(Description, -log10(p.adjust)), 
                                 y = -log10(p.adjust), 
                                 fill = Group, 
                                 size = Count)) +
    geom_point(shape = 21, alpha = 0.7) +
    facet_wrap(~Database, scales = "free_y", ncol = 1) +
    coord_flip() +
    scale_size_continuous(range = c(3, 10)) +
    scale_fill_manual(values = c("positive" = "#E41A1C", "negative" = "#377EB8", "all" = "#4DAF4A")) +
    labs(title = paste("Enrichment Summary for", gene_id),
         x = "Pathway/Term",
         y = "-log10(adjusted p-value)") +
    theme_minimal() +
    theme(axis.text.y = element_text(size = 8))
  
  return(p)
}

# 保存富集分析结果
save_enrichment_results <- function(results, output_dir = "enrichment_results", gene_id = "") {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  prefix <- ifelse(gene_id != "", paste0(gene_id, "_"), "")
  
  # 保存数据
  saveRDS(results, file.path(output_dir, paste0(prefix, "enrichment_", timestamp, ".RDS")))
  
  # 保存图片
  if (!is.null(results$plots)) {
    for (group in names(results$plots)) {
      for (plot_name in names(results$plots[[group]])) {
        if (!is.null(results$plots[[group]][[plot_name]])) {
          ggsave(
            filename = file.path(output_dir, paste0(prefix, group, "_", plot_name, "_", timestamp, ".pdf")),
            plot = results$plots[[group]][[plot_name]],
            width = 10, 
            height = 8
          )
        }
      }
    }
  }
  
  cat(paste("Results saved to", output_dir, "\n"))
}

cat("Pathway analysis functions loaded successfully!\n")
cat("Main function: dbGIST_Proteomics_ID_Pathway()\n")
cat("Enrichment function (with GSEA): dbGIST_Proteomics_Pathway_Enrichment()\n")
cat("GSEA only function: dbGIST_Proteomics_GSEA()\n")
cat("For first time use, run: split_proteomics_data()\n")

# 示例：运行完整分析（包含传统富集分析和GSEA）
# results <- dbGIST_Proteomics_Pathway_Enrichment(
#   Dataset = "Sun's Study",
#   ID = "P4HA1",
#   top_positive = 50,
#   top_negative = 50,
#   perform_enrichment = TRUE,
#   perform_gsea = TRUE,
#   gmt_files = c("GSEA_KEGG.gmt", "GSEA_hallmark.gmt"),
#   gsea_nperm = 1000
# )

# 示例：仅运行GSEA分析
# gsea_results <- dbGIST_Proteomics_GSEA(
#   Dataset = "Sun's Study",
#   ID = "P4HA1",
#   gmt_files = c("GSEA_KEGG.gmt", "GSEA_hallmark.gmt"),
#   nperm = 1000,
#   min_size = 15,
#   max_size = 500
# )