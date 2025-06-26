# 测试Pathway分析 + 富集分析的集成功能

# 加载函数
source("pathway_final.R")

# 示例1：完整的分析流程
run_complete_analysis <- function() {
  cat("\n=== 运行完整的Pathway + 富集分析 ===\n")
  
  # 执行分析
  results <- dbGIST_Proteomics_Pathway_Enrichment(
    Dataset = "Sun's Study",
    ID = "P4HA1",
    top_positive = 50,
    top_negative = 50,
    perform_enrichment = TRUE,
    plot_results = TRUE,
    OrgDb = "org.Hs.eg.db",      # 人类基因
    orgKegg = "hsa",              # 人类KEGG
    organismReactome = "human"     # 人类Reactome
  )
  
  # 查看结果
  cat("\n=== 分析结果摘要 ===\n")
  cat("正相关基因数:", length(results$pathway$positive), "\n")
  cat("负相关基因数:", length(results$pathway$negative), "\n")
  
  # 查看富集结果
  if (!is.null(results$enrichment)) {
    for (group in names(results$enrichment)) {
      cat(paste("\n", group, "组富集结果:\n"))
      
      # GO结果
      if (!is.null(results$enrichment[[group]]$go) && 
          nrow(results$enrichment[[group]]$go@result) > 0) {
        cat("  - GO terms:", nrow(results$enrichment[[group]]$go@result), "\n")
        cat("    Top 3 GO terms:\n")
        print(head(results$enrichment[[group]]$go@result[, c("Description", "p.adjust")], 3))
      }
      
      # KEGG结果
      if (!is.null(results$enrichment[[group]]$kegg) && 
          nrow(results$enrichment[[group]]$kegg@result) > 0) {
        cat("  - KEGG pathways:", nrow(results$enrichment[[group]]$kegg@result), "\n")
        cat("    Top 3 KEGG pathways:\n")
        print(head(results$enrichment[[group]]$kegg@result[, c("Description", "p.adjust")], 3))
      }
      
      # Reactome结果
      if (!is.null(results$enrichment[[group]]$reactome) && 
          nrow(results$enrichment[[group]]$reactome@result) > 0) {
        cat("  - Reactome pathways:", nrow(results$enrichment[[group]]$reactome@result), "\n")
      }
    }
  }
  
  # 保存结果
  save_enrichment_results(results, output_dir = "enrichment_results", gene_id = "P4HA1")
  
  return(results)
}

# 示例2：只进行pathway分析，不做富集
run_pathway_only <- function() {
  cat("\n=== 只运行Pathway分析 ===\n")
  
  results <- dbGIST_Proteomics_Pathway_Enrichment(
    Dataset = "Sun's Study",
    ID = "P4HA1",
    top_positive = 50,
    top_negative = 50,
    perform_enrichment = FALSE  # 不进行富集分析
  )
  
  cat("正相关基因:\n")
  print(head(results$pathway$positive, 10))
  
  cat("\n负相关基因:\n")
  print(head(results$pathway$negative, 10))
  
  return(results)
}

# 示例3：批量分析多个基因
run_batch_analysis <- function() {
  cat("\n=== 批量分析多个基因 ===\n")
  
  genes <- c("P4HA1", "CALR", "PDIA4")
  all_results <- list()
  
  for (gene in genes) {
    cat(paste("\n分析基因:", gene, "\n"))
    
    tryCatch({
      results <- dbGIST_Proteomics_Pathway_Enrichment(
        Dataset = "Sun's Study",
        ID = gene,
        top_positive = 30,
        top_negative = 30,
        perform_enrichment = TRUE,
        plot_results = FALSE  # 批量处理时可以关闭绘图以加快速度
      )
      
      all_results[[gene]] <- results
      
      # 简单汇总
      if (!is.null(results$enrichment$all$go)) {
        top_go <- head(results$enrichment$all$go@result$Description, 3)
        cat("  Top GO terms:", paste(top_go, collapse = "; "), "\n")
      }
      
    }, error = function(e) {
      cat("  错误:", e$message, "\n")
    })
  }
  
  return(all_results)
}

# 示例4：自定义可视化
custom_visualization <- function(results) {
  if (is.null(results$plots)) {
    cat("没有可用的图形\n")
    return()
  }
  
  library(patchwork)
  
  # 组合多个图
  if (!is.null(results$plots$positive$go_dot) && 
      !is.null(results$plots$negative$go_dot)) {
    
    combined_plot <- results$plots$positive$go_dot + 
                     results$plots$negative$go_dot + 
                     plot_layout(ncol = 2)
    
    ggsave("combined_go_analysis.pdf", combined_plot, width = 16, height = 8)
    cat("保存组合图到 combined_go_analysis.pdf\n")
  }
  
  # 显示汇总图
  if (!is.null(results$plots$summary)) {
    print(results$plots$summary)
  }
}

# 主测试函数
main_test <- function() {
  # 检查数据是否已预处理
  if (!dir.exists("correlation_data")) {
    cat("首次运行，正在预处理数据...\n")
    split_proteomics_data()
  }
  
  # 选择要运行的测试
  cat("\n选择测试类型:\n")
  cat("1. 完整分析（pathway + 富集 + 可视化）\n")
  cat("2. 仅pathway分析\n")
  cat("3. 批量分析\n")
  cat("4. 运行所有测试\n")
  
  choice <- readline("请输入选择 (1-4): ")
  
  if (choice == "1" || choice == "4") {
    results <- run_complete_analysis()
    if (choice == "1") custom_visualization(results)
  }
  
  if (choice == "2" || choice == "4") {
    run_pathway_only()
  }
  
  if (choice == "3" || choice == "4") {
    run_batch_analysis()
  }
}

# 直接运行测试
# results <- run_complete_analysis()

# 或交互式运行
# main_test()