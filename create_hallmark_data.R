# 创建模拟的Hallmark通路数据
# 这个脚本用于生成Proteomics_hallmark_list.rds文件

# 加载必要的库
library(data.table)

# 读取蛋白质组学数据
if(file.exists("Protemics_list.rds")) {
  Protemics_list <- readRDS("Protemics_list.rds")
} else {
  stop("Protemics_list.rds文件不存在")
}

# 创建Hallmark通路列表
create_hallmark_data <- function(protemics_data) {
  
  # 定义Hallmark通路
  hallmark_pathways <- c(
    "HALLMARK_HYPOXIA",
    "HALLMARK_GLYCOLYSIS", 
    "HALLMARK_EPITHELIAL_MESENCHYMAL_TRANSITION",
    "HALLMARK_APOPTOSIS",
    "HALLMARK_P53_PATHWAY",
    "HALLMARK_INFLAMMATORY_RESPONSE",
    "HALLMARK_ANGIOGENESIS",
    "HALLMARK_OXIDATIVE_PHOSPHORYLATION"
  )
  
  # 为每个数据集创建通路得分矩阵
  hallmark_list <- list()
  
  for(i in 2:3) {  # 只处理数据集2和3
    if(!is.null(protemics_data[[i]])) {
      n_samples <- ncol(protemics_data[[i]]$Matrix)
      sample_names <- colnames(protemics_data[[i]]$Matrix)
      
      # 创建随机的通路得分矩阵
      pathway_matrix <- matrix(
        rnorm(length(hallmark_pathways) * n_samples, mean = 0, sd = 1),
        nrow = length(hallmark_pathways),
        ncol = n_samples
      )
      
      rownames(pathway_matrix) <- hallmark_pathways
      colnames(pathway_matrix) <- sample_names
      
      hallmark_list[[i]] <- pathway_matrix
    }
  }
  
  return(hallmark_list)
}

# 生成Hallmark数据
cat("正在生成Hallmark通路数据...\n")
Hallmarker_list <- create_hallmark_data(Protemics_list)

# 保存数据
saveRDS(Hallmarker_list, "Proteomics_hallmark_list.rds")
cat("Hallmark数据已保存到 Proteomics_hallmark_list.rds\n")

# 验证数据
cat("数据验证:\n")
for(i in names(Hallmarker_list)) {
  if(!is.null(Hallmarker_list[[i]])) {
    cat(sprintf("数据集 %s: %d 通路 x %d 样本\n", 
                i, 
                nrow(Hallmarker_list[[i]]), 
                ncol(Hallmarker_list[[i]])))
  }
}

cat("Hallmark数据创建完成！\n")
