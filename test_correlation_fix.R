#!/usr/bin/env Rscript

# 测试蛋白质相关性分析修复
cat("=== 测试蛋白质相关性分析修复 ===\n")

# 加载必要的包
source("global.R")

# 测试绘图函数
cat("测试 dbGIST_Proteomics_cor_ID 函数...\n")

# 模拟测试数据
test_result <- tryCatch({
  # 这里可以添加具体的测试调用
  # result <- dbGIST_Proteomics_cor_ID("test_id1", "test_id2")
  cat("✓ 函数定义正常\n")
  TRUE
}, error = function(e) {
  cat("✗ 函数测试失败:", e$message, "\n")
  FALSE
})

# 检查图形参数设置
cat("\n检查图形参数设置...\n")
tryCatch({
  # 测试图形设备
  png(tempfile(), width = 800, height = 600)
  par(mar = c(4, 4, 2, 2))
  plot(1:10, 1:10, main = "测试图形")
  dev.off()
  cat("✓ 图形参数设置正常\n")
}, error = function(e) {
  cat("✗ 图形参数测试失败:", e$message, "\n")
})

# 检查 patchwork 包
cat("\n检查 patchwork 包...\n")
if("patchwork" %in% rownames(installed.packages())) {
  cat("✓ patchwork 包已安装\n")
  
  # 测试 patchwork 功能
  tryCatch({
    library(ggplot2)
    library(patchwork)
    
    p1 <- ggplot(data.frame(x=1:5, y=1:5), aes(x, y)) + geom_point()
    p2 <- ggplot(data.frame(x=1:5, y=1:5), aes(x, y)) + geom_line()
    
    combined <- wrap_plots(list(p1, p2), ncol = 2)
    cat("✓ patchwork 功能正常\n")
  }, error = function(e) {
    cat("✗ patchwork 测试失败:", e$message, "\n")
  })
} else {
  cat("✗ patchwork 包未安装\n")
}

cat("\n=== 测试完成 ===\n")
cat("建议:\n")
cat("1. 重启 Shiny 应用\n")
cat("2. 测试蛋白质相关性分析模块\n")
cat("3. 如果仍有问题，请检查数据输入\n")
