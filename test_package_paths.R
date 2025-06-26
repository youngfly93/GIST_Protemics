#!/usr/bin/env Rscript

# 测试包路径配置
cat("=== GIST Proteomics Package Path Test ===\n")
cat("Library paths:\n")
print(.libPaths())

cat("\n=== Testing key packages ===\n")
packages <- c('shiny', 'bs4Dash', 'waiter', 'shinyjs', 'DT', 'tidyverse', 
              'ggplot2', 'patchwork', 'pROC', 'clusterProfiler')

for(pkg in packages) {
  if(pkg %in% rownames(installed.packages())) {
    cat(sprintf("✓ %s: %s\n", pkg, find.package(pkg)))
  } else {
    cat(sprintf("✗ %s: NOT FOUND\n", pkg))
  }
}

cat("\n=== Package path consistency check ===\n")
expected_path <- "E:/R-4.4.1/library"
current_path <- .libPaths()[1]

if(current_path == expected_path) {
  cat("✓ Package path matches GIST_shiny configuration\n")
} else {
  cat(sprintf("✗ Package path mismatch. Expected: %s, Got: %s\n", expected_path, current_path))
}

cat("\n=== Test completed ===\n")
