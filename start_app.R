#!/usr/bin/env Rscript

# 启动蛋白质组学分析平台
cat("Starting GIST Proteomics Analysis Platform...\n")
cat("Platform URL: http://localhost:4965\n")
cat("Press Ctrl+C to stop the application\n\n")

# 启动应用
shiny::runApp(port = 4965)