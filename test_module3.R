# 测试Module 3集成的脚本

# 加载必要的包
library(shiny)
library(bs4Dash)
library(DT)
library(ggplot2)

# 设置环境变量（如果需要）
Sys.setenv(ENABLE_AI_ANALYSIS = "false")  # 暂时禁用AI以简化测试

# 运行应用
cat("Starting GIST Proteomics App with Module 3...\n")
cat("Please navigate to Module 3: Pathway Enrichment Analysis\n")
cat("Test with protein ID: P4HA1\n\n")

# 运行应用
runApp(launch.browser = TRUE)