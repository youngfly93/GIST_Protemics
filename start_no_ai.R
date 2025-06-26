#!/usr/bin/env Rscript

# 启动非AI版本的GIST Protemics应用
# 通过环境变量临时禁用AI功能

cat("========================================\n")
cat("   启动GIST Protemics应用 - 非AI版本\n")
cat("   端口: 4967\n")
cat("   AI功能: 禁用\n")
cat("========================================\n")

# 在加载任何包之前设置环境变量禁用AI
Sys.setenv(ENABLE_AI_ANALYSIS = "false")
Sys.setenv(USE_OPENROUTER = "false")

cat("AI功能已禁用\n")

# 验证环境变量设置
cat("环境变量验证:\n")
cat("ENABLE_AI_ANALYSIS =", Sys.getenv("ENABLE_AI_ANALYSIS"), "\n")
cat("USE_OPENROUTER =", Sys.getenv("USE_OPENROUTER"), "\n")

# 强制设置enable_ai变量
enable_ai <- FALSE
cat("强制设置 enable_ai =", enable_ai, "\n")

cat("正在启动应用...\n")

# 加载全局设置
source("global.R")

# 再次验证AI状态
cat("最终AI状态检查:\n")
cat("ENABLE_AI_ANALYSIS =", Sys.getenv("ENABLE_AI_ANALYSIS"), "\n")
cat("enable_ai =", enable_ai, "\n")

# 加载UI和Server
source("ui.R")
source("server.R")

# 启动Shiny应用
shiny::shinyApp(
  ui = ui,
  server = server,
  options = list(port = 4967, host = "127.0.0.1", launch.browser = FALSE)
)
