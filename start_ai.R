#!/usr/bin/env Rscript

# 启动AI版本的GIST Protemics应用
# 通过环境变量启用AI功能

cat("========================================\n")
cat("   启动GIST Protemics应用 - AI版本\n")
cat("   端口: 4968\n")
cat("   AI功能: 启用\n")
cat("========================================\n")

# 在加载任何包之前设置环境变量启用AI
Sys.setenv(ENABLE_AI_ANALYSIS = "true")
Sys.setenv(USE_OPENROUTER = "true")

# 手动加载.env文件中的API key
if (file.exists(".env")) {
  env_content <- readLines(".env")
  for (line in env_content) {
    if (grepl("OPENROUTER_API_KEY=", line)) {
      key_value <- sub("OPENROUTER_API_KEY=", "", line)
      Sys.setenv(OPENROUTER_API_KEY = key_value)
      cat("API Key loaded from .env file\n")
      break
    }
    if (grepl("OPENROUTER_API_URL=", line)) {
      url_value <- sub("OPENROUTER_API_URL=", "", line)
      Sys.setenv(OPENROUTER_API_URL = url_value)
    }
    if (grepl("OPENROUTER_MODEL=", line)) {
      model_value <- sub("OPENROUTER_MODEL=", "", line)
      Sys.setenv(OPENROUTER_MODEL = model_value)
    }
  }
}

cat("AI功能已启用\n")

# 验证环境变量设置
cat("环境变量验证:\n")
cat("ENABLE_AI_ANALYSIS =", Sys.getenv("ENABLE_AI_ANALYSIS"), "\n")
cat("USE_OPENROUTER =", Sys.getenv("USE_OPENROUTER"), "\n")
cat("OPENROUTER_API_KEY =", substr(Sys.getenv("OPENROUTER_API_KEY"), 1, 20), "...\n")
cat("OPENROUTER_API_URL =", Sys.getenv("OPENROUTER_API_URL"), "\n")
cat("OPENROUTER_MODEL =", Sys.getenv("OPENROUTER_MODEL"), "\n")

cat("正在启动应用...\n")

# 启动Shiny应用
shiny::runApp(
  port = 4968,
  host = "127.0.0.1",
  launch.browser = FALSE
)
