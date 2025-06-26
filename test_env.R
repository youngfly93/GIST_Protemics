# Test environment variables
cat("=== Environment Variables Test ===\n")

# Check if .env file exists
if (file.exists(".env")) {
  cat(".env file exists\n")
  
  # Read .env file content
  env_content <- readLines(".env")
  cat("Raw .env content:\n")
  for (i in seq_along(env_content)) {
    cat(sprintf("Line %d: %s\n", i, env_content[i]))
  }
  
  # Load environment variables
  env_vars <- env_content[!grepl("^#", env_content) & nchar(env_content) > 0]
  cat("\nFiltered env vars:\n")
  for (line in env_vars) {
    cat("Filtered line:", line, "\n")
    if (grepl("=", line)) {
      parts <- strsplit(line, "=", fixed = TRUE)[[1]]
      if (length(parts) >= 2) {
        var_name <- trimws(parts[1])
        var_value <- trimws(paste(parts[-1], collapse = "="))
        cat(sprintf("Variable: %s = %s\n", var_name, var_value))
      }
    }
  }
} else {
  cat(".env file does not exist\n")
}

# Check current environment variables
cat("\n=== Current Environment Variables ===\n")
cat("USE_OPENROUTER:", Sys.getenv("USE_OPENROUTER", "NOT_SET"), "\n")
cat("OPENROUTER_API_KEY:", substr(Sys.getenv("OPENROUTER_API_KEY", "NOT_SET"), 1, 20), "...\n")
cat("OPENROUTER_API_URL:", Sys.getenv("OPENROUTER_API_URL", "NOT_SET"), "\n")
cat("OPENROUTER_MODEL:", Sys.getenv("OPENROUTER_MODEL", "NOT_SET"), "\n")
cat("ENABLE_AI_ANALYSIS:", Sys.getenv("ENABLE_AI_ANALYSIS", "NOT_SET"), "\n")
