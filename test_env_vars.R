# Test environment variables for both versions
cat("=== Environment Variables Test ===\n")
cat("ENABLE_AI_ANALYSIS:", Sys.getenv("ENABLE_AI_ANALYSIS", "NOT_SET"), "\n")
cat("USE_OPENROUTER:", Sys.getenv("USE_OPENROUTER", "NOT_SET"), "\n")

enable_ai <- tolower(Sys.getenv("ENABLE_AI_ANALYSIS", "true")) == "true"
cat("Computed enable_ai:", enable_ai, "\n")

if(enable_ai) {
  cat("AI functionality is ENABLED\n")
} else {
  cat("AI functionality is DISABLED\n")
}
