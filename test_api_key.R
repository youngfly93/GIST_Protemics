# Simple test for API key loading
cat("=== API Key Test ===\n")

# Load .env file manually
if (file.exists(".env")) {
  env_content <- readLines(".env")
  for (line in env_content) {
    if (grepl("OPENROUTER_API_KEY=", line)) {
      key_value <- sub("OPENROUTER_API_KEY=", "", line)
      Sys.setenv(OPENROUTER_API_KEY = key_value)
      cat("Manually set API key:", substr(key_value, 1, 30), "...\n")
      break
    }
  }
}

# Test the key
api_key <- Sys.getenv("OPENROUTER_API_KEY")
cat("Retrieved API key:", substr(api_key, 1, 30), "...\n")
cat("Key length:", nchar(api_key), "\n")

# Test API connection
library(httr)
library(jsonlite)

test_body <- list(
  model = "google/gemini-2.5-flash",
  messages = list(
    list(role = "user", content = "Hello, this is a test.")
  ),
  max_tokens = 10
)

cat("Testing API connection...\n")
response <- POST(
  url = "https://openrouter.ai/api/v1/chat/completions",
  add_headers(
    "Content-Type" = "application/json",
    "Authorization" = paste("Bearer", api_key)
  ),
  body = toJSON(test_body, auto_unbox = TRUE),
  encode = "raw"
)

cat("Response status:", status_code(response), "\n")
if (status_code(response) == 200) {
  cat("API connection successful!\n")
} else {
  cat("API connection failed!\n")
  cat("Response content:", content(response, "text"), "\n")
}
