# Debug API key loading
cat("=== API Key Debug ===\n")

# Read .env file
if (file.exists(".env")) {
  env_content <- readLines(".env")
  cat("Found .env file with", length(env_content), "lines\n")
  
  for (i in seq_along(env_content)) {
    line <- env_content[i]
    if (grepl("OPENROUTER_API_KEY=", line)) {
      cat("Found API key line:", i, "\n")
      cat("Raw line:", line, "\n")
      key_value <- sub("OPENROUTER_API_KEY=", "", line)
      cat("Extracted key:", key_value, "\n")
      cat("Key length:", nchar(key_value), "\n")
      
      # Set environment variable
      Sys.setenv(OPENROUTER_API_KEY = key_value)
      
      # Verify
      retrieved_key <- Sys.getenv("OPENROUTER_API_KEY")
      cat("Retrieved key:", retrieved_key, "\n")
      cat("Retrieved length:", nchar(retrieved_key), "\n")
      cat("Keys match:", identical(key_value, retrieved_key), "\n")
      
      # Test first 30 characters
      cat("First 30 chars:", substr(retrieved_key, 1, 30), "\n")
      
      break
    }
  }
} else {
  cat("No .env file found\n")
}

# Test API call
library(httr)
library(jsonlite)

api_key <- Sys.getenv("OPENROUTER_API_KEY")
cat("\n=== API Test ===\n")
cat("Using key:", substr(api_key, 1, 30), "...\n")

test_body <- list(
  model = "google/gemini-2.5-flash",
  messages = list(
    list(role = "user", content = "Hello")
  ),
  max_tokens = 5
)

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
if (status_code(response) != 200) {
  cat("Response content:", content(response, "text"), "\n")
}
