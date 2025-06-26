# ==== Library Packages ====
library(shiny)
library(bs4Dash)
library(waiter)
library(shinyjs)
library(shinyBS)
library(slickR)
library(shinyFeedback)
library(shinycssloaders)
library(shinyWidgets)
library(DT)
library(htmlwidgets)

# ==== Additional Libraries for New Features ====
library(survival)    # Survival analysis
library(survminer)   # Survival analysis visualization

# ==== Load Environment Variables ====
# Load environment variables from .env file
if (file.exists(".env")) {
  cat("Loading environment variables from .env file...\n")
  env_vars <- readLines(".env")
  env_vars <- env_vars[!grepl("^#", env_vars) & nchar(env_vars) > 0]  # Remove comments and empty lines

  for (line in env_vars) {
    if (grepl("=", line)) {
      parts <- strsplit(line, "=", fixed = TRUE)[[1]]
      if (length(parts) >= 2) {
        var_name <- trimws(parts[1])
        var_value <- trimws(paste(parts[-1], collapse = "="))

        # Only load from .env file if not already set (to preserve start script settings)
        current_value <- Sys.getenv(var_name, unset = "")
        if (current_value == "") {
          env_list <- list()
          env_list[[var_name]] <- var_value
          do.call(Sys.setenv, env_list)
          cat("Set env var from .env:", var_name, "=", substr(var_value, 1, 20), "...\n")
        } else {
          cat("Env var already set (preserving):", var_name, "=", substr(current_value, 1, 20), "...\n")
        }
      }
    }
  }
  cat("Environment variables loaded successfully from .env file\n")
} else {
  cat("No .env file found, using default environment variables\n")
}

library(tidyverse)
library(data.table)
library(stringr)
require(ggplot2)
require(ggsci)
library(pROC)
library(readr)
library(ggpubr)
library(eoffice)
library(Rcpp)
library(ggridges)  # For ridge plots in GSEA analysis
library(clusterProfiler)
library(tidyverse)
library(org.Hs.eg.db)
library(EnsDb.Hsapiens.v75)
library(AnnotationDbi)
library(patchwork)
# install_github("miccec/yaGST")  # 安装包yaGST
library(yaGST)
library(R6)  # 用于面向对象编程

# ==== 加载数据和函数 ====
# 注意：数据和函数在Protemic.R中已经加载，这里不需要重复加载

# ==== Global Variables ====
# Define module information
module_info <- list(
  module1 = list(
    title = "Protein Clinical Feature Analysis",
    icon = "chart-bar",
    subtabs = list(
      "Tumor vs Normal" = "tvn",
      "Risk Level" = "risk",
      "Gender" = "gender",
      "Age" = "age",
      "Tumor Size" = "tumor_size",
      "Mitotic Count" = "mitotic",
      "Tumor Location" = "location",
      "WHO Grade" = "who",
      "Ki-67" = "ki67",
      "CD34" = "cd34",
      "Mutation" = "mutation"
    )
  ),
  module2 = list(
    title = "Protein Correlation Analysis",
    icon = "project-diagram"
  ),
  module3 = list(
    title = "Pathway Enrichment Analysis",
    icon = "sitemap"
  ),
  module4 = list(
    title = "Imatinib Resistance Prediction",
    icon = "pills"
  )
)

# Theme variables - consistent with main website
theme_colors <- list(
  primary_900 = "#0F2B2E",
  primary_700 = "#163A3D", 
  primary_500 = "#1C484C",
  primary_300 = "#3C6B6F",
  primary_100 = "#D7E4E5",
  primary_050 = "#F2F7F7",
  accent_coral = "#E87D4C",
  accent_lime = "#9CCB3B", 
  accent_sky = "#2F8FBF"
)

# ==== Load Analysis Functions ====
# Load proteomics analysis functions
cat("Loading proteomics analysis functions...\n")
source("Protemic.R", local = FALSE)
cat("Proteomics analysis functions loaded successfully\n")

# ==== AI Feature Detection ====
# Detect if AI functionality is enabled
enable_ai <- tolower(Sys.getenv("ENABLE_AI_ANALYSIS", "true")) == "true"

# Print AI functionality status
cat("========================================\n")
cat("   GIST Proteomics Application Startup\n")
cat("   AI Status:", if(enable_ai) "Enabled" else "Disabled", "\n")
if (enable_ai) {
  use_openrouter <- tolower(Sys.getenv("USE_OPENROUTER", "true")) == "true"
  cat("   AI Service:", if(use_openrouter) "OpenRouter" else "Doubao", "\n")
}
cat("========================================\n")

# Global state management - will be initialized in server.R
# global_state will be created in server function

# ==== Conditional Loading of AI Chat Module ====
if(enable_ai) {
  cat("Loading AI chat module...\n")
  source("modules/ai_chat_module.R", local = FALSE)
  cat("AI chat module loaded successfully\n")
}

# Introduction text
proteomics_intro_text <- "Welcome to the GIST Proteomics Analysis Platform! This platform is specifically designed for Gastrointestinal Stromal Tumor (GIST) proteomics research, providing comprehensive protein expression analysis tools. You can explore individual protein expression patterns under different clinical conditions, analyze protein-protein expression correlations, study drug resistance-related proteins, and access convenient, professional bioinformatics analysis services for researchers."