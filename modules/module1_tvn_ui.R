# ==== Module 1: Tumor vs Normal UI ====
source("modules/analysis_template.R")

createAnalysisUI(
  id = "tvn",
  title = "Tumor vs Normal Tissue Protein Expression Analysis",
  description = "Compare protein expression differences between tumor tissue and normal tissue. Supports paired sample analysis, displaying box plots, violin plots, and statistical test results."
)