# ==== Module 1: Risk Level UI ====
source("modules/analysis_template.R")

createAnalysisUI(
  id = "risk",
  title = "Risk Level Analysis",
  description = "Based on the NIH risk stratification system, analyze protein expression differences among GIST patients with different risk levels (low, intermediate, high)."
)