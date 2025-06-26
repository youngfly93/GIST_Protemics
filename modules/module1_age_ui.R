# ==== Module 1: Age Analysis UI ====
source("modules/analysis_template.R")

createAnalysisUI(
  id = "age",
  title = "Age Group Analysis",
  description = "Analyze protein expression differences between different age groups (≤60 years vs >60 years) in GIST patients."
)