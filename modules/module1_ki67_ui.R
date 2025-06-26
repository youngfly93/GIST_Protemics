# ==== Module 1: Ki-67 UI ====
source("modules/analysis_template.R")

createAnalysisUI(
  id = "ki67",
  title = "Ki-67 Expression Analysis",
  description = "Analyze protein expression differences based on Ki-67 expression levels (≤10% vs >10%)."
)