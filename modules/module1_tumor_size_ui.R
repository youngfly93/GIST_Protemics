# ==== Module 1: Tumor Size UI ====
source("modules/analysis_template.R")

createAnalysisUI(
  id = "tumor_size",
  title = "Tumor Size Analysis",
  description = "Analyze protein expression differences based on tumor size groups (<2cm, >2-5cm, >5-10cm, >10cm)."
)