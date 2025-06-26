# ==== Module 1: Mitotic Count UI ====
source("modules/analysis_template.R")

createAnalysisUI(
  id = "mitotic",
  title = "Mitotic Count Analysis",
  description = "Analyze protein expression differences based on mitotic count (≤5 vs >5)."
)