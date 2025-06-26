# ==== Module 2: Molecular Correlation Analysis UI ====
source("modules/analysis_template.R")

createAnalysisUI(
  id = "correlation",
  title = "Molecular Correlation Analysis",
  description = "Analyze expression correlation between two proteins, generate scatter plots and calculate correlation coefficients and P-values. Supports linear regression fitting.",
  has_second_gene = TRUE
)