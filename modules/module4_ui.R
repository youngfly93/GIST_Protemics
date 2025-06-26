# ==== Module 4: Imatinib Resistance Analysis UI ====
source("modules/analysis_template.R")

createAnalysisUI(
  id = "drug_resistance",
  title = "Imatinib Resistance Analysis",
  description = "Analyze the relationship between protein expression and imatinib drug response, generate box plots and ROC curves to evaluate the potential of proteins as resistance prediction biomarkers."
)