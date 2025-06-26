# ==== Module 1: Ki-67 Server ====
createAnalysisServer(
  id = "ki67",
  analysis_function = dbGIST_Proteomics_boxplot_Ki.67,
  global_state = global_state
)