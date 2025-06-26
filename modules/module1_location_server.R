# ==== Module 1: 肿瘤位置 Server ====
createAnalysisServer(
  id = "location",
  analysis_function = dbGIST_Proteomics_boxplot_Location,
  global_state = global_state
)
