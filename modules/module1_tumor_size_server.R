# ==== Module 1: 肿瘤大小 Server ====
createAnalysisServer(
  id = "tumor_size",
  analysis_function = dbGIST_Proteomics_boxplot_Tumor.size,
  global_state = global_state
)