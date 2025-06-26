# ==== Module 1: 有丝分裂计数 Server ====
createAnalysisServer(
  id = "mitotic",
  analysis_function = dbGIST_Proteomics_boxplot_Mitotic.count,
  global_state = global_state
)