# Test script for proteomics functions
source("Protemic.R")

# Test function 1 - 肿瘤vs正常
cat("Testing dbGIST_Proteomics_boxplot_TvsN...\n")
plot1 <- dbGIST_Proteomics_boxplot_TvsN("KIT")
if(!is.null(plot1)) {
  cat("✓ TvsN function works\n")
} else {
  cat("✗ TvsN function failed\n")
}

# Test function 2 - 相关性分析
cat("Testing dbGIST_Proteomics_cor_ID...\n") 
plot2 <- dbGIST_Proteomics_cor_ID("KIT", "PDGFRA")
if(!is.null(plot2)) {
  cat("✓ Correlation function works\n")
} else {
  cat("✗ Correlation function failed\n")
}

# Test function 4 - 伊马替尼耐药
cat("Testing dbGIST_Proteomics_boxplot_IM.Response...\n")
plot4 <- dbGIST_Proteomics_boxplot_IM.Response("KIT")
if(!is.null(plot4)) {
  cat("✓ IM Response function works\n") 
} else {
  cat("✗ IM Response function failed\n")
}

cat("All tests completed!\n")