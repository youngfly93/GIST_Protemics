Proteomics_ID_Pathway_list <- readRDS("Proteomics_ID_Pathway_list.RDS")

dbGIST_Proteomics_ID_Pathway <- function(Dataset = Dataset,
                                         ID = ID,
                                         Number = 100,
                                         Proteomics_ID_Pathway_list = Proteomics_ID_Pathway_list){
  
  cor_matrix <- Proteomics_ID_Pathway_list[[Dataset]]
  
  if("try-error" %in% class(try(cor_matrix[, ID]))){
    
    return(NULL)
    
  } else {
    
    target_cor <- cor_matrix[, ID]
    
    return(names(target_cor[order(target_cor,decreasing = T)][1:Number]))
    
  }
  
}

# Dataset 可选 "Sun's Study"，"ZZU In-depth Proteomics"
dbGIST_Proteomics_ID_Pathway(Dataset = "Sun's Study",ID = ID,Number = 100,Proteomics_ID_Pathway_list = Proteomics_ID_Pathway_list)
