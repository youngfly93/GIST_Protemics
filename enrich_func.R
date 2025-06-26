enrichment_Multi <- function(proteins_data, OrgDb = organism_db, orgKegg = organism_kegg, organismReactome = organism_reactome) {
  library(clusterProfiler)
  library(ReactomePA)
  
  gene <- unique(proteins_data)
  
  ID_trans <- function(enrich_result, from, to = "SYMBOL", OrgDb) {
    library(clusterProfiler)
    trans_result <- apply(enrich_result, 1, from = from, to = to, OrgDb2 = OrgDb, function(x, from, to, OrgDb2) {
      genelist <- unlist(strsplit(x["geneID"], split = '/'))
      genelist <- try(bitr(genelist, fromType = from, toType = to, OrgDb = OrgDb2), silent = TRUE)
      if ('try-error' %in% class(genelist)) {
        return("")
      } else {
        result <- paste0(genelist[[to]], collapse = "/")
        return(result)
      }
    })
    enrich_result  <- enrich_result %>% 
      mutate(geneID2 = trans_result, .after = "geneID")
    return(enrich_result)
  }
  
  # keytypes(org.Rn.eg.db)
  
  # GO富集分析
  GO_results <- enrichGO(gene = gene, OrgDb = OrgDb, keyType = "UNIPROT", ont = "ALL")
  GO_results@result <- ID_trans(GO_results@result, from = "UNIPROT", OrgDb = OrgDb)
  
  # KEGG富集分析
  R.utils::setOption("clusterProfiler.download.method", "auto")
  KEGG_results <- enrichKEGG(gene = gene, organism = orgKegg, keyType = "uniprot")
  KEGG_results@result <- ID_trans(KEGG_results@result, from = "UNIPROT", OrgDb = OrgDb)
  
  # Reactome富集分析
  Proteins <- bitr(geneID = gene, OrgDb = OrgDb, fromType = "UNIPROT", toType = "ENTREZID")
  Reactome_results <- enrichPathway(gene = Proteins$ENTREZID, organism = organismReactome)
  Reactome_results@result <- ID_trans(Reactome_results@result, from = "ENTREZID", OrgDb = OrgDb)
  
  enrichment_list <- list()
  enrichment_list[["go"]] <- GO_results  # GO结果
  enrichment_list[["kegg"]] <- KEGG_results  # KEGG结果
  enrichment_list[["reactome"]] <- Reactome_results  # Reactome结果
  return(enrichment_list)
} 