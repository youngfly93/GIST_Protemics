# Pathway分析 + 富集分析使用指南

## 功能概述
集成了pathway相关性分析和富集分析功能，可以：
1. 找出目标基因的正相关和负相关基因
2. 对这些基因进行GO、KEGG、Reactome富集分析
3. 生成多种可视化图表

## 快速开始

### 1. 基本使用
```r
source("pathway_final.R")

# 一步完成所有分析
results <- dbGIST_Proteomics_Pathway_Enrichment(
  Dataset = "Sun's Study",
  ID = "P4HA1",
  top_positive = 50,
  top_negative = 50
)
```

### 2. 查看结果
```r
# 相关基因
results$pathway$positive  # 正相关基因
results$pathway$negative  # 负相关基因

# 富集结果
results$enrichment$positive$go      # 正相关基因的GO富集
results$enrichment$negative$kegg    # 负相关基因的KEGG富集
results$enrichment$all$reactome     # 所有基因的Reactome富集

# 查看图形
results$plots$positive$go_dot       # 正相关基因GO点图
results$plots$summary               # 汇总图
```

### 3. 保存结果
```r
# 保存所有结果和图表
save_enrichment_results(results, output_dir = "my_results", gene_id = "P4HA1")
```

## 高级选项

### 自定义参数
```r
results <- dbGIST_Proteomics_Pathway_Enrichment(
  Dataset = "Sun's Study",           # 数据集
  ID = "P4HA1",                     # 目标基因
  top_positive = 50,                # 正相关基因数
  top_negative = 50,                # 负相关基因数
  perform_enrichment = TRUE,        # 是否进行富集分析
  plot_results = TRUE,              # 是否生成图表
  OrgDb = "org.Hs.eg.db",          # 物种数据库
  orgKegg = "hsa",                 # KEGG物种代码
  organismReactome = "human"        # Reactome物种
)
```

### 仅获取相关基因（不做富集）
```r
# 方法1：使用集成函数
results <- dbGIST_Proteomics_Pathway_Enrichment(
  Dataset = "Sun's Study",
  ID = "P4HA1",
  perform_enrichment = FALSE
)

# 方法2：使用原始函数
genes <- dbGIST_Proteomics_ID_Pathway_PosNeg(
  Dataset = "Sun's Study",
  ID = "P4HA1",
  Number = 50
)
```

### 批量分析
```r
genes <- c("P4HA1", "CALR", "PDIA4")
results_list <- list()

for (gene in genes) {
  results_list[[gene]] <- dbGIST_Proteomics_Pathway_Enrichment(
    Dataset = "Sun's Study",
    ID = gene
  )
}
```

## 可视化选项

生成的图表包括：
- **点图 (Dot Plot)**: 显示富集程度和基因数
- **条形图 (Bar Plot)**: 显示top富集条目
- **网络图 (Emap Plot)**: 显示GO terms之间的关系
- **汇总图 (Summary Plot)**: 比较正负相关基因的富集结果

## 注意事项

1. **首次使用**需要运行 `split_proteomics_data()` 预处理数据
2. **物种设置**：默认为人类，其他物种需要修改OrgDb等参数
3. **内存使用**：富集分析可能占用较多内存，建议关闭不需要的程序
4. **包依赖**：会自动安装所需的Bioconductor包

## 常见问题

### Q: 基因ID格式不匹配怎么办？
A: 函数会自动尝试SYMBOL和UNIPROT格式，通常能自动处理

### Q: 富集结果为空？
A: 可能是基因数太少（<5个）或没有显著富集的通路

### Q: 如何更改p值阈值？
A: 目前使用默认值0.05，可以在enrichment_Multi函数中修改pvalueCutoff参数

### Q: 支持哪些物种？
A: 支持所有有OrgDb包的物种，如小鼠(org.Mm.eg.db)、大鼠(org.Rn.eg.db)等