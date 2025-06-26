# Module 3: Pathway Enrichment Analysis - 集成指南

## 概述
Module 3 已成功集成到 GIST Proteomics Shiny 应用中，提供了完整的通路富集分析功能。

## 新增功能

### 1. 相关蛋白分析
- 找出目标蛋白的正相关和负相关蛋白
- 支持自定义相关蛋白数量（默认各50个）
- 优化的内存使用和计算速度

### 2. 通路富集分析
- GO (Gene Ontology) 富集分析
- KEGG 通路分析
- Reactome 通路分析
- 支持多物种（人类、小鼠、大鼠）

### 3. 可视化功能
- 点图（Dot Plot）
- 条形图（Bar Plot）
- 富集网络图（Enrichment Map）
- 综合汇总图

### 4. 数据导出
- 相关蛋白列表（CSV/Excel）
- 富集分析结果（CSV/Excel）
- 所有图表（PDF）
- 完整报告（HTML）

## 使用方法

### 1. 启动应用
```r
# 方式1：使用测试脚本
source("test_module3.R")

# 方式2：正常启动
Rscript start_app.R
```

### 2. 导航到Module 3
- 在左侧菜单中点击 "Pathway Enrichment Analysis"

### 3. 执行分析
1. 输入蛋白ID（如：P4HA1）
2. 选择数据集（Sun's Study 或 ZZU In-depth Proteomics）
3. 设置相关基因数量
4. 选择是否进行富集分析
5. 点击 "Run Analysis"

### 4. 查看结果
- **Correlated Proteins**：查看相关蛋白列表
- **Enrichment Analysis Results**：查看富集分析结果
- **Download Results**：下载所有结果

## 技术细节

### 文件结构
```
modules/
├── module3_pathway_ui.R      # UI定义
└── module3_pathway_server.R  # Server逻辑

pathway_final.R               # 核心分析函数
```

### 主要函数
- `dbGIST_Proteomics_ID_Pathway()` - 获取相关基因
- `dbGIST_Proteomics_Pathway_Enrichment()` - 完整分析流程
- `enrichment_Multi()` - 多数据库富集分析
- `generate_enrichment_plots()` - 生成可视化

### 数据要求
- 需要 `Proteomics_ID_Pathway_list.RDS` 文件
- 首次使用建议运行 `split_proteomics_data()` 优化性能

## 常见问题

### Q: 分析速度慢？
A: 首次运行时执行以下命令优化：
```r
source("pathway_final.R")
split_proteomics_data()
```

### Q: 富集分析失败？
A: 检查是否安装了必要的包：
```r
BiocManager::install(c("clusterProfiler", "ReactomePA", "enrichplot"))
BiocManager::install("org.Hs.eg.db")  # 人类
```

### Q: 内存不足？
A: Module 3 已经过优化，但处理大数据时仍可能需要较多内存。建议：
- 关闭其他应用
- 减少相关基因数量
- 使用缓存功能

## AI集成（可选）
如果启用了AI功能（ENABLE_AI_ANALYSIS=true），Module 3 会自动：
- 在分析完成后提供AI分析选项
- 将结果发送给AI助手进行解释
- 提供生物学意义的深入分析