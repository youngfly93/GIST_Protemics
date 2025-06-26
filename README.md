# GIST蛋白质组学分析平台

## 项目概述

本平台是专为胃肠道间质瘤(GIST)蛋白质组学数据分析设计的Shiny Web应用程序。提供全面的蛋白质表达分析工具，包括临床特征关联分析、分子相关性分析和药物耐药性预测。

## 功能模块

### 1. 临床性状分析模块 (Function Module 1)
- **肿瘤vs正常**: 比较蛋白质在肿瘤组织和正常组织中的表达差异
- **风险等级**: 根据NIH风险分级分析蛋白质表达差异
- **性别分析**: 分析蛋白质在男性和女性患者中的表达差异  
- **年龄分析**: 比较不同年龄组的蛋白质表达
- **肿瘤大小**: 根据肿瘤大小分组分析
- **有丝分裂计数**: 根据有丝分裂计数分析
- **肿瘤位置**: 根据肿瘤位置分析
- **WHO分级**: 根据WHO分级标准分析
- **Ki-67**: 根据Ki-67表达水平分析
- **CD34**: 根据CD34表达状态分析
- **突变分析**: 根据基因突变状态分析

### 2. 分子相关性分析模块 (Function Module 2)
- 分析两个蛋白质之间的表达相关性
- 生成散点图和线性回归拟合
- 计算相关系数和统计显著性

### 3. 伊马替尼耐药分析模块 (Function Module 4)  
- 分析蛋白质表达与伊马替尼药物响应的关系
- 生成箱线图和ROC曲线
- 评估蛋白质作为耐药预测标志物的潜力

## 安装和使用

### 1. 环境要求
```r
# 必需的R包
library(shiny)
library(bs4Dash)
library(data.table)
library(tidyverse)
library(ggplot2)
library(ggsci)
library(ggpubr)
library(patchwork)
library(pROC)
library(DT)
library(shinyWidgets)
```

### 2. 数据文件
确保以下数据文件位于正确位置：
- `Protemics_list.rds` - 蛋白质组学数据
- `Protemic.R` - 后端分析函数

### 3. 启动应用
```r
# 在proteomics_app目录下运行
shiny::runApp(port = 4965)

# 或者使用启动脚本
Rscript start_app.R

# 或者直接运行
source("app.R")
```

### 4. 访问应用
启动后访问: http://localhost:4965

### 4. 使用说明
1. 在左侧导航栏选择分析模块
2. 输入蛋白质ID（如：P4HA1）
3. 点击"开始分析"按钮
4. 查看结果图表和数据表格
5. 下载分析结果

## 项目结构
```
proteomics_app/
├── app.R                    # 主应用文件
├── global.R                 # 全局设置和数据加载
├── ui.R                     # 用户界面定义
├── server.R                 # 服务器逻辑
├── README.md               # 项目说明
└── modules/                # 分析模块
    ├── analysis_template.R  # 通用分析模板
    ├── module1_*_ui.R      # 临床分析UI模块
    ├── module1_*_server.R  # 临床分析Server模块
    ├── module2_ui.R        # 相关性分析UI
    ├── module2_server.R    # 相关性分析Server
    ├── module4_ui.R        # 耐药分析UI
    └── module4_server.R    # 耐药分析Server
```

## 技术特点

1. **模块化设计**: 使用通用模板减少代码重复
2. **响应式界面**: 基于bs4Dash的现代化界面
3. **交互式图表**: 支持下载和数据表格查看
4. **错误处理**: 完善的错误提示和异常处理
5. **中文支持**: 完整的中文界面和提示

## 开发说明

本项目基于原有GIST基因表达分析平台的架构，专门为`Protemic.R`后端函数设计。采用模块化开发方式，便于维护和扩展。

## 联系方式

如有问题或建议，请联系开发团队。