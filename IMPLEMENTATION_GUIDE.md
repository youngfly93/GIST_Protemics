# Shiny应用快速开发指南：基于现有主题风格的批量复现方法

## 概述

本指南详细说明了如何基于现有Shiny项目的主题风格和背景颜色，快速为新的后端R脚本创建一致的Web界面。这种方法适用于需要批量开发多个分析模块的场景。

## 1. 项目结构分析

### 1.1 源项目结构（GIST基因表达分析）
```
GIST_shiny/
├── global.R              # 全局设置
├── ui.R                  # 用户界面
├── server.R              # 服务器逻辑
├── www/custom.css        # 主题样式文件 ⭐
├── modules/              # 模块文件
└── original/             # 数据文件
```

### 1.2 目标项目结构（新建应用）
```
new_app/
├── global.R              # 全局设置（复制+修改）
├── ui.R                  # 用户界面（复制+修改）
├── server.R              # 服务器逻辑（复制+修改）
├── app.R                 # 应用入口
├── start_app.R           # 启动脚本
├── README.md             # 说明文档
├── modules/              # 分析模块
│   ├── analysis_template.R  # 通用模板
│   └── module_*_ui.R/server.R  # 具体模块
├── www/                  # 静态资源
│   └── custom.css        # 主题样式（直接复制）
├── backend_script.R      # 后端分析函数
└── data_file.rds         # 数据文件
```

## 2. 主题风格提取和复用

### 2.1 核心主题文件

**关键文件：`www/custom.css`**
- 包含完整的颜色方案、字体、布局样式
- 定义CSS变量，便于统一管理
- 兼容bs4Dash框架组件

**主要颜色方案：**
```css
:root {
  --primary-900: #0F2B2E;
  --primary-700: #163A3D;
  --primary-500: #1C484C;  /* 主色调 */
  --primary-300: #3C6B6F;
  --primary-100: #D7E4E5;
  --primary-050: #F2F7F7;
  --accent-coral: #E87D4C;
  --accent-lime: #9CCB3B;
  --accent-sky: #2F8FBF;
}
```

### 2.2 主题复用步骤

1. **直接复制CSS文件**
   ```bash
   cp source_project/www/custom.css new_app/www/
   ```

2. **在global.R中定义主题变量**
   ```r
   theme_colors <- list(
     primary_900 = "#0F2B2E",
     primary_700 = "#163A3D", 
     primary_500 = "#1C484C",
     primary_300 = "#3C6B6F",
     primary_100 = "#D7E4E5",
     primary_050 = "#F2F7F7"
   )
   ```

3. **在ui.R中引入样式**
   ```r
   tags$head(
     tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
   )
   ```

## 3. 后端函数分析和适配

### 3.1 后端函数分析流程

**步骤1：函数结构分析**
```r
# 示例：分析Protemic.R
source("backend_script.R")

# 查看所有函数
ls(pattern = "^dbGIST_|^analyze_|^plot_")

# 分析函数签名
# function_name(parameter1, parameter2, ...)
```

**步骤2：函数分类**
- **单参数函数**：`dbGIST_Proteomics_boxplot_TvsN(gene_id)`
- **双参数函数**：`dbGIST_Proteomics_cor_ID(gene1, gene2)`
- **多参数函数**：`dbGIST_Proteomics_boxplot_Drug(gene_id, drug_name)`

**步骤3：返回值分析**
- 大多数函数返回ggplot对象或patchwork组合图
- 需要确认数据提取方式

### 3.2 函数适配模式

**模式A：标准箱线图**
```r
# 适用于临床特征分析
function_name <- function(gene_id, clinical_feature) {
  # 数据处理
  # 返回ggplot对象
}
```

**模式B：相关性分析**
```r
# 适用于双变量分析
function_name <- function(gene1, gene2) {
  # 相关性计算
  # 返回散点图+拟合线
}
```

**模式C：ROC分析**
```r
# 适用于预测模型
function_name <- function(gene_id, outcome) {
  # ROC计算
  # 返回ROC曲线+统计指标
}
```

## 4. 模块化架构设计

### 4.1 通用模板架构

**核心文件：`modules/analysis_template.R`**

**UI模板特点：**
- 响应式布局，支持单/双基因输入
- 标准化的结果展示区域（Plot + Data tabs）
- 一致的下载功能（SVG/PDF/PNG/CSV）
- 统一的样式和交互模式

**Server模板特点：**
- 通用的错误处理机制
- 标准化的进度提示
- 灵活的函数调用接口
- 统一的下载处理器

### 4.2 模板使用方法

**UI模块创建：**
```r
# modules/new_module_ui.R
createAnalysisUI(
  id = "module_id",
  title = "分析标题",
  description = "功能描述",
  has_second_gene = FALSE  # 是否需要第二个基因输入
)
```

**Server模块创建：**
```r
# modules/new_module_server.R
createAnalysisServer(
  id = "module_id",
  analysis_function = your_backend_function,
  extract_data_function = your_data_extractor  # 可选
)
```

## 5. 批量复现具体步骤

### 5.1 环境准备

```bash
# 1. 创建新项目目录
mkdir new_analysis_app
cd new_analysis_app

# 2. 创建标准目录结构
mkdir -p modules www data config
```

### 5.2 核心文件复制和修改

**步骤1：复制核心框架文件**
```bash
# 复制主题样式
cp ../GIST_shiny/www/custom.css ./www/

# 复制模板文件
cp ../GIST_shiny/proteomics_app/modules/analysis_template.R ./modules/

# 复制启动相关文件
cp ../GIST_shiny/proteomics_app/app.R ./
cp ../GIST_shiny/proteomics_app/start_app.R ./
```

**步骤2：修改global.R**
```r
# ==== 加载必需的包 ====
library(shiny)
library(bs4Dash)
library(data.table)
library(tidyverse)
library(ggplot2)
# ... 其他必需包

# ==== 加载数据和函数 ====
source("your_backend_script.R")
your_data <- readRDS("your_data_file.rds")

# ==== 主题变量 ====
theme_colors <- list(
  primary_500 = "#1C484C",  # 保持一致的主色调
  # ... 其他颜色
)

# ==== 模块信息 ====
module_info <- list(
  module1 = list(
    title = "您的分析模块1",
    icon = "chart-bar"
  ),
  # ... 其他模块
)
```

**步骤3：修改ui.R**
```r
ui <- dashboardPage(
  title = "您的分析平台名称",
  
  dashboardHeader(
    title = dashboardBrand(
      title = "您的分析平台",
      color = "primary"
    )
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("首页", tabName = "home", icon = icon("home")),
      menuItem("分析模块1", tabName = "module1", icon = icon("chart-bar"))
      # 根据后端函数添加更多模块
    )
  ),
  
  dashboardBody(
    # 引入样式
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    
    tabItems(
      # 根据模块添加内容
    )
  )
)
```

### 5.3 模块生成自动化

**创建模块生成脚本：`create_modules.R`**
```r
create_analysis_module <- function(module_id, module_title, module_description, 
                                 backend_function, has_second_input = FALSE) {
  
  # 生成UI文件
  ui_content <- sprintf('
# ==== %s UI ====
createAnalysisUI(
  id = "%s",
  title = "%s", 
  description = "%s",
  has_second_gene = %s
)', module_title, module_id, module_title, module_description, has_second_input)
  
  writeLines(ui_content, sprintf("modules/%s_ui.R", module_id))
  
  # 生成Server文件
  server_content <- sprintf('
# ==== %s Server ====
createAnalysisServer(
  id = "%s",
  analysis_function = %s
)', module_title, module_id, backend_function)
  
  writeLines(server_content, sprintf("modules/%s_server.R", module_id))
  
  cat("Created module:", module_id, "\n")
}

# 使用示例
create_analysis_module(
  module_id = "expression_analysis",
  module_title = "基因表达分析", 
  module_description = "分析基因在不同条件下的表达差异",
  backend_function = "your_expression_function"
)
```

## 6. 质量控制和优化

### 6.1 常见问题和解决方案

**问题1：图表显示过小**
```r
# 解决方案：在analysis_template.R中设置固定尺寸
output$plot <- renderPlot({
  req(values$plot)
  values$plot
}, height = 600, width = 800)
```

**问题2：下载功能失效**
```r
# 解决方案：确保所有下载处理器都已定义
output$download_svg <- downloadHandler(...)
output$download_pdf <- downloadHandler(...)
output$download_png <- downloadHandler(...)
output$download_data <- downloadHandler(...)
```

**问题3：数据文件路径错误**
```r
# 解决方案：使用相对路径并确保文件在正确位置
your_data <- readRDS("data_file.rds")  # 不要使用../
```

### 6.2 性能优化建议

1. **数据预加载**：在global.R中加载所有数据
2. **函数缓存**：对计算密集型函数使用缓存
3. **模块化加载**：按需加载模块文件
4. **图表优化**：设置合适的图表尺寸和分辨率

## 7. 部署和维护

### 7.1 标准化启动

**创建标准启动脚本：**
```r
# start_app.R
#!/usr/bin/env Rscript
cat("Starting Analysis Platform...\n")
cat("URL: http://localhost:PORT\n")
shiny::runApp(port = PORT)
```

### 7.2 文档标准化

**README.md模板：**
```markdown
# 分析平台名称

## 功能模块
- 模块1：功能描述
- 模块2：功能描述

## 启动方法
\`\`\`bash
Rscript start_app.R
\`\`\`

## 使用说明
1. 选择分析模块
2. 输入参数
3. 查看结果
4. 下载数据
```

## 8. 批量复现checklist

### 开发前准备
- [ ] 分析后端R脚本函数结构
- [ ] 确定所需的输入参数类型
- [ ] 规划模块数量和层次结构

### 环境搭建  
- [ ] 创建项目目录结构
- [ ] 复制主题CSS文件
- [ ] 复制分析模板文件
- [ ] 准备数据文件

### 代码适配
- [ ] 修改global.R（数据加载、模块配置）
- [ ] 修改ui.R（导航菜单、标题）
- [ ] 修改server.R（模块调用）
- [ ] 生成具体分析模块

### 测试验证
- [ ] 测试应用启动
- [ ] 测试各模块功能
- [ ] 测试下载功能
- [ ] 测试错误处理

### 文档完善
- [ ] 更新README.md
- [ ] 创建使用说明
- [ ] 记录已知问题

## 结论

通过这套标准化流程，可以快速为任何后端R分析脚本创建一致的Web界面。关键在于：

1. **主题复用**：直接复制CSS文件保证视觉一致性
2. **模板化开发**：使用通用模板减少重复代码
3. **标准化流程**：遵循固定的目录结构和命名规范
4. **自动化生成**：使用脚本批量生成模块文件

这种方法特别适合需要为多个不同的分析后端快速构建Web界面的场景，能够显著提高开发效率并保证界面的一致性。