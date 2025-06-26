# 快速上手模板

## 30分钟快速创建新Shiny应用

### 第一步：准备工作（5分钟）

```bash
# 1. 创建新项目
mkdir my_new_analysis_app
cd my_new_analysis_app

# 2. 创建目录结构
mkdir -p modules www data

# 3. 复制核心文件
cp ../GIST_shiny/www/custom.css ./www/
cp ../GIST_shiny/proteomics_app/modules/analysis_template.R ./modules/
cp ../GIST_shiny/proteomics_app/create_modules.R ./
```

### 第二步：分析后端脚本（5分钟）

```r
# 在R中分析你的后端脚本
source("your_backend.R")

# 查看所有函数
ls(pattern = "^your_prefix_")

# 示例：假设你有这些函数
# - analyze_expression(gene_id)
# - analyze_correlation(gene1, gene2) 
# - analyze_pathway(gene_id, pathway)
```

### 第三步：生成核心文件（10分钟）

**创建 `global.R`：**
```r
# ==== 加载包 ====
library(shiny)
library(bs4Dash)
library(data.table)
library(tidyverse)
library(ggplot2)
library(ggsci)
library(ggpubr)

# ==== 加载数据 ====
source("your_backend.R")
your_data <- readRDS("your_data.rds")

# ==== 主题设置 ====
theme_colors <- list(
  primary_500 = "#1C484C"
)

# ==== 模块配置 ====
module_info <- list(
  module1 = list(title = "表达分析", icon = "chart-bar"),
  module2 = list(title = "相关性分析", icon = "project-diagram"),
  module3 = list(title = "通路分析", icon = "sitemap")
)
```

**创建 `ui.R`：**
```r
ui <- dashboardPage(
  title = "我的分析平台",
  
  dashboardHeader(
    title = dashboardBrand(title = "我的分析平台", color = "primary")
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("首页", tabName = "home", icon = icon("home")),
      menuItem("表达分析", tabName = "expression", icon = icon("chart-bar")),
      menuItem("相关性分析", tabName = "correlation", icon = icon("project-diagram")),
      menuItem("通路分析", tabName = "pathway", icon = icon("sitemap"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    
    tabItems(
      tabItem("home", 
        h1("欢迎使用我的分析平台")
      ),
      tabItem("expression", 
        source("modules/expression_ui.R", local = TRUE)$value
      ),
      tabItem("correlation", 
        source("modules/correlation_ui.R", local = TRUE)$value
      ),
      tabItem("pathway", 
        source("modules/pathway_ui.R", local = TRUE)$value
      )
    )
  )
)
```

**创建 `server.R`：**
```r
server <- function(input, output, session) {
  source("modules/expression_server.R", local = TRUE)
  source("modules/correlation_server.R", local = TRUE)
  source("modules/pathway_server.R", local = TRUE)
}
```

**创建 `app.R`：**
```r
source("global.R")
source("ui.R") 
source("server.R")
shinyApp(ui = ui, server = server)
```

### 第四步：生成模块（10分钟）

```r
# 加载生成脚本
source("create_modules.R")

# 方法1：单个创建
create_analysis_module(
  module_id = "expression",
  module_title = "基因表达分析",
  module_description = "分析基因表达差异",
  backend_function = "analyze_expression"
)

create_analysis_module(
  module_id = "correlation", 
  module_title = "相关性分析",
  module_description = "分析基因间相关性",
  backend_function = "analyze_correlation",
  has_second_input = TRUE
)

# 方法2：批量创建
module_config <- data.frame(
  id = c("expression", "correlation", "pathway"),
  title = c("基因表达分析", "相关性分析", "通路分析"),
  description = c("分析基因表达差异", "分析基因间相关性", "通路富集分析"),
  function_name = c("analyze_expression", "analyze_correlation", "analyze_pathway"),
  has_second_input = c(FALSE, TRUE, FALSE),
  data_extractor = c(TRUE, FALSE, TRUE),
  stringsAsFactors = FALSE
)

create_multiple_modules(module_config)
```

## 完整示例：转换现有R脚本

假设你有一个 `metabolomics.R` 脚本：

```r
# metabolomics.R
metabolite_boxplot <- function(metabolite_id, group_by = "condition") {
  # 你的分析代码
  return(ggplot_object)
}

metabolite_correlation <- function(met1, met2) {
  # 相关性分析
  return(correlation_plot)
}

metabolite_pathway <- function(metabolite_id) {
  # 通路分析
  return(pathway_plot)
}
```

**转换步骤：**

1. **复制模板文件**（已完成上述步骤）

2. **修改global.R**：
   ```r
   source("metabolomics.R")
   metabolite_data <- readRDS("metabolite_data.rds")
   ```

3. **生成模块**：
   ```r
   create_analysis_module("metabolite_box", "代谢物箱线图", "比较不同条件下代谢物含量", "metabolite_boxplot")
   create_analysis_module("metabolite_cor", "代谢物相关性", "分析代谢物间相关性", "metabolite_correlation", TRUE)
   create_analysis_module("metabolite_path", "代谢通路", "代谢通路富集分析", "metabolite_pathway")
   ```

4. **启动应用**：
   ```r
   shiny::runApp()
   ```

## 常见问题快速解决

**问题1：函数不存在**
```r
# 检查函数是否正确加载
ls(pattern = "your_function_name")
```

**问题2：数据文件找不到**
```r
# 确保数据文件在正确位置
file.exists("your_data.rds")
```

**问题3：图片显示异常**
- 检查函数返回值是否为ggplot对象
- 确认图片尺寸设置

**问题4：样式不一致**
- 确认custom.css文件已复制
- 检查ui.R中是否引入了CSS文件

就是这么简单！30分钟内你就能为任何R分析脚本创建一个专业的Web界面。