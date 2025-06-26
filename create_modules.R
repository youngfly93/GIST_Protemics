#!/usr/bin/env Rscript

# ==== Shiny模块自动生成脚本 ====
# 用于快速创建标准化的分析模块

create_analysis_module <- function(module_id, module_title, module_description, 
                                 backend_function, has_second_input = FALSE,
                                 data_extractor = NULL) {
  
  cat("创建模块:", module_title, "\n")
  
  # 生成UI文件
  ui_content <- sprintf('# ==== %s UI ====

createAnalysisUI(
  id = "%s",
  title = "%s", 
  description = "%s",
  has_second_gene = %s
)', module_title, module_id, module_title, module_description, has_second_input)
  
  ui_file <- sprintf("modules/%s_ui.R", module_id)
  writeLines(ui_content, ui_file)
  
  # 生成Server文件
  if(is.null(data_extractor)) {
    server_content <- sprintf('# ==== %s Server ====

createAnalysisServer(
  id = "%s",
  analysis_function = %s
)', module_title, module_id, backend_function)
  } else {
    server_content <- sprintf('# ==== %s Server ====

# 数据提取函数
extract_%s_data <- function(gene_id) {
  # TODO: 实现数据提取逻辑
  # 返回数据框格式的数据
  return(NULL)
}

createAnalysisServer(
  id = "%s",
  analysis_function = %s,
  extract_data_function = extract_%s_data
)', module_title, module_id, module_id, backend_function, module_id)
  }
  
  server_file <- sprintf("modules/%s_server.R", module_id)
  writeLines(server_content, server_file)
  
  cat("✓ 已创建:", ui_file, "\n")
  cat("✓ 已创建:", server_file, "\n")
  
  # 返回模块信息，用于更新ui.R和server.R
  return(list(
    id = module_id,
    title = module_title,
    ui_file = ui_file,
    server_file = server_file
  ))
}

# ==== 批量创建模块函数 ====
create_multiple_modules <- function(module_config) {
  created_modules <- list()
  
  for(i in 1:nrow(module_config)) {
    config <- module_config[i, ]
    module <- create_analysis_module(
      module_id = config$id,
      module_title = config$title,
      module_description = config$description,
      backend_function = config$function_name,
      has_second_input = config$has_second_input,
      data_extractor = config$data_extractor
    )
    created_modules[[i]] <- module
    cat("\n")
  }
  
  # 生成更新代码提示
  cat("==== 请将以下代码添加到ui.R的sidebarMenu中 ====\n")
  for(module in created_modules) {
    cat(sprintf('menuItem("%s", tabName = "%s", icon = icon("chart-bar")),\n', 
                module$title, module$id))
  }
  
  cat("\n==== 请将以下代码添加到ui.R的tabItems中 ====\n")
  for(module in created_modules) {
    cat(sprintf('tabItem(tabName = "%s", source("%s", local = TRUE)$value),\n', 
                module$id, module$ui_file))
  }
  
  cat("\n==== 请将以下代码添加到server.R中 ====\n")
  for(module in created_modules) {
    cat(sprintf('source("%s", local = TRUE)\n', module$server_file))
  }
  
  return(created_modules)
}

# ==== 使用示例 ====
if(FALSE) {
  # 示例1：创建单个模块
  create_analysis_module(
    module_id = "expression_analysis",
    module_title = "基因表达分析", 
    module_description = "分析基因在不同条件下的表达差异",
    backend_function = "dbGIST_expression_boxplot",
    has_second_input = FALSE,
    data_extractor = TRUE
  )
  
  # 示例2：批量创建模块
  module_config <- data.frame(
    id = c("tumor_normal", "correlation", "drug_resistance"),
    title = c("肿瘤vs正常", "相关性分析", "药物耐药"),
    description = c("比较肿瘤和正常组织", "分析两个基因相关性", "预测药物耐药性"),
    function_name = c("dbGIST_tumor_normal", "dbGIST_correlation", "dbGIST_drug"),
    has_second_input = c(FALSE, TRUE, FALSE),
    data_extractor = c(TRUE, FALSE, TRUE),
    stringsAsFactors = FALSE
  )
  
  create_multiple_modules(module_config)
}

cat("模块生成脚本已加载。使用create_analysis_module()或create_multiple_modules()创建模块。\n")