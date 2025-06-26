# ==== 蛋白质组学分析平台 ====
# GIST Proteomics Analysis Platform

# ==== 加载全局设置 ====
source("global.R")

# ==== 加载UI和Server ====
source("ui.R")
source("server.R")

# ==== 启动应用 ====
shinyApp(ui = ui, server = server)