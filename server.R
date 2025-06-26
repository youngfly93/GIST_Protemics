# ==== Server定义 ====
server <- function(input, output, session) {

  # ==== 全局响应式值 ====
  values <- reactiveValues(
    current_plot = NULL,
    current_data = NULL
  )

  # ==== AI功能全局状态管理 ====
  global_state <- reactiveValues(
    ai_analyzing = FALSE,
    analyzing_gene = NULL,
    last_analysis_time = NULL
  )
  
  # ==== Module 1 - 临床性状分析服务器逻辑 ====
  
  # 肿瘤vs正常
  source("modules/module1_tvn_server.R", local = TRUE)
  
  # 风险等级
  source("modules/module1_risk_server.R", local = TRUE)
  
  # 性别分析
  source("modules/module1_gender_server.R", local = TRUE)
  
  # 年龄分析
  source("modules/module1_age_server.R", local = TRUE)
  
  # 肿瘤大小
  source("modules/module1_tumor_size_server.R", local = TRUE)
  
  # 有丝分裂计数
  source("modules/module1_mitotic_server.R", local = TRUE)
  
  # 肿瘤位置
  source("modules/module1_location_server.R", local = TRUE)
  
  # WHO分级
  source("modules/module1_who_server.R", local = TRUE)
  
  # Ki-67
  source("modules/module1_ki67_server.R", local = TRUE)
  
  # CD34
  source("modules/module1_cd34_server.R", local = TRUE)
  
  # 突变
  source("modules/module1_mutation_server.R", local = TRUE)
  
  # ==== Module 2 - 分子相关性分析 ====
  source("modules/module2_server.R", local = TRUE)
  
  # ==== Module 3 - Pathway Enrichment Analysis ====
  source("modules/module3_pathway_server.R", local = TRUE)
  module3_pathway_server("module3_pathway")

  # ==== Module 4 - 伊马替尼耐药分析 ====
  source("modules/module4_server.R", local = TRUE)

  # ==== Module 5 - 生存分析 ====
  source("modules/module5_server.R", local = TRUE)

  # ==== AI聊天功能 ====
  # 条件初始化AI聊天服务器逻辑
  if(enable_ai) {
    cat("Initializing AI chat server...\n")
    tryCatch({
      ai_chat_server <- aiChatServer("ai_chat", global_state)
      cat("AI chat server initialized successfully\n")
    }, error = function(e) {
      cat("ERROR: AI chat server initialization failed:", e$message, "\n")
      cat("Stack trace:\n")
      print(traceback())
      # Disable AI functionality to prevent application crash
      enable_ai <<- FALSE
      cat("AI functionality has been disabled due to initialization error\n")
    })
  } else {
    cat("AI chat functionality disabled\n")
  }

  # ==== Global Error Handling ====
  observe({
    # Listen for errors and display user-friendly messages
    options(shiny.error = function() {
      showNotification(
        "An error occurred. Please check your input or contact the administrator.",
        type = "error",
        duration = 5
      )
    })
  })
}