# ==== Module 5: 生存分析 Server ====

# 数据提取函数
extract_survival_data <- function(gene_id, survival_type, cutoff_point) {
  tryCatch({
    # 获取临床数据
    clinical_data <- Protemics_list[[2]]$Clinical
    
    # 添加表达数据
    if(gene_id %in% rownames(Protemics_list[[2]]$Matrix)) {
      clinical_data$Expr <- as.numeric(Protemics_list[[2]]$Matrix[gene_id, ])
      
      # 根据生存类型选择相应列
      time_col <- ifelse(survival_type == "OS", "OS.time", "PFS.time")
      event_col <- ifelse(survival_type == "OS", "OS", "PFS")
      
      # 检查必需列是否存在
      required_cols <- c(time_col, event_col, "Expr")
      if(all(required_cols %in% colnames(clinical_data))) {
        
        # 计算cutpoint
        res.cut <- surv_cutpoint(clinical_data, 
                                 time = time_col, 
                                 event = event_col,
                                 variables = "Expr")
        
        if (cutoff_point == "Median") {
          res.cut$cutpoint$cutpoint <- median(clinical_data$Expr, na.rm = TRUE)
        } else if (cutoff_point == "Mean") {
          res.cut$cutpoint$cutpoint <- mean(clinical_data$Expr, na.rm = TRUE)
        }
        
        # 分类数据
        res.cat <- surv_categorize(res.cut)
        
        # 添加分组信息
        clinical_data$Group <- res.cat$Expr[match(rownames(clinical_data), rownames(res.cat))]
        
        return(clinical_data[!is.na(clinical_data$Group), ])
      }
    }
    
    return(NULL)
  }, error = function(e) {
    cat("Error in extract_survival_data:", e$message, "\n")
    return(NULL)
  })
}

# ==== Module 5: 生存分析服务器逻辑 ====

# 响应式值 - Module 5
if(!exists("values_module5")) {
  values_module5 <- reactiveValues(
    plot = NULL,
    data = NULL,
    stats = NULL
  )
}
  
  # 分析按钮事件
  observeEvent(input$analyze, {
    # 检查是否有AI分析正在进行（仅在AI启用时）
    if (!is.null(global_state) && tolower(Sys.getenv("ENABLE_AI_ANALYSIS", "true")) == "true") {
      tryCatch({
        if (!is.null(global_state$ai_analyzing) && global_state$ai_analyzing) {
          showNotification(
            paste0("⏳ AI正在分析", global_state$analyzing_gene, "，请稍候..."),
            type = "warning",
            duration = 3
          )
          return()
        }
      }, error = function(e) {
        cat("AI state check error (ignored):", e$message, "\n")
      })
    }
    
    req(input$gene1, input$survival_type, input$cutoff_point)
    
    # 显示进度条
    withProgress(message = '正在进行生存分析...', value = 0, {
      incProgress(0.3, detail = "准备数据")
      
      # 执行分析
      tryCatch({
        result <- KM_function(
          Protemics2_Clinical = Protemics_list[[2]]$Clinical,
          CutOff_point = input$cutoff_point,
          Survival_type = input$survival_type,
          ID = input$gene1
        )
        
        incProgress(0.5, detail = "生成生存曲线")
        
        if(!is.null(result)) {
          values_module5$plot <- result
          values_module5$data <- extract_survival_data(input$gene1, input$survival_type, input$cutoff_point)

          # 生成统计信息
          values_module5$stats <- paste0(
            "分析参数:\n",
            "蛋白质: ", input$gene1, "\n",
            "生存类型: ", ifelse(input$survival_type == "OS", "总生存期", "无进展生存期"), "\n",
            "分组方法: ", input$cutoff_point, "\n",
            "样本数量: ", ifelse(is.null(values_module5$data), "N/A", nrow(values_module5$data))
          )
          
          showNotification("生存分析完成！", type = "message")
          
          # 触发AI分析（如果启用）
          if(exists("enable_ai") && enable_ai) {
            # 保存图片到www目录
            plot_filename <- paste0("plot_survival_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")
            plot_path <- file.path("www", plot_filename)

            # 确保www目录存在
            if(!dir.exists("www")) {
              dir.create("www", recursive = TRUE)
            }

            # 保存图片
            tryCatch({
              png(plot_path, width = 1200, height = 800, res = 150)
              print(values_module5$plot)
              dev.off()

              # 发送AI分析请求
              session$sendCustomMessage("updateAIInput", list(
                plotPath = plot_path,
                relativePath = plot_filename,
                gene1 = input$gene1,
                gene2 = paste0(input$survival_type, "_", input$cutoff_point),
                analysisType = "survival_analysis",
                autoTriggered = TRUE
              ))

              cat("AI analysis triggered for:", plot_filename, "\n")

            }, error = function(e) {
              cat("Error saving plot for AI analysis:", e$message, "\n")
            })
          }
        } else {
          showNotification("生存分析失败，请检查数据或参数", type = "error")
        }
        
        incProgress(0.2, detail = "完成")
        
      }, error = function(e) {
        showNotification(
          paste("生存分析出错：", e$message),
          type = "error",
          duration = 5
        )
      })
    })
  })
  
  # 显示结果区域
  observeEvent(input$analyze, {
    shinyjs::show("result_container")
  })
  
  # 渲染图表
  output$plot <- renderPlot({
    req(values_module5$plot)
    values_module5$plot
  }, width = 800, height = 700, res = 96)

  # 渲染统计信息
  output$survival_stats <- renderText({
    req(values_module5$stats)
    values_module5$stats
  })
  
  # SVG下载
  output$download_svg <- downloadHandler(
    filename = function() {
      paste0("survival_", input$gene1, "_", input$survival_type, "_", Sys.Date(), ".svg")
    },
    content = function(file) {
      req(values_module5$plot)
      svg(file, width = 12, height = 10)
      print(values_module5$plot)
      dev.off()
    }
  )

  # PDF下载
  output$download_pdf <- downloadHandler(
    filename = function() {
      paste0("survival_", input$gene1, "_", input$survival_type, "_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      req(values_module5$plot)
      pdf(file, width = 12, height = 10)
      print(values_module5$plot)
      dev.off()
    }
  )

  # PNG下载
  output$download_png <- downloadHandler(
    filename = function() {
      paste0("survival_", input$gene1, "_", input$survival_type, "_", Sys.Date(), ".png")
    },
    content = function(file) {
      req(values_module5$plot)
      png(file, width = 1200, height = 1000, res = 150)
      print(values_module5$plot)
      dev.off()
    }
  )

  # 下载数据
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("survival_data_", input$gene1, "_", input$survival_type, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      if(!is.null(values_module5$data)) {
        write.csv(values_module5$data, file, row.names = FALSE)
      }
    }
  )
  
# 监控全局AI状态，控制按钮可用性（仅在AI启用时）
if (!is.null(global_state) && tolower(Sys.getenv("ENABLE_AI_ANALYSIS", "true")) == "true") {
  # 安全的AI状态监控
  observe({
    tryCatch({
      if (!is.null(global_state$ai_analyzing)) {
        if (global_state$ai_analyzing) {
          shinyjs::disable("analyze")
          shinyjs::addClass("analyze", "btn-disabled")
        } else {
          shinyjs::enable("analyze")
          shinyjs::removeClass("analyze", "btn-disabled")
        }
      }
    }, error = function(e) {
      # 如果访问AI状态失败，确保按钮可用
      shinyjs::enable("analyze")
      shinyjs::removeClass("analyze", "btn-disabled")
      cat("AI state monitoring error (ignored):", e$message, "\n")
    })
  })
}
