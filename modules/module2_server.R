# ==== Module 2: 分子相关性分析 Server ====

# 数据提取函数
extract_correlation_data <- function(gene1, gene2) {
  data_list <- list()
  
  # 数据集2
  if(gene1 %in% rownames(Protemics_list[[2]]$Matrix) && 
     gene2 %in% rownames(Protemics_list[[2]]$Matrix)) {
    values1 <- as.numeric(Protemics_list[[2]]$Matrix[gene1, ])
    values2 <- as.numeric(Protemics_list[[2]]$Matrix[gene2, ])
    
    data_list[[1]] <- data.frame(
      Dataset = Protemics_list[[2]]$ID,
      Sample = colnames(Protemics_list[[2]]$Matrix),
      Gene1 = gene1,
      Gene1_Value = values1,
      Gene2 = gene2,
      Gene2_Value = values2,
      stringsAsFactors = FALSE
    )
  }
  
  # 数据集3
  if(gene1 %in% rownames(Protemics_list[[3]]$Matrix) && 
     gene2 %in% rownames(Protemics_list[[3]]$Matrix)) {
    values1 <- as.numeric(Protemics_list[[3]]$Matrix[gene1, ])
    values2 <- as.numeric(Protemics_list[[3]]$Matrix[gene2, ])
    
    data_list[[2]] <- data.frame(
      Dataset = Protemics_list[[3]]$ID,
      Sample = colnames(Protemics_list[[3]]$Matrix),
      Gene1 = gene1,
      Gene1_Value = values1,
      Gene2 = gene2,
      Gene2_Value = values2,
      stringsAsFactors = FALSE
    )
  }
  
  # 合并数据并去除NA
  if(length(data_list) > 0) {
    combined_data <- do.call(rbind, data_list)
    return(na.omit(combined_data))
  } else {
    return(NULL)
  }
}

# 自定义分析函数包装器
correlation_analysis_wrapper <- function(gene1, gene2 = NULL) {
  if(is.null(gene2) || gene2 == "") {
    stop("请输入第二个蛋白质ID")
  }
  return(dbGIST_Proteomics_cor_ID(gene1, gene2))
}

# 自定义数据提取包装器
extract_data_wrapper <- function(gene1) {
  # 这里需要从input中获取gene2，但在模板中不容易实现
  # 暂时返回空，在实际应用中需要修改模板
  return(NULL)
}

# 创建自定义服务器逻辑
moduleServer("correlation", function(input, output, session) {
  
  # 响应式值
  values <- reactiveValues(
    plot = NULL,
    data = NULL
  )
  
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

    req(input$gene1, input$gene2)

    # 显示进度条
    withProgress(message = '正在分析...', value = 0, {
      incProgress(0.3, detail = "准备数据")
      
      # 执行分析
      tryCatch({
        result <- dbGIST_Proteomics_cor_ID(input$gene1, input$gene2)
        
        incProgress(0.5, detail = "生成图表")
        
        if(!is.null(result)) {
          values$plot <- result
          values$data <- extract_correlation_data(input$gene1, input$gene2)
          showNotification("相关性分析完成！", type = "message")

          # 触发AI分析（如果启用）
          if(exists("enable_ai") && enable_ai) {
            # 保存图片到www目录
            plot_filename <- paste0("plot_correlation_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")
            plot_path <- file.path("www", plot_filename)

            # 确保www目录存在
            if(!dir.exists("www")) {
              dir.create("www", recursive = TRUE)
            }

            # 保存图片
            tryCatch({
              png(plot_path, width = 1200, height = 800, res = 150)
              print(values$plot)
              dev.off()

              # 发送AI分析请求
              session$sendCustomMessage("updateAIInput", list(
                plotPath = plot_path,
                relativePath = plot_filename,
                gene1 = input$gene1,
                gene2 = input$gene2,
                analysisType = "correlation",
                autoTriggered = TRUE
              ))

              cat("AI analysis triggered for:", plot_filename, "\n")

            }, error = function(e) {
              cat("Error saving plot for AI analysis:", e$message, "\n")
            })
          }
        } else {
          showNotification("未找到该蛋白质数据", type = "warning")
        }
        
        incProgress(0.2, detail = "完成")
        
      }, error = function(e) {
        showNotification(
          paste("分析出错：", e$message),
          type = "error",
          duration = 5
        )
      })
    })
  })
  
  # 渲染图表
  output$plot <- renderPlot({
    req(values$plot)

    # 设置图形参数以避免边距错误
    tryCatch({
      # 重置图形参数
      par(mar = c(4, 4, 2, 2))
      values$plot
    }, error = function(e) {
      cat("Error in plot rendering:", e$message, "\n")
      # 返回一个简单的错误提示图
      plot(1, 1, type = "n", xlab = "", ylab = "", main = "绘图错误")
      text(1, 1, "绘图时发生错误\n请检查数据或联系管理员", cex = 1.2, col = "red")
    })
  }, width = 800, height = 600, res = 96)
  
  # 渲染数据表格
  output$table <- DT::renderDataTable({
    req(values$data)
    DT::datatable(
      values$data,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        language = list(
          url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Chinese.json'
        )
      )
    )
  })
  
  # SVG下载
  output$download_svg <- downloadHandler(
    filename = function() {
      paste0("correlation_", input$gene1, "_", input$gene2, "_", Sys.Date(), ".svg")
    },
    content = function(file) {
      req(values$plot)
      svg(file, width = 12, height = 8)
      print(values$plot)
      dev.off()
    }
  )

  # PDF下载
  output$download_pdf <- downloadHandler(
    filename = function() {
      paste0("correlation_", input$gene1, "_", input$gene2, "_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      req(values$plot)
      pdf(file, width = 12, height = 8)
      print(values$plot)
      dev.off()
    }
  )

  # PNG下载
  output$download_png <- downloadHandler(
    filename = function() {
      paste0("correlation_", input$gene1, "_", input$gene2, "_", Sys.Date(), ".png")
    },
    content = function(file) {
      req(values$plot)
      png(file, width = 1200, height = 800, res = 150)
      print(values$plot)
      dev.off()
    }
  )

  # 下载数据
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("correlation_data_", input$gene1, "_", input$gene2, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      if(!is.null(values$data)) {
        write.csv(values$data, file, row.names = FALSE)
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
})