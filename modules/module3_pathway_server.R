# Module 3: Pathway Enrichment Analysis Server
# 通路富集分析模块 Server

# 首先source pathway_final.R以获取所有函数
if (file.exists("pathway_final.R")) {
  source("pathway_final.R")
}

module3_pathway_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # 存储分析结果的响应式值
    analysis_results <- reactiveVal(NULL)
    enrichment_data <- reactiveVal(NULL)
    gsea_data <- reactiveVal(NULL)
    
    # 示例数据
    observeEvent(input$example, {
      updateTextInput(session, "protein_id", value = "P4HA1")
      updateSelectInput(session, "dataset", selected = "Sun's Study")
      updateNumericInput(session, "n_positive", value = 50)
      updateNumericInput(session, "n_negative", value = 50)
      
      showNotification("Example loaded: P4HA1 protein", type = "info")
    })
    
    # 主分析函数
    observeEvent(input$analyze, {
      req(input$protein_id)
      
      # 显示进度
      withProgress(message = 'Running pathway analysis...', {
        
        setProgress(0.1, detail = "Validating input...")
        
        # 验证输入
        protein_id <- trimws(input$protein_id)
        if (protein_id == "") {
          showNotification("Please enter a valid protein ID", type = "error")
          return()
        }
        
        # 获取物种相关参数
        species_params <- switch(input$species,
          "human" = list(OrgDb = "org.Hs.eg.db", orgKegg = "hsa", organismReactome = "human"),
          "mouse" = list(OrgDb = "org.Mm.eg.db", orgKegg = "mmu", organismReactome = "mouse"),
          "rat" = list(OrgDb = "org.Rn.eg.db", orgKegg = "rno", organismReactome = "rat")
        )
        
        setProgress(0.3, detail = "Finding correlated proteins...")
        
        # 执行分析
        tryCatch({
          if (input$perform_enrichment || input$perform_gsea) {
            # 获取GSEA参数
            gsea_files <- if (input$perform_gsea) input$gsea_databases else c()
            
            # 完整分析（pathway + 富集 + GSEA）
            results <- dbGIST_Proteomics_Pathway_Enrichment(
              Dataset = input$dataset,
              ID = protein_id,
              top_positive = input$n_positive,
              top_negative = input$n_negative,
              perform_enrichment = input$perform_enrichment,
              perform_gsea = input$perform_gsea,
              gmt_files = gsea_files,
              gsea_nperm = if (input$perform_gsea) input$gsea_nperm else 1000,
              gsea_min_size = if (input$perform_gsea) input$gsea_min_size else 15,
              gsea_max_size = if (input$perform_gsea) input$gsea_max_size else 500,
              enrichment_pvalue_cutoff = if (input$perform_enrichment) input$enrichment_pvalue_cutoff else 0.05,
              gsea_pvalue_cutoff = if (input$perform_gsea) input$gsea_pvalue_cutoff else 0.05,
              plot_results = input$generate_plots,
              OrgDb = species_params$OrgDb,
              orgKegg = species_params$orgKegg,
              organismReactome = species_params$organismReactome
            )
          } else {
            # 仅pathway分析
            pathway_results <- dbGIST_Proteomics_ID_Pathway_PosNeg(
              Dataset = input$dataset,
              ID = protein_id,
              Number = max(input$n_positive, input$n_negative)
            )
            
            results <- list(
              pathway = pathway_results,
              enrichment = NULL,
              gsea = NULL,
              plots = NULL,
              gsea_plots = NULL
            )
          }
          
          setProgress(0.9, detail = "Preparing results...")
          
          # 保存结果
          analysis_results(results)
          
          # 准备富集数据表格
          if (!is.null(results$enrichment)) {
            enrichment_tables <- prepare_enrichment_tables(results$enrichment)
            enrichment_data(enrichment_tables)
          }
          
          # 准备GSEA数据表格
          if (!is.null(results$gsea)) {
            gsea_tables <- prepare_gsea_tables(results$gsea)
            gsea_data(gsea_tables)
          }
          
          setProgress(1, detail = "Analysis completed!")
          showNotification("Analysis completed successfully!", type = "success", duration = 3)
          
        }, error = function(e) {
          showNotification(
            paste("Analysis failed:", e$message), 
            type = "error", 
            duration = 10
          )
          print(e)
        })
      })
    })
    
    
    # 富集分析结果展示
    # GO结果
    output$go_plot <- renderPlot({
      results <- analysis_results()
      req(results, results$plots)
      
      group <- input$go_group
      if (!is.null(results$plots[[group]]$go_dot)) {
        print(results$plots[[group]]$go_dot)
      } else {
        plot.new()
        text(0.5, 0.5, "No GO enrichment results available", cex = 1.5)
      }
    })
    
    output$go_table <- DT::renderDataTable({
      data <- enrichment_data()
      req(data)
      
      group <- input$go_group
      if (!is.null(data[[group]]$go)) {
        data[[group]]$go
      }
    }, options = list(pageLength = 10, scrollX = TRUE))
    
    # KEGG结果
    output$kegg_plot <- renderPlot({
      results <- analysis_results()
      req(results, results$plots)
      
      group <- input$kegg_group
      if (!is.null(results$plots[[group]]$kegg_dot)) {
        print(results$plots[[group]]$kegg_dot)
      } else {
        plot.new()
        text(0.5, 0.5, "No KEGG enrichment results available", cex = 1.5)
      }
    })
    
    output$kegg_table <- DT::renderDataTable({
      data <- enrichment_data()
      req(data)
      
      group <- input$kegg_group
      if (!is.null(data[[group]]$kegg)) {
        data[[group]]$kegg
      }
    }, options = list(pageLength = 10, scrollX = TRUE))
    
    # Reactome结果
    output$reactome_plot <- renderPlot({
      results <- analysis_results()
      req(results, results$plots)
      
      group <- input$reactome_group
      if (!is.null(results$plots[[group]]$reactome_dot)) {
        print(results$plots[[group]]$reactome_dot)
      } else {
        plot.new()
        text(0.5, 0.5, "No Reactome enrichment results available", cex = 1.5)
      }
    })
    
    output$reactome_table <- DT::renderDataTable({
      data <- enrichment_data()
      req(data)
      
      group <- input$reactome_group
      if (!is.null(data[[group]]$reactome)) {
        data[[group]]$reactome
      }
    }, options = list(pageLength = 10, scrollX = TRUE))
    
    # 汇总图
    output$summary_plot <- renderPlot({
      results <- analysis_results()
      req(results, results$plots)
      
      if (!is.null(results$plots$summary)) {
        print(results$plots$summary)
      } else {
        plot.new()
        text(0.5, 0.5, "No summary plot available", cex = 1.5)
      }
    })
    
    output$summary_table <- DT::renderDataTable({
      data <- enrichment_data()
      req(data)
      
      # 合并所有组的top结果
      summary_df <- NULL
      for (group in names(data)) {
        for (db in names(data[[group]])) {
          if (!is.null(data[[group]][[db]]) && nrow(data[[group]][[db]]) > 0) {
            top_terms <- head(data[[group]][[db]], 3)
            top_terms$Group <- group
            top_terms$Database <- db
            summary_df <- rbind(summary_df, top_terms[, c("Description", "p.adjust", "Count", "Group", "Database")])
          }
        }
      }
      
      summary_df
    }, options = list(pageLength = 15))
    
    # 下载功能
    output$download_proteins_csv <- downloadHandler(
      filename = function() {
        paste0(input$protein_id, "_correlated_proteins_", Sys.Date(), ".csv")
      },
      content = function(file) {
        results <- analysis_results()
        req(results)
        
        df <- rbind(
          data.frame(Protein = results$pathway$positive, Type = "Positive", Rank = seq_along(results$pathway$positive)),
          data.frame(Protein = results$pathway$negative, Type = "Negative", Rank = seq_along(results$pathway$negative))
        )
        
        write.csv(df, file, row.names = FALSE)
      }
    )
    
    output$download_proteins_excel <- downloadHandler(
      filename = function() {
        paste0(input$protein_id, "_correlated_proteins_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        results <- analysis_results()
        req(results)
        
        library(openxlsx)
        wb <- createWorkbook()
        
        # 正相关表
        addWorksheet(wb, "Positive")
        writeData(wb, "Positive", data.frame(
          Rank = seq_along(results$pathway$positive),
          Protein = results$pathway$positive
        ))
        
        # 负相关表
        addWorksheet(wb, "Negative")
        writeData(wb, "Negative", data.frame(
          Rank = seq_along(results$pathway$negative),
          Protein = results$pathway$negative
        ))
        
        saveWorkbook(wb, file, overwrite = TRUE)
      }
    )
    
    output$download_enrichment_csv <- downloadHandler(
      filename = function() {
        paste0(input$protein_id, "_enrichment_results_", Sys.Date(), ".csv")
      },
      content = function(file) {
        data <- enrichment_data()
        req(data)
        
        all_results <- NULL
        for (group in names(data)) {
          for (db in names(data[[group]])) {
            if (!is.null(data[[group]][[db]])) {
              df <- data[[group]][[db]]
              df$Group <- group
              df$Database <- db
              all_results <- rbind(all_results, df)
            }
          }
        }
        
        write.csv(all_results, file, row.names = FALSE)
      }
    )
    
    output$download_plots_pdf <- downloadHandler(
      filename = function() {
        paste0(input$protein_id, "_pathway_plots_", Sys.Date(), ".pdf")
      },
      content = function(file) {
        results <- analysis_results()
        req(results, results$plots)
        
        pdf(file, width = 10, height = 8)
        
        for (group in names(results$plots)) {
          for (plot_name in names(results$plots[[group]])) {
            if (!is.null(results$plots[[group]][[plot_name]])) {
              print(results$plots[[group]][[plot_name]])
            }
          }
        }
        
        dev.off()
      }
    )
    
    # AI分析集成
    observeEvent(input$request_ai_analysis, {
      results <- analysis_results()
      req(results)
      
      # 准备AI分析的上下文
      context <- paste(
        "Pathway enrichment analysis for protein", input$protein_id,
        "\nDataset:", input$dataset,
        "\nPositive correlated proteins:", length(results$pathway$positive),
        "\nNegative correlated proteins:", length(results$pathway$negative)
      )
      
      if (!is.null(results$enrichment)) {
        context <- paste(context, "\n\nEnrichment analysis was performed.")
        
        # 添加top富集通路
        for (group in names(results$enrichment)) {
          for (db in names(results$enrichment[[group]])) {
            if (!is.null(results$enrichment[[group]][[db]]) && 
                nrow(results$enrichment[[group]][[db]]@result) > 0) {
              top_terms <- head(results$enrichment[[group]][[db]]@result$Description, 3)
              context <- paste(context, 
                             sprintf("\n%s %s top terms: %s", 
                                     group, db, paste(top_terms, collapse = ", ")))
            }
          }
        }
      }
      
      # 发送到AI模块
      if (exists("send_to_ai_module") && is.function(send_to_ai_module)) {
        send_to_ai_module(
          module_name = "Pathway Enrichment Analysis",
          analysis_type = "pathway_enrichment",
          context = context,
          request_id = paste0("pathway_", input$protein_id, "_", Sys.time())
        )
      }
      
      showNotification("AI analysis requested", type = "info")
    })
    
    # AI分析输出
    output$ai_analysis_output <- renderUI({
      # 这里可以接收AI分析结果并显示
      # 具体实现取决于AI模块的设计
      tags$div(
        class = "ai-analysis-placeholder",
        tags$p("AI analysis will appear here when available.")
      )
    })
    
    # ============ GSEA 输出 ============
    
    # GSEA KEGG 图表
    output$gsea_kegg_plot <- renderPlot({
      results <- analysis_results()
      req(results, results$gsea_plots)
      
      if (!is.null(results$gsea_plots[["GSEA_KEGG"]]$summary)) {
        results$gsea_plots[["GSEA_KEGG"]]$summary
      } else {
        ggplot() + 
          annotate("text", x = 0.5, y = 0.5, label = "No significant KEGG pathways found", size = 5) +
          theme_void()
      }
    })
    
    # GSEA Hallmark 图表
    output$gsea_hallmark_plot <- renderPlot({
      results <- analysis_results()
      req(results, results$gsea_plots)
      
      if (!is.null(results$gsea_plots[["GSEA_hallmark"]]$summary)) {
        results$gsea_plots[["GSEA_hallmark"]]$summary
      } else {
        ggplot() + 
          annotate("text", x = 0.5, y = 0.5, label = "No significant Hallmark pathways found", size = 5) +
          theme_void()
      }
    })
    
    # GSEA KEGG 表格
    output$gsea_kegg_table <- DT::renderDataTable({
      gsea_tables <- gsea_data()
      req(gsea_tables)
      
      if (!is.null(gsea_tables[["GSEA_KEGG"]])) {
        DT::datatable(
          gsea_tables[["GSEA_KEGG"]],
          options = list(
            pageLength = 15,
            scrollX = TRUE,
            columnDefs = list(
              list(targets = 6, render = JS(
                "function(data, type, row, meta) {",
                "return type === 'display' && data != null && data.length > 50 ?",
                "'<span title=\"' + data + '\">' + data.substr(0, 50) + '...</span>' : data;",
                "}"
              ))
            )
          ),
          filter = "top",
          rownames = FALSE
        ) %>%
          DT::formatSignif(columns = c("ES", "NES", "pval", "padj"), digits = 3)
      } else {
        DT::datatable(data.frame(Message = "No KEGG GSEA results available"))
      }
    })
    
    # GSEA Hallmark 表格
    output$gsea_hallmark_table <- DT::renderDataTable({
      gsea_tables <- gsea_data()
      req(gsea_tables)
      
      if (!is.null(gsea_tables[["GSEA_hallmark"]])) {
        DT::datatable(
          gsea_tables[["GSEA_hallmark"]],
          options = list(
            pageLength = 15,
            scrollX = TRUE,
            columnDefs = list(
              list(targets = 6, render = JS(
                "function(data, type, row, meta) {",
                "return type === 'display' && data != null && data.length > 50 ?",
                "'<span title=\"' + data + '\">' + data.substr(0, 50) + '...</span>' : data;",
                "}"
              ))
            )
          ),
          filter = "top",
          rownames = FALSE
        ) %>%
          DT::formatSignif(columns = c("ES", "NES", "pval", "padj"), digits = 3)
      } else {
        DT::datatable(data.frame(Message = "No Hallmark GSEA results available"))
      }
    })
    
    # 更新GSEA通路选择器
    observe({
      results <- analysis_results()
      if (!is.null(results$gsea)) {
        db_selected <- input$gsea_database_select
        
        if (!is.null(db_selected) && !is.null(results$gsea[[db_selected]])) {
          significant_pathways <- results$gsea[[db_selected]]$results
          if (!is.null(significant_pathways) && nrow(significant_pathways) > 0) {
            pathway_choices <- setNames(significant_pathways$pathway, significant_pathways$pathway)
            updateSelectInput(session, "gsea_pathway_select", choices = pathway_choices)
          }
        }
      }
    })
    
    # GSEA 详细图
    output$gsea_detail_plot <- renderPlot({
      results <- analysis_results()
      req(results, input$gsea_database_select, input$gsea_pathway_select)
      
      if (!is.null(results$gsea_plots[[input$gsea_database_select]]$detailed[[input$gsea_pathway_select]])) {
        results$gsea_plots[[input$gsea_database_select]]$detailed[[input$gsea_pathway_select]]
      } else {
        ggplot() + 
          annotate("text", x = 0.5, y = 0.5, label = "Select a pathway to view detailed GSEA plot", size = 5) +
          theme_void()
      }
    })
    
    # GSEA Leading Edge 表格
    output$gsea_leading_edge_table <- DT::renderDataTable({
      results <- analysis_results()
      req(results, input$gsea_database_select, input$gsea_pathway_select)
      
      if (!is.null(results$gsea[[input$gsea_database_select]]$detailed_results[[input$gsea_pathway_select]])) {
        leading_edge <- results$gsea[[input$gsea_database_select]]$detailed_results[[input$gsea_pathway_select]]$leading_edge
        
        if (length(leading_edge) > 0) {
          le_df <- data.frame(
            Gene = leading_edge,
            stringsAsFactors = FALSE
          )
          
          DT::datatable(
            le_df,
            options = list(pageLength = 20, scrollY = "300px"),
            rownames = FALSE
          )
        } else {
          DT::datatable(data.frame(Message = "No leading edge genes available"))
        }
      } else {
        DT::datatable(data.frame(Message = "Select a pathway to view leading edge genes"))
      }
    })
    
    # GSEA 信息框
    output$gsea_kegg_count <- renderInfoBox({
      gsea_tables <- gsea_data()
      
      count <- if (!is.null(gsea_tables[["GSEA_KEGG"]])) {
        sum(gsea_tables[["GSEA_KEGG"]]$padj < 0.05, na.rm = TRUE)
      } else {
        0
      }
      
      infoBox(
        title = "KEGG Pathways",
        value = count,
        subtitle = "Significant (FDR < 0.05)",
        icon = icon("project-diagram"),
        color = "orange"
      )
    })
    
    output$gsea_hallmark_count <- renderInfoBox({
      gsea_tables <- gsea_data()
      
      count <- if (!is.null(gsea_tables[["GSEA_hallmark"]])) {
        sum(gsea_tables[["GSEA_hallmark"]]$padj < 0.05, na.rm = TRUE)
      } else {
        0
      }
      
      infoBox(
        title = "Hallmark Sets",
        value = count,
        subtitle = "Significant (FDR < 0.05)",
        icon = icon("star"),
        color = "purple"
      )
    })
    
    # GSEA 比较图
    output$gsea_comparison_plot <- renderPlot({
      gsea_tables <- gsea_data()
      req(gsea_tables)
      
      # 合并所有GSEA结果
      all_gsea <- list()
      for (db in names(gsea_tables)) {
        if (!is.null(gsea_tables[[db]])) {
          df <- gsea_tables[[db]] %>%
            filter(padj < 0.05) %>%
            head(10) %>%
            mutate(Database = gsub("GSEA_", "", db))
          all_gsea[[db]] <- df
        }
      }
      
      if (length(all_gsea) > 0) {
        combined_gsea <- do.call(rbind, all_gsea)
        
        if (nrow(combined_gsea) > 0) {
          ggplot(combined_gsea, aes(x = reorder(pathway, abs(NES)), y = NES, fill = Database)) +
            geom_col(position = "dodge", alpha = 0.8) +
            coord_flip() +
            scale_fill_manual(values = c("KEGG" = "#E31A1C", "hallmark" = "#1F78B4")) +
            labs(title = "Top GSEA Results Comparison",
                 x = "Pathway",
                 y = "Normalized Enrichment Score (NES)") +
            theme_minimal() +
            theme(axis.text.y = element_text(size = 8))
        } else {
          ggplot() + 
            annotate("text", x = 0.5, y = 0.5, label = "No significant GSEA results found", size = 5) +
            theme_void()
        }
      } else {
        ggplot() + 
          annotate("text", x = 0.5, y = 0.5, label = "No GSEA results available", size = 5) +
          theme_void()
      }
    })
    
    # GSEA 合并表格
    output$gsea_combined_table <- DT::renderDataTable({
      gsea_tables <- gsea_data()
      req(gsea_tables)
      
      # 合并所有GSEA结果
      all_gsea <- list()
      for (db in names(gsea_tables)) {
        if (!is.null(gsea_tables[[db]])) {
          df <- gsea_tables[[db]] %>%
            mutate(Database = gsub("GSEA_", "", db)) %>%
            dplyr::select(Database, pathway, ES, NES, pval, padj, size)
          all_gsea[[db]] <- df
        }
      }
      
      if (length(all_gsea) > 0) {
        combined_gsea <- do.call(rbind, all_gsea) %>%
          arrange(padj, desc(abs(NES)))
        
        DT::datatable(
          combined_gsea,
          options = list(
            pageLength = 20,
            scrollX = TRUE
          ),
          filter = "top",
          rownames = FALSE
        ) %>%
          DT::formatSignif(columns = c("ES", "NES", "pval", "padj"), digits = 3)
      } else {
        DT::datatable(data.frame(Message = "No GSEA results available"))
      }
    })
    
    # GSEA 下载处理器
    output$download_gsea_csv <- downloadHandler(
      filename = function() {
        paste0("gsea_results_", input$protein_id, "_", Sys.Date(), ".csv")
      },
      content = function(file) {
        gsea_tables <- gsea_data()
        req(gsea_tables)
        
        # 合并所有GSEA结果
        all_gsea <- list()
        for (db in names(gsea_tables)) {
          if (!is.null(gsea_tables[[db]])) {
            df <- gsea_tables[[db]] %>%
              mutate(Database = gsub("GSEA_", "", db))
            all_gsea[[db]] <- df
          }
        }
        
        if (length(all_gsea) > 0) {
          combined_gsea <- do.call(rbind, all_gsea)
          write.csv(combined_gsea, file, row.names = FALSE)
        }
      }
    )
    
    output$download_gsea_excel <- downloadHandler(
      filename = function() {
        paste0("gsea_results_", input$protein_id, "_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        gsea_tables <- gsea_data()
        req(gsea_tables)
        
        # 创建Excel工作簿
        wb <- openxlsx::createWorkbook()
        
        for (db in names(gsea_tables)) {
          if (!is.null(gsea_tables[[db]])) {
            sheet_name <- gsub("GSEA_", "", db)
            openxlsx::addWorksheet(wb, sheet_name)
            openxlsx::writeData(wb, sheet_name, gsea_tables[[db]])
          }
        }
        
        openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      }
    )
  })
}

# 辅助函数：准备富集数据表格
prepare_enrichment_tables <- function(enrichment_results) {
  tables <- list()
  
  for (group in names(enrichment_results)) {
    tables[[group]] <- list()
    
    # GO表格
    if (!is.null(enrichment_results[[group]]$go) && 
        nrow(enrichment_results[[group]]$go@result) > 0) {
      tables[[group]]$go <- enrichment_results[[group]]$go@result %>%
        dplyr::select(Description, GeneRatio, BgRatio, pvalue, p.adjust, qvalue, Count) %>%
        mutate(
          pvalue = signif(pvalue, 3),
          p.adjust = signif(p.adjust, 3),
          qvalue = signif(qvalue, 3)
        )
    }
    
    # KEGG表格
    if (!is.null(enrichment_results[[group]]$kegg) && 
        nrow(enrichment_results[[group]]$kegg@result) > 0) {
      tables[[group]]$kegg <- enrichment_results[[group]]$kegg@result %>%
        dplyr::select(Description, GeneRatio, BgRatio, pvalue, p.adjust, qvalue, Count) %>%
        mutate(
          pvalue = signif(pvalue, 3),
          p.adjust = signif(p.adjust, 3),
          qvalue = signif(qvalue, 3)
        )
    }
    
    # Reactome表格
    if (!is.null(enrichment_results[[group]]$reactome) && 
        nrow(enrichment_results[[group]]$reactome@result) > 0) {
      tables[[group]]$reactome <- enrichment_results[[group]]$reactome@result %>%
        dplyr::select(Description, GeneRatio, BgRatio, pvalue, p.adjust, qvalue, Count) %>%
        mutate(
          pvalue = signif(pvalue, 3),
          p.adjust = signif(p.adjust, 3),
          qvalue = signif(qvalue, 3)
        )
    }
  }
  
  return(tables)
}

# 辅助函数：准备GSEA数据表格
prepare_gsea_tables <- function(gsea_results) {
  tables <- list()
  
  for (db_name in names(gsea_results)) {
    if (!is.null(gsea_results[[db_name]]$results)) {
      tables[[db_name]] <- gsea_results[[db_name]]$results %>%
        arrange(desc(abs(NES))) %>%
        mutate(
          ES = round(ES, 3),
          NES = round(NES, 3),
          pval = signif(pval, 3),
          padj = signif(padj, 3)
        ) %>%
        dplyr::select(pathway, ES, NES, pval, padj, size, leading_edge)
    }
  }
  
  return(tables)
}