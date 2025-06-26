# Module 3: Pathway Enrichment Analysis UI
# 通路富集分析模块 UI

module3_pathway_ui <- function(id) {
  ns <- NS(id)
  
  tabItem(
    tabName = "module3_pathway",
    h2("Module 3: Pathway Enrichment Analysis"),
    h4("Analyze correlated proteins and perform pathway enrichment"),
    
    fluidRow(
      # 输入面板
      column(
        width = 4,
        bs4Card(
          title = "Input Parameters",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          
          # 蛋白质ID输入
          textInput(
            ns("protein_id"),
            "Enter Protein ID:",
            value = "P4HA1",
            placeholder = "e.g., P4HA1, CALR, PDIA4"
          ),
          
          # 数据集选择
          selectInput(
            ns("dataset"),
            "Select Dataset:",
            choices = c("Sun's Study", "ZZU In-depth Proteomics"),
            selected = "Sun's Study"
          ),
          
          # 相关基因数量设置
          fluidRow(
            column(6,
              numericInput(
                ns("n_positive"),
                "Top Positive:",
                value = 50,
                min = 10,
                max = 200,
                step = 10
              )
            ),
            column(6,
              numericInput(
                ns("n_negative"),
                "Top Negative:",
                value = 50,
                min = 10,
                max = 200,
                step = 10
              )
            )
          ),
          
          # 富集分析选项
          h5("Enrichment Options:"),
          checkboxInput(
            ns("perform_enrichment"),
            "Perform Enrichment Analysis",
            value = TRUE
          ),
          
          conditionalPanel(
            condition = paste0("input['", ns("perform_enrichment"), "'] == true"),
            
            checkboxGroupInput(
              ns("enrichment_dbs"),
              "Select Databases:",
              choices = c("GO" = "go", "KEGG" = "kegg", "Reactome" = "reactome"),
              selected = c("go", "kegg")
            ),
            
            # 物种设置
            selectInput(
              ns("species"),
              "Species:",
              choices = c(
                "Human" = "human",
                "Mouse" = "mouse",
                "Rat" = "rat"
              ),
              selected = "human"
            )
          ),
          
          # GSEA分析选项
          h5("GSEA Analysis Options:"),
          checkboxInput(
            ns("perform_gsea"),
            "Perform GSEA Analysis",
            value = TRUE
          ),
          
          conditionalPanel(
            condition = paste0("input['", ns("perform_gsea"), "'] == true"),
            
            checkboxGroupInput(
              ns("gsea_databases"),
              "Select GSEA Databases:",
              choices = c(
                "KEGG Pathways" = "GSEA_KEGG.gmt",
                "Hallmark Gene Sets" = "GSEA_hallmark.gmt"
              ),
              selected = c("GSEA_KEGG.gmt", "GSEA_hallmark.gmt")
            ),
            
            fluidRow(
              column(6,
                numericInput(
                  ns("gsea_nperm"),
                  "Permutations:",
                  value = 1000,
                  min = 100,
                  max = 10000,
                  step = 100
                )
              ),
              column(6,
                numericInput(
                  ns("gsea_min_size"),
                  "Min Gene Set Size:",
                  value = 15,
                  min = 5,
                  max = 50,
                  step = 5
                )
              )
            ),
            
            numericInput(
              ns("gsea_max_size"),
              "Max Gene Set Size:",
              value = 500,
              min = 100,
              max = 2000,
              step = 50
            )
          ),
          
          # 可视化选项
          checkboxInput(
            ns("generate_plots"),
            "Generate Visualizations",
            value = TRUE
          ),
          
          br(),
          
          # 分析按钮
          actionButton(
            ns("analyze"),
            "Run Analysis",
            class = "btn-primary btn-block",
            icon = icon("play-circle")
          ),
          
          # 示例按钮
          actionButton(
            ns("example"),
            "Load Example",
            class = "btn-info btn-block",
            icon = icon("lightbulb")
          )
        )
      ),
      
      # 结果面板
      column(
        width = 8,
        
        # 富集分析结果
        conditionalPanel(
          condition = paste0("input['", ns("perform_enrichment"), "'] == true"),
          
          bs4Card(
            title = "Enrichment Analysis Results",
            status = "info",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = FALSE,
            width = 12,
            
            tabsetPanel(
              id = ns("enrichment_tabs"),
              
              # GO富集结果
              tabPanel(
                "GO Enrichment",
                value = "go_tab",
                br(),
                fluidRow(
                  column(12,
                    selectInput(
                      ns("go_group"),
                      "Select Group:",
                      choices = c("All" = "all", "Positive" = "positive", "Negative" = "negative"),
                      selected = "all"
                    )
                  )
                ),
                plotOutput(ns("go_plot"), height = "600px"),
                br(),
                DT::dataTableOutput(ns("go_table"))
              ),
              
              # KEGG富集结果
              tabPanel(
                "KEGG Pathways",
                value = "kegg_tab",
                br(),
                fluidRow(
                  column(12,
                    selectInput(
                      ns("kegg_group"),
                      "Select Group:",
                      choices = c("All" = "all", "Positive" = "positive", "Negative" = "negative"),
                      selected = "all"
                    )
                  )
                ),
                plotOutput(ns("kegg_plot"), height = "600px"),
                br(),
                DT::dataTableOutput(ns("kegg_table"))
              ),
              
              # Reactome富集结果
              tabPanel(
                "Reactome Pathways",
                value = "reactome_tab",
                br(),
                fluidRow(
                  column(12,
                    selectInput(
                      ns("reactome_group"),
                      "Select Group:",
                      choices = c("All" = "all", "Positive" = "positive", "Negative" = "negative"),
                      selected = "all"
                    )
                  )
                ),
                plotOutput(ns("reactome_plot"), height = "600px"),
                br(),
                DT::dataTableOutput(ns("reactome_table"))
              ),
              
              # 综合汇总
              tabPanel(
                "Summary",
                value = "summary_tab",
                br(),
                plotOutput(ns("summary_plot"), height = "800px"),
                br(),
                h4("Top Enriched Pathways Summary"),
                DT::dataTableOutput(ns("summary_table"))
              )
            )
          )
        ),
        
        # GSEA分析结果
        conditionalPanel(
          condition = paste0("input['", ns("perform_gsea"), "'] == true"),
          
          bs4Card(
            title = "GSEA Analysis Results",
            status = "warning",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = FALSE,
            width = 12,
            
            tabsetPanel(
              id = ns("gsea_tabs"),
              
              # GSEA KEGG结果
              tabPanel(
                "GSEA KEGG",
                value = "gsea_kegg_tab",
                br(),
                fluidRow(
                  column(12,
                    h4("KEGG Pathway Enrichment (GSEA)"),
                    p("Gene Set Enrichment Analysis using KEGG pathways based on correlation ranking")
                  )
                ),
                plotOutput(ns("gsea_kegg_plot"), height = "600px"),
                br(),
                DT::dataTableOutput(ns("gsea_kegg_table"))
              ),
              
              # GSEA Hallmark结果
              tabPanel(
                "GSEA Hallmark",
                value = "gsea_hallmark_tab",
                br(),
                fluidRow(
                  column(12,
                    h4("Hallmark Gene Set Enrichment (GSEA)"),
                    p("Gene Set Enrichment Analysis using MSigDB Hallmark gene sets")
                  )
                ),
                plotOutput(ns("gsea_hallmark_plot"), height = "600px"),
                br(),
                DT::dataTableOutput(ns("gsea_hallmark_table"))
              ),
              
              # GSEA详细图
              tabPanel(
                "GSEA Details",
                value = "gsea_details_tab",
                br(),
                fluidRow(
                  column(6,
                    selectInput(
                      ns("gsea_database_select"),
                      "Select Database:",
                      choices = c("KEGG" = "GSEA_KEGG", "Hallmark" = "GSEA_hallmark"),
                      selected = "GSEA_KEGG"
                    )
                  ),
                  column(6,
                    selectInput(
                      ns("gsea_pathway_select"),
                      "Select Pathway:",
                      choices = NULL
                    )
                  )
                ),
                plotOutput(ns("gsea_detail_plot"), height = "700px"),
                br(),
                h5("Leading Edge Genes:"),
                DT::dataTableOutput(ns("gsea_leading_edge_table"))
              ),
              
              # GSEA汇总
              tabPanel(
                "GSEA Summary",
                value = "gsea_summary_tab",
                br(),
                fluidRow(
                  column(6,
                    infoBoxOutput(ns("gsea_kegg_count"), width = 12)
                  ),
                  column(6,
                    infoBoxOutput(ns("gsea_hallmark_count"), width = 12)
                  )
                ),
                br(),
                h4("Top GSEA Results Comparison"),
                plotOutput(ns("gsea_comparison_plot"), height = "600px"),
                br(),
                h4("Combined GSEA Results"),
                DT::dataTableOutput(ns("gsea_combined_table"))
              )
            )
          )
        ),
        
        # 下载面板
        bs4Card(
          title = "Download Results",
          status = "warning",
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = TRUE,
          width = 12,
          
          fluidRow(
            column(4,
              h5("Download Protein Lists:"),
              downloadButton(ns("download_proteins_csv"), "Proteins (CSV)", class = "btn-sm"),
              br(), br(),
              downloadButton(ns("download_proteins_excel"), "Proteins (Excel)", class = "btn-sm")
            ),
            
            column(4,
              h5("Download Enrichment Results:"),
              downloadButton(ns("download_enrichment_csv"), "Enrichment (CSV)", class = "btn-sm"),
              br(), br(),
              downloadButton(ns("download_enrichment_excel"), "Enrichment (Excel)", class = "btn-sm"),
              br(), br(),
              downloadButton(ns("download_gsea_csv"), "GSEA (CSV)", class = "btn-sm"),
              br(), br(),
              downloadButton(ns("download_gsea_excel"), "GSEA (Excel)", class = "btn-sm")
            ),
            
            column(4,
              h5("Download Plots:"),
              downloadButton(ns("download_plots_pdf"), "All Plots (PDF)", class = "btn-sm"),
              br(), br(),
              downloadButton(ns("download_report"), "Full Report (HTML)", class = "btn-sm")
            )
          )
        )
      )
    ),
    
    # AI分析面板（如果启用）
    if (isTRUE(getOption("ENABLE_AI_ANALYSIS", FALSE))) {
      fluidRow(
        column(12,
          bs4Card(
            title = "AI Analysis",
            status = "purple",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,
            width = 12,
            
            actionButton(
              ns("request_ai_analysis"),
              "Request AI Analysis",
              icon = icon("robot"),
              class = "btn-purple"
            ),
            br(), br(),
            uiOutput(ns("ai_analysis_output"))
          )
        )
      )
    }
  )
}