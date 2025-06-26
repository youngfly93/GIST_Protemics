# ==== Module 5: Survival Analysis UI ====

fluidPage(
  style = "padding: 20px;",
  
  # Page title
  fluidRow(
    column(12,
      div(
        style = "text-align: center; margin-bottom: 30px;",
        h2("Kaplan-Meier Survival Analysis",
           style = "color: #2c3e50; font-weight: bold; margin-bottom: 10px;"),
        p("Survival analysis and prognostic assessment based on protein expression levels",
          style = "color: #7f8c8d; font-size: 16px;")
      )
    )
  ),
  
  # Input parameter area
  fluidRow(
    column(12,
      box(
        title = "Analysis Parameter Settings",
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        collapsible = TRUE,
        
        fluidRow(
          column(4,
            div(
              style = "margin-bottom: 15px;",
              h4("Protein ID", style = "color: #34495e; margin-bottom: 10px;"),
              textInput(
                inputId = "gene1",
                label = NULL,
                value = "FN1",
                placeholder = "Enter protein ID (e.g., FN1)"
              ),
              helpText("Enter the protein identifier to analyze", style = "color: #7f8c8d;")
            )
          ),
          
          column(4,
            div(
              style = "margin-bottom: 15px;",
              h4("Survival Type", style = "color: #34495e; margin-bottom: 10px;"),
              selectInput(
                inputId = "survival_type",
                label = NULL,
                choices = list(
                  "Overall Survival (OS)" = "OS",
                  "Progression-Free Survival (PFS)" = "PFS"
                ),
                selected = "OS"
              ),
              helpText("Select the endpoint type for survival analysis", style = "color: #7f8c8d;")
            )
          ),
          
          column(4,
            div(
              style = "margin-bottom: 15px;",
              h4("Grouping Method", style = "color: #34495e; margin-bottom: 10px;"),
              selectInput(
                inputId = "cutoff_point",
                label = NULL,
                choices = list(
                  "Auto Optimal (Auto)" = "Auto",
                  "Median (Median)" = "Median",
                  "Mean (Mean)" = "Mean"
                ),
                selected = "Auto"
              ),
              helpText("Select cutoff method for high/low expression grouping", style = "color: #7f8c8d;")
            )
          )
        ),
        
        # Analysis button
        fluidRow(
          column(12, align = "center", style = "padding: 10px;",
            actionButton(
              inputId = "analyze",
              label = "Visualize",
              icon = icon('chart-line'),
              class = "btn-primary btn-lg"
            )
          )
        )
      )
    )
  ),
  
  # Results display area
  div(id = "result_container", style = "display: none;",
    fluidRow(
      column(12,
        box(
          title = "Survival Analysis Results",
          status = "success",
          solidHeader = TRUE,
          width = 12,
          collapsible = TRUE,

          # Chart display
          fluidRow(
            column(12,
              div(
                style = "text-align: center; padding: 20px;",
                withSpinner(
                  plotOutput("plot", height = "700px"),
                  type = 4,
                  color = "#3498db"
                )
              )
            )
          ),
          
          # Statistical information
          fluidRow(
            column(12,
              h4("Analysis Statistics", style = "color: #2c3e50; margin: 20px 0 10px 0;"),
              verbatimTextOutput("survival_stats")
            )
          )
        )
      )
    ),
    
    # Download buttons and AI analysis
    fluidRow(
      column(width = 12, align = "center", style = "padding: 20px;",
        h4("Download Results & AI Analysis"),
        fluidRow(
          column(width = 2,
            downloadButton("download_svg", "SVG", class = "btn-outline-primary")
          ),
          column(width = 2,
            downloadButton("download_pdf", "PDF", class = "btn-outline-primary")
          ),
          column(width = 2,
            downloadButton("download_png", "PNG", class = "btn-outline-primary")
          ),
          column(width = 2,
            downloadButton("download_data", "Data", class = "btn-outline-primary")
          ),
          # Conditionally display AI status indicator
          if(tolower(Sys.getenv("ENABLE_AI_ANALYSIS", "true")) == "true") {
            column(width = 4,
              div(
                style = "text-align: center; padding: 10px; color: #7fb069; font-weight: bold;",
                icon("robot"), " AI Auto-Analysis Enabled"
              )
            )
          }
        )
      )
    )
  )
)
