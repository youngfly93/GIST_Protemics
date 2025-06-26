# ==== UI Definition ====

# Check if AI functionality is enabled
enable_ai <- tolower(Sys.getenv("ENABLE_AI_ANALYSIS", "true")) == "true"

# Conditionally load AI module
if(enable_ai) {
  source("modules/ai_chat_module.R")
}

# Load Module 3 UI
source("modules/module3_pathway_ui.R")

ui <- dashboardPage(
  dark = FALSE,
  title = "GIST Proteomics Analysis Platform",

  dashboardHeader(
    title = dashboardBrand(
      title = "GIST Proteomics Analysis Platform",
      color = "primary",
      href = "#",
      image = NULL
    ),
    status = "white",
    border = TRUE,
    sidebarIcon = icon("bars"),
    controlbarIcon = icon("th"),
    fixed = FALSE,

    # Version indicator
    tags$div(
      style = "position: fixed; top: 10px; right: 10px; z-index: 9999;
               background: rgba(255,255,255,0.9); padding: 5px 10px;
               border-radius: 15px; font-size: 12px; font-weight: bold;
               box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
      if(enable_ai) {
        tags$span(style = "color: #28a745;", "AI Version (4968)")
      } else {
        tags$span(style = "color: #6c757d;", "Basic Version (4967)")
      }
    )
  ),
  
  dashboardSidebar(
    fixed = TRUE,
    width = 280,
    status = "primary", 
    elevation = 3,
    collapsed = FALSE,
    minified = TRUE,
    expandOnHover = TRUE,
    id = "sidebar",
    
    sidebarMenu(
      id = "sidebar_menu",
      flat = FALSE,
      compact = FALSE,
      childIndent = TRUE,
      
      menuItem(
        text = "Home",
        tabName = "home",
        icon = icon("home")
      ),
      menuItem(
        text = module_info$module1$title,
        tabName = "module1",
        icon = icon(module_info$module1$icon),
        menuSubItem(
          text = "Tumor vs Normal",
          tabName = "module1_tvn"
        ),
        menuSubItem(
          text = "Risk Level",
          tabName = "module1_risk"
        ),
        menuSubItem(
          text = "Gender Analysis",
          tabName = "module1_gender"
        ),
        menuSubItem(
          text = "Age Analysis",
          tabName = "module1_age"
        ),
        menuSubItem(
          text = "Tumor Size",
          tabName = "module1_tumor_size"
        ),
        menuSubItem(
          text = "Mitotic Count",
          tabName = "module1_mitotic"
        ),
        menuSubItem(
          text = "Tumor Location",
          tabName = "module1_location"
        ),
        menuSubItem(
          text = "WHO Grade",
          tabName = "module1_who"
        ),
        menuSubItem(
          text = "Ki-67",
          tabName = "module1_ki67"
        ),
        menuSubItem(
          text = "CD34",
          tabName = "module1_cd34"
        ),
        menuSubItem(
          text = "Mutation",
          tabName = "module1_mutation"
        )
      ),
      menuItem(
        text = module_info$module2$title,
        tabName = "module2",
        icon = icon(module_info$module2$icon)
      ),
      menuItem(
        text = module_info$module3$title,
        tabName = "module3_pathway",
        icon = icon(module_info$module3$icon)
      ),
      menuItem(
        text = module_info$module4$title,
        tabName = "module4",
        icon = icon(module_info$module4$icon)
      ),
      menuItem(
        text = "Survival Analysis",
        tabName = "module5",
        icon = icon("chart-line")
      )
    )
  ),
  
  dashboardBody(
    # Include custom CSS styles
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
      # Conditionally load AI chat button styles
      if(enable_ai) {
        tags$link(rel = "stylesheet", type = "text/css", href = "ai_chat_buttons.css")
      },

      # Conditionally load analysis button disabled state styles
      if(enable_ai) tags$style(HTML("
        .btn-disabled {
          opacity: 0.5 !important;
          cursor: not-allowed !important;
          pointer-events: none !important;
        }

        .btn-disabled::after {
          content: ' (AI Analyzing...)';
          font-size: 0.8em;
          color: #666;
        }
      "))
    ),

    # 使用shinyjs
    useShinyjs(),
    
    # Tab content
    tabItems(
      # Home page
      tabItem(
        tabName = "home",
        fluidRow(
          column(
            width = 12,
            h1("Welcome to GIST Proteomics Analysis Platform", class = "homeTitle"),
            div(
              class = "intro-text",
              p(proteomics_intro_text)
            )
          )
        ),
        br(),
        fluidRow(
          valueBox(
            value = "11 Types",
            subtitle = "Clinical Feature Analysis",
            icon = icon("chart-bar"),
            color = "primary",
            width = 4
          ),
          valueBox(
            value = "Correlation",
            subtitle = "Protein Association Analysis",
            icon = icon("project-diagram"),
            color = "success",
            width = 4
          ),
          valueBox(
            value = "ROC",
            subtitle = "Drug Resistance Prediction",
            icon = icon("pills"),
            color = "warning",
            width = 4
          )
        )
      ),
      
      # Module 1 - Clinical feature analysis subpages
      tabItem(
        tabName = "module1_tvn",
        source("modules/module1_tvn_ui.R", local = TRUE)$value
      ),
      tabItem(
        tabName = "module1_risk",
        source("modules/module1_risk_ui.R", local = TRUE)$value
      ),
      tabItem(
        tabName = "module1_gender",
        source("modules/module1_gender_ui.R", local = TRUE)$value
      ),
      tabItem(
        tabName = "module1_age",
        source("modules/module1_age_ui.R", local = TRUE)$value
      ),
      tabItem(
        tabName = "module1_tumor_size",
        source("modules/module1_tumor_size_ui.R", local = TRUE)$value
      ),
      tabItem(
        tabName = "module1_mitotic",
        source("modules/module1_mitotic_ui.R", local = TRUE)$value
      ),
      tabItem(
        tabName = "module1_location",
        source("modules/module1_location_ui.R", local = TRUE)$value
      ),
      tabItem(
        tabName = "module1_who",
        source("modules/module1_who_ui.R", local = TRUE)$value
      ),
      tabItem(
        tabName = "module1_ki67",
        source("modules/module1_ki67_ui.R", local = TRUE)$value
      ),
      tabItem(
        tabName = "module1_cd34",
        source("modules/module1_cd34_ui.R", local = TRUE)$value
      ),
      tabItem(
        tabName = "module1_mutation",
        source("modules/module1_mutation_ui.R", local = TRUE)$value
      ),
      
      # Module 2 - 分子相关性分析
      tabItem(
        tabName = "module2",
        source("modules/module2_ui.R", local = TRUE)$value
      ),
      
      # Module 3 - Pathway Enrichment Analysis
      tabItem(
        tabName = "module3_pathway",
        module3_pathway_ui("module3_pathway")
      ),

      # Module 4 - Imatinib resistance analysis
      tabItem(
        tabName = "module4",
        source("modules/module4_ui.R", local = TRUE)$value
      ),

      # Module 5 - Survival analysis
      tabItem(
        tabName = "module5",
        source("modules/module5_ui.R", local = TRUE)$value
      )
    ),

    # Conditionally load AI chat components
    if(enable_ai) {
      tagList(
        # AI chat floating button
        aiChatFloatingButtonUI("ai_chat"),
        # AI chat window
        aiChatUI("ai_chat")
      )
    }
  )
)