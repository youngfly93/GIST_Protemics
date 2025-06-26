# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Application Overview

This is a **GIST Proteomics Analysis Platform** - a comprehensive R Shiny web application designed for analyzing Gastrointestinal Stromal Tumor (GIST) proteomics data. The platform provides protein expression analysis tools, clinical feature associations, molecular correlations, and drug resistance predictions.

## Key Commands

### Development & Running
```bash
# Standard application startup (port 4965)
Rscript start_app.R

# AI-enabled version (port 4968)
Rscript start_ai.R

# Non-AI version (port 4967)
Rscript start_no_ai.R

# Shell script launcher (cross-platform)
./start_proteomics.sh

# Direct R console startup
shiny::runApp(port = 4965)
```

### Testing Functions
```r
# Test proteomics analysis functions
source("test_functions.R")

# Test environment setup
source("test_env.R")

# Test API key configuration
source("test_api_key.R")
```

## Architecture & Structure

### Core Components
The application uses a modular R Shiny architecture with three main analysis modules:

1. **Clinical Feature Analysis** (`modules/module1_*`): 
   - 11 sub-modules analyzing protein expression across clinical parameters
   - Features: Tumor vs Normal, Risk Level, Gender, Age, Tumor Size, Mitotic Count, Location, WHO Grade, Ki-67, CD34, Mutation

2. **Molecular Correlation Analysis** (`modules/module2_*`): 
   - Analyzes correlation between two proteins
   - Generates scatter plots with linear regression

3. **Pathway Enrichment Analysis** (`modules/module3_*`): 
   - KEGG and Hallmark pathway enrichment analysis
   - Gene set enrichment analysis (GSEA)
   - Interactive pathway visualization

4. **Drug Resistance Analysis** (`modules/module4_*`): 
   - Imatinib resistance prediction using protein expression
   - ROC curve analysis and biomarker evaluation

5. **Survival Analysis** (`modules/module5_*`): 
   - Kaplan-Meier survival curves
   - Cox regression analysis
   - Protein expression impact on survival outcomes

### Data Architecture
- **Main data file**: `Protemics_list.rds` - Contains proteomics expression matrices
- **Pathway data**: `Proteomics_ID_Pathway_list.RDS` - Pathway enrichment data
- **GMT files**: `GSEA_KEGG.gmt`, `GSEA_hallmark.gmt` - Gene set definitions
- **Analysis backend**: `Protemic.R` - Core analysis functions with naming pattern `dbGIST_Proteomics_*`
- **Correlation cache**: `correlation_cache/` - Pre-computed correlation results for performance

### Key R Functions (Protemic.R)
- `dbGIST_Proteomics_boxplot_TvsN()`: Tumor vs Normal comparison
- `dbGIST_Proteomics_boxplot_Risk()`: Risk level analysis
- `dbGIST_Proteomics_boxplot_Gender()`: Gender-based analysis
- `dbGIST_Proteomics_boxplot_Age()`: Age group analysis
- `dbGIST_Proteomics_boxplot_Tumor_Size()`: Tumor size analysis
- `dbGIST_Proteomics_boxplot_Mitotic()`: Mitotic count analysis
- `dbGIST_Proteomics_boxplot_Location()`: Tumor location analysis
- `dbGIST_Proteomics_boxplot_WHO()`: WHO grade analysis
- `dbGIST_Proteomics_boxplot_Ki67()`: Ki-67 expression analysis
- `dbGIST_Proteomics_boxplot_CD34()`: CD34 status analysis
- `dbGIST_Proteomics_boxplot_Mutation()`: Mutation status analysis
- `dbGIST_Proteomics_cor_ID()`: Protein-protein correlation
- `dbGIST_Proteomics_boxplot_Drug()`: Drug resistance analysis

### UI/UX Architecture
- **Framework**: bs4Dash for modern Bootstrap 4 interface
- **Theme**: Custom CSS (`www/custom.css`) with defined color scheme
- **Modular UI**: Template-based module generation using `modules/analysis_template.R`
- **Responsive Layout**: Collapsible sidebar, tabbed result views

### AI Integration (Optional)
- **Environment Control**: `ENABLE_AI_ANALYSIS` environment variable
- **API Support**: OpenRouter or Doubao AI services
- **Module**: `modules/ai_chat_module.R` for AI-powered analysis
- **Configuration**: `.env` file for API keys and settings

## Development Notes

### Environment Setup
```bash
# Required environment variables (.env file)
ENABLE_AI_ANALYSIS=true/false
USE_OPENROUTER=true/false
OPENROUTER_API_KEY=your_key
OPENROUTER_API_URL=https://openrouter.ai/api/v1/chat/completions
OPENROUTER_MODEL=google/gemini-2.5-flash

# Alternative AI service (Doubao)
USE_DOUBAO=true/false
DOUBAO_API_KEY=your_key
DOUBAO_API_URL=https://ark.cn-beijing.volces.com/api/v3/chat/completions
DOUBAO_MODEL=ep-20241223165849-sczng
```

### Module Creation
Use the module template system for consistent UI/server patterns:
```r
# Create new analysis module
source("create_modules.R")
# Follow the createAnalysisUI/createAnalysisServer pattern
```

### Key Technologies
- **R Shiny**: Reactive web framework
- **ggplot2**: Statistical visualization
- **patchwork**: Plot composition
- **pROC**: ROC curve analysis
- **bs4Dash**: Modern dashboard UI
- **survival/survminer**: Survival analysis and visualization
- **clusterProfiler**: Pathway enrichment analysis
- **Statistical Tests**: t-tests, correlation analysis, survival analysis
- **renv**: R package dependency management

### Common Development Tasks
- **Adding new clinical feature**: Create `module1_feature_ui.R` and `module1_feature_server.R` in modules/
- **Modifying analysis functions**: Edit `Protemic.R` and ensure function naming follows `dbGIST_Proteomics_*` pattern
- **Updating UI theme**: Modify `www/custom.css` using existing CSS variables
- **Adding new data**: Update `Protemics_list.rds` and reload in `global.R`
- **Performance optimization**: Use `correlation_cache/` for storing pre-computed results
- **Pathway analysis**: Modify GMT files and pathway enrichment functions in `pathway.R`

### Port Management
- Port 4965: Standard application
- Port 4967: Non-AI version
- Port 4968: AI-enabled version