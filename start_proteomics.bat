@echo off
echo Starting GIST Proteomics Analysis Platform...
echo Platform URL: http://localhost:4965
echo Press Ctrl+C to stop the application
echo.

cd /d "%~dp0"
Rscript start_app.R

pause
