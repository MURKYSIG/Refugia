# Header ----------------------------------------------------------------
# Project: Refugia
# File name: run_workflow.R
# Last updated: 2025-03-27
# Author: Lewis A. Jones
# Email: LewisA.Jones@outlook.com
# Repository: https://github.com/LewisAJones/Refugia

# Source files ----------------------------------------------------------
# Data preparation
source("R/01_data_preparation.R")
# Remove environment
rm(list = ls())
# Data analyses
source("R/02_data_analyses.R")
# Remove environment
rm(list = ls())
# Data visualisation
source("R/03_data_visualisation.R")
# Remove environment
rm(list = ls())

