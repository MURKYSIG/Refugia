# Header ----------------------------------------------------------------
# Project: Refugia
# File name: run_workflow.R
# Last updated: 2025-05-22
# Author: Lewis A. Jones; Danijela Dimitrijevic
# Email: LewisA.Jones@outlook.com; danijela.dimitrijevic@fau.de
# Repository: https://github.com/MURKYSIG/Refugia

# Source files ----------------------------------------------------------
# Data preparation
source("R/01a_data_preparation_reefs.R")
# Remove environment
rm(list = ls())
# Data analyses
source("R/01b_data_preparation_corals.R")
# Remove environment
rm(list = ls())
# Data visualisation
source("R/02_data_visualisation.R")
# Remove environment
rm(list = ls())

