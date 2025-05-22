# Header ----------------------------------------------------------------
# Project: Refugia
# File name: 01a_data_preparation_reefs.R
# Last updated: 2025-05-22
# Author: Lewis A. Jones; Danijela Dimitrijevic
# Email: LewisA.Jones@outlook.com; danijela.dimitrijevic@fau.de
# Repository: https://github.com/MURKYSIG/Refugia

# Load libraries --------------------------------------------------------
library(dplyr)
library(palaeoverse)

# Load data -------------------------------------------------------------
# PaleoReef Dataset
pared <- read.csv("data/raw/pared_nov2024.csv")
# Load interval table
intervals <- read.csv("data/raw/intervals.csv")

# Merge interval data ---------------------------------------------------
pared <- pared %>%
  left_join(x = ., 
            y = intervals[c("Interval", "max.ma", "min.ma")], 
            by = c("intervall" = "Interval")) %>%
  mutate(max_ma = max.ma,
         min_ma = min.ma) %>%
  select(-c("max.ma", "min.ma"))

# Data filtering --------------------------------------------------------
pared <- pared %>%
  # Exclude mud mounds
  filter(type != 3) %>%
  # Exclude non-tropical reef types
  filter(tropical != 2) %>%
  # Exclude subsurface reefs
  filter(subsurface != 1) %>%
  # Exclude deep-water reefs
  filter(bathymetry != 3) %>%
  # Retain only coral reefs (primary or secondary)
  filter(biota_main == 1 | biota_sec == 1) %>%
  # Retain only post-Cambrian reefs
  filter(max_ma <= 541) 

# Data cleaning ---------------------------------------------------------
# Age in wrong format
vec <- which(pared$max_ma < pared$min_ma)
pared[vec, c("max_ma", "min_ma")] <- pared[vec, c("min_ma", "max_ma")]

# Time binning ----------------------------------------------------------
# Get time bins
bins <- time_bins(scale = "international ages")
# Collapse Holocene equivalent bins
bins <- bins %>%
  mutate(interval_name = ifelse(interval_name == "Greenlandian", "Holocene", 
                                interval_name),
         abbr = ifelse(abbr == "Gr", "H", abbr),
         min_ma = ifelse(min_ma == 0.0082, 0.0000, min_ma),
         mid_ma = (min_ma + bins$max_ma) / 2,
         duration_myr = (max_ma - min_ma)) %>%
  filter(row_number() <= n()-2)
# Collapse Pleistocene equivalent bins
bins <- bins %>%
  mutate(interval_name = ifelse(interval_name == "Gelasian", "Pleistocene", 
                                interval_name),
         abbr = ifelse(abbr == "Ge", "Ple", abbr),
         min_ma = ifelse(min_ma == 1.8000, 0.0117, min_ma),
         mid_ma = (min_ma + bins$max_ma) / 2,
         duration_myr = (max_ma - min_ma)) %>%
  filter(!interval_name %in% c("Calabrian", "Chibanian", "Late Pleistocene")) %>%
  mutate(bin = 1:nrow(.))
# Bin reefs
pared <- bin_time(occdf = pared, bins = bins, method = "point", rep = 1)[[1]]
# Join bin data
pared <- pared %>%
  left_join(x = ., y = bins, by = c("bin_assignment" = "bin"))

# Environmental classification ------------------------------------------
pared <- pared %>%
  # Which reefs are brown mesophotic?
  mutate(photic = case_when(
    # Above storm-weather wave base (approx. > 50 m depth) and siliciclastics
    bathymetry == 1 & subenviron == "1d" ~ "Brown mesophotic",
    # Above storm-water wave (approx. > 50 m depth)
    bathymetry == 1 & subenviron != "1d" ~ "Euphotic",
    # Below storm-weather wave base (approx. > 50 m depth)
    bathymetry == 2 ~ "Blue mesophotic"
  ))
pared$photic[is.na(pared$photic)] <- "Unknown"

# Get summary -----------------------------------------------------------
counts <- pared %>%
  group_by(interval_name, photic) %>%
  count() %>%
  left_join(x = ., y = bins, by = "interval_name")

# Save data -------------------------------------------------------------
write.csv(bins, "data/time_bins.csv", row.names = FALSE)
write.csv(counts, "data/reef_counts.csv", row.names = FALSE)
