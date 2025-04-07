# Header ----------------------------------------------------------------
# Project: Refugia
# File name: 01b_data_preparation_corals.R
# Last updated: 2025-04-04
# Author: Danijela Dimitrijevic; Lewis A. Jones
# Email: danijela.dimitrijevic@fau.de; LewisA.Jones@outlook.com
# Repository: https://github.com/MURKYSIG/Refugia

# Load libraries --------------------------------------------------------
library(palaeoverse)
library(dplyr)
library(stringr)
library(divDyn)

# Load data -------------------------------------------------------------
# Load coral data
corals <- read.csv("data/raw/pbdb_data.csv", skip = 21)
# Load time bins
bins <- read.csv("data/time_bins.csv")

# Data cleaning ---------------------------------------------------------
# Extract genus (genus column contains instances of NO_GENUS_SPECIFIED)
corals$genus <- word(corals$accepted_name, 1)
corals$accepted_rank[which(corals$accepted_rank == "subgenus")] <- "genus"

# Data filtering --------------------------------------------------------
# Which corals are colonial? Extract occurrence numbers
colonial <- corals$occurrence_no[grepl(pattern = "colonial", x = corals$life_habit)]
# Which corals are photosymbiotic? Extract occurrence numbers
photosymbiotic <- corals$occurrence_no[grepl(pattern = "photosymbiotic", x = corals$diet)]
# Get unique occurrence numbers
occ_no <- unique(c(colonial, photosymbiotic))
# Which occurrences do collections belong to?
coll_no <- corals %>%
  filter(occurrence_no %in% occ_no) %>%
  select(collection_no) %>%
  distinct() %>%
  .$collection_no
# Retain collections that contain colonial/photosymbiotic corals
corals <- corals %>%
  filter(collection_no %in% coll_no)

# Time binning ----------------------------------------------------------
# Bin corals
corals <- bin_time(occdf = corals, bins = bins, method = "point", rep = 1)[[1]]

# Environmental classification ------------------------------------------
# Get divDyn keys
data(keys)
# Categorise lithologies
corals$lith <- categorize(corals$lithology1, keys$lith)
# Corals (marl was categorized as unknown in divdyn)
corals$lith[corals$lithology1 == "marl"] <- "siliciclastic"
# Categorise depth
corals$bath <- categorize(corals$environment, keys$bath)
# classify photic zone
corals <- corals %>%
  # Which reefs are brown mesophotic?
  mutate(photic = case_when(
    # Siliciclastics indicator of turbid environments
    lith == "siliciclastic" & bath == "shallow" ~ "Brown mesophotic",
    # Above storm-water wave (approx. > 50 m depth)
    lith == "carbonate" & bath == "shallow" ~ "Euphotic",
    # Below storm-weather wave base (approx. > 50 m depth)
    lith == "carbonate" & bath == "deep" ~ "Blue mesophotic"
  ))
corals$photic[is.na(corals$photic)] <- "Unknown"
# Join bin data
corals <- corals %>%
  left_join(x = ., y = bins, by = c("bin_assignment" = "bin"))

# Get summary -----------------------------------------------------------
# Collections
counts <- corals %>%
  group_by(interval_name, photic) %>%
  count() %>%
  left_join(x = ., y = bins, by = "interval_name")
# Genus
genus_counts <- corals %>%
  group_by(interval_name, photic) %>%
  summarise(genus_counts = length(unique(genus))) %>%
  left_join(x = ., y = bins, by = "interval_name")

# Save data -------------------------------------------------------------
write.csv(counts, "data/collection_counts.csv", row.names = FALSE)
write.csv(genus_counts, "data/genus_counts.csv", row.names = FALSE)
