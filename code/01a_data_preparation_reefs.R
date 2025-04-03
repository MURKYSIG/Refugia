library(here)
library(divDyn)
library(dplyr)
library(shades)
library(deeptime)

pared <- read.csv(here("data", "pared_nov2024.csv"))

pared$age <- (pared$min_ma + pared$max_ma)/2 # mean age

# Load file for bin assignment
ints <- read.csv(here("data", "l_intervall_2024.csv"))

# merge
dat <- merge(pared, ints, by.x="intervall", by.y="Interval", all.x=TRUE)

stg <- numeric(nrow(dat))
for (i in 1:nrow(dat)) {
  stg[i] <- sample(c(dat$r.stage[i], dat$r.stage2[i]),1)
} 

dat <- cbind(dat, stg)

stages <- read.csv(here("data", "gts2020.csv")) # Gradstein et al. (2020) time scale

data(keys)

comb <- merge(dat, stages, by = "stg")


# Data cleaning

comb <- subset(comb, type!=3) # exclude mud mounds
comb <- subset(comb, stg > 13) # restrict ages

# Exclude non-tropical reef types, subsurface reefs
comb <- subset(comb, tropical!=2  & subsurface != 1)

comb <- subset(comb, biota_main %in% 1 | biota_sec %in% 1) # corals as main and secondary biota

# Number of reefs at each stage

n.reefs <- table(comb$stg)

n.reefs <- as.data.frame(n.reefs)

colnames(n.reefs) <- c("stg", "n")

# Create a data frame with stg 52 and 53 and n = 0 to indicate the reef gap in early Triassic
new_rows <- data.frame(stg = as.factor(c(46, 52, 53)), n = 0)


# Combine the existing data frame with the new rows
n.reefs <- rbind(n.reefs, new_rows)

# Order the data frame by stg
n.reefs <- n.reefs[order(as.numeric(n.reefs$stg)), ]


n.reefs <- merge(n.reefs, stages) # number of all reefs

#### Euphotic vs. mesophotic ####

tur <- subset(comb, subenviron == "1d") # get turbid reefs

n.tur <- table(tur$stg)

n.tur <- as.data.frame(n.tur)

colnames(n.tur) <- c("stg", "n")

all_bins <- data.frame(stg = 17:94) # for missing stages

n.tur <- merge(all_bins, n.tur, by = "stg", all.x = TRUE) # merge missing stages

n.tur$n[is.na(n.tur$n)] <- 0

n.tur <- merge(n.tur, stages)

# Euphotic and blue mesophotic

comb$meso <- with(comb, ifelse(bathymetry > 1, "blue", "euphotic"))

comb <- subset(comb, !(subenviron == "1d" & meso == "euphotic")) # exclude turbid reefs from euphotic (clear water)

#comb <- subset(comb, !(subenviron == "1d" & meso == "blue")) # exclude turbid reefs from blue mesophotic

#comb <- comb[!is.na(comb$meso),]

n.eup <- subset(comb, meso == "euphotic")

n.eup <- as.data.frame(table(n.eup$stg))

colnames(n.eup) <- c("stg", "n")

n.eup <- merge(all_bins, n.eup, by = "stg", all.x = TRUE) # merge with missing stages

n.eup$n[is.na(n.eup$n)] <- 0

n.eup <- merge(n.eup, stages)


## Blue mesophotic

n.blue <- subset(comb, meso == "blue")

n.blue <- as.data.frame(table(n.blue$stg))

colnames(n.blue) <- c("stg", "n")

n.blue <- merge(all_bins, n.blue, by = "stg", all.x = TRUE) # merge with missing stages


n.blue$n[is.na(n.blue$n)] <- 0

n.blue <- merge(n.blue, stages)

### Calculate proportion

tur$meso <- "brown"

all_env <- rbind(tur, comb)

all_env$meso[is.na(all_env$meso)] <- "unknown"

all_env <- merge(all_bins, all_env, by = "stg", all.x = TRUE) # merge with missing stages

all_env$meso[is.na(all_env$meso)] <- "none"

all.prop <- all_env[, c("stg", "meso")]

all.prop <- merge(all.prop, stages, by.x = "stg", by.y = "stg")

ap <- table(all.prop$meso, all.prop$stg)


app <- as.data.frame(prop.table(ap, margin = 2))


colnames(app) <- c("meso","stg", "prop")

app <- merge(app, stages, by.x = "stg", by.y ="stg")


### save files ----------------

save(stages, n.reefs, n.tur, n.eup, n.blue, all.prop, app,
     file = here("data", "Reefs.RData"))

# n.reefs = number of all reefs
# n.tur = number of brown reefs
# n.eup = number of euphotic reefs
# n.blue = number of blue reefs
# all.prop = data for divdyn "ugly proportion plot"
# app = proportion data for ggplot