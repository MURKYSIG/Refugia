# Load packages

library(here)
library(divDyn)
library(chronosphere)

### load data

load(file = here("data", "ordStrat.RData")) # for Ordovician stages

pbdb <- read.csv(here("data", "pbdb_Anthozoa_April2025.csv")) # Dataset contains only class Anthozoa. Occurrences excluding uncertain genera and species. Output:coordinates, classification, stratigraphy ext., lithology, genus, collection, paleoenvironment

need <- c("occurrence_no", "collection_no", "accepted_name", 
          "accepted_rank", "early_interval", "late_interval", "max_ma", "min_ma", 
          "phylum", "class", "order", "family", "genus","lng" , "lat", "lithology1", "environment", "zone", "formation")

cor <- pbdb [, need] # make a new df with needed columns


cor <- cor[(cor$accepted_rank == "genus"| cor$accepted_rank == "species"), ] 

cor <- cor[cor$genus!="", ]

cor <- unique(cor) #omit duplicate data


### Binning at stage level ---------------


data(keys)
names(keys)

# Load timescale by Gradstein 2020

stages <- read.csv(here("data", "gts2020.csv")) # Gradstein et al. (2020) time scale

# the 'stg' entries (lookup) 

stgMin <- categorize(cor[ ,"early_interval"], keys$stgInt) 
stgMax <- categorize(cor[ ,"late_interval"], keys$stgInt)


# convert to numeric 

stgMin <- as.numeric(stgMin) 
stgMax <- as.numeric(stgMax)

# empty container 

cor$stg <- rep(NA, nrow(cor))

# select entries, where 

stgCondition <- c( 
  
  # the early and late interval fields indicate the same stg 
  
  which(stgMax==stgMin),
  
  # or the late_interval field is empty 
  
  which(stgMax==-1))

# in these entries, use the stg indicated by the early_interval 

cor$stg[stgCondition] <- stgMin[stgCondition]
cor <- cor[!is.na(cor$stg),]

# Script to assign Ordovician collections to 'stg' stages based on tables format, max.int, and zones
# last checked with data of 2018-08-31 - Adam Kocsis

# Transform to collections
new <- unique(cor[is.na(cor$stg), c(
  "collection_no", 
  "early_interval", 
  "late_interval", 
  "zone", 
  "formation",
  "max_ma",
  "min_ma",
  "stg")])

# check always

# Looping formation (added condition that period is same)
for (i in 1:nrow(format))  {
  ix <- which((as.character(new$formation) == as.character(format$formation[i])))
  new$stg[ix] <- format$stg[i]
}

#	x <- table(new$stg) # control
#   sum(x)

# Looping early_intervals 
for (i in 1:nrow(max.int))  {
  ix <- which(as.character(new$early_interval) == as.character(max.int$Max.int[i]))
  new$stg[ix] <- max.int$stg.1[i]
}

#  Looping late intervals (to check if different)
stg2 <- rep(NA, nrow(new))
for (i in 1:nrow(max.int))  {
  ix <- which(as.character(new$late_interval) == as.character(max.int$Max.int[i]))
  stg2[ix] <- max.int$stg.1[i]
}

ix <- which(new$stg<stg2) # should ignore NAs in second column
new$stg[ix] <- NA


#   x <- table(new$stg) # control
#   sum(x)


# Looping zones
for (i in 1:nrow(zones))  {
  ix <- which(as.character(new$zone) == as.character(zones$zone[i]))
  new$stg[ix] <- zones$stg[i]
}


#	x <- table(new$stg) # control
#	sum(x)
#	View(new[new$min_ma>400,])

# only that part, which has stg assignments now
new2 <- new[!is.na(new$stg), ]

# vector: stg numbers, names:collection numbers
ord <- new2$stg
names(ord) <- new2$collection_no

# the collection identifiers of occurrences in the total dataset
colls <- as.character(cor$collection_no)

# which are present in the newly gathered data?
bool <- colls%in%names(ord)

# collection identifiers of the occurrences of only these collections
subColls <- colls[bool]

# order/assign the stg accordingly
subStg<-ord[subColls]

# copy original
newStg <- cor$stg

# replace the missing entries
newStg[bool]  <- subStg

# make sure things are OK
#	origTab <- table(dat$stg)
#	newTab <- table(newStg)
#	newStg-origTab # should be all positive!!!

# add to the full table
cor$stg <- newStg



# Tabulate

tabu <- subset(cor, order %in% c("Auloporida", "Favositida", "Heliolitida", "Lichenariida", 
                                "Sarcinulida", "Tetradiida"))

# Rugosa

# Rugose coral orders

rug.ord <- subset(cor, order %in% c("Cystiphyllida", "Stauriida")) # all pbdb rugosa  

rug <- read.csv(here("data", "pbdb_rugose_coloniality.csv")) # treatise rugosa with assigned coloniality

rug_new <- read.csv2(here("data", "Rugose_missing coloniality.csv"))

rug2 <- merge(rug, rug_new, 
              by.x = c("accepted_genus", "coloniality"), 
              by.y = c("X.genus", "coloniality"), all = TRUE)

rug.pb <- merge(rug.ord, unique(rug2 [, c("accepted_genus", "coloniality")]), 
                by.x = "genus", by.y = "accepted_genus", all.x = TRUE)

# change NA in coloniality to unknown

#rug.pb$coloniality[is.na(rug.pb$coloniality)] <- "unknown"

# subset coloniality by unknown

#rug.pb$coloniality[which(rug.pb$coloniality == "")] <- "unknown"

rug.col <- subset(rug.pb, coloniality %in% c("colonial", "both"))

rug.col <- rug.col[,!(names(rug.col) %in% "coloniality")]

# Scleractinian corals

traits <- fetch(src = "SOM-kiessling-coralgenera", ser ="list")

# assign the colonial/solitary status

scler <- merge(cor, unique(traits[, c("genus.proper", "GROWTH",
                                      "ECOLOGY", "MORPH","freeLiving")]), 
               by.x="genus", by.y="genus.proper")

scler$GROWTH[which(scler$genus == "Lithophyllon")] <- "solitary" #corrected mistake in db


scler <- subset(scler, GROWTH == "colonial")

scler <- subset(scler, ECOLOGY == "z")

scler <- scler[,!(names(scler) %in% c("GROWTH", "ECOLOGY", "MORPH", "freeLiving"))] # remove extra columns

scler <- subset(scler, order == "Scleractinia")


# All colonial corals 

anth <- rbind(scler, rug.col) # bind colonial scleractinian and colonial rugose corals

anth <- rbind(anth, tabu) # bind previous with tabulate corals


### Categorize by the environment

#lithology

anth$lith <- categorize(anth$lithology1,keys$lith)

anth$lith[anth$lithology1 == "marl" & anth$lith == "unknown"] <- "siliciclastic" # marl was categorized as unknown in divdyn categorization


# Get number of brown mesophotic collections

tur <- subset(anth, lith == "siliciclastic") # get turbid occurrences (siliciclastic)

col.tur <- unique(tur[, c("collection_no", "lith", "stg")]) # get siliciclastic collections only

col.tur <- as.data.frame(table(col.tur$stg))

colnames(col.tur) <- c("stg", "n_col") # rename columns

all_bins <- data.frame(stg = 17:94) # for missing stages

col.tur <- merge(all_bins, col.tur, by = "stg", all.x = TRUE) # merge missing stages

col.tur$n_col[is.na(col.tur$n_col)] <- 0

col.tur <- merge(col.tur, stages)


#bathymetry

anth$bath <- categorize(anth$environment, keys$bath)


# Euphotic, blue and brown mesophotic

anth$meso <- "unknown" # mark all as unknown, filter below

anth$meso[anth$bath == "deep"]<- "blue" # classify as blue

anth$meso[anth$bath == "shallow" & anth$lith == "carbonate"] <- "euphotic" # classify as euphotic

anth$meso[anth$lith == "siliciclastic"] <- "brown" # classify as brown

collections_env <- unique(anth[, c("collection_no", "meso", "stg")])

# Euphotic collections

col.eup <- subset(collections_env, meso == "euphotic")

col.eup <- as.data.frame(table(col.eup$stg))

colnames(col.eup) <- c("stg", "n_col")

col.eup <- merge(all_bins, col.eup, by = "stg", all.x = TRUE) # merge with missing stages

col.eup$n_col[is.na(col.eup$n_col)] <- 0

col.eup <- merge(col.eup, stages)


# Blue mesophotic collections

col.blue <- subset(collections_env, meso == "blue")

col.blue <- as.data.frame(table(col.blue$stg))

colnames(col.blue) <- c("stg", "n_col")

col.blue <- merge(all_bins, col.blue, by = "stg", all.x = TRUE) # merge with missing stages


col.blue$n_col[is.na(col.blue$n_col)] <- 0

col.blue <- merge(col.blue, stages)

## Unknown environment

unk <- subset(collections_env, meso == "unknown")

unk <- as.data.frame(table(unk$stg))

colnames(unk) <- c("stg", "n_col")

unk <- merge(all_bins, unk, by = "stg", all.x = TRUE) # merge with missing stages


unk$n_col[is.na(unk$n_col)] <- 0

unk <- merge(unk, stages)


## Calculate diversity dynamics

#All colonial corals

dd <- divDyn(anth, bin = "stg", tax = "genus")

dd <- merge(stages, dd, by = "stg")


#Brown

brown <- subset(anth, meso == "brown")
brown.dd <- divDyn(brown, bin = "stg", tax = "genus")
brown.dd <- merge(stages, brown.dd, by = "stg")

#Euphotic

euphotic <- subset(anth, meso == "euphotic")
euph.dd <- divDyn(euphotic, bin = "stg", tax = "genus")
euph.dd <- merge(stages, euph.dd, by = "stg")


#Blue

blue <- subset(anth, meso == "blue")
blue.dd <- divDyn(blue, bin = "stg", tax = "genus")
blue.dd <- merge(stages, blue.dd, by = "stg")

#Unknown

unknown <- subset(anth, meso == "unknown")
unk.dd <- divDyn(unknown, bin = "stg", tax = "genus")
unk.dd <- merge(stages, unk.dd, by = "stg")




### save files ----------------

save(stages, anth, col.tur, col.eup, col.blue, unk, dd, brown.dd, euph.dd, blue.dd, unk.dd
     file = here("data", "Corals.RData"))

# stages = divdyn stages
# anth = colonial coral occurrences
# col.tur = turbid collections
# col.eup = euphotic collections
# col.blue = blue collections
# unk = unknown environment collections
# dd = diversity calculations of all colonial coral genera
# brown.dd = diversity of colonial coral genera in brown environments
# euph.dd = diversity of colonial coral genera in euphotic environments
# blue.dd = diversity of colonial coral genera in blue environments
# unk.dd = diversity of colonial coral genera in unknown environments