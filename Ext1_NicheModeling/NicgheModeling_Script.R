#############################################
####    EXTENSION 1 : Niche Modeling ########
#############################################

# Extension activity for UCSB EEMB 508: Intro to Ecology
# Created with R version:4.1.2 (2021-11-01) "Bird Hippie"
# run with Rstudio version: RStudio 2022.07.0+548 "Spotted Wakerobin" 

# Extension Goals:
# 1) Download species occurence data
# 2) Download climate data
# 3) Fit a 'Climate Envelope Model' or 'Species Niche Model'
#    to describe where that species occurs using climate variables
# 4) do cool and thought provoking stuff with it


########## **RESOURCES** ######################
# If you've never delt with R before: 
# - https://blog.uvm.edu/tdonovan-vtcfwru/r-for-fledglings/
# - https://www.neonscience.org/resources/learning-hub/tutorials/resources-learn-r

# A video tutorial doing all the same stuff with slightly different methods:
# https://youtu.be/1C1zVJO-Rk0

# A longer R-based tutorial that goes into things like model validation
# https://jcoliver.github.io/learn-r/011-species-distribution-models.html




###### STEP 1: Install/Load Packages ############

# skip this if you've already installed these packages
install.packages("dismo") # note: say "no" if it asks you to install things that need compiling
install.packages("maptools")
install.packages("rgdal")
install.packages("raster")
install.packages("sp")
install.packages("rgbif")
install.packages("tidyverse")

# 'load' installed packages so you have access to their functions
library(dismo)
#library(maptools)
library(rgdal)
library(raster)
library(sp)
library(rgbif)
library(tidyverse)

# note: you may get warning messages related to maptools and rgdal. just ignore them unless it says "ERROR"





###### STEP 2: Download (and clean) Species occurance data ############

# download occurence reccords from GBIF for blue oak, Quercus douglasii
occ_qudo <- occ_data(scientificName = "Quercus douglasii"
                     , hasCoordinate = T
                     , hasGeospatialIssue = F
                     , stateProvince = "California")
  # only grabbing occurances with lat-lon and no location issues, in CA (because anything outside of CA is probably planted)
qudo.raw <- occ_qudo$data # just select the data from that big list object
nrow(qudo.raw) # this many records were downloaded

## Cleand the data
# a REALLY critical step with occurence data
# note: we already defacto cleaned a lot of outliers/problems by limiting just to CA records, because we know CA is Q. douglasii's native range

# remove duplicated records
dups <- duplicated(qudo.raw %>% select(decimalLatitude, decimalLongitude))
sum(dups) # number of duplicate records (which would incorrectly weight their locations in the model)
qudo<- qudo.raw[!dups,] # remove the duplicates using the ! (NOT) boolian function

# make sure 



data("wrld_simpl") # download a simple world map for visualization

# plot our occurences
plot(wrld_simpl)
points(qudo$decimalLatitude~qudo$decimalLongitude)

# but this is REAL zoomed out. let's zoom in a little

# Determine geographic extent of our data
max.lat <- ceiling(max(qudo$decimalLatitude))
min.lat <- floor(min(qudo$decimalLatitude))
max.lon <- ceiling(max(qudo$decimalLongitude))
min.lon <- floor(min(qudo$decimalLongitude))
geographic.extent <- extent(x = c(min.lon, max.lon, min.lat, max.lat))


#________________________________________________
# Plot the world map but zoomed in
plot(wrld_simpl, 
     xlim = c(min.lon, max.lon),
     ylim = c(min.lat, max.lat),
     axes = TRUE, 
     col = "grey95")


# Add the points for individual observation
points(x = qudo$decimalLongitude, 
       y = qudo$decimalLatitude, 
       col = "olivedrab", 
       pch = 20, 
       cex = 0.75)
# And draw a little box around the graph
box()
#___________________________________________________






###### STEP 3: Load Climate Data ############
bioclim.data <- getData(name = "worldclim",
                        var = "bio",
                        res = 2.5,
                        path = "Ext1_NicheModeling/")
# this will make a folder titled "wc-2-5" in your "Ext1_NicheModeling" folder
