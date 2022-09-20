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
install.packages("geodata")

# 'load' installed packages so you have access to their functions
library(dismo)
#library(maptools)
library(rgdal)
library(raster)
library(sp)
library(rgbif)
library(tidyverse)
library(geodata)
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

## Clean the data
# a REALLY critical step with occurence data
# note: we already defacto cleaned a lot of outliers/problems by limiting just to CA records, because we know CA is Q. douglasii's native range

# remove duplicated records
dups <- duplicated(qudo.raw %>% select(decimalLatitude, decimalLongitude))
sum(dups) # number of duplicate records (which would incorrectly weight their locations in the model)
qudo<- qudo.raw[!dups,] # remove the duplicates using the ! (NOT) boolian function




## Visualize the data
data("wrld_simpl") # download a simple world map for visualization

# plot our occurences
plot(wrld_simpl)
points(qudo$decimalLatitude~qudo$decimalLongitude)

# but this is REAL zoomed out. let's zoom in a little

# Determine geographic extent of our data (and padd a little)
max.lat <- ceiling(max(qudo$decimalLatitude) + 3)
min.lat <- floor(min(qudo$decimalLatitude) - 3)
max.lon <- ceiling(max(qudo$decimalLongitude) + 3)
min.lon <- floor(min(qudo$decimalLongitude) - 3)
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


##++++++ Download modern climate data

# we're going to download gridded global climate data (temperature and precip)
# from the 'WorldClim2' dataset, because it's easy. Other gridded climate data are
# probably better, depending on your application and location, but you can't beat Worldclim for ease of use.

bio_curr <- worldclim_global(var = "bio"
                             , res=2.5
                             , path="Ext1_NicheModeling/")
# We're downloading "WorldClim" global data, with three arguements:
# - var = "bio" tells R that we want the 16 "bioclim" variables, which are temp, precip, and many combos thereof
#         you can see what these variables actually mean here: https://www.worldclim.org/data/bioclim.html
# - res = 2.5 says we want 2.5 minutes of a degree resolution data
# - path = "Ext1_..." tells R where to save the file we're downloading.

#NOTE: the first time you run this, it will make a folder titled "wc2.1_2.5" in your "Ext1_NicheModeling" folder
# every subsequent time, it will just load the data from that folder



### Crop climate data 
# to the extent of our blue oak records for ease of handling
bio_curr_CA <- crop(bio_curr, geographic.extent)

# quick take a look at the data
plot(bio_curr_CA) 
# a description of what each of these variables are can be found:
# https://www.worldclim.org/data/bioclim.html



## +++ Download future climate scenario

# We're downloading climate model simulations from the "CMIP6" archive.
# or the "Climate Model Intercomparison Project 6"
# https://www.carbonbrief.org/cmip6-the-next-generation-of-climate-models-explained/
# is a giant model intercomparison that folks run every few years for the IPCC in order to generate ensemble (i.e. averaged over a bunch of models)
# climate projections using the state-of-the-science climate models and standardized 'forcing' scenarios (e.g. GHG emissions scenarios)


bio_fut <- cmip6_world(model = "GFDL-ESM4"
                       , res=2.5
                       , var= "bioc"
                       , ssp = 370
                       , time = "2061-2080"
                       , path = "Ext1_NicheModeling/")

# specifically, we're downloading:
# - model output from only one of the many climate models, the NOAA earth system model run by the Geophysical Fluid Dynamics Laboratory
# - same 2.5 minute res as our current climate data
# - 'bioclim' variables similar to our current data
# - ssp = the Shared Socioeconomic Pathways, or 'forcing' scenarios that are run on all models to achieve the same climate forcing by 2100
#       we're using SSP3-7.0, which is a medium-high emission scenario, but not unmitigated disaster
# - time = average climate from 2061-2080
# - path = our same file to save stuff in

# also crop the future climate to CA roughly.
bio_fut_CA <- crop(bio_fut, geographic.extent)

plot(bio_fut_CA)






###### STEP 3: Load Climate Data ############











### Old CMIP5 and Worldclim code:

# https://www.carbonbrief.org/cmip6-the-next-generation-of-climate-models-explained/
bio_fut <- getData('CMIP5', var='bio', res=2.5, rcp=60, model='NO', year=50, path="Ext1_NicheModeling/")
# this is the same getData call, but now for 'Climate Model Intercomparison Project 5' data, which
# is a giant model intercomparison that folks run every few years for the IPCC in order to generate ensemble (i.e. averaged over a bunch of models)
# climate projections using the state-of-the-science climate models and standardized 'forcing' scenarios (e.g. GHG emissions scenarios)
# https://www.worldclim.org/data/v1.4/cmip5.html
# NOTE: we're using CMIP5, because it's easy to download. But CMIP6 is now the normal version that everyone uses.

# we're getting out the 'bioclim' variables, at 2.5 minute resolution.
# we're getting model output from the RCP 6.0 pathway ("Representative Concentration Pathway), which is a mid-to-high end emissions scenario (2.6 is low, 8.5 is 'business as usual')

# bio_curr <- getData(name = "worldclim",
#                         var = "bio",
#                         res = 2.5,
#                         path = "Ext1_NicheModeling/")
# We’re giving getData four critical pieces of information:
#  name = "worldclim": This indicates the name of the data set we would like to download
#  var = "bio": This tells getData that we want to download all 19 of the bioclimatic variables, rather than individual temperature or precipitation measurements
#  res = 2.5: This is the resolution of the data we want to download; in this case, it is 2.5 minutes of a degree. For other resolutions, you can check the documentation by typing ?getData into the console.
#  path = "Ext1_NicheModeling/": Finally, this sets the location to which the files are downloaded. In our case, it is the data folder we created at the beginning.
# the first time you run this, it will make a folder titled "wc2.1_2.5" in your "Ext1_NicheModeling" folder
# every subsequent time, it will just load the data from that folder

