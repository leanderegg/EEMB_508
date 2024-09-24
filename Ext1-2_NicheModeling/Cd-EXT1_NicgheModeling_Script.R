#############################################
####    EXTENSION 1 : Niche Modeling ########
#############################################

# Extension activity for UCSB EEMB 508: Intro to Ecology
# Run with R version 4.3.1 (2023-06-16) -- "Beagle Scouts"
# Run with Rstudio version: RStudio 2024.04.1+748 

# Extension 1 Goals: Niches get stitches
# 1) Download species occurrence data
# 2) Download climate data
# 3) Explore a species' realized niche:
#     - to describe where that species occurs using climate variables and other things
#     - Fit a 'Climate Envelope Model' or 'Species Niche Model'
# 4) SCIENCE 

# Extension 2 Goals: Global Change and tree ecophys
# 1) Take our same Climate Envelope Model and project future range shifts
# 2) play with some ecophysiological data to inform our projections



########## **RESOURCES** ######################
# If you've never delt with R before: 
# - https://blog.uvm.edu/tdonovan-vtcfwru/r-for-fledglings/
# - https://www.neonscience.org/resources/learning-hub/tutorials/resources-learn-r

# A video tutorial doing all the same stuff with slightly different methods:
# https://youtu.be/1C1zVJO-Rk0

# A longer R-based tutorial that goes into things like model validation
# https://jcoliver.github.io/learn-r/011-species-distribution-models.html




###### Getting started: Install/Load Packages ############

# skip this if you've already installed these packages
  #spatial packages
install.packages("terra")
install.packages("dismo") # note: say "no" if it asks you to install things that need compiling
#install.packages("maptools")
#install.packages("rgdal")
install.packages("raster")
install.packages("sf")
install.packages("sp")
install.packages("tmap")
install.packages("tmaptools")
install.packages("geodata")
install.packages("rnaturalearth")
install.packages("rnaturalearthdata")
  # other packages
install.packages("rgbif") #for using GBIF API through R
install.packages("tidyverse") # data wrangling
install.packages("RColorBrewer") # nice color palettes


# 'load' installed packages so you have access to their functions

#library(maptools)
#library(rgdal)

library(sp)

library(tmap)
library(tmaptools)

library(dismo)
library(raster)
library(geodata)
library(rnaturalearth)

library(tidyverse)
library(rgbif)
library(RColorBrewer)
# note: you may get warning messages related to maptools and rgdal. just ignore them unless it says "ERROR"




#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
###### STEP 1: Download (and clean) Species occurrence data ############
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#### . download occurrence records from GBIF for blue oak, Quercus douglasii ####
  # The Global Biodiversity Information Facility (https://www.gbif.org/) is a very powerful source for species occurance data
  # It combines a bunch of data sources (herbaria/museum specimens, iNaturalist, etc.)
  # the {rgbif} package provides a really slick way to download data through the GBIF API 

# use the occ_data() call from {rgbif} to download data for blue oak (Quercus douglasii)
occ_qudo <- occ_data(scientificName = "Quercus douglasii"
                     , hasCoordinate = T
                     , hasGeospatialIssue = F
                     , stateProvince = "California"
                     , limit=10000)
  # only grabbing occurrences with lat-lon and no location issues, in CA (because anything outside of CA is probably planted)
qudo.raw <- occ_qudo$data # just select the data from that big list object
nrow(qudo.raw) # this many records were downloaded (3486 as of 9.26.23, 4412 as of 9.24.24)


# --NOT RUN (versioning): For good versioning/internal reproducibility, it's wise to save a versioned, local copy of the data
# write.csv(x = qudo.raw[,-grep("network", colnames(qudo.raw))], file = "Ext1-2_NicheModeling/data/GBIF_Quercusdouglasii_2024-09-24.csv")
  # note, with newest GBIF call, there are three columns $networkKeys.Length, $networkKeys.Class and $networkKeys.Mode that are lists of some sort
  # so we had to remove them in order to save as a .csv file
 
# --NOT RUN (if problems): If GBIF server is down, internet sucks, or computers are slow, load old data
# qudo.raw <- read.csv("Ext1-2_NicheModeling/data/GBIF_Quercusdouglasii_2024-09-24.csv", header=T)




#### . Clean the data ####
# a REALLY critical step with occurrence data. THERE'S A LOT OF BAD DATA OUT THERE.
# note: we already defacto cleaned a lot of outliers/problems by limiting just to CA records, because we know CA is Q. douglasii's native range

# remove duplicated records
dups <- duplicated(qudo.raw %>% select(decimalLatitude, decimalLongitude))
sum(dups) # number of duplicate records (which would incorrectly weight their locations in the model)
sum(dups)/nrow(qudo.raw)*100 # % of our dataset that were duplicates
qudo<- qudo.raw[!dups,] # remove the duplicates using the ! (NOT) boolian function




#### . Visualize the data #####
wrld_simpl <- ne_coastline(scale = 110, returnclass = "sv") # download a simple world map for visualization (frome the Naturalearth project)


# plot our occurrences
plot(wrld_simpl)
points(qudo$decimalLatitude~qudo$decimalLongitude)

# but this is REAL zoomed out. let's zoom in a little

# Determine geographic extent of our data (and padd a little)
max.lat <- ceiling(max(qudo$decimalLatitude) + 3)
min.lat <- floor(min(qudo$decimalLatitude) - 3)
max.lon <- ceiling(max(qudo$decimalLongitude) + 3)
min.lon <- floor(min(qudo$decimalLongitude) - 3)
geographic.extent <- raster::extent(x = c(min.lon, max.lon, min.lat, max.lat))
# Note: I'm calling functions specifically from packages using the [package]::[function] nomenclature, because spatial packages in R just went through a major revamp and shit got complicated

#________________________________________________
# Plot the world map but zoomed in
plot(wrld_simpl, 
     xlim = c(min.lon, max.lon),
     ylim = c(min.lat, max.lat),
     axes = TRUE, 
     col = "grey65")


# Add the points for individual observation
points(x = qudo$decimalLongitude, 
       y = qudo$decimalLatitude, 
       col = "olivedrab", 
       pch = 20, 
       cex = 0.75)
# And draw a little box around the graph
# box() # not needed with newer map data

#___________________________________________________





#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
###### STEP 2: Load Climate Data ############
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#### Download modern climate data ##

# we're going to download gridded global climate data (temperature and precip)
# from the 'WorldClim2' dataset, because it's easy. Other gridded climate data are
# probably better, depending on your application and location, but you can't beat Worldclim for ease of use.

bio_curr <- raster::stack(geodata::worldclim_global(var = "bio"
                             , res=2.5
                             , path="Ext1-2_NicheModeling/data/climate/"))
  

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

# make the names of each layer easier to handle
names(bio_curr_CA) # they're currently long and unwieldy
names(bio_curr_CA) <- str_replace(string = names(bio_curr_CA),pattern = "wc2.1_2.5m_", replacement = "")
  # This cuts out the wc2.... and relplaces it with nothing (""). could also replace with some new text




### --NOT RUN: in case server is down (has happened before)
# This exports the cropped bioclim to a geotiff so I can send it to y'all
#raster::writeRaster(bio_curr_CA,filename = "Ext1-2_NicheModeling/data/climate/Bioclim_clipped.tiff",filetype="GTiff", overwrite=T)

# read in the cropped raster brick from my exported geotiff:
#bio_curr_CA <- brick("Ext1-2_NicheModeling/data/climate/Bioclim_clipped.tiff")

# let's also make the names of our variables easier to handle
#names(bio_curr_CA) # exporting and importing changed the names so need to rename things
#names(bio_curr_CA) <- str_replace(names(bio_curr_CA),"Bioclim_clipped", "bio")




# quick take a look at the data
plot(bio_curr_CA) 
# a description of what each of these variables are can be found:
# https://www.worldclim.org/data/bioclim.html







#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
###### STEP 4: Assess the Climate Envelope ############
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



#### . Extract the climate data for all of our species occurrences: ####
qudo_clim <- raster::extract(bio_curr_CA, qudo[,c("decimalLongitude","decimalLatitude")])
  # note, we had to select Longitude and then Latitude, because R expects x, then y

## let's also extract a random set of points to characterize the available climate:
set.seed(42)# Set the seed for the random-number generator to ensure results are similar
# Randomly sample points (same number as our observed points)
background <- dismo::randomPoints(mask = bio_curr_CA[[1]],     # Provides extent and resolution for sampling points, just takes the first bioclim variable
                           n = nrow(qudo) * 2,      # Number of random points = n of our obs *2, just to make sure we're sampling climate space well
                           ext = geographic.extent) # geographic extent of sampling

# extract the climate variables for our random points
background_clim <- data.frame(extract(bio_curr_CA, background))
# quick visualize our random points
plot(bio_curr_CA[[1]])
points(background)
points(qudo$decimalLatitude~qudo$decimalLongitude, col="blue", pch=16)





#________________________________________________________________________
####### + Visual Assessment ######### 
#we'll just use the good old ocular detection device, v1 to start out with

# let's make a specific color for blue oak
blueoak <- brewer.pal("Set1",n=3)[2]
blueoak.transp <- paste0(blueoak,"44") # and a transparent version



#### .. plot blue oak's climate niche (mean T and mean P) ####
plot(bio_1~bio_12 # formula of y~x using the variables we want to plot
     , data = qudo_clim # data where those variables can be found
     , xlab = "Mean Annual Precip (mm)" # an informative x label (with units!)
     , ylab = "Mean Annual Temp (degrees C)" # an informative y label
     , pch =16 # point type (16 = filled circle)
     , col=blueoak.transp # set the color of our oaks to our bespoke transparent blue
     )



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#__________ Question: Where to the limits to blue oak seem to be in climate space? ________________

# [Think, Pair, Share]




#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



#### .. plot the background climate for Mean T and Mean P ####
plot(bio_1~bio_12 # formula of the variables we're plotting
     , data=background_clim # data where to find those variables
     , xlab="Mean Annual Precip (mm)" # x axis label
     , ylab="Mean Annual Temp (degrees C)" # y axis label
     , pch=16 # point type (16=filled circle)
     , col="#66666622") # point color, in a hexidecimal notation, which is #RRGGBB and then 2 digits for transparency so we don't overplot
points(bio_1~bio_12
       , data=qudo_clim
       , pch=16
       , col=blueoak.transp)


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#__________ Question: Does this change your answer to the question above? ________________
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



#### .. choose 2 climate variables to plot ####
v1 <- "bio_6" # the variable you want
v1name <- "Min T of coldest Month" # it's actual meaning (for the axis)
v2 <- "bio_12"
v2name <- "precip"

plot(get(v1)~get(v2) # formula of the variables we're plotting
     , data=background_clim # data where to find those variables
     , xlab=v2name # x axis label
     , ylab=v1name # y axis label
     , pch=16 # point type (16=filled circle)
     , col="#66666644") # point color, in a hexidecimal notation, which is #RRGGBB and then 2 digits for transparency so we don't overplot
points(get(v1)~get(v2)
       , data=qudo_clim
       , pch=16
       , col=blueoak.transp)






#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
####### Q1: Limits to the niche ############
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# **What are some hypotheses about the climatic factors that limit blue oaks realized niche?**









#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
####### + Second, fit a bona fide CEM ######### 
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# 
# we're going to use a very easy, out-of-the-box climate envelope fitting technique.


### . Build a species distribution model ####
qudo_clim <- extract(bio_curr_CA, qudo[,c("decimalLongitude","decimalLatitude")])


# fit the model (details about how this model is constructed can be found with ?bioclim)
bc.model <- bioclim(x = bio_curr_CA, qudo %>% dplyr::select(decimalLongitude, decimalLatitude))
  # note, we're using the select() command with %>% 'piping' from the 'tidyverse' data wrangling set of packages
  # alternative way to do this woud be:
# bc.model <- bioclim(x = bio_curr_CA, qudo[,c("decimalLongitude", "decimalLatitude")])


### . Predict suitable habitat in the domain ####
qudo_pred <- dismo::predict(object=bc.model
                            , x=bio_curr_CA
                            , ext= geographic.extent
                            )
  # Note: the dismo::predict bit is to tell R to use the predict() function from the dismo package. Cause there are many predict() functions from many different packages, and whatever package was loaded last usually trumps all the others


### . Plot our model predictions ####
plot(wrld_simpl, 
     xlim = c(min.lon, max.lon),
     ylim = c(min.lat, max.lat),
     axes = TRUE, 
     col = "grey95")
# Add model probabilities
plot(qudo_pred, add = TRUE)

# Add actual occurances
points(decimalLatitude~decimalLongitude, qudo
       , pch=".")


#______________________________________________________
### INTERACTIVE PLOTTING!
# even better than base plot, we can use tmap to interactively zoom in and out!

###  use tmap to make interactive plots
tmap_mode("view") # if we set this to "view" rather than "plot", we can make pretty maps that we can zoom around

# make a figure of blue oak distribution on Mean Annual Temp background
qudofig <- tm_shape(bio_curr_CA[[1]])+
  tm_raster(style= "pretty",
            title="MAT")+
  tm_layout(legend.outside = T) +
  tm_shape(SpatialPoints(qudo %>% dplyr::select(decimalLongitude, decimalLatitude))) + 
  tm_dots()

qudofig # plot the figure

# make a figure of blue oak on our predicted suitability
predsfig <- tm_shape(qudo_pred)+
  tm_raster(style= "pretty",
            title="Suitable Habitat")+
  tm_layout(legend.outside = T) +
  tm_shape(SpatialPoints(qudo %>% dplyr::select(decimalLongitude, decimalLatitude))) + 
  tm_dots(col = blueoak)

predsfig # plot the figure




#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
########### Q: Why might blue oak not fill all of it's 'suitable habitat'? #########
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++













#___________________________________________________________________________________
### . Bringing in another species

# download occurrence records from GBIF for black oak, Quercus kelloggii
occ_quke <- occ_data(scientificName = "Quercus kelloggii"
                     , hasCoordinate = T
                     , hasGeospatialIssue = F
                     , stateProvince = "California"
                     ,limit =10000 )
# only grabbing occurances with lat-lon and no location issues, in CA (because anything outside of CA is probably planted)
quke.raw <- occ_quke$data # just select the data from that big list object
# remove duplicated records
dups <- duplicated(quke.raw %>% select(decimalLatitude, decimalLongitude))
sum(dups) # number of duplicate records (which would incorrectly weight their locations in the model)
quke<- quke.raw[!dups,] # remove the duplicates using the ! (NOT) boolian function
## Extract the climate data for all of our species occurances:
quke_clim <- extract(bio_curr_CA, quke[,c("decimalLongitude","decimalLatitude")])

# let's make a specific color for black oak
blackoak <- brewer.pal("Set1",n=3)[3]
blackoak.transp <- paste0(blackoak,"11") # and a transparent version




## Now let's look at them in geographic space
plot(bio_curr_CA[[1]])
points(decimalLatitude~decimalLongitude
       , data=qudo
       , pch=16
       , col=blueoak.transp)
points(decimalLatitude~decimalLongitude
       , data=quke
       , pch=16
       , col=blackoak.transp)




## And now in climate space
# plot the background climate for Mean T and Mean P
plot(bio_1~bio_12 # formula of the variables we're plotting
     , data=background_clim # data where to find those variables
     , xlab="Mean Annual Precip (mm)" # x axis label
     , ylab="Mean Annual Temp (degrees C)" # y axis label
     , pch=16 # point type (16=filled circle)
     , col="#66666622") # point color, in a hexidecimal notation, which is #RRGGBB and then 2 digits for transparency so we don't overplot
points(bio_1~bio_12
       , data=qudo_clim
       , pch=16
       , col=blueoak.transp)
points(bio_1~bio_12
       , data=quke_clim
       , pch=16
       , col=blackoak.transp)
legend("topright"
       , legend=c("climate space","Q. douglasii","Q. kellogii")
       , pch=16
       , col=c("#666666", blueoak, blackoak))



























####### OLD CODE: for downloading previous climate versions using old packages

### Old CMIP5 and Worldclim v1 code:
# 
# # https://www.carbonbrief.org/cmip6-the-next-generation-of-climate-models-explained/
# bio_fut <- getData('CMIP5', var='bio', res=2.5, rcp=60, model='NO', year=50, path="Ext1_NicheModeling/")
# # this is the same getData call, but now for 'Climate Model Intercomparison Project 5' data, which
# # is a giant model intercomparison that folks run every few years for the IPCC in order to generate ensemble (i.e. averaged over a bunch of models)
# # climate projections using the state-of-the-science climate models and standardized 'forcing' scenarios (e.g. GHG emissions scenarios)
# # https://www.worldclim.org/data/v1.4/cmip5.html
# # NOTE: we're using CMIP5, because it's easy to download. But CMIP6 is now the normal version that everyone uses.
# 
# # we're getting out the 'bioclim' variables, at 2.5 minute resolution.
# # we're getting model output from the RCP 6.0 pathway ("Representative Concentration Pathway), which is a mid-to-high end emissions scenario (2.6 is low, 8.5 is 'business as usual')
# 
# bio_curr <- getData(name = "worldclim",
#                         var = "bio",
#                         res = 2.5,
#                         path = "Ext1_NicheModeling/")
# # We’re giving getData four critical pieces of information:
# #  name = "worldclim": This indicates the name of the data set we would like to download
# #  var = "bio": This tells getData that we want to download all 19 of the bioclimatic variables, rather than individual temperature or precipitation measurements
# #  res = 2.5: This is the resolution of the data we want to download; in this case, it is 2.5 minutes of a degree. For other resolutions, you can check the documentation by typing ?getData into the console.
# #  path = "Ext1_NicheModeling/": Finally, this sets the location to which the files are downloaded. In our case, it is the data folder we created at the beginning.
# # the first time you run this, it will make a folder titled "wc2.1_2.5" in your "Ext1_NicheModeling" folder
# # every subsequent time, it will just load the data from that folder
# 
