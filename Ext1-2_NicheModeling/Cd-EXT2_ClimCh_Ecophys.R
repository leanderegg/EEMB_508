#############################################
####    EXTENSION 2 : Climate Change and Ecophysiology ########
#############################################

# Extension activity for UCSB EEMB 508: Intro to Ecology
# Created with R version:4.1.2 (2021-11-01) "Bird Hippie"
# run with Rstudio version: RStudio 2022.07.0+548 "Spotted Wakerobin" 


# Extension 2 Goals: Global Change and tree ecophys
# 1) Take our same Climate Envelope Model and project future range shifts
# 2) play with some ecophysiological data to inform our projections
# 3) SCIENCE!



###### Getting started: Install/Load Packages ############

# # skip this if you've already installed these packages
# install.packages("dismo") # note: say "no" if it asks you to install things that need compiling
# install.packages("maptools")
# install.packages("rgdal")
# install.packages("raster")
# install.packages("sp")
# install.packages("rgbif")
# install.packages("tidyverse")
# install.packages("geodata")
# install.packages("RColorBrewer")
# install.packages("tmap")
# install.packages("tmaptools")
  # I commented these out because you all should have them installed already

# 'load' installed packages so you have access to their functions
library(dismo)
library(maptools)
library(rgdal)
library(raster)
library(sp)
library(rgbif)
library(tidyverse)
library(geodata)
library(RColorBrewer)
library(tmap)
library(tmaptools)
# note: you may get warning messages related to maptools and rgdal. just ignore them unless it says "ERROR"






#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
###### STEP 1: Repeat everything we did in Ext 1 ############
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# quick repeat of all the stuff from Ext 1, with streamlined code
# NOTE: another approach you could take for this is to export all of your data from EXT1 at the end of that script
#       and import the specific products at the beginning of this script. But then you have to deal with version control
#       and our process just isn't that time consuming. So we'll just repeat it.


#### . Download occurrence records from GBIF for blue oak, Quercus douglasii ####
occ_qudo <- occ_data(scientificName = "Quercus douglasii"
                     , hasCoordinate = T
                     , hasGeospatialIssue = F
                     , stateProvince = "California"
                     , limit=10000)
# only grabbing occurrences with lat-lon and no location issues, in CA (because anything outside of CA is probably planted)
qudo.raw <- occ_qudo$data # just select the data from that big list object
nrow(qudo.raw) # this many records were downloaded

## Clean the data
# a REALLY critical step with occurrence data
# note: we already defacto cleaned a lot of outliers/problems by limiting just to CA records, because we know CA is Q. douglasii's native range

# remove duplicated records
dups <- duplicated(qudo.raw %>% select(decimalLatitude, decimalLongitude))
sum(dups) # number of duplicate records (which would incorrectly weight their locations in the model)
qudo<- qudo.raw[!dups,] # remove the duplicates using the ! (NOT) boolian function


# download world outlines #
data("wrld_simpl") # download a simple world map for visualization

# Determine geographic extent of our data (and pad a little)
max.lat <- ceiling(max(qudo$decimalLatitude) + 3)
min.lat <- floor(min(qudo$decimalLatitude) - 3)
max.lon <- ceiling(max(qudo$decimalLongitude) + 3)
min.lon <- floor(min(qudo$decimalLongitude) - 3)
geographic.extent <- extent(x = c(min.lon, max.lon, min.lat, max.lat))


#### . Load modern climate data #####

# we're going to download gridded global climate data (temperature and precip)
# from the 'WorldClim2' dataset, because it's easy. Other gridded climate data are
# probably better, depending on your application and location, but you can't beat Worldclim for ease of use.

bio_curr <- raster::stack(worldclim_global(var = "bio"
                                           , res=2.5
                                           , path="Ext1_NicheModeling/"))
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
# let's also make the names of our variables easier to handle
names(bio_curr_CA)
names(bio_curr_CA) <- str_replace(names(bio_curr_CA),"wc2.1_2.5m_","")

# a description of what each of these variables are can be found:
# https://www.worldclim.org/data/bioclim.html




######## . Download future climate scenario ##########

# We're downloading climate model simulations from the "CMIP6" archive.
# or the "Climate Model Intercomparison Project 6"
# https://www.carbonbrief.org/cmip6-the-next-generation-of-climate-models-explained/
# is a giant model intercomparison that folks run every few years for the IPCC in order to generate ensemble (i.e. averaged over a bunch of models)
# climate projections using the state-of-the-science climate models and standardized 'forcing' scenarios (e.g. GHG emissions scenarios)


bio_fut <- raster::stack( cmip6_world(model = "GFDL-ESM4"
                       , res=2.5
                       , var= "bioc"
                       , ssp = 370
                       , time = "2061-2080"
                       , path = "Ext1_NicheModeling/"))

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
  # note: I turned this into a "raster brick" just because it was originally in a format that doesn't play well with many of our functions

# take a quick look at the climate projections
plot(bio_fut_CA)

# note: the variables are named differently in our two different climate raster stacks
# that will be trouble if we leave it as is.
names(bio_fut_CA)
names(bio_curr_CA)

# two ways of fixing this:
# 1) since they have the same dimensions, we just use the _curr names for _fut
names(bio_fut_CA) <- names(bio_curr_CA)

# 2) we replace the "wc2" string with "bio" string
names(bio_fut_CA) <- str_replace(string=names(bio_fut_CA),pattern = "wc2", replacement = "bio") 
  # note: this will run and just not do anything even if you've already fixed the names with option 1)

# double check that this worked.
all.equal(names(bio_fut_CA), names(bio_curr_CA))
  # yay!


#### . Look at how temps change in future #######

# you can do 'raster math' just like normal math. as long as the rasters are in the same resolution and domain, things work per cell
tchange <- bio_fut_CA[[1]] - bio_curr_CA[[1]]

pchange <- bio_fut_CA[[12]] - bio_curr_CA[[12]]

plot(tchange)
  # awww shit. Things get HOT in this scenario in this model...
plot(pchange)
  # double shit. 

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
############## Q1: What do you notice about the spatial pattern of MAT & MAP change? ###############
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++




#### . Extract the climate data for all of our species occurrences: ####
qudo_clim <- extract(bio_curr_CA, qudo[,c("decimalLongitude","decimalLatitude")])
# note, we had to select Longitude and then Latitude, because R expects x, then y



#### . Extract random background points ####
set.seed(42)# Set the seed for the random-number generator to ensure results are similar
# Randomly sample points (same number as our observed points)
background <- randomPoints(mask = bio_curr_CA[[1]],     # Provides extent and resolution for sampling points, just takes the first bioclim variable
                           n = nrow(qudo) * 2,      # Number of random points = n of our obs *2, just to make sure we're sampling climate space well
                           ext = geographic.extent) # geographic extent of sampling

# extract the current climate variables for our random points
background_clim <- data.frame(extract(bio_curr_CA, background))

# extract the future climate variables for our background points
background_fut <- data.frame(extract(bio_fut_CA, background))


### . Visualize how climate is changing across the domain
plot(bio_1~bio_12 # formula of the variables we're plotting
     , data=background_clim # data where to find those variables
     , xlab="Mean Annual Precip (mm)" # x axis label
     , ylab="Mean Annual Temp (degrees C)" # y axis label
     , pch=16 # point type (16=filled circle)
     , col="#66666622") # point color, in a hexidecimal notation, which is #RRGGBB and then 2 digits for transparency so we don't overplot
## add in the future climate
points(bio_1~bio_12
       , data= background_fut
       , pch=16
       , col="#aa332222")
## add in blue oaks
# let's make a specific color for blue oak
blueoak <- brewer.pal("Set1",n=3)[2]
blueoak.transp <- paste0(blueoak,"44") # and a transparent version
points(bio_1~bio_12
       , data=qudo_clim
       , pch=16
       , col=blueoak)



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
############## Q2: What do you think this figure means for the future of blue oak? ###############
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++








#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
########## Step 2, project geographic shifts in suitable habitat with our CEM ######### 
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# 
# we're going to use a very easy, out-of-the-box climate envelope fitting technique.


### . Build a species distribution model ####
qudo_clim <- extract(bio_curr_CA, qudo[,c("decimalLongitude","decimalLatitude")])

bc.model <- bioclim(x = bio_curr_CA, qudo %>% select(decimalLongitude, decimalLatitude))
# note, we're using the select() command with %>% 'piping' from the 'tidyverse' data wrangling set of packages
# details about how this model is constructed can be found with ?bioclim



### . Predict suitable habitat in the domain ####
qudo_pred <- dismo::predict(object=bc.model
                            , x=bio_curr_CA
                            , ext= geographic.extent)
# Note: the dismo::predict bit is to tell R to use the predict() function from the dismo package. Cause there are many predict() functions from many different packages, and whatever package was loaded last usually trumps all the others

qudo_fut <- dismo::predict(object=bc.model
                           , x=bio_fut_CA
                           , ext= geographic.extent)

### . Plot our model predictions ####
plot(wrld_simpl, 
     xlim = c(min.lon, max.lon),
     ylim = c(min.lat, max.lat),
     axes = TRUE, 
     col = "grey95")
# Add model probabilities
plot(qudo_pred, add = TRUE)


### . Plot our model predictions with future climate ####
# make a figure of blue oak on our predicted suitability
tmap_mode("view")
predsfig <- tm_shape(qudo_fut)+
  tm_raster(style= "pretty",
            title="Suitable Habitat")+
  tm_layout(legend.outside = T) +
  tm_shape(SpatialPoints(qudo %>% select(decimalLongitude, decimalLatitude))) + 
  tm_dots(col = blueoak)

predsfig # plot the figure


#### . Map change in suitability ####

qudo_change <- qudo_fut - qudo_pred
  # do some quick raster math, since predictions are 0-1 (0 meaning it doesn't live there, 1 meaning THIS IS PARADISE),
  # we can just subtract the present suitability from the future, and anything negative means decreased suitability (range contraction)
  # and anything positive means increased suitability (range expansion)
# and quick plot it
tm_shape(qudo_change)+
  tm_raster(style= "pretty",
            title="Suitable Habitat")+
  tm_layout(legend.outside = T) 



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
############## Q3: Do you believe these dire predictions? ###############
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
############## Q4: What do you think the mechanisms controlling Q. douglasii are? ###############
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# based on our exploration of the Q. douglasii climate niche last week, which physiological mechanisms
# do you think constrain the fundamental niche, and what climatic changes will therefore cause range shifts,
# particularly range contractions?



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
###### STEP 3: GIVE ME SOME MECHANISM! ############
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# now we're going to pull in some of my data on blue oak water stress.
# let's see how it informs our predictions about blue oak range contractions















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
  tm_shape(SpatialPoints(qudo %>% select(decimalLongitude, decimalLatitude))) + 
  tm_dots()

qudofig # plot the figure

# make a figure of blue oak on our predicted suitability
predsfig <- tm_shape(qudo_pred)+
  tm_raster(style= "pretty",
            title="Suitable Habitat")+
  tm_layout(legend.outside = T) +
  tm_shape(SpatialPoints(qudo %>% select(decimalLongitude, decimalLatitude))) + 
  tm_dots(col = blueoak)

predsfig # plot the figure




#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
########### Q2: Why might blue oak not fill all of it's 'suitable habitat'? #########
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++










