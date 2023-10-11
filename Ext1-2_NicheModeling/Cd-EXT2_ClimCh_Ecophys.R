#############################################
####    EXTENSION 2 : Climate Change and Ecophysiology ########
#############################################

# Extension activity for UCSB EEMB 508: Intro to Ecology
# Run with R version 4.2.3 (2023-03-15) -- "Shortstop Beagle"
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



version <- "202310012" # set a version for saving files. Versioning your s&#! will save you many headaches in the future


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

# write.csv(qudo.raw, paste("blue_oak_occurence_data",version,".csv"))

## Clean the data
# a REALLY critical step with occurrence data
# note: we already defacto cleaned a lot of outliers/problems by limiting just to CA records, because we know CA is Q. douglasii's native range

# remove duplicated records
dups <- duplicated(qudo.raw %>% select(decimalLatitude, decimalLongitude))
sum(dups) # number of duplicate records (which would incorrectly weight their locations in the model)
qudo<- qudo.raw[!dups,] # remove the duplicates using the ! (NOT) boolian function


# download world outlines #
#data("wrld_simpl") # download a simple world map for visualization
wrld_simpl <- ne_coastline() # download a simple world map for visualization (frome the Naturalearth project)

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
# let's also make the names of our variables easier to handle
names(bio_curr_CA)
names(bio_curr_CA) <- str_replace(names(bio_curr_CA),"wc2.1_2.5m_","")

# a description of what each of these variables are can be found:
# https://www.worldclim.org/data/bioclim.html




### in case server is down (has happened before)
#raster::writeRaster(bio_curr_CA,filename = "Ext1-2_NicheModeling/data/climate/Bioclim_clipped.tiff",filetype="GTiff", overwrite=T)
# This exports the cropped bioclim to a geotiff so I can send it to y'all

# read in the cropped raster brick from my exported geotiff:
#bio_curr_CA <- brick("Ext1-2_NicheModeling/data/climate/Bioclim_clipped.tiff")

# let's also make the names of our variables easier to handle
#names(bio_curr_CA) # exporting and importing changed the names so need to rename things
#names(bio_curr_CA) <- str_replace(names(bio_curr_CA),"Bioclim_clipped", "bio")




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
                       , path = "Ext1-2_NicheModeling/data/climate/"))

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
# 1) since they have the same dimensions and are in exactly the same order, we just use the _curr names for _fut
names(bio_fut_CA) <- names(bio_curr_CA)

# 2) we replace the "wc2..." string with "bio" string
names(bio_fut_CA) <- str_replace(string=names(bio_fut_CA),pattern = "wc2.1_2.5m_bioc_GFDL.ESM4_ssp370_2061.2080", replacement = "bio") 
  # note: this will run and just not do anything even if you've already fixed the names with option 1)

# double check that this worked.
all.equal(names(bio_fut_CA), names(bio_curr_CA))
  # yay!




#__________________________________________________________________________
###### ^^ RUN BEFORE CLASS ^^ ################################################
#__________________________________________________________________________







#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
###### STEP 2: Look at projected changes in climate and oak distribution ############
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


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




#### . Now let's look in climate space rather than geographic space #######


### (stuff we already did in last class)
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



###### . Visualize how climate is changing across the domain ####
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

bc.model <- bioclim(x = bio_curr_CA, qudo %>% select(decimalLongitude, decimalLatitude))
# note, we're using the select() command with %>% 'piping' from the 'tidyverse' data wrangling set of packages
# details about how this model is constructed can be found with ?bioclim



### . Predict suitable habitat in the domain ####
qudo_pred <- dismo::predict(object=bc.model
                            , x=bio_curr_CA
                            , ext= geographic.extent)
# Note: the dismo::predict bit is to tell R to use the predict() function from the dismo package. Cause there are many predict() functions from many different packages, and whatever package was loaded last usually trumps all the others

#____________________________________________________________________________
### . NOW Le'ts predict suitable habitat in 2080! #####################

qudo_fut <- dismo::predict(object=bc.model
                           , x=bio_fut_CA
                           , ext= geographic.extent)

### CODING QUESTIONS: What did we change and why does this simple function predict future blue oak disttributions?

### . Plot our model predictions ####
plot(wrld_simpl, 
     xlim = c(min.lon, max.lon),
     ylim = c(min.lat, max.lat),
     axes = TRUE, 
     col = "grey65")
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


#_____________________________________________________________
######## . Map change in suitability ##########

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

fielddat <- read.csv("Ext1-2_NicheModeling/data/BlueOak_WP_and_AlAs_20190528.csv", header=T)
# This is tree-level average data from a fall 2018 measurement campaign
# we drove around the state and measured plat water potentials and some leaf traits
# at the end of the growing season (late Sept/ early Oct)
# when the soil moisture was at its lowest.


#_________________________________________________________________________
####### First things first (always), let's check out the data frame: #
dim(fielddat)
  # 87 indivdiual trees
xtabs(~SiteName, fielddat)
  # 15 sites
mean(xtabs(~SiteName, fielddat))
  # 5-6 trees per site

summary(fielddat)
  # we've got Site/SiteName and Tree character columns (could be turned to factors)
  # then we've got
  # - MD_WP_Mpa (midday water potentials, in MPa) --> this is the maximum water stress the plant is experiencing
  # - PD_WP_Mpa (predawn water potentials, in MPa) --> this is how dry the soil is (when the tree is equilibrated with the soil at night)
# NOTE: more negative numbers for water potentials == drier/more drought stressed
  # - mAl_As --> tree mean Leaf Area (Al) to sapwood area (As) ratio (cm2/mm2). a metric of allocation
  # - mLMA --> tree mean Leaf Mass per Area (g/cm2)
  # - bunch of site stuff


# let's see where these plots are (using our CEM as the background)
tm_shape(qudo_pred)+
  tm_raster(style= "pretty",
            title="Suitable Habitat")+
  tm_layout(legend.outside = T) +
  tm_shape(SpatialPoints(fielddat %>% select(Lon, Lat))) + 
  tm_dots(col = blueoak)


####### . extract some climate data for these locations: 
fielddat.clim <- extract(bio_curr_CA, fielddat[,c("Lon","Lat")])
  # this extracts all of our climate data (Bio_1 through Bio_19)
  # and makes a new dataframe

fielddat <- cbind(fielddat, fielddat.clim)
  # this 'column binds' our climate data to our field data to make one large data frame






#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### CODING CHALLENGE ############################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  # Can you use the example code below to find a climate variable that predicts blue oak water stress?
  # i.e. use PD_WP_Mpa as your y variable and a climate predictor as your x

### Example:
  # Mean Annual Temp does not predict water stress...
plot(PD_WP_Mpa~bio_1, fielddat, ylab="Water Potential_PD (MPa)")

# do some statistics to confirm this visual inference
  # fit a linear model with lm()
mod1 <- lm(PD_WP_Mpa~bio_1, fielddat)
  # look at the output
summary(mod1)

# add the prediction to our plot
abline(mod1)

# add a p-value to our plot 
mtext(text=paste("R^2=", round(summary(mod1)$coefficients[2,4], 2)), side = 3, adj = 0)
  # note: in the summary object for our model, the P-value is the t-test result in the 4th column, 2nd row (the slope of bio_1) of the $coefficients part of the list
  #       we rounded it to two decimal places for easy viewing, and then paste()'ed it into something easy to read
    
    
####### ADVANCED CHALLENGE: ###########
# Can you make the trend line solid if the relationship is statistically significant (p<0.05)
# and dotted if it is non-significant?





#___________________________________________________________________________
######## . Does our Climate Envelope Model help explain water stress?
#___________________________________________________________________________


# First extract our 'predicted suitability' for each of our field locations
fielddat$suit_curr <- extract(qudo_pred, fielddat[,c("Lon","Lat")])
fielddat$suit_fut <- extract(qudo_fut, fielddat[,c("Lon","Lat")])



# Now let's see whether the suitability predicts water stress
plot(PD_WP_Mpa~suit_curr, fielddat)
mod1 <- lm(PD_WP_Mpa~suit_curr, fielddat)
# add the trend line
abline(mod1, lty = ifelse(test=summary(mod1)$coefficients[2,4]<0.05,yes = 1,no = 2))
mtext(text=paste("p = ",summary(mod1)$coefficients[2,4]),side = 3)





#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### Q5: What does this figure mean?? ############################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# And what does it imply about the putative mechanisms under the hood of our CEM?









#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### Q6: What else should we think about to try to predict water stress in blue oaks? ############################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


















############ Advanced Coding Solution (one of many) #############3
 # make the plot
plot(PD_WP_Mpa~bio_1, fielddat)
 # fit the model
mod1 <- lm(PD_WP_Mpa~bio_1, fielddat)
 # add the trend line
abline(mod1, lty = ifelse(test=summary(mod1)$coefficients[2,4]<0.05,yes = 1,no = 2))

















