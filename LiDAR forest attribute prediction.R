##################################################
  ## LiDAR for Alex Fraser Research Forest ##
##################################################

#### Lab set up
  library(lidR)
  library(terra)
  library(tidyverse)
  
  wd <- setwd("C:\\Users\\wendiz3.stu\\Documents\\Data")

#### Load aerial photo ####

  # Load RGB image
  rgb_afrf <- rast("Aerial_Photo\\AFRF_Aerial_Photo.TIF")
  
  # check structure
  str(rgb_afrf) 
  
  # plot RGB
  plotRGB(rgb_afrf)
  
#### Read multiple LAS files into LAScatalog object ####
  
  # Create LAScatalog object from a folder or a collection of file names (afrf LAS tiles)
  cat_afrf <- readLAScatalog("LAS")
  
  # check structure of the LAScatalog object 
  las_check(cat_afrf)
  
  # check summary of the LAScatalog object
  summary(cat_afrf)
  
  # plot the LAS catalog object with all afrf tiles
  plot(cat_afrf)
  
  # Examine single .las tile to determine duplication
  tile_1_afrf <- readLAS("LAS/AFRF_Tile1.LAS")
  las_check(tile_1_afrf)
  
  # Filter the duplicate points
  # Store the filtered las data in a folder called Filtered under the working directory
  opt_output_files(cat_afrf) <- paste("C:", "/Filtered/filtered_AFRF_{ID}", sep = "")
  cat_afrf <- filter_duplicates(cat_afrf)
  
  # confirm that duplicates were removed for Tile 1
  filtered_t1_afrf <- readLAS("C:/Filtered/filtered_afrf_1.las")
  las_check(filtered_t1_afrf)
  
  # read all filtered las files into a LAScatalog object
  filtered_cat_afrf <- readLAScatalog("C:/Filtered")
  
  # get summary information of the LAScatalog
  summary(filtered_cat_afrf)
  
  #plot the LAScatalog object 
  plot(filtered_cat_afrf)
  
#### Normalize the catalog ####
  
  # Create DEM
  dem_allLAS_afrf <- rasterize_terrain(filtered_cat_afrf, 2, tin())
  
  # Create colour palatte
  col_1 <- height.colors(50) 
  
  # Plot DEM using colour palatte in 2D
  DEM_2D <- plot(dem_allLAS_afrf, col = col_1)
  
  # plot in 3D **DO NOT RUN IF YOUR COMPUTER IS SLOW**
  DEM_3D <- plot_dtm3d(dem_allLAS_afrf)
  
  # define LAScatalog engine options
  opt_output_files(filtered_cat_afrf) <- paste("C:", "/Normalized/norm_afrf_{ID}", sep = "")
  
  # normalize all tiles in cat_afrf with the DEM 
  norm_tiles_afrf <- normalize_height(filtered_cat_afrf, dem_allLAS_afrf) #check your folder when complete 
  
  # check to see if the normalization worked
  norm_afrf_1 <- readLAS("C:/Normalized/norm_afrf_1.las")
  plot(norm_afrf_1)
  las_check(norm_afrf_1) # normalization: maybe????
  
  ## test the effect of the outliers by creating a CHM
  ## check if the CHM indicates a normal tree height of around 60m
  
  # Create a CHM for normalized Tile 1
  chm_afrf_1 <- rasterize_canopy(norm_afrf_1, 
                                 2, # resolution: our Lidar data has 3.1 returns/m2, assuming a spatial resolution of 2m ensures every raster cell has at least one return. 
                                 p2r()) # point to raster algorithm, returns the max height value of the point cloud for each area
  
  # plot 2D CHM
  chm_afrf_1_2D <- plot(chm_afrf_1, col = col_1)
  
  # Plot 3D CHM
  chm_afrf_1_3D <- plot_dtm3d(chm_afrf_1) # some points are abnormally high
  
  # Remedy the issue
  # read normalized las into catalog to continue processing
  norm_cat_afrf <- readLAScatalog("C:/Normalized")
  
  # filter undesired data points
  opt_filter(norm_cat_afrf) <- '-drop_z_below 0 -drop_z_above 55'
  
  # ensure the entire study area was processed
  plot(norm_cat_afrf)
  summary(norm_cat_afrf)
  
#### Produce CHM from normalized catalog ####
  
  # check if the outliers have been removed by creating CHM for all normalized tiles
  chm_afrf <- rasterize_canopy(norm_cat_afrf, 2, p2r()) 
  
  # plot 2D CHM
  plot(chm_afrf, col = col_1)
  
  # plot 3D CHM
  plot_dtm3d(chm_afrf)

########################################################################################
    
#### Read and process CSV files ####
  plot_table <- read_csv("Plots/Plot_Table.csv")
  afrf_plot_metrics <- read_csv("Plots/afrf_Plot_Metrics.csv")
  
  #Add column to "afrf_plot_metrics' called Plot_ID which will be used as a join key
  afrf_plot_metrics$Plot_ID = 1:38
  
  #Join 'Plot_Table' and 'afrf_Plot_Metrics' into 'data_table'
  data_table <- plot_table %>% 
    full_join(afrf_plot_metrics)
  
##########################
###### AGB ###############
##########################

#### Data exploration ####
  
  # explore relationship between LiDAR metrics and forest attributes (AGB)
  plot(Total_AGB ~ zq85, data = data_table)
  plot(Total_AGB ~ zq90, data = data_table)
  plot(Total_AGB ~ zq95, data = data_table)
  
#### Model 1: linear model ####
  
  # zq 95 and AGB
  model_1 = lm(Total_AGB ~ zq95, data = data_table)
  summary(model_1)
  model_1$coefficients
  
#### Explore correlations between metrics ####
  
  #list metrics
  colnames(data_table)
  
  # create a matrix of scatterplots between less-correlated variables
  pairs(~ zq95 + pzabovezmean + zskew + zentropy + zkurt, data = data_table)

#### Model 2: multiple variables ####
  
  # selected variables:
  # zq95: elevation at 95% quantile
  # pzabovezmean: percentage of returns above mean elevation
  # zentropy: normalized Shanon diversity index/vertical complexity index
  # zskew: skewness
  # zkurt: kurtosis of height distribution
  
  # We start with no variables in our model
  model_2 = lm(Total_AGB ~ 1, data = data_table)
  
  # Now, we test which variable is the most significant
  # use add1() to compute single terms that can be added to the model, fit those model and compute a table of the changes to fit
  add1(model_2,~  zq95 + pzabovezmean + zskew + zentropy + zkurt, test = 'F')
  
  # zq95 was the most significant (lowest Pr(>F)), so we add it to our model
  model_2 = lm(Total_AGB ~ zq95, data = data_table)
  
  # Now, We add each remaining variable to the new model one by one, to see 
  # if any variable is a significant addition
  add1(model_2,~  zq95 + pzabovezmean + zskew + zentropy + zkurt, test = 'F')
  
  # zkurt was the most significant (lowest Pr(>F)), so we add it to our model
  model_2 = lm(Total_AGB ~ zq95 + zkurt, data = data_table)
  
  # test other variables
  add1(model_2,~  zq95 + pzabovezmean + zskew + zentropy + zkurt, test = 'F')
  
  # zentropy was the most significant, add to the model
  model_2 = lm(Total_AGB ~ zq95 + zkurt + zentropy, data = data_table)
  
  # test other variables
  add1(model_2,~  zq95 + pzabovezmean + zskew + zentropy + zkurt, test = 'F')
  
  # no other variables are significant
  
  # Get the summary of the final model
  summary(model_2)
  
  # Plot our predicted volume against our measured volume
  plot(Total_AGB ~ model_2$fitted,data = data_table,xlab = 'Predicted',ylab = 'Measured')
  abline(0,1) #Adds a one to one line
  
  # Get the coefficients to our model
  model_2$coefficients

  # equation: AGB = -545614.18 + 10649.59*zq95 + 31376.49*zkurt + 397450.46*zentropy
  
######################################
###### Dominant height ###############
######################################
  
#### Data exploration ####
  
  # explore relationship between LiDAR metrics and forest attributes (AGB)
  plot(Dominant_Height ~ zq95, data = data_table)
  
#### Model 1: linear model ####
  
  # zq 95 and dominant height
  model_3 = lm(Dominant_Height ~ zq95, data = data_table)
  summary(model_3)
  model_3$coefficients
  
#### Explore correlations between metrics ####
  
  #list metrics
  colnames(data_table)
  
  # create a matrix of scatterplots between less-correlated variables
  pairs(~ zq95 + pzabovezmean + zskew + zentropy + zkurt, data = data_table)
  
#### Model 2: multiple variables ####
  
  # selected variables:
  # zq95: elevation at 95% quantile
  # pzabovezmean: percentage of returns above mean elevation
  # zentropy: normalized Shanon diversity index/vertical complexity index
  # zskew: skewness
  # zkurt: kurtosis of height distribution
  
  # We start with no variables in our model
  model_4 = lm(Dominant_Height ~ 1, data = data_table)
  
  # Now, we test which variable is the most significant
  # use add1() to compute single terms that can be added to the model, fit those model and compute a table of the changes to fit
  add1(model_4,~  zq95 + pzabovezmean + zskew + zentropy + zkurt, test = 'F')
  
  # zq95 was the most significant (lowest Pr(>F)), so we add it to our model
  model_4 = lm(Dominant_Height ~ zq95, data = data_table)
  
  # Now, We add each remaining variable to the new model one by one, to see 
  # if any variable is a significant addition
  add1(model_4,~  zq95 + pzabovezmean + zskew + zentropy + zkurt, test = 'F')
  
  # zentropy was the most significant (lowest Pr(>F)), so we add it to our model
  model_4 = lm(Dominant_Height ~ zq95 + zentropy, data = data_table)
  
  # test other variables
  add1(model_4,~  zq95 + pzabovezmean + zskew + zentropy + zkurt, test = 'F')
  
  # no other variables are significant
  
  # Get the summary of the final model
  summary(model_4)
  
  # Plot our predicted volume against our measured volume
  plot(Dominant_Height ~ model_4$fitted,data = data_table,xlab = 'Predicted',ylab = 'Measured')
  abline(0,1) #Adds a one to one line
  
  # Get the coefficients to our model
  model_4$coefficients
  
  # equation: Dominant Height = 0.5679597 + 0.7500043*zq95 + 10.7752354*zentropy
  
#### calculate biomass and dominant height rasters

  norm_cat_afrf <- readLAScatalog("C:/Normalized")
  opt_filter(norm_cat_afrf) <- '-keep_z_above 2 -drop_z_above 55'
  plot(norm_cat_afrf)
  
  # Calculate grid metrics of mean Z at 10 m resolution for entire study area
  pixel_metrics_afrf <- pixel_metrics(norm_cat_afrf, .stdmetrics_z, 10) 
  plot(pixel_metrics_afrf)
  
  # subset each variable to use in the function
  zq95 <- terra::subset(pixel_metrics_afrf, "zq95")
  zkurt <- terra::subset(pixel_metrics_afrf, "zkurt")
  zentropy <- terra::subset(pixel_metrics_afrf, "zentropy")
  
  # create a list of variables (raster stack)
  AGB_variables <- c(zq95, zkurt, zentropy)
  height_variables <- c(zq95, zentropy)
  
  #Create function from model2 coefficients
  f_AGB <- function(x, y, z){
    -545614.18 + 10649.59*x + 31376.49*y + 397450.46*z
  }
  
  f_height <- function(x, y){
    0.5679597 + 0.7500043*x + 10.7752354*y
  }
  
  #Apply function to raster
  AGB <-terra::lapp(AGB_variables, f_AGB) #feed in
  plot(AGB)
  
  height <- terra::lapp(height_variables, f_height)
  plot(height)
  