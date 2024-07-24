# Geocomputation Project
# Date: Apr 2023 
# Author: Matt Xi (20064350)

# libraries
library(tidyverse)
library(sf)
library(tmap)
library(janitor)
library(dbscan)
library(tmap)
library(gstat)
library(spdep)
library(spatstat)
library(RColorBrewer)
library(readxl)
library(spDataLarge)
library(devtools)

#Installing Cycling Infrastructure (TFL Cycling Infrastructure Database)
devtools::install_github("PublicHealthDataGeek/CycleInfraLnd")
library(CycleInfraLnd)

# Load Borough Boundaries (from London Datastore)
borough_shp <- read_sf("geocomp_project/boundaries/boroughs/London_Borough_Excluding_MHW.shp")

# Load LSOA Boundaries (from ONS Open Geography Portal)
lsoa_shp <- read_sf("geocomp_project/boundaries/LSOA_2021_EW_BGC.shp")

# Load Method of Travel to Work 2021 Data (from NOMIS)
method_of_travel <- read_csv("geocomp_project/method_to_work_2021.csv")

# Load Car Ownership 2021 Data (from NOMIS)
car_ownership <- read_csv("geocomp_project/car_ownership_2021.csv")

# Load TFL Public Transport Accessibility Levels 2015 (London Datastore)
ptal_values <- read_csv("geocomp_project/ptal_2015/PTAL2015.csv")
#Load LSOA11 to LSOA 21 lookup
lsoa_lookup <- read_csv("geocomp_project/LSOA2011_to_LSOA2021_Lookup.csv")
# Combine with 2021 LSOA Lookup
ptal_values <- merge(lsoa_lookup, ptal_values, by.x = "LSOA11CD",
                     by.y = "LSOA2011")

# Load London Tube & Train Stations (from TFL)
london_stations <- read_sf("geocomp_project/tfl_stations.kml") %>%
  st_transform(27700)

# Load Cycling Infrastructures
cid_cycle_lanes = get_cid_lines(type = "cycle_lane_track")
cid_cycle_parking = get_cid_points(type = "cycle_parking")







# PART 1
# data wrangling ----------------------------------------------------------

# Creating "sustainable" & "nsustainable" aggregate categories
method_of_travel$nsustainable <- rowSums(method_of_travel[ ,c("taxi", "motorcycle", "car","passenger_in_car")])
method_of_travel$sustainable <- rowSums(method_of_travel[ , c("tube", "train", "bus", "bicycle", "on_foot")])

# Cleaning up  (mtw in short for method of travel to work)
mtw_df <- method_of_travel %>%
  dplyr::select(area_name, area_code, nsustainable, sustainable) %>%
  clean_names() 

# Merging method_of_travel df to lsoa sdf for analysis
mtw_sdf <- merge(lsoa_shp, mtw_df, by.x = "LSOA21CD",
                 by.y = "area_code") 

# plot the histogram of travelling to work by car data
hist(mtw_sdf$sustainable, breaks=20, 
     main = "Distribution of Travelling to Work by Public Transport in London", 
     xlab="Percentage of people travelling to work by public transport")


# Map 1 -------------------------------------------------------------------
tmap_mode("plot")

# Selecting target boroughs
target_boroughs_shp <- borough_shp %>%
  dplyr::filter(NAME == "Barnet"|NAME == "Newham")

# Plotting Map 1
travel_sustainable_map <-
  # Adding basemap
  tm_shape(mtw_sdf) +
  tm_polygons("gray", border.col = "gray") +
  # Plotting percentage of people in each LSOA using sustainable methods to travel to work
  tm_shape(mtw_sdf) +
  tm_polygons(
    col = "sustainable", n = 7, style = "quantile",
    palette = "YlGn", lty = "blank",
    title = "% Travelling by SMOT"
  ) +
  # Highlight on certain target boroughs
  tm_shape(target_boroughs_shp) +
  tm_polygons(alpha = 0, lwd = 1, border.col = "black") +
  tm_layout(
    frame = FALSE,
    main.title = "Sustainable Transport to Work London, 2021",
    main.title.fontface = 2,
    fontfamily = "Helvetica",
    legend.outside = TRUE,
    legend.position = c("left", "top"),
    legend.title.size = 0.8,
    legend.title.fontface = 2,
    legend.text.size = 0.8
  ) +
  # add North arrow
  tm_compass(
    position = c("left", "top")
  ) +
  # add scale bar
  tm_scale_bar(
    breaks = c(0, 5, 10, 15, 20),
    position = c("left", "bottom")
  )


travel_sustainable_map
tmap_save(travel_sustainable_map, filename = "geocomp_project/map1.png")


# Map 2 (Cluster Map) ----------------------------------------------------------

# Creating a neighbours list based on queen neighbour definition
mtw_neighbours_queen <- mtw_sdf %>%
  poly2nb(., queen = T)

# Creating a neighbours weights list
lsoa_spatial_weights_queen <- mtw_neighbours_queen %>%
  nb2listw(., style = "C")

# Running local Gi* test on our data
sustainable_LGO <- mtw_sdf %>%
  pull(sustainable) %>%
  as.vector() %>%
  localG(., lsoa_spatial_weights_queen)

# join the local Gi* statistic to `mtw_sdf` spatial dataframe
mtw_LGO_sdf <- mtw_sdf %>%
  mutate(sustainable_LGO_G = as.numeric(sustainable_LGO))

# create a colour palette
GIColours <- rev(brewer.pal(8, "RdBu"))

# plot the clusters
getis_plot <- tm_shape(mtw_LGO_sdf) +
  tm_polygons("sustainable_LGO_G",
              style = "pretty", palette = GIColours,
              midpoint = 0, lty = "blank", title = "Local Gi* statistic"
  ) +
  # Highlight on certain target boroughs
  tm_shape(target_boroughs_shp) +
  tm_polygons(alpha = 0, lwd = 1, border.col = "black") +
  tm_layout(
    frame = FALSE,
    main.title = "Hot/Coldspot Map of Sustainable Travel to Work, 2021",
    main.title.fontface = 2, fontfamily = "Helvetica",
    legend.outside = TRUE,
    legend.position = c("left","top"),
    legend.title.size = 1,
    legend.title.fontface = 2
  ) +
  tm_compass(type = "arrow", position = c("left", "top")) +
  tm_scale_bar(breaks = c(0, 5, 10, 15, 20), position = c("left", "bottom"))

getis_plot
tmap_save(getis_plot, filename = "geocomp_project/map2.png")


# Map 3 -------------------------------------------------------------------

# Creating column for car ownership
car_ownership$car_owners <- 100-car_ownership$no_cars_or_vans

# Merging Car Ownership to lsoa sdf for analysis 
car_ownership_sdf <- merge(lsoa_shp, car_ownership, by.x = "LSOA21CD",
                           by.y = "area_code")

car_ownership_map <-
  tm_shape(mtw_sdf) +
  tm_polygons("gray", border.col = "gray") +
  tm_shape(car_ownership_sdf) +
  tm_polygons(
    col = "car_owners", n = 7, style = "quantile",
    palette = "Reds", border.col = "white",lty = "blank",
    title = "% Car Ownership"
  ) +
  tm_shape(target_boroughs_shp) +
  tm_polygons(alpha = 0, lwd = 1, border.col = "black") +
  # add title
  tm_layout(
    frame = FALSE,
    main.title = "Car Ownership in London, 2021",
    main.title.fontface = 2,
    fontfamily = "Helvetica",
    legend.outside = TRUE,
    legend.position = c("left", "top"),
    legend.title.size = 1,
    legend.title.fontface = 2
  ) +
  # add North arrow
  tm_compass(
    type = "arrow",
    position = c("left", "top")
  ) +
  # add scale bar
  tm_scale_bar(
    breaks = c(0, 5, 10, 15, 20),
    position = c("left", "bottom")
  )
  
car_ownership_map
tmap_save(car_ownership_map, filename = "geocomp_project/map3.png")






# PART 2
# Manipulating TFL Accessibility spatial data ----------------------------------

london_boroughs = spData::lnd
Newham = london_boroughs %>% 
  filter(NAME == "Newham")
Barnet = london_boroughs %>% 
  filter(NAME == "Barnet")

# Merging ptal data with 
ptal_sdf <- merge(lsoa_shp, ptal_values, by.x = "LSOA21CD",
                  by.y = "LSOA21CD")

Newham_ptal_sdf <- ptal_sdf %>% filter(grepl('Newham', LAD22NM))
Barnet_ptal_sdf <- ptal_sdf %>% filter(grepl('Barnet', LAD22NM))

# In-depth analysis-------------------------------------------------------------
tmap_mode("plot")

# Selecting Newham
Newham_shp <- borough_shp %>%
  dplyr::filter(NAME == "Newham") %>%
  st_transform(27700)

# Filtering mtw_sdf to Newham
Newham_mtw_sdf <- mtw_sdf %>% filter(grepl('Newham', area_name))


# Map 4 (Buffer of Newham stations) -----------------------------------------

# Creating a layer of buffers
Newham_stn_500m_buffer <- london_stations %>%
  st_buffer(dist = 500) %>%
  st_union() %>%
  st_intersection(Newham_shp)

# Clipping to Newham_shp
Newham_stn <- london_stations %>%
  st_intersection(Newham_shp)

# Map 4 
Newham_stn_map <-
  # Adding basemap
  tm_shape(Newham_shp) +
  tm_polygons("white", border.col = "black") +
  tm_shape(Newham_mtw_sdf) +
  tm_polygons(
    col = "sustainable", alpha = 0.5, n = 7, style = "quantile",
    palette = "YlGn", lty = "blank",
    title = "% Travelling by SMOT"
  ) +
  # Adding station points
  tm_shape(Newham_stn) + tm_dots(col = "violetred4", size=0.2) +
  # Adding buffers
  tm_shape(Newham_stn_500m_buffer) +
  tm_polygons(col="pink", alpha=0.5)+
  tm_layout(
    frame = FALSE,
    main.title = "Buffers For Newham Train/Tube Stations",
    main.title.fontface = 2,
    fontfamily = "Helvetica",
    legend.outside = TRUE,
    legend.position = c("left", "top"),
    legend.title.size = 0.8,
    legend.title.fontface = 2
  ) +
  # add North arrow
  tm_compass(
    type = "arrow",
    position = c("left", "top")
  ) +
  # add scale bar
  tm_scale_bar(
    breaks = c(0, 0.5, 1, 1.5),
    position = c("left", "bottom")
  )

Newham_stn_map
tmap_save(Newham_stn_map, filename = "geocomp_project/map4.png")

# Map 5 (Buffer of Barnet stations) -----------------------------------------

# Selecting Barnet
Barnet_shp <- borough_shp %>%
  dplyr::filter(NAME == "Barnet") %>%
  st_transform(27700)

# Filtering mtw_sdf to Barnet
Barnet_mtw_sdf <- mtw_sdf %>% filter(grepl('Barnet', area_name))

# Creating a layer of buffers
Barnet_stn_500m_buffer <- london_stations %>%
  st_buffer(dist = 500) %>%
  st_union() %>%
  st_intersection(Barnet_shp)

# Clipping to Barnet_shp
Barnet_stn <- london_stations %>%
  st_intersection(Barnet_shp)

# Map 5
Barnet_stn_map <-
  # Adding basemap
  tm_shape(Barnet_shp) +
  tm_polygons("white", border.col = "black") +
  tm_shape(Barnet_mtw_sdf) +
  tm_polygons(
    col = "sustainable", alpha = 0.5, n = 7, style = "quantile",
    palette = "YlGn", lty = "blank",
    title = "% Travelling by SMOT"
  ) +
  # Adding station points
  tm_shape(Barnet_stn) + tm_dots(col = "violetred4", size=0.2) +
  # Adding buffers
  tm_shape(Barnet_stn_500m_buffer) +
  tm_polygons(col="pink", alpha=0.5)+
  tm_layout(
    frame = FALSE,
    main.title = "Buffers For Barnet Train/Tube Stations",
    main.title.fontface = 2,
    fontfamily = "Helvetica",
    legend.outside = TRUE,
    legend.position = c("left", "top"),
    legend.title.size = 0.8,
    legend.title.fontface = 2
  ) +
  # add North arrow
  tm_compass(
    type = "arrow",
    position = c("left", "top")
  ) +
  # add scale bar
  tm_scale_bar(
    breaks = c(0, 1, 2, 3),
    position = c("left", "bottom")
  )

Barnet_stn_map
tmap_save(Barnet_stn_map, filename = "geocomp_project/map5.png")


# Map 6 (PTAL values for Newham) -------------------------------------------
# create a colour palette based on PTAL
pal <- c("#9494C7", "#BDCFF5", "#8DF6F5", "#94FA94", "#FBFA94","#F3BDA8",
                  "#F58F8F", "#C79493")

Newham_base <-
  # Adding basemap
  tm_shape(Newham_shp) +
  tm_polygons("white", border.col = "black") +
  tm_shape(Newham_mtw_sdf) +
  tm_polygons(
    col = "sustainable", n = 7, style = "quantile",
    palette = "YlGn", lty = "blank",
    title = "% Travelling by SMOT"
  ) +
  tm_layout(
    main.title = "PTAL and Sustainable Transport to Work Newham, 2021",
    main.title.fontface = 2,
    fontfamily = "Helvetica",
    legend.outside = TRUE,
    legend.position = c("left", "top"),
    legend.title.size = 0.8,
    legend.title.fontface = 2,
    legend.text.size = 0.8
  ) +
  tm_compass(
    position = c("left", "top")
  ) +
  tm_scale_bar(
    breaks = c(0, 0.5, 1, 1.5),
    position = c("left", "bottom")
  )

Newham_ptal_map <-
  tm_shape(Newham_shp) +
  tm_polygons("white", border.col = "black") +
  tm_shape(Newham_ptal_sdf) +
  tm_polygons("gray", border.col = "transparent") +
  tm_shape(Newham_ptal_sdf) +
  tm_polygons(
    col = "PTAL", alpha= 0.8, n = 7, style = "quantile",
    palette = pal, border.col = "white",lty = "blank",
    title = "PTALs"
  ) +
  tm_layout(
    legend.outside = TRUE,
    legend.position = c("left", "top"),
    legend.title.size = 1,
    legend.title.fontface = 2
  ) +
  # add North arrow
  tm_compass(
    type = "arrow",
    position = c("left", "top")
  ) +
  # add scale bar
  tm_scale_bar(
    breaks = c(0, 0.5, 1, 1.5),
    position = c("left", "bottom")
  )

Newham_ptal_map

map6 <- tmap_arrange(Newham_base, Newham_ptal_map, nrow = 2, ncol = 1)

tmap_save(map6, filename = "geocomp_project/map6.png")


# Map 7 (PTAL values for Barnet) -------------------------------------------

Barnet_base <-
  # Adding basemap
  tm_shape(Barnet_shp) +
  tm_polygons("white", border.col = "black") +
  tm_shape(Barnet_mtw_sdf) +
  tm_polygons(
    col = "sustainable", n = 7, style = "quantile",
    palette = "YlGn", lty = "blank",
    title = "% Travelling by SMOT"
  ) +
  tm_layout(
    frame = TRUE,
    main.title = "PTAL and Sustainable Transport to Work Barnet, 2021",
    main.title.fontface = 2,
    fontfamily = "Helvetica",
    legend.outside = TRUE,
    legend.position = c("left", "top"),
    legend.title.size = 0.8,
    legend.title.fontface = 2,
    legend.text.size = 0.8
  ) +
  tm_compass(
    position = c("left", "top")
  ) +
  tm_scale_bar(
    breaks = c(0, 1, 2, 3),
    position = c("left", "bottom")
  )

Barnet_ptal_map <-
  tm_shape(Barnet_shp) +
  tm_polygons("white", border.col = "black") +
  tm_shape(Barnet_ptal_sdf) +
  tm_polygons("gray", border.col = "transparent") +
  tm_shape(Barnet_ptal_sdf) +
  tm_polygons(
    col = "PTAL", alpha =0.8, n = 7, style = "quantile",
    palette = pal, border.col = "white",lty = "blank",
    title = "PTALs"
  ) +
  # add title
  tm_layout(
    legend.outside = TRUE,
    legend.position = c("left", "top"),
    legend.title.size = 1,
    legend.title.fontface = 2
  ) +
  # add North arrow
  tm_compass(
    type = "arrow",
    position = c("left", "top")
  ) +
  # add scale bar
  tm_scale_bar(
    breaks = c(0, 1, 2, 3),
    position = c("left", "bottom")
  )

Barnet_ptal_map

map7 <- tmap_arrange(Barnet_base, Barnet_ptal_map, nrow = 2, ncol = 1)

tmap_save(map7, filename = "geocomp_project/map7.png")


# Manipulating TFL Cycling Data -------------------------------------------

cid_cycle_lanes_Newham = cid_cycle_lanes[Newham, , op = sf::st_within] %>%
  st_transform(27700)
cid_cycle_lanes_Barnet = cid_cycle_lanes[Barnet, , op = sf::st_within] %>%
  st_transform(27700)
cid_cycle_parking_Newham = cid_cycle_parking[Newham, , op = sf::st_within] %>%
  st_transform(27700)
cid_cycle_parking_Barnet = cid_cycle_parking[Barnet, , op = sf::st_within] %>%
  st_transform(27700)

# DBSCAN for Newham ------------------------------------------------------

# create a window of observation
window <- as.owin(Newham_shp$geometry)  
cycle_infrastructure_xy <-  cid_cycle_parking_Newham %>%
  st_coordinates()

plot(window)

# create a ppp object
cycling_ppp <- ppp(x = cycle_infrastructure_xy[, 1], y = cycle_infrastructure_xy[, 2], window = window)
plot(cycling_ppp)

# run dbscan
cycling_dbscan <- dbscan(cycle_infrastructure_xy, eps = 300, minPts = 15)

cycling_dbscan

# add the cluster number column to points data frame
cid_cycle_parking_Newham <- cid_cycle_parking_Newham %>%
  mutate(dbcluster = cycling_dbscan$cluster)

# create an empty list to store the resulting convex hull geometries Set the
# length of this list to the total number of clusters found
geometry_list <- vector(mode = "list", length = max(cid_cycle_parking_Newham$dbcluster))

# create a counter to keep track
counter <- 0

# begin loop
for (cluster_index in seq(0, max(cid_cycle_parking_Newham$dbcluster))) {
  
  # filter our entire bike_theft_2019 sdf by the cluster index returns only
  # points for *that* cluster
  cycling_cluster_subset <- filter(cid_cycle_parking_Newham, dbcluster == cluster_index)
  
  # for these points, first union them, then calculate the convex hull
  cluster_polygon <- cycling_cluster_subset %>%
    st_union %>%
    st_convex_hull()
  
  # add the geometry of the polygon into our list
  geometry_list[counter] <- (cluster_polygon)
  
  # update the counter
  counter <- counter + 1
}

# change the list to a multi-polygon geometry
cycling_clusters_Newham <- st_sfc(geometry_list, crs = 27700)

# Map 8 (Cycling Accessibility for Newham) ---------------------------------


cycling_infrastructure_Newham_map <-
  tm_shape(Newham_shp) +
  tm_polygons("white", border.col = "black") +
  tm_shape(Newham_mtw_sdf) +
  tm_polygons(
    col = "sustainable", alpha = 0.4, n = 7, style = "quantile",
    palette = "YlGn", border.col = "white",lty = "blank",
    title = "% Travelling by SMOT"
  ) +
  tm_shape(cid_cycle_lanes_Newham) +
  tm_lines(col="violetred")+
  tm_shape(cid_cycle_parking_Newham)+
  tm_dots(col = "violetred4")+
  tm_shape(cycling_clusters_Newham)+ 
  tm_borders() + tm_polygons(col="violetred", alpha = 0.2)+
  # add title
  tm_layout(
    frame = FALSE,
    main.title = "Cycle Infrastructure Clustering in Newham, 2021",
    main.title.fontface = 2,
    fontfamily = "Helvetica",
    legend.outside = TRUE,
    legend.position = c("left", "top"),
    legend.title.size = 0.8,
    legend.title.fontface = 2
  ) +
  # add North arrow
  tm_compass(
    type = "arrow",
    position = c("left", "top")
  ) +
  # add scale bar
  tm_scale_bar(
    breaks = c(0.5, 1, 1.5),
    position = c("left", "bottom")
  )

cycling_infrastructure_Newham_map
tmap_save(cycling_infrastructure_Newham_map, filename = "geocomp_project/map8.png")



# DBSCAN Barnet ------------------------------------------------------

# create a window of observation
window <- as.owin(Barnet_shp$geometry)  
cycle_infrastructure_xy2 <-  cid_cycle_parking_Barnet %>%
  st_coordinates()

plot(window)

# create a ppp object
cycling_ppp2 <- ppp(x = cycle_infrastructure_xy2[, 1], y = cycle_infrastructure_xy2[, 2], window = window)
plot(cycling_ppp2)

# run dbscan
cycling_dbscan2 <- dbscan(cycle_infrastructure_xy2, eps = 300, minPts = 15)

cycling_dbscan2

# add the cluster number column to points data frame
cid_cycle_parking_Barnet <- cid_cycle_parking_Barnet %>%
  mutate(dbcluster = cycling_dbscan2$cluster)

# create an empty list to store the resulting convex hull geometries Set the
# length of this list to the total number of clusters found
geometry_list <- vector(mode = "list", length = max(cid_cycle_parking_Barnet$dbcluster))

# create a counter to keep track
counter <- 0

# begin loop
for (cluster_index in seq(0, max(cid_cycle_parking_Barnet$dbcluster))) {
  
  # filter our entire bike_theft_2019 sdf by the cluster index returns only
  # points for *that* cluster
  cycling_cluster_subset <- filter(cid_cycle_parking_Barnet, dbcluster == cluster_index)
  
  # for these points, first union them, then calculate the convex hull
  cluster_polygon <- cycling_cluster_subset %>%
    st_union %>%
    st_convex_hull()
  
  # add the geometry of the polygon into our list
  geometry_list[counter] <- (cluster_polygon)
  
  # update the counter
  counter <- counter + 1
}

# change the list to a multi-polygon geometry
cycling_clusters_Barnet <- st_sfc(geometry_list, crs = 27700)

# Map 6 (Cycling Accessibility for Barnet) ---------------------------------


cycling_infrastructure_Barnet_map <-
  tm_shape(Barnet_shp) +
  tm_polygons("white", border.col = "black") +
  tm_shape(Barnet_mtw_sdf) +
  tm_polygons(
    col = "sustainable", alpha = 0.4, n = 7, style = "quantile",
    palette = "YlGn", border.col = "white",lty = "blank",
    title = "% Travelling by SMOT"
  ) +
  tm_shape(cid_cycle_lanes_Barnet) +
  tm_lines(col="violetred")+
  tm_shape(cid_cycle_parking_Barnet)+
  tm_dots(col = "violetred4")+
  tm_shape(cycling_clusters_Barnet)+ 
  tm_borders() + tm_polygons(col="violetred", alpha = 0.2)+
  # add title
  tm_layout(
    frame= FALSE,
    main.title = "Cycle Infrastructure Clustering in Barnet, 2021",
    main.title.fontface = 2,
    fontfamily = "Helvetica",
    legend.outside = TRUE,
    legend.position = c("left", "top"),
    legend.title.size = 0.8,
    legend.title.fontface = 2
  ) +
  # add North arrow
  tm_compass(
    type = "arrow",
    position = c("left", "top")
  ) +
  # add scale bar
  tm_scale_bar(
    breaks = c(0, 1, 2, 3),
    position = c("left", "bottom")
  )

cycling_infrastructure_Barnet_map
tmap_save(cycling_infrastructure_Barnet_map, filename = "geocomp_project/map9.png")
