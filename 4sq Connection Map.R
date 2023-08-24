library(ggplot2)
library(dplyr)
library(sf)

# Load world map
world <- st_read("/home/hammett/src/R Scripts/World Geo/World_Countries.shp")
additional <- read.csv("/home/hammett/locations.txt", sep = ",", header = FALSE) %>% rename(venue.location.lat = V1, venue.location.lng = V2, type = V3)

# Load cities shapefile
cities <- st_read("/home/hammett/src/R Scripts/World Geo/ne_10m_urban_areas.shp")

# Load csv file with foursquare checkin venue location
fqs <- read.csv("/home/hammett/src/R Scripts/fq_checkins.csv", sep = ";", header = TRUE, encoding = "UTF-8")
fqs <- select(fqs,3,4) %>% 
   mutate(type = "destination") %>% 
   bind_rows(additional) %>%
   tibble::rowid_to_column() #rowid_to_column to create index on destination

# Set origin for connections
origin <- c(41.41700471231859, 2.1936888311886014)

# Create df with origin lon,lat and extend it to size of venue checkins
origen <- data.frame(venue.location.lat = rep(origin[1],nrow(fqs)),
                     venue.location.lng = rep(origin[2],nrow(fqs)),
                     type = rep("origin", nrow(fqs))) %>%
  tibble::rowid_to_column()

routes <- rbind(fqs,origen)
routes <- st_as_sf(routes, coords = c("venue.location.lng","venue.location.lat"),  crs = 4326)

# Create multipoint and linestring sf object and measure distance (st_segnement()) from origin to destination, by id
routes_lines <- routes %>%
  group_by(rowid) %>%
  summarise(do_union = FALSE) %>%
  st_cast("LINESTRING") %>%
  st_segmentize(units::set_units(20,km))

# Plot chart
# list of available projections: https://proj.org/en/9.2/operations/projections
ggplot() +
  geom_sf(data= world, fill = "#2a2a2a", colour = "#8E8E8E", size = 0.05) +
  geom_sf(data = cities, colour = NA ) +
  geom_sf(data = st_set_crs(st_as_sf(filter(fqs,type == "destination"), coords = c("venue.location.lng", "venue.location.lat")), 4326), colour = "red", stroke = 0, size = 0.15, shape = 1) +
  geom_sf(data = routes_lines, colour = "#E5FF99", size = 0.15, alpha = 0.5, lineend = "round") +
  #coord_sf(xlim = c(-10,150), ylim = c(0,80)) +
  coord_sf(crs = st_crs("+proj=wink2")) +
  theme_void() +
  theme(panel.background = element_rect(fill = "#001328"))
