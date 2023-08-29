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

# General projection
crs <- "+proj=fahey"
lats <- c(90:-90, -90:90, 90)
longs <- c(rep(c(180, -180), each = 181), 180)

# turn into correctly projected sf collection
ocean <- 
  list(cbind(longs, lats)) %>%
  st_polygon() %>%
  st_sfc( # create sf geometry list column
    crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  ) %>% 
  st_sf() %>%
  st_transform_proj(crs = crs)

grat <- 
  st_graticule(lat = c(-89.9, seq(-80, 80, 20), 89.9)) %>%
  st_transform_proj(crs = crs)
  
# Plot chart
# list of available projections: https://proj.org/en/9.2/operations/projections
ggplot() +
  geom_sf(data = ocean, fill = "#03101E", colour = NA) + 
  geom_sf(data = grat, colour = "grey40", linewidth = 0.1) +
  geom_sf(data = world, fill = "#2a2a2a", colour = "#8E8E8E", linewidth = 0.05) +
  geom_sf(data = cities, colour = NA ) +
  geom_sf(data = routes_lines,  colour = "#E5FF99", linewidth = 0.15, alpha = 0.50, lineend = "round") +
  coord_sf(crs = st_crs(crs)) +
  #scale_x_continuous(breaks = seq(-180,180,20), limits = c(-185e5,185e5)) +
  #scale_y_continuous(breaks = seq(-100,100,10), limits = c(-100e5,100e5)) +
  theme(panel.background = element_blank()) +
  theme_void()


# Wintri Tipel projection
crs <- "+proj=wintri +datum=WGS84 +no_defs +over"
lats <- c(90:-90, -90:90, 90)
longs <- c(rep(c(180, -180), each = 181), 180)

ocean <- 
  list(cbind(longs, lats)) %>%
  st_polygon() %>%
  st_sfc( # create sf geometry list column
    crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  ) %>% 
  st_sf() %>%
  st_transform_proj(crs = crs)

grat <- 
  st_graticule(lat = c(-89.9, seq(-80, 80, 20), 89.9)) %>%
  st_transform_proj(crs = crs)

# Plot chart
# list of available projections: https://proj.org/en/9.2/operations/projections
ggplot() +
  geom_sf(data = ocean, fill = "#03101E", colour = NA) + 
  geom_sf(data = grat, colour = "grey50", linewidth = 0.1) +
  geom_sf(data = st_transform_proj(world, crs = crs), fill = "#2a2a2a", colour = "#8E8E8E", linewidth = 0.05) +
  geom_sf(data = st_transform_proj(cities, crs = crs), colour = NA ) +
  geom_sf(data = st_transform_proj(routes_lines, crs = crs), colour = "#E5FF99", linewidth = 0.15, alpha = 0.50, lineend = "round") +
  coord_sf(datum = NULL) +
  #scale_x_continuous(breaks = seq(-180,180,20), limits = c(-185e5,185e5)) +
  #scale_y_continuous(breaks = seq(-100,100,10), limits = c(-100e5,100e5)) +
  theme(panel.background = element_blank()) +
  theme_void()



### Barcelona ###
bcn <- st_read("C://Users//Jordi Vidal//Desktop//R//Catalunya GEO//terme-municipal.geojson")

fqs <- read.csv("C://Users//Jordi Vidal//Desktop//R//fq_checkins.csv", sep = ";", header = TRUE, encoding = "UTF-8")
fqs <- 
  fqs %>%
  filter(venue.location.city == "Barcelona", venue.location.lng < 2.4, venue.location.lat < 41.5) %>%
  select(2,3,4) %>%
  group_by(venue.location.lat, venue.location.lng, venue.name) %>%
  summarise(n = n(), .groups = "drop" ) %>%
  arrange(-n)

library(ggrepel)
ggplot() +
  geom_sf(data = bcn) +
  geom_sf(data = st_set_crs(st_as_sf(fqs, coords = c("venue.location.lng", "venue.location.lat")), 4326), fill = "black", shape = 16, aes(alpha = n), size = 0.1) +
  geom_text_repel(data = top_n(fqs,10), aes(label = paste0(venue.name, "(",n,")"), y = venue.location.lat, x = venue.location.lng), size = 1, min.segment.length = 0, segment.size = 0.1, arrow = arrow(length = unit(0.002, "npc")), point.padding = 0)+
  theme_void() +
  scale_alpha(range = c(0.1,1)) +
  theme(legend.position = "none")

### Old plot ###
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
