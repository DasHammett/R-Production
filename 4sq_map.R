library(ggplot2)
library(dplyr)
library(sf)

world <- st_read(file.choose())

fqs <- read.csv(file.choose(), sep = ";", header = TRUE, encoding = "UTF-8")
fqs <- select(fqs,3,4) %>% mutate(type = "destination") %>% tibble::rowid_to_column()

origin <- c(2.1951747749949106,41.41819546685941)
origen <- data.frame(venue.location.lat=origin[2], venue.location.lng = origin[1], type = "origin")
origen <- data.frame(t(replicate(nrow(fqs), origen, simplify = TRUE))) %>% tibble::rowid_to_column()

routes <- rbind(fqs,origen)
routes <- st_as_sf(routes, coords = c("venue.location.lng","venue.location.lat"),  crs = 4326)

routes_lines <- routes %>%
  group_by(rowid) %>%
  summarise(do_union = FALSE) %>%
  st_cast("LINESTRING") %>%
  st_segmentize(units::set_units(20,km))

ggplot() +
  geom_sf(data = world, fill = "#2a2a2a", colour = "#8E8E8E", size = 0.05) +
  geom_sf(data = routes_lines, colour = "yellow", size = 0.1, alpha = 0.5) +
  theme_void() +
  theme(panel.background = element_rect(fill = "#261d11"))
