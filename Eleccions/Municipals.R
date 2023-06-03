library(ggplot2)
library(dplyr)
library(sf)
library(reshape2)
library(purrr)
library(cowplot)
library(ggmap)

df <- read.csv("dades.csv", sep = ";", header = TRUE)
mapdf <- st_read("GEO/0301040100_SecCens_UNITATS_ADM.shp")
mapa <- select(mapdf, DISTRICTE, BARRI, AEB, SEC_CENS, geometry) %>% mutate(across(!geometry, as.numeric))

mapa <- left_join(mapa,  select(df,  c("Dte", "Barri", "AEB", "SC", "VOX", "TriasxBCN", "PSC.CP", "BCOMU.C", "Partitmésvotat")),  by = c("DISTRICTE" = "Dte",  "BARRI" = "Barri",  "AEB" = "AEB",  "SEC_CENS" = "SC"))


ggplot() +
    geom_sf(data = mapa, aes(fill = PSC.CP)) +
#    geom_sf_label(data = top_n(df$VOX, 3), aes(label = BARRI)) +
    coord_sf(crs = st_crs("+proj=natearth")) +
    scale_fill_gradient(low = "white", high = "green") +
    theme_void()


mapa_long <- df %>%
    select(Dte, Barri, AEB, SC, TriasxBCN, PSC.CP, BCOMU.C, ERC.AM, PP, VOX, CUP.AMUNT) %>%
    melt(id.vars = c("Dte", "Barri", "AEB", "SC")) %>%
    left_join(select(mapa, c(DISTRICTE, BARRI, AEB, SEC_CENS, geometry)), by = c("Dte" = "DISTRICTE", "Barri" = "BARRI", "AEB" = "AEB", "SC" = "SEC_CENS")) %>%
    st_as_sf()

ggplot() +
    geom_sf(data = mapa_long, aes(fill = value)) +
    facet_wrap(~variable, ncol = 4, shrink = FALSE) +
    theme_void()

partits <- c("TriasxBCN", "PSC.CP", "BCOMU.C", "VOX", "ERC.AM", "PP")
colors <- c("#00C5B3", "#E10B17", "#9a2eb1", "#58c036", "#fcc34f", "#0357a0")

maps <- map2(.x = partits,
             .y = colors,
             .f = function(x, y) {
                 mapa_long %>%
                     filter(variable == x) %>%
                     ggplot(.) +
                     geom_sf(aes(fill = value)) +
                     scale_fill_gradient(low = "white", high = y) +
                     labs(title = paste("Vots de", x)) +
                     theme_void() +
                     theme(plot.margin = unit(c(0.0, 0.5, 0.0, 0.5), "cm"),
                           plot.title = element_text(hjust = 0.5))
             }
)

plot_grid(plotlist = maps)


# Big map

st_transform(st_as_sfc(st_bbox(mapdf)), crs = 4326)
topo <- get_stamenmap(bbox = c(left = 2.051707, bottom = 41.31612, right = 2.229129, top = 41.4686), zoom = 14)

ggmap_bbox <- function(map) {
  if (!inherits(map, "ggmap")) stop("map must be a ggmap object")
  # Extract the bounding box (in lat/lon) from the ggmap to a numeric vector,
  # and set the names to what sf::st_bbox expects:
  map_bbox <- setNames(unlist(attr(map, "bb")),
                       c("ymin", "xmin", "ymax", "xmax"))

  # Coonvert the bbox to an sf polygon, transform it to 3857,
  # and convert back to a bbox (convoluted, but it works)
  bbox_3857 <- st_bbox(st_transform(st_as_sfc(st_bbox(map_bbox, crs = 4326)), 3857))

  # Overwrite the bbox of the ggmap object with the transformed coordinates
  attr(map, "bb")$ll.lat <- bbox_3857["ymin"]
  attr(map, "bb")$ll.lon <- bbox_3857["xmin"]
  attr(map, "bb")$ur.lat <- bbox_3857["ymax"]
  attr(map, "bb")$ur.lon <- bbox_3857["xmax"]
  map
}

topo <- ggmap_bbox(topo)

ggmap(topo) +
    geom_sf(data = mapa_long,
            fill = "grey",
            inherit.aes = FALSE
            )
