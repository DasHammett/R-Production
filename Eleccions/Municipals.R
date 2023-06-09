library(ggplot2)
library(dplyr)
library(sf)
library(reshape2)
library(purrr)
library(cowplot)
library(ggmap)
library(ggrepel)

df <- read.csv("dades.csv", sep = ";", header = TRUE)
colnames(df) <- gsub("\\.", "-", colnames(df))
districtes <- read.csv("districtes_i_barris.csv", sep = ",", header = TRUE, fileEncoding = "UTF-8")
mapdf <- st_read("GEO/0301040100_SecCens_UNITATS_ADM.shp")
mapa <- select(mapdf, DISTRICTE, BARRI, AEB, SEC_CENS, geometry) %>% mutate(across(!geometry, as.numeric))

top_barri <- df %>%
  select(Dte, Barri, AEB, SC, Partitmesvotat, TriasxBCN, "PSC-CP", "BCOMU-C", "ERC-AM", PP, VOX) %>%
  melt(id.vars = c("Dte", "Barri", "AEB", "SC", "Partitmesvotat")) %>%
  group_by(variable) %>%
  arrange(-value, .by_group = TRUE) %>%
  slice(1) %>%
  left_join(select(mapa, c("SEC_CENS", "geometry", "BARRI", "DISTRICTE", "AEB")), by = c("SC" = "SEC_CENS", "Dte" = "DISTRICTE", "Barri" = "BARRI", "AEB" = "AEB")) %>%
  left_join(select(districtes, c("CODI_BARRI", "NOM_BARRI")), by = c("Barri" = "CODI_BARRI")) %>%
  st_as_sf()

mapa <- left_join(mapa,  select(df,  c("Dte", "Barri", "AEB", "SC", "Electors", "Votants", "VOX", "PP", "TriasxBCN", "PSC-CP", "BCOMU-C", "Partitmesvotat")),  by = c("DISTRICTE" = "Dte",  "BARRI" = "Barri",  "AEB" = "AEB",  "SEC_CENS" = "SC")) %>%
    mutate(pc_vots = Votants / Electors) %>%
    st_as_sf()


ggplot() +
    geom_sf(data = mapa, aes(fill = PP)) +
#    geom_sf_label(data = top_n(df$VOX, 3), aes(label = BARRI)) +
    coord_sf(crs = st_crs("+proj=natearth")) +
    scale_fill_gradient(low = "white", high = "darkblue") +
    theme_void()


mapa_long <- df %>%
    select(Dte, Barri, AEB, SC, Partitmesvotat, TriasxBCN, "PSC-CP", "BCOMU-C", "ERC-AM", PP, VOX) %>%
    melt(id.vars = c("Dte", "Barri", "AEB", "SC", "Partitmesvotat")) %>%
    left_join(select(mapa, c(DISTRICTE, BARRI, AEB, SEC_CENS, geometry)), by = c("Dte" = "DISTRICTE", "Barri" = "BARRI", "AEB" = "AEB", "SC" = "SEC_CENS")) %>%
    st_as_sf()

ggplot() +
    geom_sf(data = mapa_long, aes(fill = value)) +
    facet_wrap(~variable, ncol = 4, shrink = FALSE) +
    theme_void()

partits <- c("TriasxBCN", "PSC-CP", "BCOMU-C", "VOX", "ERC-AM", "PP")
colors <- c("#05b9a9", "#E10B17", "#9a2eb1", "#58c036", "#fcc34f", "#0357a0")
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
bbox <- as.data.frame(t(as.matrix(st_bbox(st_transform(mapdf, crs = 4326)))))
topo <- get_stamenmap(bbox = c(left = bbox$xmin, bottom = bbox$ymin, right = bbox$xmax, top = bbox$ymax), zoom = 15)


final_topo <- ggmap(topo) +
    geom_sf(data = mapa,
            aes(fill = Partitmesvotat, alpha = pc_vots),
            inherit.aes = FALSE,
            lwd = 0.2
            ) +
    coord_sf(crs = 4326) +
    geom_label_repel(
      data = top_barri,
      aes(label = paste0(variable, "\n",
                        "Barri amb més vots: ", NOM_BARRI, "\n",
                        "Num. vots: ", value),
          geometry = geometry),
      stat = "sf_coordinates",
      size = 2,
      fill = alpha(c("white"), 0.7),
      inherit.aes = FALSE,
      min.segment.length = 0.01) +
    scale_fill_manual(values = colors, breaks = partits) +
    scale_alpha(trans = scales::trans_new("x2", function(x) x^3, function(x) x^1 / 3)) +
    labs(title = "Partit més votat a Barcelona per Secció Censal") +
    theme_void() +
    theme(legend.position = "none")

ggsave(final_topo, file = "/mnt/Multimedia/.cache/R/R.cache/ggplot.pdf", height = 10, width = 9, units = "in")
system2("zathura", c("/mnt/Multimedia/.cache/R/R.cache/ggplot.pdf"))



library(rayshader)

topo_attributes <- attributes(topo)
topo_trans <- matrix(adjustcolor(topo, alpha.f = 0), nrow = nrow(topo))
attributes(topo_trans) <- topo_attributes

maps_height <- ggmap(topo_trans) +
    geom_sf(data = mapa,
            aes(fill = pc_vots),
            inherit.aes = FALSE,
            lwd = 0.1,
            color = "white"
            ) +
    coord_sf(crs = 4326) +
    labs(title = "Partit més votat a Barcelona per Secció Censal",
         subtitle = "Altura representa percentatge de participació") +
    scale_fill_viridis_c(trans = scales::trans_new("x3", function(x) x^3, function(x) x^1 / 3)) +
#    theme_void() +
    theme(legend.position = "none",
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          plot.title = element_text(size = 8),
          plot.subtitle = element_text(size = 4, face = "italic", vjust = 6),
          panel.grid = element_blank(),
          panel.border = element_blank()
          )

final_map <- ggmap(topo) +
    geom_sf(data = mapa,
            aes(fill = Partitmesvotat, alpha = 0.9),
            inherit.aes = FALSE,
            lwd = 0.1,
            ) +
    coord_sf(crs = 4326) +
    scale_fill_manual(values = colors, breaks = partits) +
    labs(title = "Partit més votat a Barcelona per Secció Censal",
         subtitle = "Altura representa percentatge de participació") +
#    theme_void() +
    theme(legend.position = "none",
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          plot.title = element_text(size = 8),
          panel.grid = element_blank(),
          plot.subtitle = element_text(size = 4, face = "italic", vjust = 6),
          panel.border = element_blank()
          )

plot_gg(ggobj = final_map,
        ggobj_height = maps_height,
        multicore = TRUE,
        scale = 300,
        raytrace = TRUE,
        pointcontract = 0.3,
        triangulate = TRUE,
        shadow = TRUE)

phivechalf <- 30 + 60 * 1/(1 + exp(seq(-7, 20, length.out = 180)/2))
phivecfull <- c(phivechalf, rev(phivechalf))
thetavec <- 45 * sin(seq(0,359,length.out = 360) * pi/180)
zoomvec <- 0.45 + 0.2 * 1/(1 + exp(seq(-5, 20, length.out = 180)))
zoomvecfull <- c(zoomvec, rev(zoomvec))

render_movie(
    filename = "eleccions.mp4", 
    fps = 60, 
    type = "custom", 
    frames = 360,
    phi = phivecfull,
    zoom = zoomvecfull,
    theta = thetavec)
