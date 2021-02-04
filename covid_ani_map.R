library(ggplot2)
library(dplyr)
library(sf)
library(lubridate)
library(RcppRoll)
library(tidyr)
library(stringr)
library(gganimate)
library(purrr)

mapa <- st_read("Catalunya GEO/LIMADM_MUNICIPI.shp", options = "ENCODING=ISO-8859-15")
raw <- read.csv("https://analisi.transparenciacatalunya.cat/api/views/jj6z-iyrp/rows.csv?accessType=DOWNLOAD&sorting=true",
                header = TRUE,
                sep=",",
                fileEncoding = "UTF-8")
raw$TipusCasData <- dmy(raw$TipusCasData)
raw$ComarcaDescripcio <- trimws(raw$ComarcaDescripcio,whitespace=" ")
raw$MunicipiCodi <- stringr::str_pad(raw$MunicipiCodi, 5, side = "left", pad = 0)
raw$MunicipiDescripcio <- str_to_title(raw$MunicipiDescripcio)

pob <- read.csv("https://analisi.transparenciacatalunya.cat/api/views/b4rr-d25b/rows.csv?accessType=DOWNLOAD&sorting=true",
                header = TRUE,
                sep = ",",
                fileEncoding = "UTF-8")

pob <- pob %>% rowwise() %>% mutate(Cens = sum(across(starts_with("Total")))) 
pob$CodiINE <- gsub(".{1}$", "", pob$Codi)
pob$CodiINE <- stringr::str_pad(pob$CodiINE, 5, side = "left", pad = 0)
pob_total <- pob %>% filter(Literal == "Catalunya") %>% select(Cens) %>% pull()

max_data <- ymd(max(raw$TipusCasData) - 2)
resum <- raw %>%
   filter(TipusCasDescripcio != "Sospitós", TipusCasData <= max_data) %>%
   group_by(TipusCasData, MunicipiDescripcio, MunicipiCodi) %>%
   summarise(NumCasos = sum(NumCasos, na.rm = TRUE)) %>%
   filter(MunicipiCodi != "<NA>") %>%
   ungroup() %>%
   group_by(MunicipiDescripcio) %>% 
   arrange(MunicipiDescripcio) %>% 
   complete(TipusCasData = full_seq(c(min(TipusCasData),max_data), period = 1), fill = list(NumCasos = 0)) %>%
   mutate(NAcumulats = roll_sum(NumCasos,n = 14, align = "right", fill = NA)) %>%
   fill(MunicipiCodi) %>%
   ungroup()

mapa <- as.data.frame(mapa)
evolucio_mapa <- map_df(
    full_seq(range(resum$TipusCasData),period = 1),
    ~left_join(
        select(mapa, CODIINE,geometry),#,NOM_MUNI),
        select(resum, TipusCasData, MunicipiCodi,NumCasos, NAcumulats) %>% filter(TipusCasData == .x),
        by = c("CODIINE" = "MunicipiCodi")
        )
    ) %>% 
   fill(TipusCasData,.direction = "updown") %>%
   left_join(select(pob,CodiINE,Cens), by = c("CODIINE" = "CodiINE")) %>%
   mutate(Incidencia = NAcumulats/Cens*1e5) %>%
   arrange(TipusCasData)

map <- ggplot(evolucio_mapa) +
   geom_sf(aes(fill = Incidencia, geometry = geometry),color="grey40",size=0.3) +
   scale_fill_gradientn(trans = "log10",
         		        na.value = "#1CA108",
                        colours = c("#1CA108","green","yellow","red","#40004D"),
                        breaks = c(35,100,300,1000,3000),
                        limits = c(35,3000)
			          ) +
   theme_void() +
   labs(title = "Incidència COVID-19 per dia:\nDia: {current_frame}") +
   transition_manual(TipusCasData)

nframes = length(full_seq(range(resum$TipusCasData),period = 1))
animate(map, end_pause = 3, duration = 30, nframes = nframes, renderer = av_renderer("out.mkv"))
anim_save("anim.mkv")

