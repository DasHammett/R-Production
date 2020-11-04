library(ggplot2)
library(dplyr)
library(sf)
library(lubridate)
library(scales)
library(gridExtra)
library(RcppRoll)
library(tidyr)

mapa <- st_read("Catalunya GEO/LIMADM_MUNICIPI.shp", options = "ENCODING=ISO-8859-15")
raw <- read.csv("https://analisi.transparenciacatalunya.cat/api/views/jj6z-iyrp/rows.csv?accessType=DOWNLOAD&sorting=true",
                header = TRUE,
                sep=",",
                fileEncoding = "UTF-8")
raw$TipusCasData <- dmy(raw$TipusCasData)
raw$ComarcaDescripcio <- trimws(raw$ComarcaDescripcio,whitespace=" ")
raw$MunicipiCodi <- stringr::str_pad(raw$MunicipiCodi, 5, side = "left", pad = 0)

pob <- read.csv("https://analisi.transparenciacatalunya.cat/api/views/b4rr-d25b/rows.csv?accessType=DOWNLOAD&sorting=true",
                header = TRUE,
                sep = ",",
                fileEncoding = "UTF-8")

pob <- pob %>% rowwise() %>% mutate(Cens = sum(across(starts_with("Total")))) 
pob$CodiINE <- gsub(".{1}$", "", pob$Codi)
pob$CodiINE <- stringr::str_pad(pob$CodiINE, 5, side = "left", pad = 0)
pob_total <- pob %>% filter(Literal == "Catalunya") %>% select(Cens) %>% pull()

max_data <- ymd(max(raw$TipusCasData))
resum <- raw %>%
   filter(TipusCasDescripcio != "Sospitós") %>%
   group_by(TipusCasData, MunicipiDescripcio, MunicipiCodi) %>%
   summarise(NumCasos = sum(NumCasos, na.rm = TRUE)) %>%
   filter(MunicipiCodi != "<NA>") %>%
   ungroup() %>%
   group_by(MunicipiDescripcio) %>% 
   arrange(MunicipiDescripcio) %>% 
   complete(TipusCasData = full_seq(c(min(TipusCasData),max_data), period = 1), fill = list(NumCasos = 0)) %>%
   mutate(NAcumulats = roll_sum(NumCasos,n = 14, align = "right", fill = NA)) %>%
   fill(MunicipiCodi) 

evolucio <- resum %>% 
   group_by(TipusCasData) %>% 
   summarise(NumCasos = sum(NumCasos,na.rm = TRUE),NAcumulats = sum(NAcumulats, na.rm = TRUE)) %>% 
   mutate(Incidencia = NAcumulats/pob_total * 1e5) %>% 
   ggplot(.,aes(TipusCasData,Incidencia)) + 
      geom_line() +
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            plot.title = element_text(size = 9, hjust = 1,face = "italic")) +
   labs(title = "Evolució incidencia a Catalunya") +
   scale_x_date(date_breaks = "1 month", labels = label_date_short())

mapa_N <- left_join(mapa, filter(resum, TipusCasData == max_data), by = c("CODIINE" = "MunicipiCodi"))
mapa_N <- left_join(mapa_N, pob[,c("CodiINE","Cens")], by = c("CODIINE" = "CodiINE"))

mapa_N <- mapa_N %>% 
   mutate(PROVINCIA1 = strtrim(CODIINE,2),
          Incidencia = NAcumulats/Cens*1e5,
          Nivell = case_when(Incidencia <= 25 ~ "Molt baix",
	                     between(Incidencia, 26,50) ~ "Baix",
	                     between(Incidencia,51,150) ~ "Moderat",
	                     between(Incidencia,151,250) ~ "Alt",
	                     TRUE ~ "Extrem")
	  )

Incidencia <- round(sum(mapa_N$NAcumulats, na.rm = T)/pob_total*1e5,0)

ggplot(mapa_N) +
   geom_sf(aes(fill = Incidencia),color="grey50") +
   scale_fill_gradientn(trans = "log10",
         		        na.value = muted("green"),
                        colours = c(muted("darkgreen"),"green","yellow","red","#40004D"),
		            	values = c(0,0.1,0.154192,0.7,1)
			          ) +
   theme_void() +
   labs(fill = "Incidencia",
        title = "Incidencia COVID-19 últims 14 dies per cada 100.000 habitants",
        subtitle = paste("Incidència a Catalunya:",Incidencia, sep = " ")) +
   theme(plot.title = element_text(family="Droid Sans", size = 16),
         plot.subtitle = element_text(family="Droid Sans", size = 12,face = "italic")) +
   annotation_custom(
	tableGrob(
         mapa_N %>% 
	     as.data.frame() %>% 
	     arrange(desc(Cens)) %>% 
	     slice(1:7) %>% 
	     select(NOM_MUNI,NAcumulats,Cens,Incidencia,Nivell) %>%
	     mutate(Incidencia = round(Incidencia)) %>%
	     rename(Municipi=NOM_MUNI,Confirmats=NAcumulats),
	  rows = NULL,
	  theme = ttheme_minimal(core=list(bg_params=list(fill=c("#E9E9E9","white")),fg_params=list(cex = 0.8)))
	  ),
	xmin = 430000,
	ymax = 4565000
   ) +
   annotation_custom(
    ggplotGrob(evolucio),
    xmin = 440000,
    xmax = 570000,
    ymin = 4778000,
    ymax = 4705000
    )
