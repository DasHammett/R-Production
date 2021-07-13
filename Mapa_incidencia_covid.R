library(ggplot2)
library(dplyr)
library(sf)
library(lubridate)
library(scales)
library(gridExtra)
library(RcppRoll)
library(tidyr)
library(stringr)
library(cowplot)

mapa <- st_read("Catalunya GEO/LIMADM_MUNICIPI.shp", options = "ENCODING=ISO-8859-15")
raw <- read.csv("https://analisi.transparenciacatalunya.cat/api/views/jj6z-iyrp/rows.csv?accessType=DOWNLOAD&sorting=true",
                header = TRUE,
                sep=",",
                fileEncoding = "UTF-8")
raw$TipusCasData <- dmy(raw$TipusCasData)
raw$ComarcaDescripcio <- trimws(raw$ComarcaDescripcio,whitespace=" ")
raw$MunicipiCodi <- stringr::str_pad(raw$MunicipiCodi, 5, side = "left", pad = 0)
raw$MunicipiDescripcio <- str_to_title(raw$MunicipiDescripcio)
max_data <- ymd(max(raw$TipusCasData) - 0)

pob <- read.csv("https://analisi.transparenciacatalunya.cat/api/views/b4rr-d25b/rows.csv?accessType=DOWNLOAD&sorting=true",
                header = TRUE,
                sep = ",",
                fileEncoding = "UTF-8")

pob <- pob %>% rowwise() %>% mutate(Cens = sum(across(starts_with("Total")))) 
pob$CodiINE <- gsub(".{1}$", "", pob$Codi)
pob$CodiINE <- stringr::str_pad(pob$CodiINE, 5, side = "left", pad = 0)
pob_total <- pob %>% filter(Literal == "Catalunya") %>% select(Cens) %>% pull()

resum <- raw %>%
   filter(TipusCasDescripcio != "Sospitós", TipusCasData <= max_data) %>%
   group_by(TipusCasData, MunicipiDescripcio, MunicipiCodi) %>%
   summarise(NumCasos = sum(NumCasos, na.rm = TRUE)) %>%
   filter(MunicipiCodi != "<NA>") %>%
   ungroup() %>%
   group_by(MunicipiDescripcio) %>% 
   arrange(MunicipiDescripcio) %>% 
   complete(TipusCasData = full_seq(c(min(TipusCasData),max_data), period = 1), fill = list(NumCasos = 0)) %>%
   mutate(NAcumulats = roll_sum(NumCasos,n = 14, align = "right", fill = NA),
          diff = paste0("(" ,sprintf("%+d",NAcumulats-lag(NAcumulats,7)),")")) %>%
   fill(MunicipiCodi) 

evolucio <- resum %>% 
   group_by(TipusCasData) %>% 
   summarise(NumCasos = sum(NumCasos,na.rm = TRUE),NAcumulats = sum(NAcumulats, na.rm = TRUE)) %>% 
   mutate(Incidencia = NAcumulats/pob_total * 1e5) %>% 
   ggplot(.,aes(TipusCasData,Incidencia)) + 
      geom_line(color="#3374B0") +
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            plot.title = element_text(size = 9, hjust = 1,face = "italic")) +
      labs(title = "Evolució incidència a Catalunya") +
      scale_x_date(date_breaks = "2 month", labels = label_date_short())

mapa_N <- left_join(mapa, filter(resum, TipusCasData == max_data), by = c("CODIINE" = "MunicipiCodi"))
mapa_N <- left_join(mapa_N, pob[,c("CodiINE","Cens")], by = c("CODIINE" = "CodiINE"))

mapa_N <- mapa_N %>% 
   mutate(PROVINCIA1 = strtrim(CODIINE,2),
          Incidencia = NAcumulats/Cens*1e5,
          Nivell = case_when(Incidencia <= 25 ~ "Molt baix",
                            between(round(Incidencia,0),26,50) ~ "Baix",
                            between(round(Incidencia,0),51,150) ~ "Moderat",
                            between(round(Incidencia,0),151,250) ~ "Alt",
                            TRUE ~ "Extrem")
	  )

Incidencia <- round(sum(mapa_N$NAcumulats, na.rm = T)/pob_total*1e5,0)
Incidencia_dia_anterior <- resum %>% ungroup() %>% filter(TipusCasData == max_data-7) %>% summarise(N = round(sum(NAcumulats, na.rm = TRUE)/pob_total*1e5,0)) %>% pull()
#min_green <- rescale(100,from=range(mapa_N$Incidencia, na.rm = TRUE))
#min_yellow <- rescale(300,from=range(mapa_N$Incidencia, na.rm = TRUE))
#min_red <- rescale(1000,from=range(mapa_N$Incidencia, na.rm = TRUE))

ggplot(mapa_N) +
   geom_sf(aes(fill = Incidencia),color="grey40",size=0.3) +
   scale_fill_gradientn(trans = "log10",
         		        na.value = "#1CA108",
                        colours = c("#1CA108","green","yellow","red","#40004D"),
#		            	values = c(35,100,300,1000,Inf),
		            	#values = color_values,
                        breaks = c(35,100,300,1000,3000),
                        limits = c(35,3000)
			          ) +
   theme_void() +
   labs(fill = "Incidencia",
        title = "Incidència COVID-19 últims 14 dies per cada 100.000 habitants",
        subtitle = paste0("Incidència a Catalunya: ",Incidencia, " (" ,sprintf("%+d",Incidencia-Incidencia_dia_anterior)," vs setmana anterior",")")) +
   theme(plot.title = element_text(family="Helvetica Neue LT Std", size = 14),
         plot.subtitle = element_text(family="Helvetica Neue LT Std", size = 12,face = "italic")) +
   annotation_custom(
	tableGrob(
         mapa_N %>% 
            as.data.frame() %>% 
            arrange(desc(Cens)) %>% 
            slice(1:7) %>% 
            select(NOM_MUNI,NAcumulats,Cens,Incidencia,Nivell,diff) %>%
            mutate(NAcumulats = paste(NAcumulats,diff,sep = " "), Incidencia = round(Incidencia)) %>%
            select(-diff) %>%
            rename(Municipi=NOM_MUNI,"Confirmats\n(diferencia 7d)"=NAcumulats,Incidència = Incidencia),
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

df <- filter(raw, MunicipiDescripcio == "Barcelona", TipusCasData >= max_data -28) %>% 
   group_by(TipusCasData, DistricteDescripcio) %>% 
   summarise(N = sum(NumCasos)) %>% 
   mutate(PC = N/sum(N))

p1 <- df %>%
   ggplot(.,aes(TipusCasData,PC)) + 
       geom_area(aes(color = reorder(DistricteDescripcio,PC), fill = reorder(DistricteDescripcio, PC)), alpha = 0.5) + 
       #geom_col(aes(y =  N), alpha = 0.2) +
       scale_fill_brewer(palette="Paired", name = "Districte") +
       scale_color_brewer(palette = "Paired", name = "Districte") +
       #scale_x_date(date_breaks = "2 days",date_labels = "%d/%m") +
       scale_x_date(date_breaks = "2 day",date_labels = "%d/%m", expand = c(0.01,0)) +
       scale_y_continuous(label = percent) + 
       labs(title = "% confirmats i núm. casos COVID-19 per districte de Barcelona. Últims 28 dies")

p2 <- df  %>% 
   ggplot(.,aes(TipusCasData,N)) + 
       geom_col(aes(y =  N), alpha = 0.2) +
       geom_text(data = . %>% group_by(TipusCasData) %>% summarise(N = sum(N)), aes( label = N, vjust = 1.5),colour = "grey20") + 
       scale_fill_brewer(palette="Paired") +
       scale_color_brewer(palette = "Paired") +
       scale_x_date(date_breaks = "2 day",date_labels = "%d/%m", expand = c(0.01,0)) +
       theme_void()

aligned_plots <- align_plots(p1, p2, align="hv", axis="tb")
ggdraw(aligned_plots[[1]]) + draw_plot(aligned_plots[[2]])

