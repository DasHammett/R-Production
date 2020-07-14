library(dplyr)
library(ggplot2)
library(RcppRoll)
library(lubridate)

raw <- read.csv("https://analisi.transparenciacatalunya.cat/api/views/jj6z-iyrp/rows.csv?accessType=DOWNLOAD&sorting=true", header = TRUE, sep=",")
raw$TipusCasData <- dmy(raw$TipusCasData)

# Selecció del top9 municipal
top9 <- raw %>%
   filter(!grepl("Sospitós", TipusCasDescripcio), MunicipiDescripcio != "No classificat") %>%
   group_by(MunicipiDescripcio) %>%
   summarise(Casos = sum(NumCasos)) %>%
   arrange(desc(Casos)) %>%
   top_n(9) %>%
   pull(MunicipiDescripcio)
  
# Crear DF final amb sumaritzacions per al grafic
resum_ciutat <- raw %>% 
   filter(MunicipiDescripcio %in% top9,
          !grepl("Sospitós", TipusCasDescripcio)) %>%
   arrange(TipusCasData) %>% 
   group_by(MunicipiDescripcio, TipusCasData) %>%
   summarise(NumCasos = sum(NumCasos)) %>%
   mutate(CumSum = cumsum(NumCasos),
          RollingMean14 = roll_mean(NumCasos, 14, align = "right", fill = NA),
          MunicipiDescripcio = factor(MunicipiDescripcio, levels = top9))

# Crear mitjana mobil sense agregacio per municipi
rolling14_all <-  raw %>% 
   filter(MunicipiDescripcio %in% top9, 
	  !grepl("Sospitós", TipusCasDescripcio)) %>%  
   arrange(TipusCasData) %>% 
   group_by(TipusCasData) %>% 
   summarise(RollingMean14 = sum(NumCasos)) %>% 
   mutate(Rolling = roll_mean(RollingMean14,14,align="right",fill=NA))

# Funcio auxiliar per a etiquetar els facets de ggplot
etiquetes <- function(x) {
   lapply(x, function(y) {
      tolabel <- data.frame(MunicipiDescripcio = y) %>%
                 left_join(resum_ciutat)
      paste(unique(tolabel$MunicipiDescripcio),
	    "\nCasos totals:", max(tolabel$CumSum), 
	    "\nMitjana 14d: ", round(tail(tolabel$RollingMean14,1),0), 
	    sep = " ")
      }
   )
}

# Creació del gràfic
ggplot(resum_ciutat, aes(TipusCasData, NumCasos)) +
   geom_line(aes(color = "color.diari"), alpha = 0.5, size = 0.3) +
   geom_line(aes(y=RollingMean14, color = "color.mitjana")) +
   #stat_smooth(geom = "line", alpha = 0.5, size = 0.2, aes(color = "color.smooth")) +
   labs(title=paste("Top 9 municipis afectats per COVID-19 a Catalunya",
		    "\nNum Casos:", sum(resum_ciutat$NumCasos),
		    "\nMitjana ultims 14 dies:", round(tail(rolling14_all$Rolling,1),0), "casos diaris",
		    sep=" ")
                    ) + 
   facet_wrap(~MunicipiDescripcio,ncol = 3, scales = "free_y", labeller = labeller(MunicipiDescripcio = etiquetes)) +
   theme(legend.margin=margin(b = -0.6, t = -0.5, unit = "cm"),
         legend.position = "top",
         legend.justification = "right",
         panel.grid.major = element_blank(), 
         panel.grid.minor = element_blank(),
         strip.background = element_blank(),
         strip.text = element_text(size=10)) +
   scale_color_manual(values = c("color.diari" = "grey80",
                                 "color.mitjana" = "#1279A1",
				 "color.smooth" = "red"),
                      labels = c("Casos diaris", "Mitjana casos diaris", "Tendencia"),
                      name = "")
