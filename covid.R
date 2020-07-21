library(dplyr)
library(ggplot2)
library(RcppRoll)
library(lubridate)
library(scales)
library(jsonlite)

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
          !grepl("Sospitós", TipusCasDescripcio),
	  TipusCasData != max(TipusCasData)) %>%
   arrange(TipusCasData) %>% 
   group_by(MunicipiDescripcio, TipusCasData) %>%
   summarise(NumCasos = sum(NumCasos)) %>%
   mutate(CumSum = cumsum(NumCasos),
          RollingMean14 = roll_mean(NumCasos, 14, align = "right", fill = NA),
          MunicipiDescripcio = factor(MunicipiDescripcio, levels = top9))

# Crear mitjana mobil sense agregacio per municipi
rolling14_all <-  raw %>% 
   filter(MunicipiDescripcio %in% top9, 
	  !grepl("Sospitós", TipusCasDescripcio),
	  TipusCasData != max(TipusCasData)) %>%  
   arrange(TipusCasData) %>% 
   group_by(TipusCasData) %>% 
   summarise(NumCasos = sum(NumCasos)) %>% 
   mutate(Rolling = roll_mean(NumCasos,14,align="right",fill=NA))

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
   geom_point(data = . %>% do(.[round(seq(1,nrow(.),length.out = 10)),]),fill="grey80", shape = 21, size = 1.1, color = "grey80") +
   geom_line(aes(color = "color.diari"), alpha = 0.5, size = 0.3) +
   geom_line(aes(y=RollingMean14, color = "color.mitjana")) +
   geom_text(data = . %>% do(.[round(seq(1,nrow(.),length.out = 10)),]), aes(label=NumCasos),size=3,color = "grey50",vjust=-1) + 
   #stat_smooth(geom = "line", alpha = 0.5, size = 0.2, aes(color = "color.smooth")) +
   labs(title=paste("Top municipis afectats per COVID-19 a Catalunya",
		    "\nNúm. Casos confirmats:", sum(resum_ciutat$NumCasos),
		    "\nMitjana últims 14 dies:", round(tail(rolling14_all$Rolling,1),0), "casos diaris",
		    sep=" "),
	caption = paste0("Última actualització de ",max(resum_ciutat$TipusCasData)),
	x = element_blank(),
	y = element_blank()
	) + 
   facet_wrap(~MunicipiDescripcio,ncol = 3, scales = "free_y", labeller = labeller(MunicipiDescripcio = etiquetes)) +
   theme_bw() +
   theme(legend.margin=margin(b = -0.6, t = -0.5, unit = "cm"),
         legend.position = "top",
         legend.justification = "right",
         panel.grid.major = element_blank(), 
         panel.grid.minor = element_blank(),
         strip.background = element_blank(),
         strip.text = element_text(size=11)) +
   scale_color_manual(values = c("color.diari" = "grey80",
                                 "color.mitjana" = "#1279A1",
				 "color.smooth" = "red"),
                      labels = c("Casos diaris", "Mitjana casos diaris", "Tendencia"),
                      name = "") +
  scale_y_continuous(expand=c(0.2,0.4))

us <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv", header = TRUE, sep=",")

us <-  us %>% 
   select(c(7,12:ncol(.))) %>%
   pivot_longer(2:ncol(.)) %>%
   #filter(Province_State == "California", value > 0) %>%
   group_by(Province_State, name) %>%
   summarise(value = sum(value)) %>%
   mutate(name = gsub("X","",name),
	  name = mdy(name)) %>%
   ungroup() %>%
   arrange(name) %>%
   group_by(Province_State) %>%
   mutate(row_num = row_number())



world <- fromJSON("https://pomber.github.io/covid19/timeseries.json") %>% bind_rows(.id = "Country")
world <- world %>%
   group_by(Country) %>%
#   filter(confirmed > 0) %>%
   mutate(row_num = row_number(),
	  date = ymd(date))

top9_world <- world %>%
   group_by(Country) %>%
   summarise(N = max(confirmed)) %>%
   arrange(desc(N)) %>%
   top_n(9) %>%
   pull(Country)
top9_world <-  factor(top9_world, level = top9_world)

etiquetes <- function(x) {
   lapply(x, function(y) {
      tolabel <- data.frame(Country = y) %>%
                 left_join(world)
      paste(unique(tolabel$Country),
	    paste("Total cases", format(max(tolabel$confirmed),big.mark = "."), sep = " "),
	    paste("Last day increased by:", format(summarise(tolabel,Daily = confirmed-lag(confirmed)) %>% tail(1),big.mark = "."), sep = " "),
	    sep = "\n")
      }
   )
}

world %>%
   filter(Country %in% top9_world) %>%
   mutate(Country = factor(Country, level = top9_world)) %>% {
   ggplot(.,aes(row_num,confirmed)) +
   geom_line(stat = "smooth",
	     method = "nls",
	     formula = y ~ p1 * exp(-p2*exp(-p3*x)),
	     method.args = list(nls.control(maxiter = 1000,
					    minFactor = 1e-10),
				start=c(p1=max(.$confirmed), p2=18, p3 = 0.02)
				),
	     se = FALSE,
	     color = "grey20",
	     alpha = 0.5,
	     linetype = "dashed",
	     fullrange = TRUE
            ) +
#   geom_line(stat = "smooth",
#	     method = "nls",
#	     formula = pmax(y,0.0001) ~ SSgompertz(x, p1, p2, p3),
#	     se = FALSE,
#	     color = "grey80",
#	     fullrange = TRUE,
#	     linetype = "dashed") +
   geom_line(color = "#1279A1", size = 0.6) +
   facet_wrap(~Country, scale = "free_y", labeller = labeller(Country = etiquetes)) +
   labs(title =  "Top countries for COVID-19 confirmed cases and projection",
        subtitle = expression(fitted~from~a~Gompertz~"function:"~N~"*"~italic(e)^{-b~"*"~italic(e)^{-k~"*"~t}}),
        x = element_blank(),
        y = element_blank(),
        caption = paste("last available data from:",max(.$date),sep=" ")) +
   scale_y_continuous(labels = label_number_si(accuracy = 0.1)) +
   scale_x_continuous(breaks = seq(1,501,100), 
		      labels = format(seq(as.Date(1,origin = min(.$date)),
					  as.Date(501,origin=min(.$date)),
					  100),
				      "%Y-%m"), 
		      limits=c(0,501)
		      )+
   theme_bw() +
   theme(legend.margin=margin(b = -0.6, t = -0.5, unit = "cm"),
         legend.position = "top",
         legend.justification = "right",
         panel.grid.major = element_blank(), 
         panel.grid.minor = element_blank(),
         strip.background = element_blank(),
         strip.text = element_text(size=11,hjust = 0),
	 plot.subtitle = element_text(size = 12,vjust = 4)
   )}
