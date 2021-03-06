library(dplyr)
library(ggplot2)
library(RcppRoll)
library(lubridate)
library(scales)
library(jsonlite)
library(ggrepel)
library(stringr)
library(tidyr)

raw <- read.csv("https://analisi.transparenciacatalunya.cat/api/views/jj6z-iyrp/rows.csv?accessType=DOWNLOAD&sorting=true", header = TRUE, sep=",")
raw$TipusCasData <- dmy(raw$TipusCasData)
raw$ComarcaDescripcio <- trimws(raw$ComarcaDescripcio,whitespace=" ")
raw$MunicipiDescripcio <- str_to_title(raw$MunicipiDescripcio)

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
          RollingMean7 = roll_mean(NumCasos, 7, align = "right", fill = NA),
          MunicipiDescripcio = factor(MunicipiDescripcio, levels = top9))

# Crear mitjana mobil sense agregacio per municipi
rolling14_all <-  raw %>% 
   filter(MunicipiDescripcio %in% top9, 
	  !grepl("Sospitós", TipusCasDescripcio),
	  TipusCasData != max(TipusCasData)) %>%  
   arrange(TipusCasData) %>% 
   group_by(TipusCasData) %>% 
   summarise(NumCasos = sum(NumCasos)) %>% 
   mutate(Rolling = roll_mean(NumCasos,7,align="right",fill=NA))

# Funcio auxiliar per a etiquetar els facets de ggplot

etiquetes <- function(x) { 
   lapply(x, function(y) {
      tolabel <- data.frame(MunicipiDescripcio = y) %>%
                 left_join(resum_ciutat)
      paste(unique(tolabel$MunicipiDescripcio),
	    "\nCasos totals:", format(max(tolabel$CumSum), big.mark = "."),
	    "\nMitjana mòbil 7d: ", round(tail(tolabel$RollingMean7,1),0), 
	    sep = " ")
      }
   )
}

# Creació del gràfic
ggplot(resum_ciutat, aes(TipusCasData, NumCasos)) +
   geom_point(data = . %>% do(.[round(seq(1,nrow(.),length.out = 10)),]),fill="grey80", shape = 21, size = 1.1, color = "grey80") +
   geom_line(aes(color = "color.diari"), alpha = 0.5, size = 0.3) +
   geom_line(aes(y=RollingMean7, color = "color.mitjana")) +
   geom_text(data = . %>% do(.[round(seq(1,nrow(.),length.out = 10)),]), aes(label=NumCasos),size=3,color = "grey50",vjust=-1) + 
#   stat_smooth(geom = "line", alpha = 0.5, size = 0.2, aes(color = "color.smooth")) +
   labs(title=paste("Top municipis afectats per COVID-19 a Catalunya",
		    "\nNúm. Casos confirmats:", format(sum(resum_ciutat$NumCasos),big.mark = "."),
		    "\nMitjana mòbil 7 dies:", round(tail(rolling14_all$Rolling,1),0), "casos diaris",
		    sep=" "),
	caption = paste0("Última actualització de ",max(resum_ciutat$TipusCasData)),
	x = element_blank(),
	y = element_blank()
	) + 
   facet_wrap(~MunicipiDescripcio, scales = "free_y", labeller = labeller(MunicipiDescripcio = etiquetes)) +
   theme_bw() +
   theme(legend.margin=margin(b = -0.6, t = -0.5, unit = "cm"),
         legend.position = "top",
         legend.justification = "right",
	 legend.key = element_rect(fill=NA),
         panel.grid.major = element_blank(), 
         panel.grid.minor = element_blank(),
         strip.background = element_blank(),
         strip.text = element_text(size=11),
	 plot.margin=unit(c(0.1,0.5,0.1,0.1),"cm")) + 
   scale_color_manual(values = c("color.diari" = "grey80",
                                 "color.mitjana" = "#1279A1",
				 "color.smooth" = "red"),
                      labels = c("Casos diaris", "Mitjana mòbil 7 dies", "Tendencia"),
                      name = "") +
  scale_y_continuous(expand=c(0.2,0.4)) +
  scale_x_date(labels = label_date_short(), date_breaks = "2 month")

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



world <- fromJSON("https://pomber.github.io/covid19/timeseries.json")
world[names(world) == ""] <- NULL
world <- bind_rows(world, .id = "Country")

world <- world %>%
   group_by(Country) %>%
#   filter(confirmed > 0) %>%
   mutate(row_num = row_number(),
	  date = ymd(date))

top9_world <- world %>%
   group_by(Country) %>%
   summarise(N = max(confirmed)) %>%
   arrange(desc(N)) %>%
   top_n(20) %>%
   pull(Country)

etiquetes <- function(x) {
   lapply(x, function(y) {
      tolabel <- data.frame(Country = y) %>%
                 left_join(world)
      paste(unique(tolabel$Country),
	    paste("Total cases", format(max(tolabel$confirmed),big.mark = "."), sep = " "),
	    paste("Confirmed cases last day:", format(summarise(tolabel,Daily = confirmed-lag(confirmed)) %>% tail(1),big.mark = "."), sep = " "),
	    sep = "\n")
      }
   )
}

# Top 9 amb projecció Gompertz
library(nls2)
world %>%
   filter(Country %in% top9_world) %>%
   mutate(Country = factor(Country, level = top9_world)) %>% {
   ggplot(.,aes(row_num,confirmed)) +
#   geom_line(stat = "smooth",
#             method = "nls2",
#             formula = y ~ p1 * exp(-p2*exp(-p3*x)),
#             method.args = list(nls.control(maxiter = 200),
#        				    #minFactor = 1e-10),
#        		#	start=c(p1=max(.$confirmed), 
#               		#		p2= log(max(.$confirmed)), 
#        		#		p3 = 0.018)
#			        start = data.frame(p1 = c(1,1e8), p2 = c(1, 100), p3 = c(0.01, 1))
#        			),
#             se = FALSE,
#             color = "grey20",
#             alpha = 0.5,
#             linetype = "dashed",
#             fullrange = TRUE
#            ) +
   geom_line(stat = "smooth",
	     method = "nls",
	     formula = pmax(y,0.01) ~ SSgompertz(x, p1, p2, p3),
	     se = FALSE,
	     color = "grey40",
	     fullrange = TRUE,
	     linetype = "dashed") +
   geom_line(color = "#1279A1", size = 0.6) +
   facet_wrap(~Country, scale = "free_y", labeller = labeller(Country = etiquetes)) +
   labs(title =  "Top countries for COVID-19 confirmed cases and projection",
        subtitle = expression(Forecast~fitted~from~a~Gompertz~"function:"~N~"*"~italic(e)^{-b~"*"~italic(e)^{-k~"*"~t}}),
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
	 plot.subtitle = element_text(size = 12,vjust = 4),
	 plot.margin=unit(c(0.1,0.5,0.1,0.1),"cm")
   )
   }

vaccinated <- fromJSON("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.json",flatten = TRUE)
vaccinated <- unnest(vaccinated,data)
vaccinated <- filter(vaccinated, total_vaccinations > 0)

vaccinated_date <- vaccinated %>% group_by(iso_code) %>% filter(date == min(date)) %>% select(country,date) %>% mutate(country = gsub("United States","US",country))
vaccinated_date <- filter(vaccinated_date, country %in% top9_world)
colnames(vaccinated_date)[colnames(vaccinated_date) == "country"] <- "Country"
vaccinated_date$Country <- factor(vaccinated_date$Country, levels = top9_world)

# Top 9 diari amb mitjana mobil
resum_world <- world %>% 
   filter(Country %in% top9_world) %>%
   arrange(date) %>% 
   group_by(Country, date) %>%
   summarise(confirmed = sum(confirmed)) %>%
   mutate(daily = confirmed - lag(confirmed)) %>%
   filter(daily >= 0) %>%
   mutate(RollingMean14 = roll_mean(daily, 14, align = "right", fill = NA),
          Country = factor(Country, levels = top9_world))

ggplot(resum_world, aes(date, daily)) +
   geom_point(data = . %>% do(.[round(seq(1,nrow(.),length.out = 10)),]),
	      fill="grey80", 
	      shape = 21, 
	      size = 1.1, 
	      color = "grey80") +
   geom_line(aes(color = "color.diari"), 
	     size = 0.3) +
#   geom_line(aes(y=roll_mean(daily, n = 14, align = "right", fill=NA), 
#		 color = "color.mitjana")) +
   geom_line(data = resum_world, aes(y = RollingMean14, color = "color.mitjana")) +
   geom_text_repel(data = . %>% do(.[round(seq(1,nrow(.),length.out = 10)),]),
                   aes(label=format(daily,big.mark = ".")),
                   size=3,
                   color = "grey50") +
   geom_vline(data = vaccinated_date, aes(xintercept = as.Date(date), color = "color.vacuna"), alpha = 0.5, show.legend = FALSE, linetype = "longdash") +
   labs(title="Top countries for COVID-19 confirmed cases",
	subtitle = paste(
		    "Overall Confirmed cases:", format(sum(resum_world$daily,na.rm = TRUE),big.mark = "."),
		    "\nNew cases from last day:", resum_world %>% 
						     filter(date %in% tail(date,2)) %>% 
						     group_by(date) %>% 
						     summarise(N = sum(confirmed)) %>% 
						     mutate(lag = N - lag(N)) %>% 
						     filter(lag != "NA") %>%
						     pull(lag) %>%
						     format(.,big.mark = "."),
		    sep=" "),
	caption = paste0("Last update from: ",max(resum_world$date)),
	x = element_blank(),
	y = element_blank()
	) + 
   facet_wrap(~Country, scales = "free_y", labeller = labeller(Country = etiquetes)) +
   theme_bw() +
   theme(legend.margin=margin(b = -0.4, t= -0.9, unit = "cm"),
         legend.position = "top",
         legend.justification = "right",
	 legend.key = element_rect(fill=NA),
         panel.grid.major = element_blank(), 
         panel.grid.minor = element_blank(),
         strip.background = element_blank(),
         strip.text = element_text(size=11,hjust = 0),
	 plot.subtitle = element_text(vjust = 2,face = "italic"),
	 plot.margin=unit(c(0.1,0.5,0.1,0.1),"cm")) +
   scale_color_manual(values = c("color.diari" = "grey80",
                                 "color.mitjana" = "#1279A1",
				                 "color.vacuna" = "red"),
                      labels = c("Daily confirmed", "Rolling 14d average","vaccination start"),
                      name = "") +
  scale_y_continuous(expand=c(0.2,0.4),label = label_number_si()) +
  scale_x_date(date_breaks = "2 month", labels = label_date_short())
