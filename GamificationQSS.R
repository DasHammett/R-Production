library(dplyr)
library(tidyr)
setwd("/Users/etettey/Desktop/R scripts/") # Set working directory

file <- file.path(file.choose()) # Load PartnerAnalytics csv file (Attribute_Heat_data.csv)
system(paste("iconv -f UTF-16LE -t UTF-8",file,"> raw_data.csv", sep = " ")) # Convert UTF-16LE encoding to UTF-8 using bash
rm(file) # Remove object file as it is no longer needed

Raw.data <- read.csv("raw_data.csv",header = T, stringsAsFactors = F, sep = "\t", dec = ".") # Load file into object Raw.data

Raw.data <- Raw.data %>%
  mutate_all(funs(gsub("%","",.))) %>% # Remove % sign
  mutate_at(vars(case_id,Attr..,Adoption..Self.,Adoption..IQE.),as.numeric) %>% # Transform columns of intereset to numeric
  mutate_if(is.numeric,funs(./100)) %>% # Divide numeric columns from previous step by 100 so everything is base 1
  select(Attribute..copy.,case_id,Attr..,Adoption..Self.,Adoption..IQE.,L1.Mgr,Site) # Select columns needed for script

Gamification <- list() # Create empty list to avoid cluttering the Global Environment. Next objects will be stored inside list

## Knowledge
Gamification$K <- Raw.data %>% # Store output in Gamification list with name "K"
  filter(Attribute..copy. == "Knowledge") %>% # Filter Attribute to Knowledge
  distinct(case_id, .keep_all = T) %>% # Remove duplicated case_id
  group_by(Site,L1.Mgr) %>% # Do calculations per TL
  summarise(Knowledge = mean(Attr.., na.rm = T)) # Calculate average of Knowledge

## Compliance
Gamification$C <- Raw.data %>%
  filter(Attribute..copy. == "Compliance") %>%
  distinct(case_id, .keep_all = T) %>%
  group_by(Site,L1.Mgr) %>%
  summarise(Compliance = mean(Attr.., na.rm = T))

## Adoption 
Gamification$A <- Raw.data %>%
  mutate(Adoption = ifelse(is.na(Adoption..Self.), Adoption..IQE.,Adoption..Self.)) %>% # Create new column Adoption with the scores from self and IQE evaluations
  distinct(case_id, .keep_all = T) %>%
  group_by(Site,L1.Mgr) %>%
  summarise(Adoption = mean(Adoption, na.rm = T))

## Merge all together
Gamification$QSS <- left_join(Gamification$K,Gamification$C, by = c("Site","L1.Mgr")) %>% # Join Knowledge and Compliance objects by TL and Number of evaluations (to avoid multiple columns)
  left_join(.,Gamification$A, by = c("Site","L1.Mgr")) %>% # Join previous step with Adoption
  mutate(QSS = Knowledge * 0.25 + Adoption * 0.25 + Compliance * 0.5,
         Date = Sys.Date()) %>%
  select(Site, L1.Mgr, Date, everything()) %>%
  gather(KPI, Value, -Site:-Date)

## Create data frame with Team names
Gamification$Team_Names <- data.frame(
  TL = c("ismael rios",
         "Irene Ramos",
         "Carolina Uran Mannocci",
         "Pablo Tello",
         "Alejandro Perez",
         "Rodrigo Sarceno",
         "Mario Casone",
         "David Ricardo Vera Vasquez",
         "Debora Diaz Perez",
         "Cristian Baron Villanueva",
         "Eva Marin"),
  Name = c("Callback Mountain",
           "iTeam Superheroes",
           "Rocket Raccoon",
           "The First Order",
           "Mac+ Gyvers",
           "Special Forces",
           "Tanti Baci",
           "Los Rockmanz",
           "Equipo A",
           "Apple Invaders",
           "iBlondie"),
  stringsAsFactors = F # Do not create factors from strings
)

## Join Team_Names table with QSS and replace L1.Mgr column with their corresponding names
Gamification$QSS <- left_join(Gamification$QSS, Gamification$Team_Names, by = c("L1.Mgr" = "TL")) %>% 
  mutate(Name = coalesce(Name, L1.Mgr)) %>% # Use L1.Mgr name if no Team_Name is provided
  select(-L1.Mgr) %>% 
  select(Site, Name, Date, everything()) %>%
  filter(Name %in% Gamification$Team_Names$Name | Site == "SBT (Lisbon)") 

## Write output file
write.csv2(Gamification$QSS,"Gamification_QSS.csv",row.names = F)


## Fake data for 10 days
Gamification$Days <- rep(Sys.Date()-1:10)
Gamification$Fake <- lapply(Gamification$Days, function(x) mutate(Gamification$QSS, Date = x, L1.Mgr = LETTERS[1:13])) %>% bind_rows()
Gamification$Fake <- Gamification$Fake %>% 
  group_by(Date) %>% 
  mutate_at(vars(Knowledge, Compliance, Adoption), funs(.*runif(13,min=0,max=1))) %>% 
  mutate(QSS = Knowledge * 0.25 + Adoption * 0.25 + Compliance * 0.5)

Gamification$Fake <- Gamification$Fake %>% gather(KPI, value, -Site:-Date)
write.csv2(Gamification$Fake,"Gamification_QSS.csv",row.names = F)
