library(reshape2)
library(magrittr)
library(tidyr)
library(scales)
library(stringr)
library(purrr)
library(writexl)

setwd("/Users/jvidal/Desktop/ R scripts/git/R")

# Create object with name of LOBs for later filtering
lobs <- c("EMEA Tier 1 iOS Phone Spanish","EMEA Tier 1 Mac+ Phone Spanish","EMEA Tier 2 iOS Phone Spanish")

# Load and process PQS data for both internal and IQE evaluations.
Raw.MMIK <- read.csv2(file.choose(),header = T,stringsAsFactors = F) #Load PQS-NS_IQE.csv in SUPPORT_STAFF/TMS_TEAM/iPerform
colnames(Raw.MMIK)[1] <- "Fiscal.Week"
colnames(Raw.MMIK) <- gsub(0,"no",names(Raw.MMIK))
colnames(Raw.MMIK) <- gsub("^.*\\.{3}|\\.$","",colnames(Raw.MMIK))
colnames(Raw.MMIK) <- make.names(names(Raw.MMIK),unique = T)
Raw.MMIK <- filter(Raw.MMIK,grepl("Barcelona",Advisor.Site))
colnames(Raw.MMIK) <- gsub("\\.{2}","\\.",colnames(Raw.MMIK))
colnames(Raw.MMIK) <- make.unique(colnames(Raw.MMIK))
Raw.MMIK$Call.Duration <- sapply(strsplit(Raw.MMIK$Call.Duration,":"), function(x)as.numeric(x[1])*60+as.numeric(x[2])+as.numeric(x[3])/60) #Convert time to decimal
Raw.MMIK$Period <- str_extract(Raw.MMIK$Fiscal.Week,"[[:digit:]]+P[[:digit:]]{2}")
Raw.MMIK <- Raw.MMIK %>% mutate(Quarter = case_when(grepl("P01|P02|P03",Period) ~ paste0(str_extract(Fiscal.Week,"[[:digit:]]{4}"),"Q1"),
                                                    grepl("P04|P05|P06",Period) ~ paste0(str_extract(Fiscal.Week,"[[:digit:]]{4}"),"Q2"),
                                                    grepl("P07|P08|P09",Period) ~ paste0(str_extract(Fiscal.Week,"[[:digit:]]{4}"),"Q3"),
                                                    grepl("P10|P11|P12",Period) ~ paste0(str_extract(Fiscal.Week,"[[:digit:]]{4}"),"Q4")),
                                Issue_Reason = if_else(Issue == "",Reason,Issue))
Attributes <- list()
Attributes$Assure <- select(Raw.MMIK, Assure:(Knowledge-1))
Attributes$Knowledge <- select(Raw.MMIK, Knowledge:(Guidance-1))
Attributes$Guidance <- select(Raw.MMIK, Guidance:(Professionalism-1))
Attributes$Professionalism <- select(Raw.MMIK, Professionalism:(Holds-1))
Attributes$Holds <- select(Raw.MMIK, Holds:(Case.Duration-1))
Attributes$Case.Duration <- select(Raw.MMIK, Case.Duration:(Logging-1))
Attributes$Logging <- select(Raw.MMIK, Logging:(Tools-1))
Attributes$Tools <- select(Raw.MMIK, Tools:(Refunds-1))
Attributes$Refunds <- select(Raw.MMIK, Refunds:(Consultations-1))
Attributes$Consultations <- select(Raw.MMIK, Consultations:(Ownership-1))
Attributes$Ownership <- select(Raw.MMIK, Ownership:(Compliance-1))
Attributes$Compliance <- select(Raw.MMIK, Compliance:(Was.the.issue.resolved.during.the.interaction-1))
Attributes <- map(Attributes,colnames)
Attributes$Attributes <- names(Attributes)


## Attribute results per advisor
NS <- Raw.MMIK %>%
  filter(!grepl("Calibration|DSAT",Call.Monitor.Type),
         Advisor.Staff.Type %in% lobs) %>%
  select(Quarter, Period, Fiscal.Week, Advisor.Staff.Type, Call.Monitor.Type, Advisor, Advisor.DSID, Attributes$Attributes) %>%
  mutate_at(vars(Attributes$Attributes,Advisor.DSID),funs(as.numeric)) %>%
  mutate(QSS = rowMeans(.[-1:-7],na.rm = T)) %>%
  group_by(Quarter,Period, Fiscal.Week, Advisor.Staff.Type, Advisor, Advisor.DSID) %>%
  mutate(Evaluations = n()) %>%
  summarise_at(vars(Attributes$Attributes,Evaluations,QSS), funs(mean(.,na.rm = T)))

# Masterfile QT from MasterfileQ.R
excel_file <- file.choose()
Raw.MF <- lapply(c("CFST CSAT (Case ID)","Raw data AHT"),function(x)load_excel(excel_file,sheet=x)) #Load Masterfile QT AHT and CSAT tabs
rm(excel_file)

Raw.MF[[3]] <- Raw.MF[[2]] %>% #AHT-ACW
  group_by(Fiscal.Period.Week..Name.,
           Queue.Local.Segment,
           Staffed.Type,
           Advisor.Full.Name,
           Advisor.DS.Id) %>%
  summarise(Call.Volume = sum(ACD.Calls.Answered,na.rm = T),
            AHT = sum(Minutes.AHT,na.rm = T)/60/Call.Volume,
            ACW = sum(Total.ACD.After.Call.Work,na.rm = T)/60/Call.Volume,
            Escalation = sum(Transfers.to.Tier.2,na.rm = T)/Call.Volume,
            TakeOver = sum(TakeOver.Rate.Del,na.rm = T)/sum(TakeOver.Rate.Div,na.rm = T)) %>%
  filter(!is.na(AHT))

Raw.MF[[4]] <- Raw.MF[[1]] %>% #CSAT-IR
  filter(Site == "SBT (Barcelona)") %>%
  group_by(Fiscal.Week,
           Queue.Type.Name,
           Staff.Type.Name,
           Agent.Full.Name,
           Agent.DS.ID) %>%
  summarise(CSAT.Surveys = sum(Survey.Response.Count,na.rm = T),
            IR.Surveys = sum(Issue.Resolution.Response.Count,na.rm = T),
            CSAT = sum(CSAT,na.rm = T)/CSAT.Surveys,
            IR = sum(Issue.Resolution,na.rm = T)/IR.Surveys) %>%
  ungroup() %>%
  filter(CSAT.Surveys != 0 && IR.Surveys != 0)

Raw.MF[[5]] <- full_join(Raw.MF[[4]],Raw.MF[[3]],by=c("Fiscal.Week" = "Fiscal.Period.Week..Name.",
                                       "Queue.Type.Name" = "Queue.Local.Segment",
                                       "Staff.Type.Name" = "Staffed.Type",
                                       "Agent.DS.ID" = "Advisor.DS.Id",
                                       "Agent.Full.Name" = "Advisor.Full.Name")
          ) %>%
  select(Fiscal.Week,Queue.Type.Name,Staff.Type.Name,Agent.Full.Name,Agent.DS.ID,everything())

Raw.MF[[5]] <- Raw.MF[[5]] %>%
  filter(Staff.Type.Name %in% lobs) %>%
        # Queue.Type.Name %in% c("EMEA Tier 1 iOS Phone Spanish","EMEA Tier 2 iOS Phone Spanish","EMEA Tier 1 Mac Phone Spanish")) %>%
  ungroup() %>%
  mutate(Fiscal.Week = gsub("FY","20",Fiscal.Week),
         Fiscal.Week = gsub("W0","W",Fiscal.Week))

Grand_MF <- full_join(NS,Raw.MF[[5]],by = c("Fiscal.Week" = "Fiscal.Week",
                                        "Advisor.Staff.Type" = "Staff.Type.Name",
                                        "Advisor" = "Agent.Full.Name", 
                                        "Advisor.DSID" = "Agent.DS.ID")
                  ) %>%
  ungroup() %>%
  group_by(Fiscal.Week) %>%
  fill(Quarter,Period) %>%
  select(Quarter:Advisor.Staff.Type,Queue.Type.Name,everything()) %>%
  filter(Period >= "2018P11") %>%
  na_if("NA") %>%
  group_by(Advisor.DSID,Fiscal.Week) %>%
  mutate(CSAT_ST = weighted.mean(CSAT,CSAT.Surveys,na.rm = T),
         IR_ST = weighted.mean(IR,IR.Surveys,na.rm = T),
         AHT_ST = weighted.mean(AHT,Call.Volume,na.rm = T),
         ACW_ST = weighted.mean(ACW,Call.Volume,na.rm = T),
         Escalation_ST = weighted.mean(Escalation,Call.Volume,na.rm = T),
         TakeOver_ST = weighted.mean(TakeOver,Call.Volume, na.rm = T))
write_xlsx(Grand_MF,"Grand_Masterfile.xlsx")
#write.csv2(Grand_MF,"Grand_Masterfile.csv",row.names = F)
