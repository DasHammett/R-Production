library(reshape2)
library(magrittr)
library(lazyeval)
Raw.MMIK <- load_excel(file.choose(),sheet = 8)
colnames(Raw.MMIK) <- gsub(0,"no",names(Raw.MMIK))
Raw.MMIK <- filter(Raw.MMIK,grepl("Barcelona",Advisor.Site))
Raw.MMIK <- filter(Raw.MMIK,Fiscal.Week %in% tail(sort(unique(Fiscal.Week)),6))

Assure <- c(1,14,28:31) #From BVHQ_MMIK report
Knowledge <- c(1,14,32:43) #From BVHQ_MMIK report
Guidance <- c(1,14,44:49) #From BVHQ_MMIK report
Professionalism <- c(1,14,50:60) #From BVHQ_MMIK report
Holds <- c(1,14,61:63) #From BVHQ_MMIK report
Case.Duration <- c(1,14,68:87) #From BVHQ_MMIK report
Logging <- c(1,14,88:107) #From BVHQ_MMIK report
Tools <- c(1,14,108:116) #From BVHQ_MMIK report
Appeasements <- c(1,14,117:127) #From BVHQ_MMIK report
Consultations <- c(1,14,128:135) #From BVHQ_MMIK report
Ownership <- c(1,14,136:144) #From BVHQ_MMIK report
Compliance <- c(1,14,145:168) #From BVHQ_MMIK report
Attribute <- c(1,14,28,32,44,50,61,68,88,108,117,128,136,145) #From BVHQ_MMIK report
T1 <- "EMEA Tier 1 iOS Phone Spanish"
T2 <- "EMEA Tier 2 iOS Phone Spanish"
Mac <- "EMEA Tier 1 Mac+ Phone Spanish"

Drivers <- function(attribute,lob) {
   att <- attribute
   att2 <- names(Raw.MMIK)[attribute[3]]
   att3 <- names(Raw.MMIK)[attribute[1]] 
   MMIK <- Raw.MMIK %>% select(c(att)) %>% group_by_(c(att3))
   if(!missing(lob)){
      MMIK %>%
    	 filter(Advisor.Staff.Type == lob) %>%
    	 mutate_at(vars(4:ncol(.)),funs_(interp(~(round(sum(. == "Driver", na.rm = T)/sum(var == 0),2)),var = as.name(att2)))) %>%
    	 summarise_all(first) %>%
    	 select(-3) %>%
    	 melt(id.vars=c("Fiscal.Week","Advisor.Staff.Type")) %>%
    	 dcast(variable~Fiscal.Week,value.var = "value")
   } else {
      MMIK %>%
    	 summarise_at(vars(4:ncol(.)),funs_(interp(~(round(sum(. == "Driver", na.rm = T)/sum(var == 0),2)),var = as.name(att2)))) %>%
    	 melt() %>%
    	 dcast(variable~Fiscal.Week,value.var = "value")
   }
}

Raw.MMIK %>%
   select(c(Attribute)) %>%
  filter(grepl("Mac",Advisor.Staff.Type)) %>%
   group_by(Fiscal.Week) %>%
   mutate_at(vars(3:ncol(.)),funs(sum(. == 1)/sum(. != "N/A"))) %>%
   summarise_all(first) %>%
   select(-2) %>%
   melt() %>%
   dcast(variable~Fiscal.Week,value.var = "value")




######################

Raw.BVHQ <- load_excel(file.choose(),sheet = 9)

Raw.BVHQ %>%
  filter(grepl("Random|Coaching",Call.Monitor.Type),
         grepl("Remote",Monitor.Method),
         Site == "SBT (Barcelona)") %>%
  mutate(NS.Duration = as.numeric(Last.Modified..GMT. - Date.Created..GMT.)) %>%
  #filter(!(abs(NS.Duration - median(NS.Duration)) > 1*sd(NS.Duration))) %>% # Remove outliers
  filter(!NS.Duration %in% unique(boxplot.stats(as.numeric(NS.Duration))$out)) %>% # Remove outliers with IQR range
  group_by(Period) %>%
  #group_by(Overall.Critical,add = T) %>%
  summarise(N = n(),
            Min = min(NS.Duration),
            Q25 = quantile(NS.Duration,probs = 0.25),
            Median = median(NS.Duration, na.rm = T),
            Avg = mean(NS.Duration, na.rm = T),
            Q75 = quantile(NS.Duration,probs = 0.75),
            Max = max(NS.Duration)) %>%
  #arrange(Overall.Critical)
  melt(id.vars=c("Evaluator","Period")) %>%
  filter(variable == "Avg", Period > "2016P11") %>%
  ggplot(.,aes(Period,value,group=Evaluator,colour=Evaluator))+
  geom_line()+
  geom_point()

input.files <- list.files("/Users/jvidal/Desktop/ R Scripts/",pattern = "Case",full.names = T)
Raw.AHT <- lapply(input.files,function(x)load_excel(x,slice=-1))
Raw.AHT <- do.call(rbind,Raw.AHT)
Raw.AHT <- select(Raw.AHT,Case.ID,Average.Handle.Time)
Raw.complete <- inner_join(Raw.BVHQ,Raw.AHT, by = c("Case.Number" = "Case.ID"))

Raw.filtered <- Raw.complete %>%
  filter(grepl("Random|Coaching",Call.Monitor.Type),
         grepl("Remote",Monitor.Method),
         Site == "SBT (Barcelona)") %>%
  mutate(NS.Duration = as.numeric(Last.Modified..GMT. - Date.Created..GMT.),
         Diff = NS.Duration/Average.Handle.Time)

Raw.filtered %>%
  filter(!is.na(Diff),
         !Diff %in% unique(boxplot.stats(Diff)$out)) %>%
  mutate(Split = cut(Average.Handle.Time,breaks = c(seq(0,90,5)))) %>%
  ggplot(.,aes(Split,Diff))+geom_boxplot(varwidth = T)+scale_y_continuous(breaks = seq(0,10,1))

Raw.filtered %>% 
  filter(!is.na(Diff),
         Diff %in% unique(boxplot.stats(Diff)$out),
         !NS.Duration %in% unique(boxplot.stats(NS.Duration)$out),
         !Average.Handle.Time %in% boxplot.stats(Average.Handle.Time)$out) %>%
  mutate(Bucket = cut(Average.Handle.Time,seq(0,50,5))) %>%
  group_by(Evaluator,Bucket) %>% 
  ggplot(aes(Evaluator,Diff))+
  geom_boxplot(outlier.color = "grey70")+
  stat_summary(geom = "text", 
               fun.y = function(x)quantile(x,c(0.25,0.5,0.75)),
               aes(label = round(..y..,2)),
               colour="grey50",
               size=3,
               position = position_nudge(y = 0.30))+
  stat_summary(geom = "text",
               fun.data = give.n,
               size = 3)+
  scale_y_continuous(breaks = seq(0,10,1))+
  theme(axis.text.x = element_text(angle=45,vjust=1,hjust=1))+
  facet_grid(Overall.Critical~Bucket)
