library(readxl)
library(reshape2)
library(grid)
library(gridExtra)
library(scales)
setwd("/Users/jvidal/Desktop/ R Scripts/Shrinkage FY18Q3")

data_preparation <- function(path){
  paths <- dir(path, full.names = T)
  excel <- list()
  excel <- lapply(paths,function(f)
      f %>%
        excel_sheets() %>%
        set_names() %>%
        map_df(~read_excel(path = f, sheet = .x, range = "J3:X400"), .id = "sheet") %>%
        filter(!is.na(Calculation), 
               Calculation != "Calculation") %>%
        mutate_at(vars(4:ncol(.)), funs(as.numeric)) %>%
        mutate(Period = stringr::str_extract(f,"P[:digit:]{2}")) %>%
        select(sheet,Period,everything())
  )
  Raw.data <- bind_rows(excel)
  colnames(Raw.data) <- make.names(names(Raw.data),unique=T)
  names(Raw.data)[names(Raw.data) == "sheet"] <- "LOB"
  names(Raw.data)[names(Raw.data) == "Calculation"] <- "Hour"
  Raw.data$Day <- factor(Raw.data$Day,levels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
  Raw.data$Hour <- gsub("([[:digit:]])(-)","\\1 \\2",Raw.data$Hour) #Make sure there is a space between nubmer and dash
  Raw.data <<- Raw.data %>% mutate(Tot.Shrink.wo.Unpl = rowSums(.[c(6,8:17)]))
}

data_preparation("Raw_files")

lobs <- unique(Raw.data$LOB)
charts <- function(grouping,lob){
  Aux <- list()
  group1 <- c(grouping,"Lateness","Aux.7.Proj","Aux.8.IRC","Aux.9.B.Off")
  group2 <- c(grouping,"Aux.0.Def","Aux.1.Meet","Aux.3.Out","Aux.6.Offl")
  group3 <- c(grouping,"Unplanned","Aux.2.Train","Aux.4.Coach","Aux.5.Break")
  groups <- list(group1,group2,group3)
  title <- paste("Shrinkage analysis for all LOBs:",grouping)
  Auxcode <- Raw.data %>%
      group_by_(grouping)
    if(!missing(lob)) {
      Auxcode <- filter(Auxcode,LOB==lob)
      title <- paste("Shrinkage analysis per LOB:",lob,"(",grouping,")")
    }
  plots <- function(groups,grouping,Auxcode){
    plot <- Auxcode %>%
      summarise_at(vars(Lateness:Aux.9.B.Off),funs(sum(., na.rm = T)/sum(Staffed.Time.Corrected, na.rm = T))) %>%
      select_(.dots=groups) %>%
      melt() %>%
      ggplot(.,aes_string(grouping))+
      geom_point(aes(y=value),shape=19)+
      geom_smooth(aes(y=value,group=variable),size=0.5,se=F,method="lm")+
      geom_line(aes(y=value,group=variable))+
      scale_y_continuous(label = percent)+
      theme(axis.title.x=element_blank(),
            strip.text=element_text(size=12),
            axis.text.y = element_text(size = 16),
            plot.margin = unit(c(0,0.5,0.2,0.7),"cm"),
            axis.title.y = element_blank())+
      facet_wrap(~variable,nrow=1)

    if(grouping == "Hour"){
      plot <- plot +
        #scale_x_discrete(breaks=c("0800 - 0815","0900 - 0915", "1000 - 1015", "1100 - 1115","1200 - 1215", "1300 - 1315", "1400 - 1415","1500 - 1515","1600 - 1615", "1700 - 1715", "1800 - 1815", "1900 - 1915","2000 - 2015"),labels=seq(8,20,1))
        scale_x_discrete(breaks = unique(Raw.data$Hour)[seq(1,length(unique(Raw.data$Hour)),4)],labels = seq(8,20,1))
    }
    Aux[[length(Aux)+1]] <<- plot
  }
  lapply(groups,function(x)plots(x,grouping,Auxcode))

  Shrinkage <- Raw.data %>%
    group_by_(grouping)
  if(!missing(lob)) {
    Shrinkage <- filter(Shrinkage,LOB==lob)
  }
  Shrinkage <- 
    summarise_at(Shrinkage,vars(Unplanned,Tot.Shrink.wo.Unpl),funs(sum(., na.rm = T)/sum(Staffed.Time.Corrected, na.rm = T))) %>%
    mutate(Tot.Shrinkage = 1-(1-Unplanned)*(1-Tot.Shrink.wo.Unpl)) %>%
    select(-Unplanned) %>%
    melt() %>%
    ggplot(.,aes_string(grouping))+
    geom_point(aes(y=value),shape=19)+
    geom_smooth(aes(y=value,group=variable),size=0.5,se=F,method="lm")+
    geom_line(aes(y=value,group=variable))+
    scale_y_continuous(label = percent)+
    theme(axis.title.x=element_blank(),
          strip.text=element_text(size=12),
          axis.text.y = element_text(size = 16),
          plot.margin = unit(c(0,0.5,0.2,0.7),"cm"),
          axis.title.y = element_blank())+
    facet_wrap(~variable)

   if(grouping == "Hour"){
    Shrinkage <- Shrinkage +
      scale_x_discrete(breaks = unique(Raw.data$Hour)[seq(1,length(unique(Raw.data$Hour)),4)],labels = seq(8,20,1))
   }

  gAux1 <- ggplotGrob(Aux[[1]])
  gAux2 <- ggplotGrob(Aux[[2]])
  gAux3 <- ggplotGrob(Aux[[3]])
  gShrinkage <- ggplotGrob(Shrinkage)
  gAux1$widths[4] <- gAux2$widths[4]
  gAux3$widths[4] <- gAux2$widths[4]
  gShrinkage$widths[4] <- gAux2$widths[4]
  
  final.plot <- grid.arrange(gAux1,gAux2,gAux3,gShrinkage,
               heights=c(2,2,2,4),
               ncol=1,
               top=textGrob(title,gp=gpar(fontsize=20)))
  
  ## Save arranged plot in PDF in the working directory
  ggsave(paste(title,"pdf",sep="."),final.plot,device="pdf",width=20,height=12,units="in")
}

### Analysis per LOB, all facets
charts("Period")
charts("Day")
charts("Hour")

LOB <- Raw.data %>%
  group_by(LOB) %>%
  #summarise_each(funs(sum(., na.rm = T)/sum(Staffed.Time.Corrected, na.rm = T)),Lateness:Tot.Shrink.wo.Unpl) %>%
  summarise_at(vars(Lateness:Tot.Shrink.wo.Unpl),funs(sum(., na.rm = T)/sum(Staffed.Time.Corrected, na.rm = T))) %>%
  mutate(Tot.Shrinkage = 1-(1-Unplanned)*(1-Tot.Shrink.wo.Unpl))

final.plot <- ggplot(melt(LOB),aes(LOB,value,group=variable))+
  geom_bar(stat="identity",alpha=0.7,colour="grey20")+
  facet_wrap(~variable,scales="free")+
  labs(title="Overview of all LOBs")+
  scale_y_continuous(label = percent)+
  theme(plot.title=element_text(size=36,vjust=1.5),
        axis.text.x=element_text(size=20,angle = 45, vjust = 1, hjust = 1),
        axis.text.y=element_text(size=18),
        strip.text=element_text(size=20))

ggsave(paste("Overview all LOBs","pdf",sep="."),final.plot,device="pdf",width=30,height=25,units="in")

### Period per each LOB
invisible(lapply(lobs,function(x) charts("Period",x)))

### Weekday per each LOB
invisible(lapply(lobs, function(x) charts("Day",x)))

### Hours per each LOB
invisible(lapply(lobs,function(x) charts("Hour",x)))

### Write tables for each LOB and for Period, Day and Hour
for (i in lobs){
  for (j in c("Period","Day","Hour")){
    lob <- group_by(Raw.data,LOB) %>%
      group_by_(j) %>%
      filter(LOB==i) %>%
      #summarise_each(funs(sum(.)/sum(Staffed.Time.Corrected)),Lateness:Tot.Shrink.wo.Unpl) %>%
      summarise_at(vars(Lateness:Tot.Shrink.wo.Unpl),funs(sum(.)/sum(Staffed.Time.Corrected))) %>%
      mutate(Tot.Shrinkage = 1-(1-Unplanned)*(1-Tot.Shrink.wo.Unpl)) %>%
      mutate(LOB=i) %>%
      select(LOB,everything()) %>%
      inner_join(Raw.data %>%
                   group_by_(j) %>%
                   filter(LOB==i) %>%
                   summarise(Staffed.Time=sum(Staffed.Time.Corrected)),
                 .)
    write.csv2(lob,file=paste(i,j,"csv",sep="."),row.names=F)
  }
  rm(list=c("i","j","lob"))
}

Period.LOB <- Raw.data %>%
  group_by(Period,LOB) %>%
  #summarise_each(funs(sum(., na.rm = T)/sum(Staffed.Time.Corrected, na.rm = T)),Lateness:Tot.Shrink.wo.Unpl) %>%
  summarise_at(vars(Lateness:Tot.Shrink.wo.Unpl),funs(sum(., na.rm = T)/sum(Staffed.Time.Corrected, na.rm = T))) %>%
  mutate(Tot.Shrinkage = 1-(1-Unplanned)*(1-Tot.Shrink.wo.Unpl))

final.plot <- ggplot(Period.LOB,aes(Period,Tot.Shrinkage,group=LOB))+
  geom_point(shape=19)+
  geom_line()+
  facet_wrap(~LOB,ncol=1)+
  labs(title="Total Shrinkage per Period")+
  scale_y_continuous(label = percent)+
  theme(plot.title=element_text(size=26,vjust=1.5),
        axis.text=element_text(size=12),
        strip.text=element_text(size=16))

ggsave(paste("Total Shrinkage per Period","pdf",sep="."),final.plot,device="pdf",width=15,height=14,units="in")
