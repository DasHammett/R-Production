library(readxl)
library(reshape2)
library(grid)
library(gridExtra)
Raw.data <- read_excel(file.choose())
colnames(Raw.data) <- make.names(names(Raw.data),unique=T)
Raw.data$LOB <- gsub("CPU Spanish","Mac+ Spanish",Raw.data$LOB)
Raw.data$Day <- factor(Raw.data$Day,levels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
Raw.data <- Raw.data %>% mutate(Tot.Shrink.wo.Unpl = rowSums(.[c(6,8:17)]))
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
      summarise_each(funs(sum(.)/sum(Staffed.Time.Corrected)),Lateness:Aux.9.B.Off) %>%
      select_(.dots=groups) %>%
      melt() %>%
      ggplot(.,aes_string(grouping))+
      geom_point(aes(y=value),shape=19)+
      geom_smooth(aes(y=value,group=variable),size=0.5,se=F,method="lm")+
      geom_line(aes(y=value,group=variable))+
      theme(axis.title.x=element_blank(),
            strip.text=element_text(size=12))+
      facet_wrap(~variable,nrow=1)

    if(grouping == "Hour"){
      plot <- plot +
        scale_x_discrete(breaks=c("0800 - 0815","0900 - 0915", "1000 - 1015", "1100 - 1115","1200 - 1215", "1300 - 1315", "1400 - 1415","1500 - 1515","1600 - 1615", "1700 - 1715", "1800 - 1815", "1900 - 1915"),labels=seq(8,19,1))
    }
    Aux[[length(Aux)+1]] <<- plot
  }
  lapply(groups,function(x)plots(x,grouping,Auxcode))

  Shrinkage <- Raw.data %>%
    group_by_(grouping)
  if(!missing(lob)) {
    Shrinkage <- filter(Shrinkage,LOB==lob)
  }
  Shrinkage <- summarise_each(Shrinkage,funs(sum(.)/sum(Staffed.Time.Corrected)),Unplanned,Tot.Shrink.wo.Unpl) %>%
    mutate(Tot.Shrinkage = 1-(1-Unplanned)*(1-Tot.Shrink.wo.Unpl)) %>%
    select(-Unplanned) %>%
    melt() %>%
    ggplot(.,aes_string(grouping))+
    geom_point(aes(y=value),shape=19)+
    geom_smooth(aes(y=value,group=variable),size=0.5,se=F,method="lm")+
    geom_line(aes(y=value,group=variable))+
    theme(axis.title.x=element_blank(),
          strip.text=element_text(size=12))+
    facet_wrap(~variable)

   if(grouping == "Hour"){
    Shrinkage <- Shrinkage +
      scale_x_discrete(breaks=c("0800 - 0815","0900 - 0915", "1000 - 1015", "1100 - 1115","1200 - 1215", "1300 - 1315", "1400 - 1415","1500 - 1515","1600 - 1615", "1700 - 1715", "1800 - 1815", "1900 - 1915"),labels=seq(8,19,1))
   }

  gAux1 <- ggplotGrob(Aux[[1]])
  gAux2 <- ggplotGrob(Aux[[2]])
  gAux3 <- ggplotGrob(Aux[[3]])
  gShrinkage <- ggplotGrob(Shrinkage)
  gAux1$widths[1:3] <- gAux2$widths[1:3]
  gAux3$widths[1:3] <- gAux2$widths[1:3]
  gShrinkage$widths[1:3] <- gAux2$widths[1:3]
  
  grid.arrange(gAux1,gAux2,gAux3,gShrinkage,
               heights=c(2,2,2,4),
               ncol=1,
               top=textGrob(title,gp=gpar(fontsize=20)))
}

### Analysis per LOB, all facets
charts("Period")
charts("Day")
charts("Hour")

LOB <- Raw.data %>%
  group_by(LOB) %>%
  summarise_each(funs(sum(.)/sum(Staffed.Time.Corrected)),Lateness:Tot.Shrink.wo.Unpl) %>%
  mutate(Tot.Shrinkage = 1-(1-Unplanned)*(1-Tot.Shrink.wo.Unpl))

ggplot(melt(LOB),aes(LOB,value,group=variable))+
  geom_bar(stat="identity",alpha=0.7,colour="grey20")+
  facet_wrap(~variable,scales="free")+
  labs(title="Overview of all LOBs")+
  theme(plot.title=element_text(size=36,vjust=1.5),
        axis.text=element_text(size=18),
        strip.text=element_text(size=20))

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
      summarise_each(funs(sum(.)/sum(Staffed.Time.Corrected)),Lateness:Tot.Shrink.wo.Unpl) %>%
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
  summarise_each(funs(sum(.)/sum(Staffed.Time.Corrected)),Lateness:Tot.Shrink.wo.Unpl) %>%
  mutate(Tot.Shrinkage = 1-(1-Unplanned)*(1-Tot.Shrink.wo.Unpl))

ggplot(Period.LOB,aes(Period,Tot.Shrinkage,group=LOB))+
  geom_point(shape=19)+
  geom_line()+
  facet_wrap(~LOB,ncol=1)+
  labs(title="Total Shrinkage per Period")+
  theme(plot.title=element_text(size=36,vjust=1.5),
        axis.text=element_text(size=18),
        strip.text=element_text(size=20))
