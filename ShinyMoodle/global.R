## ----setup, include=FALSE------------------------------------------------------------------------------------------------
# Code folding oprion needs echo = TRUE to work 

knitr::opts_chunk$set( message = FALSE, warning = FALSE, 
                       results =FALSE, echo = TRUE, 
                       dev="png", 
                       fig.align = 'center',
                       dev.args=list(type="cairo"), dpi=96)

# Some colors we may want to use
SIAP.color <- "#0385a8"




## ----libraries-----------------------------------------------------------------------------------------------------------
# get knit worked for Mac
library(Cairo)
library(tidyverse)
library(stringr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)



## ----data----------------------------------------------------------------------------------------------------------------

date <- 20210217  #20210201     # 20210127

data_log <- read.csv(paste0("../Data/logs_DTV20_", date,".csv"))

file <- paste0("../Data/logs_DTV20_", date,".csv") 


# I have a strange pb with the name of the Time variable. fixing it quick and dirty mode !
data_log$Time <- data_log$ï..Time #Time variable was removed when running this line




## ------------------------------------------------------------------------------------------------------------------------
#remove administrator 
data_log <- data_log[!(data_log$User.full.name=="Christophe Bontemps" | data_log$User.full.name=="Ni Ni Thein" | data_log$Component=="Recycle bin"),]



## ------------------------------------------------------------------------------------------------------------------------
r <- unique(data_log$Event.name)
r

## ------------------------------------------------------------------------------------------------------------------------
#remove less relevant events
# the events to remove
rem_events <- c("Tour ended", "Tour ended", "Group member added","Role assigned", "Course searched", "User list viewed", "User profile viewed", "Subscription created", tail(r, n=11))
rem_events

# remove events
data_log <- data_log[!data_log$Event.name %in% rem_events, ]



## ------------------------------------------------------------------------------------------------------------------------
CourseID <- str_match(data_log$Description, "(?i)\\bcourse with id '?\\s*(\\d+)")[,2]
head(CourseID)
unique(CourseID)


## ------------------------------------------------------------------------------------------------------------------------
LearnerID <- str_match(data_log$Description, "(?i)\\buser with id '?\\s*(\\d+)")[,2]
head(LearnerID)


## ------------------------------------------------------------------------------------------------------------------------
ModuleID <- str_match(data_log$Description, "(?i)\\bsection number '?\\s*(\\d+)")[,2]
unique(ModuleID) # the actions applied on Modules that are hidden from students are from stuffs like Shimizu san, Eunkoo, and me. We are not removed before. 



## ------------------------------------------------------------------------------------------------------------------------
ActID <- str_match(data_log$Description, "(?i)\\bcourse module id '?\\s*(\\d+)")[,2]
head(ActID)
unique(ActID)


## ------------------------------------------------------------------------------------------------------------------------
Act_type <- str_match(data_log$Event.context,  "(?<=\\[).+?(?=\\])")[,1]
unique(Act_type)# Many students also accessed old version such as "OLD-Video".


## ------------------------------------------------------------------------------------------------------------------------
#CourseID is not added since it means this datavis course
log_new <- data.frame(data_log, ModuleID, LearnerID, ActID, Act_type)

NbLearner <- count(unique((log_new %>%
              filter(!is.na(Event.name) & !is.na(ModuleID)) %>%
              select(LearnerID))))



## ------------------------------------------------------------------------------------------------------------------------
log_new %>%
  filter(!is.na(ActID),
         !is.na(ModuleID))


## ------------------------------------------------------------------------------------------------------------------------
log_new <- log_new  %>%
           mutate(Date = str_split(log_new$Time, ", ", simplify = TRUE)[,1],
                    Times = str_split(log_new$Time, ", ", simplify = TRUE)[,2])



## ------------------------------------------------------------------------------------------------------------------------
log_new <- log_new  %>%
          mutate(Times = format(strptime(log_new$Times, "%H:%M"), format = "%H:%M"))%>%
          mutate(Date = as.Date(log_new$Date, "%d/%m/%y"))%>%
          filter(Date >= "2021-01-13")
          
  
        


## ------------------------------------------------------------------------------------------------------------------------
# extract the hour and create time range. 
t <- log_new %>%
  select(Times) %>%
  extract(Times, 'Time_range', regex = "([\\d.]+)[^\\d.]", remove = FALSE) %>%
  mutate(Time_range = as.numeric(Time_range))

# a loop for time range
  for (i in list(t$Time_range)) {
  i = paste0(formatC(i, format="d",flag="0",width=2),":00", "-", formatC(i+1, format="d",flag="0",width=2), ":00")
  log_new$Time_range <- i
}

unique(log_new$Time_range)


## ------------------------------------------------------------------------------------------------------------------------
log_new <- log_new %>%
  mutate(DateID = group_indices(., Date))%>%
  mutate(TimeID = as.numeric(str_extract(log_new$Time_range, "([\\d.]+)")))



## ------------------------------------------------------------------------------------------------------------------------
log_new %>%
  filter(!is.na(Event.name)) %>%
  group_by(Event.name) %>%
  summarize(Learners = n_distinct(LearnerID)) %>% 
  #mutate(rank = dense_rank(desc(Learners))) %>% 
  #filter(rank <= 25) %>% 
  ggplot()+
  aes(x=reorder(Event.name, Learners), y=Learners)+
  geom_bar(stat='identity')+
  labs(x="Events", y="Number of Learners", 
       subtitle = paste(length(unique(log_new$Event.name))-1,"events (Final date ", as.Date(as.character(date),format = "%Y%m%d"),")"))+
  ggtitle("Events conducted by number of Learners")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        plot.title = element_text(hjust = 0.5, size=12),
        panel.background = element_blank(), text = element_text(size=8), 
        axis.line = element_line(colour = "black"), 
        plot.subtitle = element_text(hjust = 0.5, size=8),
        axis.title.y = element_text(size = 8))+
  coord_flip()
  
