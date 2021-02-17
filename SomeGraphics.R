

## ------------------------------------------------------------------------------------------------------------------------
log_new %>%
  filter(!is.na(LearnerID)) %>%
  count(LearnerID) %>%
  mutate(rank = dense_rank(desc(n))) %>% 
  filter(row_number() >= max(row_number()) - 5 | rank <= 5) %>%
  ggplot()+
  aes(x=reorder(LearnerID, -n), y=n, 
      fill=factor(ifelse(rank <= 5,"Top 5","Bottom 5")))+
  geom_bar(stat='identity')+
  labs(x="Learners' ID", y="Number of events", fill="",
       subtitle = paste(NbLearner,"Learners (Final Date ", as.Date(as.character(date),format = "%Y%m%d"),")"))+
  ggtitle("Learners event counts (top and bottom Learners)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        plot.title = element_text(hjust = 0.5, size=12), 
        plot.subtitle = element_text(hjust = 0.5, size=8),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))



## ------------------------------------------------------------------------------------------------------------------------
log_new %>%
  filter(!is.na(LearnerID)) %>%
  ggplot()+
  #aes(x=fct_infreq(Date))+
  aes(x = Date)+
  geom_bar()+
  labs(x="Date", y="Number of events", 
       subtitle = paste(length(unique(log_new$Date)),"days (Final date ", as.Date(as.character(date),format = "%Y%m%d"),")"))+
  ggtitle("Number of events was conducted by date")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        plot.title = element_text(hjust = 0.5, size=12),
        panel.background = element_blank(), text = element_text(size=8), 
        axis.line = element_line(colour = "black"), 
        plot.subtitle = element_text(hjust = 0.5, size=8),
        axis.title.y = element_text(size = 10))
#  coord_flip()


## ------------------------------------------------------------------------------------------------------------------------
log_new %>%
  filter(!is.na(ActID)) %>%
  mutate( ActID.lab = paste(ActID,"-",Event.context)) %>%
  group_by(ActID.lab) %>%
  summarize(Learners = n_distinct(LearnerID)) %>% 
  mutate(rank = dense_rank(desc(Learners))) %>% 
  filter(rank <= 40) %>% 
  ggplot()+
  aes(x=reorder(ActID.lab, Learners), y=Learners)+
  geom_bar(stat='identity')+
  labs(x="Activities", y="Number of Learners", 
       subtitle = paste(length(unique(ActID))-1,"activities (Final Date ", as.Date(as.character(date),format = "%Y%m%d"),")"))+
  ggtitle("Activities accessed by number of Learners (top 30 activities)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        plot.title = element_text(hjust = 0.5, size=12),
        panel.background = element_blank(), text = element_text(size=8), 
        axis.line = element_line(colour = "black"), 
        plot.subtitle = element_text(hjust = 0.5, size=8),
        axis.title.y = element_text(size = 8))+
  coord_flip()



## ------------------------------------------------------------------------------------------------------------------------
log_new %>%
  filter(!is.na(Time_range)) %>%
  group_by(Time_range) %>%
  summarize(Learners = n_distinct(LearnerID)) %>% 
  ggplot()+
  aes(x=reorder(Time_range, Time_range), y=Learners)+
  geom_bar(stat='identity')+
  labs(x="Time", y="Number of Learners", 
       subtitle = paste("JST (Final date ", as.Date(as.character(date),format = "%Y%m%d"),")"))+
  ggtitle("Active time by Learners")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        plot.title = element_text(hjust = 0.5, size=12),
        panel.background = element_blank(), text = element_text(size=8), 
        axis.line = element_line(colour = "black"), 
        plot.subtitle = element_text(hjust = 0.5, size=8),
        axis.title.y = element_text(size = 8), 
        axis.text.x = element_text(angle = 60))

#  coord_flip()



## ------------------------------------------------------------------------------------------------------------------------

final_date = as.Date(paste(date), format = "%Y%m%d")

log_new %>%
  filter(!is.na(ActID),
         Date == final_date) %>%
  mutate( ActID.lab = paste(ActID,"-",Event.context)) %>%
  group_by(ActID.lab) %>%
  summarize(Learners = n_distinct(LearnerID)) %>% 
  #mutate(rank = dense_rank(desc(Learners))) %>% 
  #filter(rank <= 30) %>% 
  ggplot()+
  aes(x=reorder(ActID.lab, Learners), y=Learners)+
  geom_bar(stat='identity')+
  labs(x="Activities", y="Number of Learners", 
       subtitle = paste(nrow(subset(unique(log_new[c("ActID", "Date")]), Date==final_date)),"activities (Date ", final_date,")"))+
  ggtitle("Activities accessed by number of Learners")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        plot.title = element_text(hjust = 0.5, size=12),
        panel.background = element_blank(), text = element_text(size=8), 
        axis.line = element_line(colour = "black"), 
        plot.subtitle = element_text(hjust = 0.5, size=8),
        axis.title.y = element_text(size = 8))+
  coord_flip()




## ------------------------------------------------------------------------------------------------------------------------
final_date2 = as.Date(paste(date), format = "%Y%m%d")

log_new %>%
  filter(!is.na(ActID),
         Date == final_date2) %>%
  mutate( ActID.lab = paste(ActID,"-",Event.context)) %>%
  group_by(ActID.lab) %>%
  summarize(Learners = n_distinct(LearnerID)) %>% 
  #mutate(rank = dense_rank(desc(Learners))) %>% 
  #filter(rank <= 30) %>% 
  ggplot()+
  aes(x=reorder(ActID.lab, Learners), y=Learners)+
  geom_bar(stat='identity')+
  labs(x="Activities", y="Number of Learners", 
       subtitle = paste(nrow(subset(unique(log_new[c("ActID", "Date")]), Date==final_date2)),"activities (Date ", final_date2,")"))+
  ggtitle("Activities accessed by number of Learners")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        plot.title = element_text(hjust = 0.5, size=12),
        panel.background = element_blank(), text = element_text(size=8), 
        axis.line = element_line(colour = "black"), 
        plot.subtitle = element_text(hjust = 0.5, size=8),
        axis.title.y = element_text(size = 8))+
  coord_flip()


## ---- fig.width=12,fig.height=12-----------------------------------------------------------------------------------------
#Both Date and Date ID work. Date: the specific date since beginning; DateID: the first date = 1, and the second = 2, ...
#sort by the ActID in ascending order
log_new %>%
  filter(!is.na(ActID)) %>%
  # mutate(ActID.lab = paste(fct_rev(fct_inorder(ActID)),"-",Event.context)) %>%
  mutate(ActID.lab = paste(ActID,"-",Event.context, "-")) %>%
  group_by(ActID.lab, Date) %>%
  summarize(Learners = n_distinct(LearnerID)) %>% 
  ggplot()+
  aes(x=Date, y= ActID.lab, fill=Learners)+
  geom_tile()+
  labs(x="Date", y="Activities", 
       subtitle = paste("Final date ", as.Date(as.character(date),format = "%Y%m%d")))+
  ggtitle("Heatmap of the activities")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        plot.title = element_text(hjust = 0.5, size=14),
        panel.background = element_blank(), text = element_text(size=10), 
        axis.line = element_line(colour = "black"), 
        plot.subtitle = element_text(hjust = 0.5, size=8),
        axis.title.y = element_text(size = 10))+
  coord_fixed(ratio = 0.5)




## ---- fig.width=12,fig.height=12-----------------------------------------------------------------------------------------
#Both Date and Date ID work. Date: the specific date since beginning; DateID: the first date = 1, and the second = 2, ...
log_new %>%
  filter(!is.na(ActID)) %>%
  mutate(ActID.lab = paste(ActID,"-",Event.context, "-")) %>%
  group_by(ActID.lab, Date) %>%
  summarize(Learners = n_distinct(LearnerID)) %>% 
  ggplot()+
  aes(x=Date, y=fct_rev(fct_infreq(ActID.lab)), fill=Learners)+
  geom_tile()+
  labs(x="Date", y="Activities", 
       subtitle = paste("Final date ", as.Date(as.character(date),format = "%Y%m%d")))+
  ggtitle("Heatmap of the activities")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        plot.title = element_text(hjust = 0.5, size=14),
        panel.background = element_blank(), text = element_text(size=10), 
        axis.line = element_line(colour = "black"), 
        plot.subtitle = element_text(hjust = 0.5, size=8),
        axis.title.y = element_text(size = 10))+
  coord_fixed(ratio = 0.5)


