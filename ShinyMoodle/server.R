#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

final_date2 = as.Date(paste(date), format = "%Y%m%d")


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    output$myTitle <- renderText({
        
        paste(" Today's data file is",final_date)
        
    })

    output$BarPlot <- renderPlot({
        
        log_new %>%
            filter(!is.na(ActID)) %>%
            mutate( ActID.lab = paste(ActID,"-",Event.context)) %>%
            group_by(ActID.lab) %>%
            summarize(Learners = n_distinct(LearnerID)) %>% 
            mutate(rank = dense_rank(desc(Learners))) %>% 
            filter(rank <= input$NbAct) %>% 
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
        
        
    })
    
    
    
    

})
