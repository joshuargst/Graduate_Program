

library(shiny)
library(shinydashboard)
library(plotly)
library(tidyverse)
library(dplyr)



Mortalitydata<- read.csv("https://raw.gith ubusercontent.com/charleyferrari/CUNY_DATA608/master/lecture3/data/cleaned-cdc-mortality-1999-2010-2.csv")


Mortality2010<-Mortalitydata %>% filter(Year==2010)

CauseofDeaths<-as.vector(unique(Mortality2010$ICD.Chapter))
Location<-as.vector(unique(Mortalitydata$State))



{ui <- 
        dashboardPage(
            dashboardHeader(title = "Joshua Registe"),
            dashboardSidebar(HTML("<br><br><br>The following is shiny app is deployed for assignment 3 of Data 608.<br><br> <a href = https://github.com/charleyferrari/CUNY_DATA608/tree/master/lecture3/data> Click here for link to Mortality Data</a>. <br> <br> More data can be collected from <a href=https://wonder.cdc.gov/ucd-icd10.html> CDC here </a>.<br><br>This assignment is done in R using the shiny package and uses support packages 'shiny', 'shinydashboard', 'plotly', 'tidyverse', and 'dplyr'.<br>The app has 2 tabs for each question from Module 3 from Data 608 - Data Visualization" )
            ),
            
            dashboardBody(
                
                
                fluidRow(
                    tabBox(width = 100, height = 5000,
                           tabPanel("Question 1",
                                    column(10,HTML("Question 1: <br><br> As a researcher, you frequently compare mortality rates from particular causes across different States. You need a visualization that will let you see (for 2010 only) the crude mortality rate, across all States, from one cause (for example, Neoplasms, which are effectively cancers). Create a visualization that allows you to rank States by crude mortality for each cause of death.<br><br><br><br><br>")),
                                    
                                    column(10,plotlyOutput("Question1_PlotA")),
                                    column(10, selectInput(width = 10000,inputId = "CauseofDeath", label = "Select Cause of Death", 
                                                           choices = sort(CauseofDeaths),
                                                           selected = 1))
                           ),
                           
                           tabPanel("Question 2",
                                    column(10, HTML("Question 2: <br><br>Often you are asked whether particular States are improving their mortality rates (per cause) faster than, or slower than, the national average. Create a visualization that lets your clients see this for themselves for one cause of death at the time. Keep in mind that the national average should be weighted by the national population.<br><br><br><br><br>")),
                                    column(10,plotlyOutput("Question2_PlotA")),
                                    column(10, selectInput(width = 10000,inputId = "LocationofDeath", label = "Select Location of Interest", 
                                                           choices = sort(Location),
                                                           selected = 1)),
                                    column(10, selectInput(width = 10000,inputId = "CauseofDeath2", label = "Select Cause of Death", 
                                                           choices = sort(CauseofDeaths),
                                                           selected = 1))
                           )
                           
                    )
                )))
    
    
    # Define server logic ----
    server <- function(input, output) {
        
        # coolplot1a --------------------------------------------------------------
        
        
        
        output$Question1_PlotA<-renderPlotly({
            
            Mortality2010<-Mortality2010 %>% filter(ICD.Chapter==input$CauseofDeath)
            
            
            ggplotly(
                ggplot(Mortality2010)+
                    geom_col(mapping = aes(x = reorder(State,Crude.Rate, FUN = sum), y = Crude.Rate), fill = 'grey50', alpha = .5)+
                    theme_minimal()+
                    theme(axis.text.x=element_text(angle = 90))+
                    labs(x = "Crude Death Rate",
                         y = "State",
                         title = paste0("States Ranked by Crude Mortality Rate\n",input$CauseofDeath)
                    )
            )
        })
        # coolplot1b --------------------------------------------------------------
        
        output$Question2_PlotA<-renderPlotly({
            
            Mortalitydata_sub<- Mortalitydata %>% filter(State==input$LocationofDeath,ICD.Chapter==input$CauseofDeath2)
            Allannot<-mean(filter(Mortalitydata,Year==2010, ICD.Chapter==input$CauseofDeath2)$Crude.Rate)
            Subannot<-mean(filter(Mortalitydata_sub,Year==2010, ICD.Chapter==input$CauseofDeath2)$Crude.Rate)
            
            ggplotly(
                ggplot()+
                    stat_summary(data = filter(Mortalitydata,ICD.Chapter ==input$CauseofDeath2), geom = "line", mapping = aes(x = Year, y = Crude.Rate), fun.y = mean, color = "red")+
                    stat_summary(data = Mortalitydata_sub, geom = "line", mapping = aes(x = Year, y = Crude.Rate), fun.y = mean, color = "black")+
                    theme_classic()+
                    theme(panel.grid.major = element_blank(),
                          panel.grid.minor = element_blank())+
                    annotate(geom = "text", x = 2010.5, y =Allannot, label = "United\nStates")+
                    annotate(geom = "text", x = 2010.5, y =Subannot, label =input$LocationofDeath)+
                    labs(y = "Crude Mortality Rate",
                         x = "Year")
            )
        })
        
    }
    
    # Run the app ----
    shinyApp(ui = ui, server = server)
    
    
}

