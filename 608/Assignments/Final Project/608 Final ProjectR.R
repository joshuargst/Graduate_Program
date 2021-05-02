library(tidyverse)
library(readxl)
library(tidyr)
library(stringr)
library(httr)
library(shiny)
library(shinydashboard)
library(plotly)
library(RColorBrewer)
library(ggpmisc)


# Dataset URLs. First url provides state energy related carbon emissions by year. second url provides 2016 state energy-related carbon dioxide emissions by fuel. 3rd url provides state energy-related carbon emissions by sector. 4th URL provides per-capita carbon dioxide emissions by state for 2005-2016. 5th url provides energy intensity by state and 6th url provides carbon intensity by state. 
rm(list=ls())

#Questions to answer?
#Page 1) (3 plots)
#  1)difference in emissions overtime by state. time plot overtime
#2) look at same information in 1 but per capita, tie this into first plot with selection
#3) look at energy intensity by state
#4) look at carbon intensity by state
#
#Page 2) (2 plots)
#1) look at overall carbon emissions by state and by type, bar plot 
#2) add bar plot to the bottom to show the same information by by sector
#
#
#page 3) 
#1) search relationships energy intensity  and carbon emissions by state
#







url_list<-list("http://www.eia.gov/environment/emissions/state/analysis/excel/table2.xlsx",
               "http://www.eia.gov/environment/emissions/state/analysis/excel/table3.xlsx",
               "http://www.eia.gov/environment/emissions/state/analysis/excel/table4.xlsx",
               "http://www.eia.gov/environment/emissions/state/analysis/excel/table6.xlsx",
               "http://www.eia.gov/environment/emissions/state/analysis/excel/table7.xlsx",
               "http://www.eia.gov/environment/emissions/state/analysis/excel/table8.xlsx")
           

datadescription<- c("Emissions_By_Year",
                    "Emissions_By_Fuel",
                    "Emissions_By_Sector",
                    "Emissions_Per_Capita",
                    "Energy_Intensity",
                    "Carbon_Intensity")

dataunits<-c("Million Metric Tons of CO2",
             "Million Metric Tons of CO2",
             "Million Metric Tons of CO2",
             "Metric Tons of CO2",
             "Thousand BTU per GDP",
             "Kg of Energy-related CO2 per Million BTU")

# extracting all of the data sets, passing them into a list and naming that list by the datadescriptions vector
DFlist<-list()
{
  for (i in 1:length(url_list)){
    GET(url_list[[i]], write_disk(tf <- tempfile(fileext = ".xlsx")))
    DFlist[[i]]<- 
      read_excel(tf,
                 skip =  str_which(read_excel(tf)[[1]],"State")[1]) %>% 
      select_if(~ !all(is.na(.))) %>% 
      drop_na() %>% 
      filter(str_detect(.[[1]],"Total")==FALSE) %>% 
      filter(str_detect(.[[1]],"Average")==FALSE) %>% 
      select_if(!str_detect(colnames(.),"Percent")) %>% 
      select_if(!str_detect(colnames(.),"Absolute")) %>% 
      select_if(!str_detect(colnames(.),"Total"))
  }
  
  names(DFlist)<-datadescription
  
}
DFlist$Emissions_By_Fuel<-DFlist$Emissions_By_Fuel[1:4]
colnames(DFlist[[2]])<-c("State","Coal","Petroleum","Natural Gas")


for (i in 1:length(DFlist)){
  
DF_Transformed[[i]]<-  DFlist[[i]] %>%
  mutate_if(colnames(.)!="State",as.double) %>% 
  pivot_longer(str_which(colnames(.),"State",negate = T)) %>% 
  mutate("Dataset"=datadescription[i]) %>% 
  mutate(Units=dataunits[i])
  
}

for (i in c(1,4,5,6)){
  
  names(DF_Transformed[[i]])[names(DF_Transformed[[i]])=="name"]<-"Year"
  DF_Transformed[[i]]$Year<- as.integer(DF_Transformed[[i]]$Year)
}

  
names(DF_Transformed[[2]])[names(DF_Transformed[[2]])=="name"]<-"Fuel"
names(DF_Transformed[[3]])[names(DF_Transformed[[3]])=="name"]<-"Sector"


#is this necessary?
DF_Combined_Long<-bind_rows(DF_Transformed)

plottheme<- list(theme(panel.background = element_blank(),
                  panel.border = element_blank(),
                  panel.grid = element_blank()),
                 scale_color_brewer(palette = "Dark2"),
                 scale_fill_brewer(palette = "Dark2"))



x_Axis<-c("Coal","Petroleum","Commercial","Residential","Industrial","Transportation")
y_Axis<-c("Energy_Intensity","Carbon_Intensity")
dropdownButton <- function(label = "", status = c("default", "primary", "success", "info", "warning", "danger"), ..., width = NULL) {
  
  status <- match.arg(status)
  # dropdown button content
  html_ul <- list(
    class = "dropdown-menu",
    style = if (!is.null(width)) 
      paste0("width: ", validateCssUnit(width), ";"),
    lapply(X = list(...), FUN = tags$li, style = "margin-left: 10px; margin-right: 10px;")
  )
  # dropdown button appearence
  html_button <- list(
    class = paste0("btn btn-", status," dropdown-toggle"),
    type = "button", 
    `data-toggle` = "dropdown"
  )
  html_button <- c(html_button, list(label))
  html_button <- c(html_button, list(tags$span(class = "caret")))
  # final result
  tags$div(
    class = "dropdown",
    do.call(tags$button, html_button),
    do.call(tags$ul, html_ul),
    tags$script(
      "$('.dropdown-menu').click(function(e) {
      e.stopPropagation();
});")
  )
}

uniquestates<-unique(DF_Combined_Long$State) %>% na.omit()
uniquesector<-unique(DF_Combined_Long$Sector) %>% na.omit()
uniquesfuel<-unique(DF_Combined_Long$Fuel) %>% na.omit()

{ui <- 
    dashboardPage(
      dashboardHeader(title = "Joshua Registe"),
      dashboardSidebar(HTML("<br><br><br>The following is shiny app is deployed for the Final Project Data 608.<br><br> <a href =https://www.eia.gov/environment/emissions/state/analysis/> Click here for link to Energy Data</a>. <br> <br> This data was collected from the U.S. Energy Information Administration. <br><br>This assignment is done in R using the shiny package and uses support packages 'shiny', 'shinydashboard', 'plotly', 'tidyverse', and 'dplyr'.<br>The app has 3 tabs for explaining the data collected from EIA." )
      ),
      
      dashboardBody(
        
        
        fluidRow(
          tabBox(width = 100, height = 5000,
                 tabPanel("Page 1",
                          column(10,HTML(" <br><br> Use the dropdown button below to select any number of states that you would like to visualize for the plots below. The three plots below provide visualisations that show the carbon emissions over time, energy usage over time, and carbon emissions from energy over time.<br>This can be visualised for any state and some of the questions that one may be interested in answering with this dataset are:<br> 1) What is the difference in emissions overtime by state? and what is this when normalized per capita? <br>2) How do the trendlines for carbon emissions both normalized and not normalized by population compare to energy usage and carbon emissions from energy use? <br><br>")),
                          
                          #column(10, selectInput(width = 10000,inputId = "", label = "Select Ta", 
                          #                       choices = sort(datadescription),
                          #                       selected = 1)),
                          column(10, dropdownButton(
                            label = "States of Interest", status = "default", width = 10000,
                            checkboxGroupInput(inputId = "page1checkbox1", label = "Select Parameters", 
                                               choices = sort(uniquestates),
                                               selected = "Alabama"))),
                          column(10,HTML("<br><br><br> The first plot here provides information on carbon emissions over time and you can choose to normalize this dataset by population with the 'select normalization' radio button. This helps when looking at two states with largely different populations and can help compare with each other on a per-person basis. For example, if you select California and New York and Alaska, you will notice that California produces significantly more carbon overall with Alaska as the smallest emitter, but when normalizing this per capita, you will notice that New York and California have similar intensities while Alaska is larger emitter per capita.  <br><br>")),
                          
                          column(10,radioButtons(width = 10000,
                                                 inputId = "page1radio1",
                                                 label = "Select Normalization", 
                                                 choices = sort(c("Per Capita","Total")),
                                                 selected = "Total")),
                          
                          column(10,plotlyOutput("Page1_PlotA")),
                          
                          column(10,HTML("br><br><br><br> This second plot shows the energy usage in Thousand BTU per GDP by state. Energy us usually a metric that is very closely tracked when assessing carbon emissions because a large part of carbon emissions are from transportation, power plants, building power, utility power, etc. Fuels are used to generate this energy such as natural gas, coal, petroleum,etc. each with their own greenhouse gas potential (strength of impact to global warming). If we continue to look at NY and California, we will notice that although California has similar, are only slightly higher Carbon emissions per capita, the energy usage has been greater than NYs from all of 2000 to 2015, but the down trend does follow. <br><br>")),
                          
                          column(10,plotlyOutput("Page1_PlotB")),
                          
                          column(10,HTML("br><br> The following plot more specifically measures energy related CO2 emissions per million BTU. A normilization of carbon emisions to energy and we can notice that the energy related CO2 emissions in NY drop overtime while they stay fairly constant for california. Again, this assessment can be done for any number of states<br><br>")),
                          
                          column(10,plotlyOutput("Page1_PlotC")),
                          
                 ),
                 
                 tabPanel("Page 2",
                          column(10, HTML("<br><br>After loking at Page 1 and assessing emissions over time, we may be interested in drilling down further into the dataset. The following plot provides 3 levels of information:<br>- It allows us too look at emissions by fuel type (natural gas, coal, or petroleum)<br>- It allos us to look at emissions by sector (commercial, electric power, industrial, residential, or transportation)<br>- It allows us to look at the emissions for either facet for any of the states.<br>This is all donee via the drop down to select any number of states, and the radio button that allows you to either view this information by fuel type or sector. additionally, if we simply want to look at this information totalized for any state side by side, we can change the bartype via a second radio button to 'stack' and that will allow us to do that.<br><br> Using California and New York as an example again, we can see the breakdown of how each sector contributes to emissions and we notice that transportation is a significant contributor in both states. We also notice that California has a more polution in the industrial sector relative to its other sectors as compared to NYC where industrial is the least or their emissions. As for fuel type, they both have petroleum as their largest source of emissions, followed by natural gas, and then coal.<br><br><br><br>")),
                          column(3, 
                                 dropdownButton(
                                 label = "States of Interest", 
                                 status = "default", 
                                 width = 100,
                                 checkboxGroupInput(inputId = "page2checkbox1", 
                                               label = "Select Parameters", 
                                               choices = sort(uniquestates),
                                               selected = "Alabama"))),

                          column(3,radioButtons(width = 100,
                                                 inputId = "page2radio1", 
                                                 label = "Emissions by: ", 
                                                 choices = sort(c("Fuel Type","Sector")),
                                                 selected = "Fuel Type")),
                          
                          column(3,radioButtons(width = 100,
                                                 inputId = "page2radio2", 
                                                 label = "Bar Type ", 
                                                 choices = sort(c("dodge","stack")),
                                                 selected = "dodge")),
                          
                          column(10,plotlyOutput("Page2_PlotA")),
                          column(10, HTML("<br><br>text1<br><br><br><br><br>")),

                 ),
                 tabPanel("Page 3",
                          column(10, HTML("<br><br>The last page on our plot allows us to look at the relationship between different sectors or fuel types to carbon intensity or energy intensity as a response variable. There are 3 radio buttons:<br>- Outliers - This allows us to look at our relationships with and without outliers included in the dataset. outlier removal is based off of points outside 3 standard deviations from the sample mean.<br> X Axis - This radio button allows us to select our predictor variable which is either sector or fuel based carbon emissions.<br>Y Axis - This radio button allows us to select our response variable which is either carbon intensity or energy intensity and we can see how this is related to our predictor variables.<br> An example is looking at coal vs carbon intensity, where a clear trend is shown and that regression coefficient gets stronger (from 0.20 to 0.32) when removing outliers. <br>")),
                          
                          
                          column(3,radioButtons(width = 100,
                                                inputId = "page3radio1", 
                                                label = "Outliers?", 
                                                choices = sort(c("Keep Outliers","Remove Outliers")),
                                                selected = "Keep Outliers")),
                          
                          column(3,radioButtons(width = 100,
                                                inputId = "page3select1", 
                                                label = "X Axis", 
                                                choices = sort(x_Axis),
                                                selected = x_Axis[1])),
                          column(3,radioButtons(width = 100,
                                                inputId = "page3select2", 
                                                label = "Y Axis", 
                                                choices = sort(y_Axis),
                                                selected = y_Axis[1])),
                          
                          column(10,plotOutput("Page3_PlotA")),
                          
                 )
                 
          )
        )))
  
  
  # Define server logic ----
  server <- function(input, output) {
    
    # Page1 --------------------------------------------------------------
    
    
    
    output$Page1_PlotA<-renderPlotly({
      if (input$page1radio1 =="Total"){
        i<-1
      }else{
        i<-4
      }
      
      ggplotly(
        DF_Combined_Long %>% 
          filter(Dataset==datadescription[i]) %>% 
          filter(State %in% input$page1checkbox1) %>% 
          ggplot(mapping = aes(x = Year,y = value, color = State))+
          geom_line()+
          plottheme+
          labs(y = paste0("Carbon Emissions - ", dataunits[i]),
               x = "")
      )
    })
    
    output$Page1_PlotB<-renderPlotly({
      i<-5
      ggplotly(
        DF_Combined_Long %>% 
          filter(Dataset==datadescription[i]) %>% 
          filter(State %in% input$page1checkbox1) %>% 
          ggplot(mapping = aes(x = Year,y = value, color = State))+
          geom_line()+
          plottheme+
          labs(y = paste0("Carbon Emissions - ", dataunits[i]),
               x = "")
      )
    })
    

    
    output$Page1_PlotC<-renderPlotly({
     
      i<-6
      ggplotly(
        DF_Combined_Long %>% 
          filter(Dataset==datadescription[i]) %>% 
          filter(State %in% input$page1checkbox1) %>% 
          ggplot(mapping = aes(x = Year,y = value, color = State))+
          geom_line()+
          plottheme+
          labs(y = paste0("Carbon Emissions - ", dataunits[i]),
               x = "")
      )
    })
    
    
    
    
    # Page2--------------------------------------------------------------
    
    output$Page2_PlotA<-renderPlotly({

      if (input$page2radio1 =="Fuel Type"){
        i<-2
        plottype<-geom_col(position = input$page2radio2,alpha = 0.5, aes(fill = Fuel))
      }else{
        i<-3
        plottype<-geom_col(position = input$page2radio2,alpha = 0.5, aes(fill = Sector))
      }
      
      ggplotly(
        
        DF_Combined_Long %>% 
          filter(Dataset==datadescription[i]) %>% 
          filter(State %in% input$page2checkbox1) %>% 
          ggplot(mapping = aes(x = fct_reorder(State, value),y = value))+
          plottype+
          plottheme+
          labs(y = paste0("Carbon Emissions - ", dataunits[i]),
               x = "")
      )
    })
    

# page 3 ------------------------------------------------------------------

    
    
    output$Page3_PlotA<-renderPlot({
      
      t<-merge(
        DF_Combined_Long %>% 
          filter(Dataset==datadescription[2]) %>% 
          pivot_wider(names_from = Fuel),
        DF_Combined_Long %>% 
          filter(Dataset==datadescription[3]) %>% 
          pivot_wider(names_from = Sector),
        by="State") %>% 
        merge(aggregate(data = DF_Combined_Long %>% 
                          filter(Dataset==datadescription[5]) %>% 
                          mutate(Energy_Intensity= value),Energy_Intensity~State,FUN=mean),
              by = "State") %>% 
        merge(aggregate(data = DF_Combined_Long %>% 
                          filter(Dataset==datadescription[6]) %>% 
                          mutate(Carbon_Intensity= value),Carbon_Intensity~State,FUN=mean),
              by = "State")
      
      
      if (input$page3radio1 =="Keep Outliers"){
        sd_dist<-Inf
      }else{
        sd_dist<-3
      }

      t %>% 
        mutate(z = scale(t[input$page3select1])) %>% 
        filter(abs(z)<sd_dist) %>% 
        ggplot(mapping = aes_string(y = input$page3select2, x = input$page3select1))+
        geom_point(color = "skyblue", size = 3.5, alpha = 0.8)+
        geom_smooth(method = "lm",alpha = 0.2)+
        stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                     label.x.npc = "right", label.y.npc = .9,
                     formula = y~x, parse = TRUE, size = 4)+
        plottheme

    })
    
  }

  # Run the app ----
  shinyApp(ui = ui, server = server)
  
  
  }




