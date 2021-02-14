library(shiny)
library(shinydashboard)
library(ggplot2)
library(gridExtra)
library(DT)
library(usmap) 
library(data.table)
library(plyr)
library("stringr")







#Data Cleanup
data <- read.table(file = "annual_generation_state.csv", sep = ",", header = TRUE,stringsAsFactors=FALSE)
data$GENERATION..Megawatthours. <- gsub(",","",data$GENERATION..Megawatthours.)
data$GENERATION..Megawatthours.<- as.numeric(data$GENERATION..Megawatthours.)
data <- subset(data,data$STATE!= "  ")

data$STATE <- toupper(data$STATE)
data$STATE <- as.factor(data$STATE)
data$TYPE.OF.PRODUCER <- as.factor(data$TYPE.OF.PRODUCER)
data$ENERGY.SOURCE	 <- as.factor(data$ENERGY.SOURCE)
data1 <-  subset(data, data$GENERATION..Megawatthours.>=0)
data2 <-  subset(data1,TYPE.OF.PRODUCER=="Total Electric Power Industry"& ENERGY.SOURCE!="Total"&ENERGY.SOURCE!="Other" & ENERGY.SOURCE!="Other Biomass"& ENERGY.SOURCE!="Other Gases" & ENERGY.SOURCE!="Pumped Storage")
data2$ENERGY.SOURCE<-factor(data2$ENERGY.SOURCE)
levels(data2$ENERGY.SOURCE)[levels(data2$ENERGY.SOURCE)=="Hydroelectric Conventional"] ="Hydro"
levels(data2$ENERGY.SOURCE)[levels(data2$ENERGY.SOURCE)=="Wood and Wood Derived Fuels"] ="Wood"
levels(data2$ENERGY.SOURCE)[levels(data2$ENERGY.SOURCE)=="Solar Thermal and Photovoltaic"] ="Solar"
rownames(data2) <- 1:nrow(data2)

#renaming states 
data2$STATE <- as.character(data2$STATE)
data2[data2 == "AK"] <- "Alaska"
data2[data2 == "AL"] <- "Alabama"
data2[data2 == "AR"] <- "Arkansas"
data2[data2 == "AZ"] <- "Arizona"
data2[data2 == "CA"] <- "California"
data2[data2 == "CO"] <- "Colorado"
data2[data2 == "CT"] <- "Connecticut"
data2[data2 == "DC"] <- "District of Columbia"
data2[data2 == "DE"] <- "Delaware"
data2[data2 == "FL"] <- "Florida"
data2[data2 == "GA"] <- "Georgia"
data2[data2 == "HI"] <- "Hawaii"
data2[data2 == "ID"] <- "Idaho"
data2[data2 == "IA"] <- "Iowa"
data2[data2 == "IL"] <- "Illinois"
data2[data2 == "IN"] <- "Indiana"
data2[data2 == "KS"] <- "Kansas"
data2[data2 == "KY"] <- "Kentucky"
data2[data2 == "LA"] <- "Louisiana"
data2[data2 == "MA"] <- "Massachusetts"
data2[data2 == "MD"] <- "MaryLand"
data2[data2 == "ME"] <- "Maine"
data2[data2 == "MI"] <- "Michigan"
data2[data2 == "MN"] <- "Minnesota"
data2[data2 == "MO"] <- "Missouri"
data2[data2 == "MS"] <- "Mississippi"
data2[data2 == "MT"] <- "Montana"
data2[data2 == "NC"] <- "North Carolina"
data2[data2 == "ND"] <- "North Dakota"
data2[data2 == "NE"] <- "Nebraska"
data2[data2 == "NH"] <- "New Hampshire"
data2[data2 == "NJ"] <- "New Jersey"
data2[data2 == "NM"] <- "New Mexico"
data2[data2 == "NV"] <- "Nevada"
data2[data2 == "NY"] <- "New York"
data2[data2 == "OH"] <- "Ohio"
data2[data2 == "OK"] <- "Oklahoma"
data2[data2 == "OR"] <- "Orgeon"
data2[data2 == "PA"] <- "Pennsylvania"
data2[data2 == "RI"] <- "Rhode Island"
data2[data2 == "SC"] <- "South Carolina"
data2[data2 == "SD"] <- "South Dakota"
data2[data2 == "TN"] <- "Tennesse"
data2[data2 == "TX"] <- "Texas"
data2[data2 == "UT"] <- "Utah"
data2[data2 == "VA"] <- "Virginia"
data2[data2 == "VT"] <- "Vermont"
data2[data2 == "WA"] <- "Washington"
data2[data2 == "WI"] <- "Wisconsin"
data2[data2 == "WV"] <- "West Virginia"
data2[data2 == "WY"] <- "Wyoming"
data2$STATE <- as.factor(data2$STATE)

data5 = data2
data5 = ddply(data5, .(YEAR), transform, percent =round(GENERATION..Megawatthours./sum(GENERATION..Megawatthours.)*100,4))
colnames(data5)[2] <- "state"

#making states list,energy list and year list
states_List <- c(
  "US-TOTAL",
   "Alaska",
   "Alabama",
   "Arkansas",
   "Arizona",
   "California",
   "Colorado",
  "Connecticut",
  "District of Columbia",
  "Delaware",
  "Florida",
  "Georgia",
  "Hawaii",
 "Idaho",
  "Iowa",
   "Illinois",
  "Indiana",
  "Kansas",
 "Kentucky",
  "Louisiana",
  "Massachusetts",
   "MaryLand",
   "Maine",
 "Michigan",
  "Minnesota",
  "Missouri",
   "Mississippi",
 "Montana",
 "North Carolina",
  "North Dakota",
   "Nebraska",
 "New Hampshire",
  "New Jersey",
   "New Mexico",
   "Nevada",
  "New York",
   "Ohio",
   "Oklahoma",
   "Orgeon",
 "Pennsylvania",
  "Rhode Island",
  "South Carolina",
   "South Dakota",
  "Tennesse",
   "Texas",
 "Utah",
   "Virginia",
  "Vermont",
  "Washington",
   "Wisconsin",
 "West Virginia",
 "Wyoming"
)
energy_List <-c(
  "All",
  "Coal",
  "Geothermal",
  "Hydro" ,
  "Natural Gas" ,
  "Nuclear" ,
  "Petroleum",
  "Solar" ,
  "Wind",
  "Wood"
  )
  years_List <-c(
    "All Years",
    1990,
    1991,
    1992,
    1993,
    1994,
    1995,
    1996,
    1997,
    1998,
    1999,
    2000,
    2001,
    2002,
    2003,
    2004,
    2005,
    2006,
    2007,
    2008,
    2009,
    2010,
    2011,
    2012,
    2013,
    2014,
    2015,
    2016,
    2017,
    2018,
    2019
  )


ui<- fluidPage(
  tags$head(
    tags$style(HTML("
      .selectize-input {
        height: 50px;
        width: 60px;
        font-size: 10pt;
        padding-top: 5px;
      }
    "))
   ),
  dashboardPage(
    dashboardHeader(title = "CS 424 Spring 2021 Project 1"),
    dashboardSidebar(disable = FALSE, collapsed = FALSE, width = 300,
    fluidRow(
                       
      column(5,selectInput("Year", "Select Year(LeftPlot) ",years_List ,selected = "All years")),
      column(5,offset = 1,selectInput("Year1", "Select Year(RightPlot) ",years_List ,selected = "All years")),
      
      column(5,selectInput("State", "Select State(LeftPlot) ",states_List , selected = "Illinois")),
      column(5,offset=1,selectInput("State1", "Select State(RightPlot) ",states_List , selected = "US-TOTAL")),
      
      column(5,checkboxGroupInput('Energy', 'Select Energy(LeftPlot):', choices=energy_List,selected = "All")),
      column(5,offset=1,checkboxGroupInput('Energy1', 'Select Energy(RightPlot):', choices=energy_List,selected = "All")),
    )
    ),
  dashboardBody( 
    tabsetPanel(
      tabPanel(title="Stacked Bar Chart Visuals",
               fluidRow(
                 shinydashboard::box(title = "The Amount of Each Energy Source Per Year From 1990 - 2019", solidHeader = TRUE, status = "primary", width = 12,
                     splitLayout(cellWidths = c("50%", "50%"), plotOutput("plotOne",height = 200), plotOutput("plotOne1",height=200)))
               ),
               fluidRow(
                 shinydashboard::box(title = "The Percent of the Total Production for Each Energy SourcePer Year From 1990 - 2019", solidHeader = TRUE, status = "primary", width = 12,
                     splitLayout(cellWidths = c("50%", "50%"), plotOutput("plotTwo",height = 200), plotOutput("plotTwo1",height=200)))
               )
               
               ),
      tabPanel(title="Line Chart Visuals",
               
               
               fluidRow(
                 shinydashboard::box(title = "The Amount of Each Energy Source Per Year From 1990 - 2019", solidHeader = TRUE, status = "primary", width = 12,
                     splitLayout(cellWidths = c("50%", "50%"), plotOutput("plotFour",height = 200), plotOutput("plotFour1",height=200)))
               ),
               fluidRow(
                 shinydashboard::box(title = "The Amount of Each Energy Source Per Year From 1990 - 2019", solidHeader = TRUE, status = "primary", width = 12,
                     splitLayout(cellWidths = c("50%", "50%"), plotOutput("plotFive",height = 200), plotOutput("plotFive1",height=200)))
               )
      )

      ,
      tabPanel(title="Table Visual",
               shinydashboard::box(title = "The amount and percent of each energy source per year from 1990 - 2019", solidHeader = TRUE, status = "primary", width = 12,
                   splitLayout(cellWidths = c("50%", "50%"), DT::dataTableOutput("plotThree",height=200), DT::dataTableOutput("plotThree1",height=200))
               

                   
               )
               


                 
               

               ),
    
      tabPanel(title="US HeatMap Visual",
               fluidRow(
                 shinydashboard::box( title = "The amount of each energy source per year from 1990 - 2019", solidHeader = TRUE, status = "primary", width = 12,
                      splitLayout(cellWidths = c("50%", "50%"), plotOutput("plotSix", height = 200),
                                  plotOutput("plotSix1", height = 200)
)
                      
                 )
               ),
fluidRow(
  shinydashboard::box( title = "The percent of each energy source per year from 1990 - 2019", solidHeader = TRUE, status = "primary", width = 12,
       splitLayout(cellWidths = c("50%", "50%"), plotOutput("plotSeven", height = 200),
                   plotOutput("plotSeven1", height = 200)
       )
       
  )
)

              
                   
               ),
      tabPanel(title="About",
               "This project was done by AmalJosy Johnson on 2/13/2021.
               The data used for this project is from https://www.eia.gov/electricity/data/state/.
               This project shows variety of visualizations like stacked barchart,linechart,table
               and US heatmap to show and compare the amount and percent of energy sources in different states of
               United States between the years 1990-2019. The project has a custom input selection for 
               the user to select the state,year and energy source to view and can compare
               it with another data from a different state
               
               ")
               
               
               
               
               
               
      )
    
    )
       
   
  )
 )

server <- function(input,output,session){
  
  myReactiveFunc <- reactive({
   
     if(input$Year != 'All Years' & input$Energy != 'All' & input$State == "Illinois" )
     {
       newData <-data5[data5$YEAR == input$Year & data5$ENERGY.SOURCE == input$Energy & data5$state=="Illinois", ]
     } 
     else if(input$Year != 'All Years' & input$Energy == 'All' & input$State == "Illinois")
     {
       newData <-data5[data5$YEAR == input$Year & data5$ENERGY.SOURCE ==data5$ENERGY.SOURCE&data5$state=="Illinois"  ,]
     }  
     else if(input$Year == 'All Years' & input$Energy != 'All'& input$State == "Illinois")
     {
       newData <-data5[data5$YEAR == data5$YEAR  & data5$ENERGY.SOURCE ==input$Energy & data5$state=="Illinois" ,]
     } 
     else if(input$Year == 'All Years' & input$Energy == 'All'& input$State == "Illinois")
     {
       newData <-data5[data5$YEAR == data5$YEAR  & data5$ENERGY.SOURCE == data5$ENERGY.SOURCE & data5$state=="Illinois" ,]
     } 
     else if(input$Year != 'All Years' & input$Energy != 'All' & input$State != "Illinois"  & input$State !="US-TOTAL")
     {
       newData <-data5[data5$YEAR == input$Year & data5$ENERGY.SOURCE == input$Energy & data5$state==input$State, ]
     } 
     else if(input$Year != 'All Years' & input$Energy == 'All' & input$State != "Illinois"& input$State !="US-TOTAL")
     {
       newData <-data5[data5$YEAR == input$Year & data5$ENERGY.SOURCE ==data5$ENERGY.SOURCE&data5$state==input$State  ,]
     }  
     else if(input$Year == 'All Years' & input$Energy != 'All'& input$State != "Illinois"& input$State !="US-TOTAL")
     {
       newData <-data5[data5$YEAR == data5$YEAR  & data5$ENERGY.SOURCE ==input$Energy & data5$state==input$State,]
     } 
     else if(input$Year == 'All Years' & input$Energy == 'All'& input$State != "Illinois" & input$State !="US-TOTAL")
     {
       newData <-data5[data5$YEAR == data5$YEAR  & data5$ENERGY.SOURCE == data5$ENERGY.SOURCE & data5$state==input$State,]
     }
    
    else if(input$Year != 'All Years' & input$Energy != 'All' & input$State == "US-TOTAL"  )
    {
      newData <-data5[data5$YEAR == input$Year & data5$ENERGY.SOURCE == input$Energy & data5$state==data5$state, ]
    } 
    else if(input$Year != 'All Years' & input$Energy == 'All' & input$State == "US-TOTAL")
    {
      newData <-data5[data5$YEAR == input$Year & data5$ENERGY.SOURCE ==data5$ENERGY.SOURCE&data5$state==data5$state  ,]
    }  
    else if(input$Year == 'All Years' & input$Energy != 'All'& input$State  == "US-TOTAL")
    {
      newData <-data5[data5$YEAR == data5$YEAR  & data5$ENERGY.SOURCE ==input$Energy & data5$state==data5$state,]
    } 
    else if(input$Year == 'All Years' & input$Energy == 'All'& input$State  == "US-TOTAL" )
    {
      newData <-data5[data5$YEAR == data5$YEAR  & data5$ENERGY.SOURCE == data5$ENERGY.SOURCE & data5$state==data5$state,]
    }
    
     else
     {
       print("hi")
       data5
       #newData <-data5[data5$YEAR == input$Year  & data5$ENERGY.SOURCE == input$Energy & data5$state==data5$state,data5$GENERATION..Megawatthours.==subset(data5,data5$GENERATION..Megawatthours. %in% c(input$Energy)),]
     }
 })
 
 myReactiveFunc1 <- reactive({
   if(input$Year1 != 'All Years' & input$Energy1 != 'All' & input$State1 == "US-TOTAL" )
   {
     newData <-data5[data5$YEAR == input$Year1 & data5$ENERGY.SOURCE == input$Energy1 & data5$state==data5$state, ]
   } 
   else if(input$Year1 != 'All Years' & input$Energy1 == 'All' & input$State1 == "US-TOTAL")
   {
     newData <-data5[data5$YEAR == input$Year1 & data5$ENERGY.SOURCE ==data5$ENERGY.SOURCE&data5$state==data5$state  ,]
   }  
   else if(input$Year1 == 'All Years' & input$Energy1 != 'All'& input$State1 == "US-TOTAL")
   {
     newData <-data5[data5$YEAR == data5$YEAR  & data5$ENERGY.SOURCE ==input$Energy1 & data5$state==data5$state ,]
   } 
   else if(input$Year1 == 'All Years' & input$Energy1 == 'All'& input$State1 == "US-TOTAL")
   {
     newData <-data5[data5$YEAR == data5$YEAR  & data5$ENERGY.SOURCE == data5$ENERGY.SOURCE & data5$state==data5$state ,]
   } 
   else if(input$Year1 != 'All Years' & input$Energy1 != 'All' & input$State1 != "US-TOTAL" )
   {
     newData <-data5[data5$YEAR == input$Year1 & data5$ENERGY.SOURCE == input$Energy1 & data5$state==input$State1, ]
   } 
   else if(input$Year1 != 'All Years' & input$Energy1 == 'All' & input$State1 != "US-TOTAL")
   {
     newData <-data5[data5$YEAR == input$Year1 & data5$ENERGY.SOURCE ==data5$ENERGY.SOURCE&data5$state==input$State1  ,]
   }  
   else if(input$Year1 == 'All Years' & input$Energy1 != 'All'& input$State1 != "US-TOTAL")
   {
     newData <-data5[data5$YEAR == data5$YEAR  & data5$ENERGY.SOURCE ==input$Energy1 & data5$state==input$State1,]
   } 
   else if(input$Year1 == 'All Years' & input$Energy1 == 'All'& input$State1 != "US-TOTAL")
   {
     newData <-data5[data5$YEAR == data5$YEAR  & data5$ENERGY.SOURCE == data5$ENERGY.SOURCE & data5$state==input$State1,]
   }
   
   else
   {
     print("hi")
     data5
     #newData <-data5[data5$YEAR == input$Year  & data5$ENERGY.SOURCE == input$Energy & data5$state==data5$state,data5$GENERATION..Megawatthours.==subset(data5,data5$GENERATION..Megawatthours. %in% c(input$Energy)),]
   }
 })
 
output$plotOne <- renderPlot({
  reactiveFunc <- myReactiveFunc()

  irisColors1 <-
   setNames( c('brown2', 'darkolivegreen4', 'springgreen3','pink','brown','orange2','green4','deepskyblue3','blue','deeppink2')
             , levels(data2$ENERGY.SOURCE)  )
  irisColors <-
    setNames( c('red', 'forestgreen', 'blue','Yellow','green','orange','brown','black','navy')
              , levels(data2$ENERGY.SOURCE)  )
  ggplot() + geom_bar(aes_string(y = reactiveFunc$GENERATION..Megawatthours.,  x = reactiveFunc$YEAR, 
                                 fill =reactiveFunc$ENERGY.SOURCE), data =reactiveFunc,
                                 stat="identity") +
    guides(fill = guide_legend(ncol = 8, title.position = "top")) +
    theme(legend.position = "bottom")+
    scale_x_continuous(breaks=seq(1990,2019,5))+
    scale_y_continuous(labels = scales::comma) +
    labs(x=paste("Energy in", input$Year," ,",input$State), y="Amount of Power",fill="Energy Source") +
    theme(text = element_text(size = 8))+ scale_fill_manual(values = irisColors)

  })

output$plotOne1 <- renderPlot({
  reactiveFunc <- myReactiveFunc1()
  
  irisColors <-
    setNames( c('red', 'forestgreen', 'blue','Yellow','green','orange','brown','black','navy')
              , levels(data2$ENERGY.SOURCE)  )
  ggplot() + geom_bar(aes_string(y = reactiveFunc$GENERATION..Megawatthours.,  x = reactiveFunc$YEAR,
                                 fill =reactiveFunc$ENERGY.SOURCE), data =reactiveFunc,
                                 stat="identity") +
    guides(fill = guide_legend(ncol = 8, title.position = "top")) +
    theme(legend.position = "bottom")+
    scale_x_continuous(breaks=seq(1990,2019,5))+
    scale_y_continuous(labels = scales::comma) +
    labs(x=paste("Energy in", input$Year1," ,",input$State1), y="Amount of Power",fill="Energy Source") +
    theme(text = element_text(size = 8)) + scale_fill_manual(values = irisColors)
})

output$plotTwo <- renderPlot({
  reactiveFunc <- myReactiveFunc()
  
  irisColors <-
    setNames( c('red', 'forestgreen', 'blue','Yellow','green','orange','brown','black','navy')
              , levels(data2$ENERGY.SOURCE)  )
  n_id <- length(unique(data2$GENERATION..Megawatthours.))
  
  ggplot() + geom_bar(aes(x = reactiveFunc$YEAR,y = reactiveFunc$percent,
                                 fill =reactiveFunc$ENERGY.SOURCE), data =reactiveFunc,
                      stat="identity") +
    guides(fill = guide_legend(ncol = 8, title.position = "top")) +
    theme(legend.position = "bottom")+
    scale_x_continuous(breaks=seq(1990,2019,5))+
    scale_y_continuous(breaks = scales::pretty_breaks(10),label = scales::percent_format(scale = 100 ))+
    labs(x=paste("Energy in", input$Year," ,",input$State), y="Amount of Power",fill="Energy Source") +
    theme(text = element_text(size = 8)) + scale_fill_manual(values = irisColors)
})

output$plotTwo1 <- renderPlot({
  reactiveFunc <- myReactiveFunc1()
  
  irisColors <-
    setNames( c('red', 'forestgreen', 'blue','Yellow','green','orange','brown','black','navy')
              , levels(data2$ENERGY.SOURCE)  )
  n_id <- length(unique(data2$GENERATION..Megawatthours.))
  
  ggplot() + geom_bar(aes(y = reactiveFunc$percent,  x = reactiveFunc$YEAR,
                                 fill =reactiveFunc$ENERGY.SOURCE), data =reactiveFunc,
                      stat="identity") +
    guides(fill = guide_legend(ncol = 8, title.position = "top")) +
    theme(legend.position = "bottom")+
    scale_x_continuous(breaks=seq(1990,2019,5))+
    scale_y_continuous(breaks = scales::pretty_breaks(10),label = scales::percent_format(scale = 100 ))+
    labs(x=paste("Energy in", input$Year1," ,",input$State1), y="Amount of Power",fill="Energy Source") +
    theme(text = element_text(size = 8)) + scale_fill_manual(values = irisColors)
})


output$plotThree <- DT::renderDataTable(
  
   DT::datatable({ 
     
    reactiveFunc <-  myReactiveFunc()
    data <- data.table(reactiveFunc[,c("YEAR","state","ENERGY.SOURCE","GENERATION..Megawatthours.","percent")])
    }, 
  options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc'))
  ), rownames = FALSE 
  )
)
output$plotThree1 <- DT::renderDataTable({
  irisColors <-
    setNames( c('red', 'forestgreen', 'blue','Yellow','green','orange','brown','black','navy')
              , levels(data2$ENERGY.SOURCE)  )
  DT::datatable({ 
    reactiveFunc <-  myReactiveFunc1()
    data <- data.table(reactiveFunc[,c("YEAR","state","ENERGY.SOURCE","GENERATION..Megawatthours.","percent")])
  }, 
  options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc'))
  ), rownames = FALSE 
  )

   
})
output$plotFour <- renderPlot({
  reactiveFunc <- myReactiveFunc()
  irisColors <-
    setNames( c('red', 'forestgreen', 'blue','Yellow','green','orange','brown','black','navy')
              , levels(data2$ENERGY.SOURCE)  )
  ggplot(reactiveFunc, aes(x=reactiveFunc$YEAR, y=reactiveFunc$GENERATION..Megawatthours.,colour=reactiveFunc$ENERGY.SOURCE)) +scale_x_continuous(breaks=seq(1990,2019,1))+
    scale_y_continuous(labels = scales::comma)+
    geom_line()+
    geom_point()+
    guides(fill = guide_legend(ncol = 8, title.position = "top")) +
    theme(legend.position = "bottom")+
    labs(x=paste("Energy in", input$Year," ,",input$State), y="Amount of Power",color="Energy Source") +
    theme(text = element_text(size = 8)) + scale_color_manual(values = irisColors)+
    scale_x_continuous(breaks=seq(1990,2019,5))+
    scale_y_continuous(labels = scales::comma) 
  
  
  
  
})
output$plotFour1 <- renderPlot({
  reactiveFunc <- myReactiveFunc1()
  irisColors <-
    setNames( c('red', 'forestgreen', 'blue','Yellow','green','orange','brown','black','navy')
              , levels(data2$ENERGY.SOURCE)  )
  ggplot(reactiveFunc, aes(x=reactiveFunc$YEAR, y=reactiveFunc$GENERATION..Megawatthours.,colour=reactiveFunc$ENERGY.SOURCE)) +scale_x_continuous(breaks=seq(1990,2019,1))+
    scale_y_continuous(labels = scales::comma)+
    geom_line()+
    geom_point()+
    guides(fill = guide_legend(ncol = 8, title.position = "top")) +
    theme(legend.position = "bottom")+
    labs(x=paste("Energy in", input$Year1," ,",input$State1), y="Amount of Power",color="Energy Source") +
    theme(text = element_text(size = 8)) + scale_color_manual(values = irisColors)+
    scale_x_continuous(breaks=seq(1990,2019,5))+
    scale_y_continuous(labels = scales::comma) 
  
  
  
  
})
output$plotFive <- renderPlot({
  reactiveFunc <- myReactiveFunc()
  irisColors <-
    setNames( c('red', 'forestgreen', 'blue','Yellow','green','orange','brown','black','navy')
              , levels(data2$ENERGY.SOURCE)  )
  ggplot(reactiveFunc, aes(x=reactiveFunc$YEAR, y=reactiveFunc$GENERATION..Megawatthours./max(reactiveFunc$GENERATION..Megawatthours.),colour=reactiveFunc$ENERGY.SOURCE)) +
    geom_line()+
    geom_point()+
    guides(fill = guide_legend(ncol = 8, title.position = "top")) +
    theme(legend.position = "bottom")+
    labs(x=paste("Energy in", input$Year," ,",input$State), y="Amount of Power",color="Energy Source") +
    theme(text = element_text(size = 8)) + scale_color_manual(values = irisColors)+
    scale_x_continuous(breaks=seq(1990,2019,5))+
    scale_y_continuous(breaks = scales::pretty_breaks(10),label = scales::percent_format(scale = 100 ))
  
  
  
  
})
output$plotFive1 <- renderPlot({
  reactiveFunc <- myReactiveFunc1()
  irisColors <-
    setNames( c('red', 'forestgreen', 'blue','Yellow','green','orange','brown','black','navy')
              , levels(data2$ENERGY.SOURCE)  )
  ggplot(reactiveFunc, aes(x=reactiveFunc$YEAR, y=reactiveFunc$GENERATION..Megawatthours./max(reactiveFunc$GENERATION..Megawatthours.),colour=reactiveFunc$ENERGY.SOURCE))+
    geom_line()+
    geom_point()+
    guides(fill = guide_legend(ncol = 8, title.position = "top")) +
    theme(legend.position = "bottom")+
    labs(x=paste("Energy in", input$Year1," ,",input$State1), y="Amount of Power",color="Energy Source") +
    theme(text = element_text(size = 8)) + scale_color_manual(values = irisColors)+
    scale_x_continuous(breaks=seq(1990,2019,5))+
    scale_y_continuous(breaks = scales::pretty_breaks(10),label = scales::percent_format(scale = 100 ))
    
  
  
  
})

output$plotSix <- renderPlot({

  reactiveFunc <-  myReactiveFunc()


 
    plot_usmap(data = reactiveFunc, values = "GENERATION..Megawatthours.",labels=TRUE, color = "red",exclude="DC") + 
      scale_fill_continuous(low = "lightyellow", high = "red", name = "Amount of Energy Source", label = scales::comma) + 
      theme(legend.position = "right")
   
})
output$plotSix1 <- renderPlot({
  
  reactiveFunc <-  myReactiveFunc1()
  
  
  
  
  plot_usmap(data = reactiveFunc, values = "GENERATION..Megawatthours.",labels=TRUE, color = "red",exclude="DC") + 
    scale_fill_continuous(low = "lightyellow", high = "red", name = "Amount of Energy Source", label = scales::comma) + 
    theme(legend.position = "right")
  
})
output$plotSeven <- renderPlot({
  
  reactiveFunc <-  myReactiveFunc()
  
  

 plot_usmap(data = reactiveFunc, values = "percent",labels=TRUE, color = "red",exclude="DC") + 
    scale_fill_continuous(low = "lightyellow", high = "red", name = "Percent of Energy Source", label = scales::percent_format(scale = 100 )) + 
    theme(legend.position = "right")
  
  
})
output$plotSeven1 <- renderPlot({
  
  reactiveFunc <-  myReactiveFunc1()
  
  
  
  
  plot_usmap(data = reactiveFunc, values = "percent",labels=TRUE, color = "red",exclude="DC") + 
    scale_fill_continuous(low = "lightyellow", high = "red", name = "Percent of Energy Source", label = scales::percent_format(scale = 100 )) + 
    theme(legend.position = "right")
  
  
})
}
shinyApp(ui=ui,server=server)

