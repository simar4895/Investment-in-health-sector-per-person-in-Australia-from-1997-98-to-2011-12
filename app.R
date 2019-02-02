# Load libraries and prepare data

library(shiny)
library(tidyverse)
library(xlsx)
library(lubridate)
library(plotly)
health_expenditure <- read_csv("healthexpenditurebyareaandsource.csv")
health_expenditure$state <- as.factor(health_expenditure$state)
health_expenditure$financial_year <- as.factor(health_expenditure$financial_year)
health_expenditure$broad_source_of_funding <- as.factor(health_expenditure$broad_source_of_funding)
health_expenditure <- health_expenditure %>% filter(is.na(health_expenditure$real_expenditure_millions) == FALSE,area_of_expenditure %in% c("Medical expense tax rebate")==FALSE)
health_expenditure$area_of_expenditure <- as.factor(health_expenditure$area_of_expenditure)
health_expenditure <- health_expenditure %>% group_by(financial_year,area_of_expenditure,state,broad_source_of_funding) %>% summarise(expenditure_million = sum(real_expenditure_millions))
population <- read.xlsx("310101.xls",sheetIndex = 2,startRow = 11,header = T)
population <- population %>% select(1,20:27)
colnames(population) <- c("Year","NSW","VIC","QLD","SA","WA","TAS","NT","ACT")
population <- population %>% filter(month(population$Year)==3,year(population$Year)>1997,year(population$Year)<2013)
population$financial_year <- paste0(year(population$Year)-1,"-",str_sub(year(population$Year),start = 3))
population <- population %>% select(10,2:9)
population <- population %>% gather(c("NSW":"ACT"),key = "state",value = "population")
population$financial_year <- as.factor(population$financial_year)
population$state <- as.factor(population$state)
health_expenditure <- left_join(health_expenditure,population,by=c("financial_year","state"))
health_expenditure <- health_expenditure %>% mutate(expenditure_per_person = expenditure_million/population*1000000)

# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel(h1("Investment in health sector per person in Australia from 1997-98 to 2011-12",align = "center"),windowTitle = "Medical investment"),
  
  
  sidebarLayout(position = "right", 
    sidebarPanel(width = 3,br(),
      p("This app displays the medical investment in Australia per person over the financial years 1997-98 to 2011-12 and the statewise breakdown.",align = "center"),
      br(),
      h2("Options",align = "center"),
      h3("Funding Sector",align = "center"),
      selectInput("category", label = NULL, 
                         choices = levels(health_expenditure$area_of_expenditure),
                         selected = "Community health"),
      p("Choose a sector in the medical domain to look at its funding trends",align = "center"),
      
      br(),
      h3("Funding Source",align = "center"),
      checkboxGroupInput("source", label = NULL, 
                         choices = levels(health_expenditure$broad_source_of_funding),
                         selected = "Government",inline = T),
      p("Select atleast one funding source",align = "center"),
      br()
    ),
    
    
    
    # Show a plot of the generated distribution
    mainPanel(width=9,
      plotlyOutput("line", height = "260px"),
      br(),
      plotlyOutput("bar", height = "260px")
      )
    )
  )


# Define server logic
server <- function(input, output) {
  
  
  output$line <- renderPlotly({
    
    health_expenditure_plot <- health_expenditure %>% filter(area_of_expenditure == input$category,broad_source_of_funding %in% input$source) %>% group_by(financial_year) %>% summarise(`expenditure per person` = sum(expenditure_per_person))
      
   ggplotly(ggplot(data = health_expenditure_plot, aes(x = financial_year, 
                           y = `expenditure per person`)) +
      geom_point(color = "#8da0cb") + geom_line(aes(group = 1),color = "#8da0cb") + labs(x = "Financial Year", y = "Investment/person ($)")+ ggtitle("Funding per year per person across all states"))
    
    })
  
  output$bar <- renderPlotly({
    
    health_expenditure_bar <- health_expenditure %>% filter(area_of_expenditure == input$category,broad_source_of_funding %in% input$source) %>% group_by(state) %>% summarise(`expenditure per person` = mean(expenditure_per_person))
    
    ggplotly(ggplot(data = health_expenditure_bar, aes(x = state, 
                                               y = `expenditure per person`)) +
      geom_bar(stat = "identity",fill = "#8da0cb") + labs(x = "State", y = "Average investment/person ($)") + geom_bar(data=subset(health_expenditure_bar, `expenditure per person`==max(`expenditure per person`)), aes(state, `expenditure per person`),
                                                                                                                          fill="#7fc97f", stat="identity")+ ggtitle("Average funding per person per state"))
    
     })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

