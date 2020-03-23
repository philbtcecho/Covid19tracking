#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse) 
library(lubridate)
confirmed <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv") %>% select(-c(Lat,Long))
death <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv") %>% select(-c(Lat,Long))
recovered <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv") %>% select(-c(Lat,Long))

transform_tibble_temp <- function(tab)
    tab %>% select(-`Province/State`) %>% pivot_longer(names_to = "date",values_to = "vals",-`Country/Region`) %>% group_by(`Country/Region`,date) %>% summarise(vals=sum(vals)) %>% mutate(date=as.Date(date, format="%m/%e/%y"))

simpleCap <- function(x) {
    s <- strsplit(x, "-")[[1]]
    paste(toupper(substring(s, 1,1)), substring(s, 2),
          sep="", collapse="-")
}

confirmed_temp <- transform_tibble_temp(confirmed) %>% rename(Confirmed = vals)
death_temp <- transform_tibble_temp(death) %>% rename(Death = vals)
recovered_temp <- transform_tibble_temp(recovered) %>% rename(Recovered = vals)
data_final <- confirmed_temp %>% left_join(death_temp, by=c("Country/Region","date")) %>% left_join(recovered_temp, by=c("Country/Region","date")) %>% rename(Country=`Country/Region`, Date = date)

data_final <- data_final %>% mutate(Active = Confirmed - Death - Recovered,`Death per Confirmed`=Death/Confirmed,`Recovered per Confirmed`=Recovered/Confirmed,`Recovered per Death`=Recovered/Death)

vars <- names(data_final)[-1]
countries <- data_final %>% arrange(Date) %>% filter(Date<as_date("2020-03-21")) %>% group_by(Country) %>% slice(n()) %>% arrange(desc(Confirmed)) %>% select(Country)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Coronatracker"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput('selection1', 'Variable 1', vars,selected = vars[1]),
            selectInput('selection2', 'Variable 2', vars,selected = vars[2]),
            checkboxInput("Log2", "Log?", value = TRUE, width = NULL),
            selectInput('countries', 'Countries', countries, multiple=TRUE, selectize=FALSE,selected = "Germany"),
            dateRangeInput('daterange','Date Range',start = as_date("2020-01-01"), end = Sys.Date()-1),
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$distPlot <- renderPlot({
       g <- data_final %>% filter(Country %in% input$countries, between(Date,input$daterange[1],input$daterange[2]))%>% ggplot(aes_q(as.name(input$selection1), as.name(input$selection2))) + geom_point(aes(col=Country)) + labs(col="")
       if(input$Log2)
           g + scale_y_log10()
       else
           g
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
