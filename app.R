library(shiny)
library(tidyverse)
library(DT)
#setwd("~/fantasyfootball/yearly")

# Load in and modify data
"All_years.csv" %>%
  read_csv() %>% 
  arrange(Year) %>%
  filter(FantasyPoints >= 50.0) %>%
  select(Player, Year, Tm, Pos, G, PassAtt, PassYards, RushAtt, RushYards, Rec, RecYards, FantasyPoints) %>%
  filter(Pos != "0") %>%
  mutate(TotalYds = RushYards + RecYards + PassYards,
         TotalTouches = RushAtt + Rec + PassAtt) -> data

# Make UI for app
ui <- shinyUI(fluidPage(
  h1("Historical Fantasy Football Graphs"),
  h5("Select a year, and see the fantasy football leaders from then!"),
  p("Made by Pranav Rajaram. All data was taken from", 
    a("here.",
      href = "https://github.com/fantasydatapros/data", target="blank")),
  fluidRow(
    sidebarLayout(
      sidebarPanel(                                     
        selectInput("year","Choose a Year", choices = data$Year),
        selectInput("position", "Choose a Position", choices = data$Pos)
      ),
      
      mainPanel(
        plotOutput("playerplot"),
        DT::dataTableOutput("mytable")
      )
    )
  )
))


# Make server function for app
server <- function(input, output) {
  
  # Filters data for player selected above
  selectedData <- reactive({
    data %>%
      filter(Year == input$year) %>%
      filter(Pos == input$position) %>%
      head(n = 36) %>%
      arrange(desc(FantasyPoints)) %>%
      mutate(AVG1 = FantasyPoints/G) %>%
      mutate(AVG = round(AVG1, digits = 2)) %>%
      select(Player, FantasyPoints, Tm, G, AVG, TotalTouches)
  })
  
  # Plot
  output$playerplot <- renderPlot({
    ggplot(data = data) +
      geom_point(data = selectedData(),
                 aes(x = TotalTouches,
                     y = FantasyPoints)) +
      geom_text(alpha = 0.7,
                size = 3.5,
                data = selectedData(),
                aes(x = TotalTouches, 
                    y = FantasyPoints,
                    label = Player),
                nudge_y = 4) +
      labs(title = "Fantasy Football Historical Plots",
           subtitle = "PPR Scoring",
           x = "Total Touches(Pass Attempts + Receptions + Carries)",
           y = "Fantasy Points") +
      theme(legend.position = "bottom")
  })
  
  output$mytable = DT::renderDataTable({
    selectedData()
  })
}

# Runs App
shinyApp(ui = ui, server = server)





