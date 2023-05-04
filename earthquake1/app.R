library(shiny)
library(DBI)
library(ggplot2)
library(lubridate)
library(rsconnect)
library(RSQLite)

library(RPostgres)
library(pool)

pool <- dbPool(
  drv = Postgres(),
  dbname = "Mydatabase",
  host = "127.0.0.1",
  port = 5432,
  user = "postgres",
  password = "0823"
)

# Define UI
ui <- fluidPage(
  titlePanel("GDP Rate After Earthquake"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("place", "Select Country:",
                  choices = c("Japan", "Taiwan", "Mexico", "Philippines", 
                              "New Zealand", "Chile", "Bolivia", "Turkey",
                              "Russia", "Pakistan", "Peru", "Indonesia",
                              "Fiji", "United States", "Iran", "China", "Bolivia",
                              "Egypt", "South Africa", "Venezuela", "El Salvador", "Iraq",
                              "Nepal", "Bangladesh","Haiti", "Yemen" )),
      sliderInput("year", "Select Year:",
                  min = 1990, max = 2023, value = 2010)
    ),
    
    mainPanel(
      plotOutput("gdp_plot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  server <- function(input, output) {
    
    # Define a reactive expression to get a database connection
    db_con <- reactive({
      poolCheckout(pool)
    })
    
    # Disconnect from database when the session is closed
    session$onSessionEnded(function() {
      poolClose(pool)
    })
    
    # Use the database connection in reactive expressions
    eq_data <- reactive({
      query <- sprintf("SELECT * FROM earthquake WHERE place = '%s' AND extract(year from date) = %s", 
                       input$place, input$year)
      eq_data <- dbGetQuery(db_con(), query)
      return(eq_data)
    })
    
    gdp_data <- reactive({
      # Use db_con() instead of pool in the following queries
      if (nrow(eq_data()) > 0) {
        date_one_year_after <- as.Date(eq_data()$date) %m+% years(1)
        query <- sprintf("SELECT * FROM gdp WHERE place = '%s'", input$place)
        gdp_data <- dbGetQuery(db_con(), query)
        if (nrow(gdp_data) > 0) {
          if (!is.na(eq_data()$date)) {
            gdp_data <- gdp_data[gdp_data$date >= date_one_year_after & gdp_data$date <= as.Date(eq_data()$date) %m+% years(5),]
          } else {
            gdp_data <- NULL
          }
        }
        return(gdp_data)
      } else {
        query <- sprintf("SELECT * FROM gdp WHERE place = '%s'", input$place)
        gdp_data <- dbGetQuery(db_con(), query)
        return(gdp_data)
      }
    })
    
    # Release the database connection with poolReturn() after using it
    observeEvent(eq_data(), {
      poolReturn(db_con())
    })
    
    observeEvent(gdp_data(), {
      poolReturn(db_con())
    })
    
    # Render plot
    output$gdp_plot <- renderPlot({
      if (!is.null(eq_data())) {
        earthquake_date <- format(as.Date(eq_data()$date), "%Y")
        subtitle <- paste("Earthquake Date:", earthquake_date)
      } else {
        subtitle <- ""
      }
      if (!is.null(gdp_data())) {
        ggplot(data = gdp_data(), aes(x = date, y = gdprate)) +
          geom_line() +
          labs(title = paste("GDP Rate After Earthquake in", input$place),
               subtitle = subtitle,
               x = "Date", y = "GDP Rate")
      } else {
        ggplot() + labs(title = paste("No GDP Data Available for", input$place))
      }
    })
  }
  
 

# sqllite
# Be sure the data file must be in same folder
sqlite_conn <- dbConnect(RSQLite::SQLite(), dbname ='earthquake.db')



# Deploy


#library(rsconnect)
#rsconnect::setAccountInfo(name='michellebomikim',token='EACFB8C3507EDEC3DD24DD9EB1B83FAD',secret='seP/2P6kjDOhiX8NxvP8luflOLuJOmrzQyJxIHJb')
                          
                          

