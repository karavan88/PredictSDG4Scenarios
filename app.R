library(shiny)
library(highcharter)
library(dplyr)
library(readr)

# Load the data outside the server function so it only loads once
df_full <- read_csv("Country Data SDG4.csv")

# Define the user interface
ui <- fluidPage(
  titlePanel("Dynamic Highcharter Plot"),
  
  # Define the selection inputs layout
  fluidRow(
    column(8, # Increase width for longer names
           selectInput("country", "Select Country", choices = unique(df_full$COUNTRY))
    ),
    column(8, # Increase width for longer names
           selectInput("indicator_label", "Select Indicator", choices = unique(df_full$INDICATOR_CODE))
    )
  ),
  
  # Highchart output
  highchartOutput("hchart", height = "600px")
  
  # # Place selection inputs above the chart in a row
  # fluidRow(
  #   column(4, selectInput("country", "Select Country", choices = unique(df_full$COUNTRY))),
  #   column(4, selectInput("indicator_label", "Select Indicator", choices = NULL))
  # ),
  # 
  # # Highchart output
  # highchartOutput("hchart")
)

# Define server logic
server <- function(input, output, session) {
  
  # Update indicator_code choices based on iso3 selection
  observe({
    df_iso <- df_full %>% filter(COUNTRY == input$country)
    updateSelectInput(session, "indicator_label", choices = unique(df_iso$INDICATOR_LABEL))
  })
  
  # Render the Highcharter plot
  output$hchart <- renderHighchart({
    # Filter the dataset based on the current input
    df <- df_full %>%
      filter(COUNTRY == input$country, INDICATOR_LABEL == input$indicator_label)
    
    # Filter the dataset for the different series we want to plot
    historic_data <- df %>%
      filter(VAL_TYPE == "EST") %>%
      select(YEAR, VALUE)
    
    bau_data <- df %>%
      filter(VAL_TYPE == "PROJ", SCENARIO == "BAU") %>%
      select(YEAR, VALUE)
    
    mtt_data <- df %>%
      filter(VAL_TYPE == "PROJ", SCENARIO == "MTT") %>%
      select(YEAR, VALUE)
    
    target_data <- df %>%
      filter(VAL_ROLE == "TRGT") %>%
      select(YEAR, VALUE)
    
    # Create the list of x, y pairs for highcharter
    historic_list <- historic_data %>% mutate(x = YEAR, y = VALUE) %>% select(x, y) %>% list_parse()
    bau_list <- bau_data %>% mutate(x = YEAR, y = VALUE) %>% select(x, y) %>% list_parse()
    mtt_list <- mtt_data %>% mutate(x = YEAR, y = VALUE) %>% select(x, y) %>% list_parse()
    target_list <- target_data %>% mutate(x = YEAR, y = VALUE) %>% select(x, y) %>% list_parse()
    
    
    
    # Generate the highchart with a navigator
    highchart() %>%
      hc_chart(
        zoomType = 'x', # Enables zooming along the x-axis
        animation = TRUE, # Chart initial animation
        backgroundColor = "#ECF0F1" # Light grey background
      ) %>%
      hc_title(text = "Completion Rate Projections") %>%
      hc_subtitle(text = "Historic and Projected Completion Rates") %>%
      hc_xAxis(
        categories = unique(df$YEAR),
        crosshair = TRUE
      ) %>%
      hc_yAxis(
        title = list(text = "Completion Rate (%)")
      ) %>%
      hc_add_series(name = "Historic Data", type = 'line', data = historic_list, color = "#3498DB") %>%
      hc_add_series(name = "Business As Usual", type = 'line', data = bau_list, color = "#F1C40F") %>%
      hc_add_series(name = "Meet The Target", type = 'line', data = mtt_list, color = "#E74C3C") %>%
      hc_add_series(name = "Targets", type = 'scatter', data = target_list, color = '#2ECC71', 
                    marker = list(radius = 6, symbol = 'circle')) %>%
      hc_tooltip(
        shared = TRUE,
        crosshairs = TRUE,
        headerFormat = '<b>{series.name}</b><br>',
        pointFormat = 'Year: <b>{point.x}</b><br>Completion Rate: <b>{point.y}%</b>'
      ) %>%
      hc_navigator(enabled = TRUE) %>% # Enable the navigator
      hc_scrollbar(enabled = TRUE) %>% # Enable the scrollbar for the navigator
      hc_rangeSelector(selected = 1) %>% # Enable the range selector buttons
      hc_legend(enabled = TRUE) %>%
      hc_exporting(enabled = TRUE) %>% # Enables chart exporting
      hc_responsive(rules = list(list(
        condition = list(maxWidth = 500),
        chartOptions = list(
          legend = list(
            align = 'center',
            verticalAlign = 'bottom',
            layout = 'horizontal'
          )
        )
      ))) %>%
      hc_add_theme(hc_theme_flat()) # Apply a predefined theme
    
  })
}

# Run the application
shinyApp(ui = ui, server = server)
