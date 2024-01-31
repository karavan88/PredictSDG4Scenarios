# Load necessary libraries
library(highcharter)
library(dplyr)
library(readr)

df <- 
  read_csv("Country Data SDG4.csv") %>%
  filter(ISO3 == "AFG" & INDICATOR_CODE == "ED_ROFST_L1")

# Filter the dataset for the different series we want to plot
historic_data <- df %>%
  filter(VAL_TYPE %in% c("EST", "ACT")) %>%
  select(YEAR, VALUE)

bau_data <- df %>%
  filter(VAL_TYPE == "PROJ", SCENARIO == "BAU") %>%
  select(YEAR, VALUE)

mtt_data <- df %>%
  filter(VAL_TYPE == "PROJ", SCENARIO == "MTT") %>%
  select(YEAR, VALUE)

target_data <- df %>%
  filter(VAL_ROLE == "TRGT" & YEAR != 2025) %>%
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
  hc_subtitle(text = "Historic Values and Projected Scenarios") %>%
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
  hc_add_series(name = "Target 2030", type = 'scatter', data = target_list, color = '#2ECC71', 
                marker = list(radius = 8, symbol = 'circle')) %>%
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


