#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)
library(tidycensus)
library(tigris)
library(sf)
library(readxl)
library(leaflet)
library(zipcodeR)
library(scales)
library(htmltools)
library(gt)
library(DT)
library(rsconnect)

#deployApp()

options(tigris_use_cache = TRUE)
options(tigris_cache_dir = "tigris_cache")

format_number <- function(x) {
  if (is.na(x)) return(NA)
  if (x < 1000) return(as.character(round(x)))
  if (x < 1000000) return(paste0(round(x/1000, 1), "k"))
  if (x < 1000000000) return(paste0(round(x/1000000, 1), "m"))
  return(paste0(round(x/1000000000, 1), "b"))
}
#census_api_key("c28ebea92c2fa98acaa5aa8d6b7fd3630595e46c", install = TRUE, overwrite = TRUE)

# US census population data
pop_county <- get_acs(geography = "county", variables = "B01003_001")

# zipcode to county
zipcode_county_map <- zip_code_db %>%
  select(zipcode, county, state)

counties_data <- counties(cb = TRUE, resolution = "20m")
counties_sf <- st_as_sf(counties_data, crs = 4326)

all_orders_clean <- function(df){
  cleaned_data <- df %>%
    select(c(sku, asin, quantity, ship.city, ship.state, purchase.date, order.status, purchase.date,)) %>%
    mutate(sku = substr(sku, 1, 15))
  return(cleaned_data)
}

amazon_heatmap <- function(df) {

  loveBirdHM_filt <- df %>%
    select(c(amazon.order.id, quantity, ship.postal.code, purchase.date)) %>%
    mutate(zipcode = substr(ship.postal.code, 1, 5),
           count = 1)

  loveBirdHM_uniq_ship.codes <- loveBirdHM_filt %>%
    distinct(ship.postal.code, .keep_all = TRUE)

  #glimpse(loveBirdHM_uniq_ship.codes)

  latLong_df <- geocode_zip(loveBirdHM_filt$zipcode)

  loveBirdHM_filt_with_county <- loveBirdHM_filt %>%
    left_join(zipcode_county_map, by = c("zipcode" = "zipcode")) %>%
    left_join(latLong_df, by = c("zipcode" = "zipcode")) %>%
    filter(!is.na(lat),
           !is.na(lng)) %>%
    mutate(county_n_state = paste(county, state, sep = ", "))

  df_sf <- st_as_sf(loveBirdHM_filt_with_county, coords = c("lng", "lat"), crs = 'NAD83')

  counties_sf <- st_make_valid(counties_sf)
  df_sf <- st_make_valid(df_sf)
  df_sf <- st_transform(df_sf, st_crs(counties_sf))
  df_joined <- st_join(counties_sf, df_sf, join = st_contains)

  # Aggregate the sales by county

  sales_by_county <- df_joined %>%
    group_by(GEOID) %>%
    summarise(total_uniq_sales = sum(count, na.rm = TRUE))

  sales_by_county_df <- as.data.frame(sales_by_county)

  counties_merged <- counties_sf %>%
    left_join(sales_by_county_df, by = "GEOID") %>%
    left_join(pop_county, by = "GEOID") %>%
    mutate(onePurchase = ifelse(total_uniq_sales == 0, 0, 1),
           adj_total_uniq_sales = ifelse(onePurchase == 0, total_uniq_sales + .1, total_uniq_sales),
           buyers_per_pop = (adj_total_uniq_sales / estimate),
           buyers_per_logPop =  adj_total_uniq_sales / log(estimate),
           log_buyers_per_pop = log(buyers_per_pop),
           oneInPop = estimate/(total_uniq_sales + .01),
           oneInPop_formatted = ifelse(onePurchase == 1, sapply(oneInPop, format_number), sapply(estimate, format_number)),
           #test1 = ifelse(onePurchase == 0, log_buyers_per_pop, log_buyers_per_pop),
           test2 = log_buyers_per_pop)

  # Create custom color palettes
  red_white_palette <- colorRamp(c("red3", "white"))
  white_green_palette <- colorRamp(c("white", "darkgreen"))

  # Create a custom color function
  custom_color <- function(x, purchase) {
    if (purchase == 0) {
      rgb(red_white_palette(pnorm(x)), maxColorValue = 255)
    } else {
      rgb(white_green_palette(pnorm(x)), maxColorValue = 255)
    }
  }

  # Standardize the test1 values separately for onePurchase == 0 and onePurchase == 1
  counties_merged <- counties_merged %>%
    group_by(onePurchase) %>%
    mutate(standardized_test2 = (test2 - mean(test2)) / sd(test2)) %>%
    ungroup()

  # Create the map
  heatMapPlot <- leaflet(counties_merged) %>%
    addTiles() %>%
    addPolygons(
      fillColor = ~sapply(seq_along(test2), function(i) custom_color(standardized_test2[i], onePurchase[i])),
      weight = 1,
      opacity = 1,
      color = 'white',
      dashArray = '3',
      fillOpacity = 0.7,
      highlightOptions = highlightOptions(
        weight = 5,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE),
      label = ~lapply(paste0("<b>County:</b> ", NAME.x, "<br><b>Total Sales:</b> ", total_uniq_sales, "<br><b>", onePurchase, "</b> in every <b>", oneInPop_formatted, "</b> people purchase"), HTML),
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto")) %>%
    addLegend(
      colors = c("red", "white", "white", "green"),
      labels = c("No Sales, Large Population", "Low Sales or Low Population",
                 "", "High Sales for Population"),
      title = "Total Sales (Standardized)",
      position = "bottomright"
    )

  return(heatMapPlot)
}


# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("Amazon Shipping Heatmap"),

  br(),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(
      # Input: Select a file ----
      fileInput("file1", "Upload 'All Orders' Report",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),

    ),
    # Main panel for displaying outputs ----
    mainPanel(
      # Output: Data file ----
      DTOutput("contents")

    )
  ),
  br(),

  fluidRow(
    column(12, leafletOutput("heatmap", height = "70vh")),
  ),
  br()
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  userData <- reactive({
    req(input$file1)
    tryCatch(
      {
        read.csv(input$file1$datapath,
                 header = TRUE,
                 sep = '\t',
                 quote = '"')
      },
      error = function(e) {
        stop(safeError(e))
      }
    )
  })

  output$contents <- renderDT(all_orders_clean(userData()), selection = "none", options = list(pageLength = 3), server = FALSE, editable = TRUE)

  # Render the leaflet plot
  output$heatmap <- renderLeaflet({
    req(data())
    print("Rendering heatmap")
    tryCatch(
      {
        #browser()
        plot <- amazon_heatmap(userData())%>%
          setView(lng = -98.583, lat = 39.833, zoom = 4)
        print("Heatmap created successfully")
        plot
      },
      error = function(e) {
        print("Waiting for user data:")
        print(e)
        # Return a basic leaflet map if there's an error
        leaflet() %>% addTiles()%>%
          setView(lng = -98.583, lat = 39.833, zoom = 4)
      }
    )
  })

}

# Run the application
shinyApp(ui = ui, server = server)
