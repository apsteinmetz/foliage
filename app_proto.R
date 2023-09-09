# adapted from Bob Rudis
# https://github.com/hrbrmstr/foliage/tree/master

library(shiny)
library(sf)
library(tidyverse)

# setup code

# "borrow" the files from SmokyMountains.com, but be nice and cache them to
# avoid hitting their web server for every iteration

c("https://s3.amazonaws.com/smc0m-tech-stor/static/js/us.min.json",
  "https://cdn.smokymountains.com/static/maps/rendered2023.csv") |>
  walk(~{
    sav_tmp <- paste0("data/", basename(.x))
    if (!file.exists(sav_tmp)) download.file(.x, sav_tmp)
  })

# next, we read in the GeoJSON file twice. first, to get the counties
states_sf <- read_sf("data/us.min.json", "states", 
                     stringsAsFactors = FALSE)

# we only want the continental US
states_sf <- filter(states_sf, !(id %in% c("2", "15", "72", "78")))

# it doesn't have a CRS so we give it one
st_crs(states_sf) <- 4326

# next we read in the states
counties_sf <- read_sf("data/us.min.json", "counties", stringsAsFactors = FALSE)
st_crs(counties_sf) <- 4326

# now, we read in the foliage data
foliage <- read_csv("data/rendered2023.csv",
                    show_col_types = FALSE)

foliage$id <- as.character(foliage$id)
colnames(foliage) <- c("id", sprintf("week_%d", 1:13))

# and, since we have a lovely sf tidy data frame, bind it together
foliage_sf <- counties_sf |>
  left_join(foliage,  by="id") |>
  filter(!is.na(week_1)) |> 
  rename("county_id"= "id")

# now, we do some munging so we have better labels and so we can
# iterate over the weeks
week_labels <-format(
  seq(
    from = as.Date("2023-09-04"),
    to = as.Date("2023-11-20"),
    by = "1 week"
  ), "%b %d")


foliage_sf <- foliage_sf |>
  pivot_longer(cols = starts_with("week"), names_to = "week") |>
  mutate(value = factor(value)) |>
  filter(week != "week_1") |>
  mutate(week = factor(week,
                       levels = unique(week),
                       labels = week_labels))

weeks <- levels(foliage_sf$week)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("U.S.A. Autumn Foliage 2023"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("cur_week",
                        "Week:",
                        min = 1,
                        max = 8,
                        value = 1)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("leafPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$leafPlot <- renderPlot({
    weeks <- levels(foliage_sf$week)
    
    xdf <- foliage_sf |>
      dplyr::filter(week == as.character(weeks[input$cur_week]))
    
    ggplot() +
      geom_sf(
        data = xdf,
        aes(fill = value),
        linewidth = 0.125,
        #    color = "white"
        #    color = "tan"
      ) +
      geom_sf(
        data = states_sf,
        # color = "#2b2b2b"
        color = "grey",
        linewidth = 0.125,
        fill = NA
      ) +
      viridis::scale_fill_viridis(
        name = NULL,
        option = "magma",
        direction = -1,
        discrete = TRUE,
        labels = c("No Change", "Minimal", "Patchy", "Partial", "Near Peak", "Peak", "Past Peak"),
        drop = FALSE
      ) +
      labs(
        title = sprintf("Foliage: %s ", unique(xdf$week))
      ) +
      ggthemes::theme_map() +
      theme(
        panel.grid = element_line(color = "#00000000"),
        panel.grid.major = element_line(color = "#00000000"),
        legend.position = "right"
      )
    
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
