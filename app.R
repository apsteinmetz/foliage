# adapted from Bob Rudis
# https://github.com/hrbrmstr/foliage/tree/master

library(shiny)
library(sf)
library(bslib)
library(bsicons)
library(tidyverse)


# setup code

# "borrow" the files from SmokyMountains.com, but be nice and cache them to
# avoid hitting their web server for every iteration

c("https://s3.amazonaws.com/smc0m-tech-stor/static/js/us.min.json",
  "https://cdn.smokymountains.com/static/maps/rendered2023.csv") |>
  walk( ~ {
    sav_tmp <- paste0("data/", basename(.x))
    if (!file.exists(sav_tmp))
      download.file(.x, sav_tmp)
  })

# next, we read in the GeoJSON file twice. first, to get the counties
states_sf <- read_sf("data/us.min.json", "states",
                     stringsAsFactors = FALSE)

# we only want the continental US
states_sf <- filter(states_sf,!(id %in% c("2", "15", "72", "78")))

# it doesn't have a CRS so we give it one
st_crs(states_sf) <- 4326

# next we read in the states
counties_sf <-
  read_sf("data/us.min.json", "counties", stringsAsFactors = FALSE)
st_crs(counties_sf) <- 4326

# now, we read in the foliage data
foliage <- read_csv("data/rendered2023.csv",
                    show_col_types = FALSE)

foliage$id <- as.character(foliage$id)
colnames(foliage) <- c("id", sprintf("week_%d", 1:13))


color_range <- c("#8ba780",
                 "#fbf6bb",
                 "#f6ce6f" ,
                 "#e89150" ,
                 "#dc4d33" ,
                 "#ae3130",
                 "#8d3d28")


week_labels <- format(seq(
  from = as.Date("2023-09-04"),
  to = as.Date("2023-11-20"),
  by = "1 week"),
  "%b %d")

color_labels = c("No Change",
                 "Minimal",
                 "Patchy",
                 "Partial",
                 "Near Peak",
                 "Peak",
                 "Past Peak")

color_pal <- set_names(color_range, color_labels)

foliage_sf <- counties_sf |>
  left_join(foliage,  by = "id") |>
  filter(!is.na(week_1)) |>
  rename("county_id" = "id") |>
  pivot_longer(cols = starts_with("week"), names_to = "week") |>
  filter(week != "week_1") |>
  mutate(week = factor(week,
                       levels = unique(week),
                       labels = week_labels)) |>
  mutate(value = factor(value,
                        labels = color_labels))


weeks <- levels(foliage_sf$week)


pct_at_color <-  foliage_sf |>
  as_tibble() |>
  group_by(week) |>
  mutate(value = fct_collapse(
    value,
    "Too Early" = c("No Change", "Minimal", "Patchy", "Partial", "Near Peak"),
    "Too Late" = c("Past Peak"),
    "Peak" = "Peak"
  )) |>
  count(value) |>
  complete(value, fill = list(n = 0)) |>
  mutate(pct = n / sum(n))

stage_pct <- function(cur_week = 1,
                      stage = c("Too Early", "Peak", "Too Late")) {
    val <- pct_at_color |>
      filter(week == as.character(weeks[cur_week])) |>
      filter(value == stage) |>
      pull(pct)
    val <- paste0(round(val * 100), "%")
    return(val)
  }

leaf_plot <- function(cur_week = 1) {
  foliage_sf |>
    dplyr::filter(week == as.character(weeks[cur_week])) |> 
  ggplot() +
    geom_sf(aes(fill = value),
            linewidth = 0.125,) +
    geom_sf(
      data = states_sf,
      color = "grey",
      linewidth = 0.125,
      fill = NA
    ) +
    scale_fill_manual(values = color_pal, drop = FALSE) +
    labs(title = sprintf("Autumn Leaves: %s ", as.character(weeks[cur_week])),
         caption = "Original Code by Bob Rudis, @hrbrmstr. Adapted for Shiny by Art Steinmetz @adababbage") +
    ggthemes::theme_map() +
    theme(
     plot.background = element_rect(fill = "black"),
      panel.grid = element_line(color = "#00000000"),
      panel.grid.major = element_line(color = "#00000000"),
      legend.position = "right",
      plot.title = element_text(
        family = NULL,
        face = NULL,
        colour = "white",
        size = 64
      ),
      plot.caption = element_text(
        family = NULL,
        face = NULL,
        colour = "white",
        size = 14
      )
    )
}

# set up slider
column_slider <- function() {
  column(width = 3,
         sliderInput(
           "cur_week",
           "Week:",
           min = 1,
           max = 12,
           value = 1,
           animate = TRUE
         ))
}

# set up UI value text elements
column_stage_pct <- function(label){
  col_template <- switch (label,
    "Not Yet" = column(width = 3,
                           value_box(
                             title = label,
                             value = textOutput("early_pct"),
                             theme_color = "success",
                             showcase = bs_icon("tree"),
                             showcase_layout = "top right"
                           )),
    "Peak" = column(width = 3,
                       value_box(
                         title = label,
                         value = textOutput("peak_pct"),
                         theme_color = "warning",
                         showcase = bs_icon("tree"),
                         showcase_layout = "top right"
                       )),
    "Too Late" = column(width = 3,
                       value_box(
                         title = label,
                         value = textOutput("late_pct"),
                         theme_color = "fg",
                         showcase = bs_icon("tree"),
                         showcase_layout = "top right"
                       ))
    )
  return(col_template)
}  

# UI ---------------------------------------------------------------------------

ui <- fluidPage(
  theme = bslib::bs_theme(preset = "darkly"),
  
  titlePanel("U.S.A. Autumn Foliage 2023"),
  h6("source: https://github.com/apsteinmetz/foliage.git"),
  
  # top bar with a slider input for weeks
  fluidRow(
    column_slider(),
    column_stage_pct("Not Yet"),
    column_stage_pct("Peak"),
    column_stage_pct("Too Late")
  ),
  # plot of map
  # problem: I can't change whitespace to black with theme
  fluidRow(column(
    width = 12, plotOutput("leafPlot", height = "800px")
  ))
)

# SERVER ----------------------------------------------------------------------
server <- function(input, output) {
  output$week_label <- renderText({
    as.character(weeks[input$cur_week])
  })
  
  output$early_pct <- renderText({
    stage_pct(input$cur_week, "Too Early")
  })
  
  output$peak_pct <- renderText({
    stage_pct(input$cur_week, "Peak")
  })
  output$late_pct <- renderText({
    stage_pct(input$cur_week, "Too Late")
  })
  
  output$leafPlot <- renderPlot({
    leaf_plot(input$cur_week)
  })
}

shinyApp(ui = ui, server = server)
