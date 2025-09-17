library(shiny)
library(readr)
library(dplyr)
library(stringr)
library(stringdist)

# ////////////////////////////////////////////////////////////////////////////
# Setup
# ////////////////////////////////////////////////////////////////////////////
load_data <- function() {
  names <- read_csv("https://raw.githubusercontent.com/janelleshane/DnD-characters/refs/heads/master/DnD_characters_May2018.txt", show_col_types = FALSE) |>
    rename(name = 1, race = 2, class = 3)

  names <- names |>
    mutate(
      race = sample(sample(race, 3), nrow(names), TRUE),
      class = sample(sample(class, 3), nrow(names), TRUE)
    )

  return(names)
}

# Load data once
names <- load_data()

# Pick a random name for the default placeholder
default_name <- sample(names$name, 1)

# ////////////////////////////////////////////////////////////////////////////
# UI
# ////////////////////////////////////////////////////////////////////////////
ui <- fluidPage(
  titlePanel(title = "D&D Names 🐲"),
  textInput("name_input", "Your Name"),
  radioButtons("class_buttons", "Class", choices = unique(names$class)),
  radioButtons("race_buttons", "Race", choices = unique(names$race)),
  actionButton("show_text", "Go go magic name generator!"),
  textOutput("name_output")
)

# ////////////////////////////////////////////////////////////////////////////
# Server
# ////////////////////////////////////////////////////////////////////////////
server <- function(input, output, session) {
  text_reactive <- eventReactive(input$show_text, {
    # Input validation
    if (is.null(input$name_input) || trimws(input$name_input) == "") {
      return("Please enter your name first!")
    }

    # Clean the input
    user_name <- trimws(input$name_input)
    # Filter with fallback logic
    filtered_names <- names |>
      filter(
        class == input$class_buttons,
        race == input$race_buttons,
        str_detect(name, user_name, negate = TRUE)
      )

    # If no results excluding user name, try without exclusion
    if (nrow(filtered_names) == 0) {
      filtered_names <- names |>
        filter(
          class == input$class_buttons,
          race == input$race_buttons
        )
    }

    # Final check
    if (nrow(filtered_names) == 0) {
      return("No characters found for this combination. Try different options!")
    }

    # Find closest match
    dd_name <- filtered_names |>
      mutate(dist = stringdist(user_name, name)) |>
      slice_min(dist, n = 1, with_ties = FALSE) |>
      pull(name)

    # Safety check for empty result
    if (length(dd_name) == 0 || is.na(dd_name)) {
      return("Unable to generate a name. Please try again!")
    }

    return(paste("Your D&D name is:", dd_name))
  })

  output$name_output <- renderText({
    text_reactive()
  })
}

# ////////////////////////////////////////////////////////////////////////////
# Run the app
# ////////////////////////////////////////////////////////////////////////////
shinyApp(ui = ui, server = server)