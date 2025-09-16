library(shiny)
library(readr)
library(dplyr)
library(stringr)
library(stringdist)

# ////////////////////////////////////////////////////////////////////////////
# Setup
# ////////////////////////////////////////////////////////////////////////////
names <- read_csv("https://raw.githubusercontent.com/janelleshane/DnD-characters/refs/heads/master/DnD_characters_May2018.txt", show_col_types = FALSE) |> 
  rename(name = 1, race = 2, class = 3) 

names <- names |> 
  mutate(
    race = sample(sample(race, 3), nrow(names), TRUE),
    class = sample(sample(class, 3), nrow(names), TRUE)
  )
# ////////////////////////////////////////////////////////////////////////////
# UI
# ////////////////////////////////////////////////////////////////////////////
ui <- fluidPage(
  titlePanel(title = "D&D Names 🐲"),
        textInput("name-input", "Your Name"),
        radioButtons("class-buttons", "Class", choices = unique(names$class)),
        radioButtons("race-buttons", "Race", choices = unique(names$race)),
        actionButton("show_text", "Go go magic name generator!"),
        textOutput("name-output")
)

# ////////////////////////////////////////////////////////////////////////////
# Server
# ////////////////////////////////////////////////////////////////////////////
server <- function(input, output, session) {
  text_reactive <- eventReactive(input$show_text, {
    dd_name <- names |> 
      filter(
        class == input$class-buttons,
        race == input$race-buttons,
        stringr::str_detect(name, input$name-input, negate = TRUE)
      ) |> 
      mutate(dist = stringdist::stringdist(input$name-input, name))  |>
      slice_min(dist, n = 1, with_ties = FALSE) |> 
      pull(name)
    
    return(dd_name)
  })
  
  output$name-output <- renderText({
    text_reactive()
  })
}

# ////////////////////////////////////////////////////////////////////////////
# Run the app
# ////////////////////////////////////////////////////////////////////////////
shinyApp(ui = ui, server = server)