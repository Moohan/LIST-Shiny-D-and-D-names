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
    dd_name <- names |> 
      filter(
        class == input$class_buttons,
        race == input$race_buttons,
        str_detect(name, input$name_input, negate = TRUE)
      ) |> 
      mutate(dist = stringdist(input$name_input, name))  |>
      slice_min(dist, n = 1, with_ties = FALSE) |> 
      pull(name)
    
    return(dd_name)
  })
  
  output$name_output <- renderText({
    text_reactive()
  })
}

# ////////////////////////////////////////////////////////////////////////////
# Run the app
# ////////////////////////////////////////////////////////////////////////////
shinyApp(ui = ui, server = server)