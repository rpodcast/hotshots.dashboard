#' leaderboard UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import reactable
#' @import dplyr
mod_leaderboard_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      col_12(
        h1("Racing Leaderboard!"),
        uiOutput(ns("title"))
      )
    ),
    fluidRow(
      col_6(
        reactable::reactableOutput(ns("leaderboard_table"))
      )
    )
 
  )
}
    
#' leaderboard Server Function
#'
#' @noRd 
mod_leaderboard_server <- function(input, output, session, hotshot_stat_df){
  ns <- session$ns
  
  output$title <- renderUI({
    req(hotshot_stat_df())
    # derive number of races completed
    n_races <- hotshot_stat_df() %>%
      select(grand_prix, track, direction) %>%
      distinct() %>%
      nrow(.)
    
    h2(glue::glue("Here is the leaderboard after {n_races} races. Click the little arrows to see individual grand prix details for a player"))
  })
  
  output$leaderboard_table <- reactable::renderReactable({
    req(hotshot_stat_df())
    create_leaderboard(hotshot_stat_df())
  })
 
}
    
## To be copied in the UI
# mod_leaderboard_ui("leaderboard_ui_1")
    
## To be copied in the server
# callModule(mod_leaderboard_server, "leaderboard_ui_1")
 
