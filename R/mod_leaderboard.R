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
        reactable::reactableOutput(ns("leaderboard_table")),
        verbatimTextOutput(ns("row_print"))
      ),
      col_6(
        reactable::reactableOutput(ns("gp_table")),
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
  
  df_position <- reactive({
    # generate position data
    req(hotshot_stat_df())
    
    df_position <- hotshot_stat_df() %>%
      gen_tidy_race_data() %>%
      gen_summary_gp_data()
    
    return(df_position)
  })
  
  df_overall <- reactive({
    # generate overall data
    req(hotshot_stat_df())
    req(df_position())
    
    df_overall <- gen_summary_overall(df_position()) %>%
      arrange(desc(total_points)) %>%
      mutate(position = row_number()) %>%
      mutate(position_emo = case_when(
        position == 1 ~ "1st_place_medal",
        position == 2 ~ "2nd_place_medal",
        position == 3 ~ "3rd_place_medal",
        position == 8 ~ "8ball",
        TRUE ~ "checkered_flag"
      )) %>%
      left_join(player_data, by = "player_name") %>%
      left_join(country_data, by = "country") %>%
      select(position_emo, player_name, country, url, total_points)
    
    return(df_overall)
  })
  
  output$leaderboard_table <- reactable::renderReactable({
    req(df_position())
    req(df_overall())
    
    create_leaderboard(df_position(), df_overall())
  })
  
  hotshot_row_selected <- reactive({
    req(df_overall())
    getReactableState("leaderboard_table", "selected")
  })
  
  player_selected <- reactive({
    req(hotshot_row_selected())
    req(df_overall())
    
    player_name <- df_overall() %>%
      slice(hotshot_row_selected()) %>%
      pull(player_name)
    
    return(player_name)
  })
  
  output$gp_table <- reactable::renderReactable({
    req(df_position())
    create_gp_table(df_position(), player_selected())
  })
  
  output$row_print <- renderPrint({
    req(player_selected())
    player_selected()
  })
 
}
    
## To be copied in the UI
# mod_leaderboard_ui("leaderboard_ui_1")
    
## To be copied in the server
# callModule(mod_leaderboard_server, "leaderboard_ui_1")
 
