#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import pins
#' @noRd
app_server <- function( input, output, session ) {
  # establish connection to data pin
  board_register_dospace()
  
  # set default options for echarts
  echarts4r::e_common(font_family = "TTSupermolotNeue-Bold")
  
  # create reactive value for overall data
  hotshot_stat_df <- reactiveVal(pin_get("hotshots_race_results", board = "dospace"))
  
  # refresh data when button pressed in nav bar
  observeEvent(input$refresh, {
    res <- pin_get("hotshots_race_results", board = "dospace")
    if (!identical(hotshot_stat_df(), res)) {
      hotshot_stat_df(res)
    }
  })
  
  # List the first level callModules here
  callModule(mod_welcome_server, "welcome_ui_1", hotshot_stat_df)
  
  callModule(mod_leaderboard_server, "leaderboard_ui_1", hotshot_stat_df)
  
  callModule(mod_trackstats_server, "trackstats_ui_1",  hotshot_stat_df)
}
