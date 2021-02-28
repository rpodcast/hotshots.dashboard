#' welcome UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import countup
#' @import plotly
#' @import echarts4r
mod_welcome_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      col_12(
        jumbotron(
          title = "Hotshot Racing Dashboard!",
          lead = "Welcome to the Official Wimpy's World of Linux Gaming Most Official Unofficial Hotshot Racing League Spring Championship (WWOLGMOUHRLSC) Dashboard!  We've been working hard to track the results of each race and other geeky stats with a multitude of over-engineered apps and utilities. Would you expect anything less from a bunch of Linux and open-source enthusiasts?  I think not!",
          "If you are viewing this dashboard on a race day, hit the refresh data button above to ensure you have the most up-to-date snapshot of data.",
          status = "primary",
          btnName = "Watch previous races!",
          href = "https://www.youtube.com/channel/UC6D0aBP5pnWTGhQAvEmhUNw"
        )
      )
    ),
    # fluidRow(
    #   col_12(
    #     div(
    #       align = "center",
    #       h1("High-level statistics!")
    #     )
    #   )
    # ),
    fluidRow(
      col_12(
        box(
          title = "High-level Statistics!",
          solidHeader = FALSE,
          width = 12,
          background = "info",
          tagList(
            fluidRow(
              col_4(
                div(
                  align = "center",
                  h4("Average Margin of Victory"),
                  echarts4rOutput(ns("mov"))
                )
              ),
              col_4(
                div(
                  align = "center",
                  h4("Season Progress"),
                  echarts4rOutput(ns("n_laps"))
                )
              ),
              col_4(
                div(
                  align = "center",
                  h4("Avg Top 3 Separation"),
                  echarts4rOutput(ns("avg_top3"))
                )
              )
            ),
            fluidRow(
              col_4(
                div(
                  align = "center",
                  h4("Unique Winners"),
                  countup::odometerOutput(ns("n_first"), height = "100px"),
                )
              ),
              col_4(
                div(
                  align = "center",
                  h4("Did Not Finishers (DNF)"),
                  countup::odometerOutput(ns("n_dnf"), height = "100px")
                )
              ),
              col_4(
                div(
                  align = "center",
                  h4("Total Racing Hours"),
                  countup::odometerOutput(ns("total_time"), height = "100px")
                )
              )
            )
          )
        )
      )
    ),
    fluidRow(
      col_12(
        box(
          title = "Racing Progress in Stats!",
          solidHeader = FALSE,
          width = 12,
          background = "info",
          maximizable = TRUE,
          fluidRow(
            col_12(
              selectInput(
                ns("plot_stat"),
                "Select Plot Statistic",
                choices = c("Total Race Points" = "points_running", 
                            "Total Top 3 Finishes" = "top3_running",
                            "Total DNF Finishes" = "dnf_running"),
                selected = "points_running",
                multiple = FALSE
              ),
              plotly::plotlyOutput(ns("points_plot"), height = "600px")
            )
          )
        )
      )
    )
  )
}
    
#' welcome Server Function
#'
#' @noRd 
mod_welcome_server <- function(input, output, session, hotshot_stat_df){
  ns <- session$ns
  
  # reactive for processed data
  df_ovstats <- reactive({
    req(hotshot_stat_df())
    res <- gen_tidy_race_data(hotshot_stat_df()) %>%
      gen_overall_summary()
    
    return(res)
  })
  
  output$n_first <- countup::renderOdometer({
    req(df_ovstats())
    countup::odometer(
      count = df_ovstats()$n_firstplace_winners,
      format = "d",
      theme = "car",
      width = "100%",
      height = "100%"
    )
  })
  
  output$n_dnf <- countup::renderOdometer({
    req(df_ovstats())
    countup::odometer(
      count = df_ovstats()$n_dnf_losers,
      format = "d",
      theme = "car",
      width = "100%",
      height = "100%"
    )
  })
  
  output$total_time <- countup::renderOdometer({
    req(df_ovstats())
    countup::odometer(
      count = df_ovstats()$total_time_hours,
      format = "dd.ddd",
      theme = "car",
      width = "100%",
      height = "100%"
    )
  })
  
  output$mov <- renderEcharts4r({
    req(df_ovstats())

    e_charts() %>%
      e_gauge(
        round(df_ovstats()$avg_margin_victory, 2), 
        name = "Seconds",
        min = 0,
        max = 2,
        axisTick = list(
          lineStyle = list(
            color = "#fff"
          )
        ),
        splitLine = list(
          lineStyle = list(
            color = "#fff"
          )
        ),
        pointer = list(
          itemStyle = list(
            color = "#fefd03"
          )
        ),
        axisLabel = list(
          color = "#fff",
          fontSize = 20,
          fontFamily = "TTSupermolotNeue-Bold"
        ),
        title = list(
          color = "#fff",
          fontSize = 20,
          fontFamily = "TTSupermolotNeue-Bold"
        ),
        detail = list(
          color = "#fff",
          fontSize = 30,
          fontFamily = "TTSupermolotNeue-Bold"
        ),
        animationDuration = 2000)
  })
  
  output$avg_top3 <- renderEcharts4r({
    req(df_ovstats())
    
    e_charts() %>%
      e_gauge(
        round(df_ovstats()$avg_top_3_sep, 2), 
        name = "Seconds",
        min = 0,
        max = 2,
        axisTick = list(
          lineStyle = list(
            color = "#fff"
          )
        ),
        splitLine = list(
          lineStyle = list(
            color = "#fff"
          )
        ),
        pointer = list(
          itemStyle = list(
            color = "#fefd03"
          )
        ),
        axisLabel = list(
          color = "#fff",
          fontSize = 20,
          fontFamily = "TTSupermolotNeue-Bold"
        ),
        title = list(
          color = "#fff",
          fontSize = 20,
          fontFamily = "TTSupermolotNeue-Bold"
        ),
        detail = list(
          color = "#fff",
          fontSize = 30,
          fontFamily = "TTSupermolotNeue-Bold"
        ),
        animationDuration = 2000)
  })
  
  output$n_laps <- renderEcharts4r({
    req(df_ovstats())
    
    e_charts() %>%
      e_gauge(
        round(df_ovstats()$n_laps_complete, 2), 
        name = "Laps",
        min = 0,
        max = 40,
        axisTick = list(
          lineStyle = list(
            color = "#fff"
          )
        ),
        splitLine = list(
          lineStyle = list(
            color = "#fff"
          )
        ),
        pointer = list(
          itemStyle = list(
            color = "#fefd03"
          )
        ),
        axisLabel = list(
          color = "#fff",
          fontSize = 20,
          fontFamily = "TTSupermolotNeue-Bold"
        ),
        title = list(
          color = "#fff",
          fontSize = 20,
          fontFamily = "TTSupermolotNeue-Bold"
        ),
        detail = list(
          color = "#fff",
          fontSize = 30,
          fontFamily = "TTSupermolotNeue-Bold"
        ),
        animationDuration = 2000)
  })
  
  output$points_plot <- renderPlotly({
    
    req(hotshot_stat_df())
    req(input$plot_stat)
    
    df2 <- gen_tidy_race_data(hotshot_stat_df())
    
    df_chart <- gen_plot_data(df2, all_drivers = TRUE)
    
    if (input$plot_stat == "points_running") {
      xaxis_label <- "Total Points"
    } else if (input$plot_stat == "top3_running") {
      xaxis_label <- "Total Top 3 Finishes"
    } else if (input$plot_stat == "dnf_running") {
      xaxis_label <- "Total DNF"
    }
    
    df_chart <- dplyr::mutate(df_chart, plot_var = .data[[input$plot_stat]])
    
    n_players <- length(unique(df_chart$player_name))
    
    plot_ly(df_chart) %>%
      add_segments(
        y = ~player_name, 
        yend = ~player_name, 
        x = ~plot_var, 
        xend = 0, 
        color = ~player_name,
        colors = RColorBrewer::brewer.pal(n_players, "Set3"),
        frame = ~frame_label_fct,
        line = list(width = 40)
      ) %>%
      # layout(
      #   annotations = list(
      #     x = df_chart$points_running,
      #     y = df_chart$player_name,
      #     text = df_chart$points_running,
      #     xanchor = "left",
      #     yanchor = "center",
      #     showarrow = FALSE
      #   )
      # ) %>%
      animation_slider(
        currentvalue = list(
          prefix = ""
        ),
        step = list(visible = FALSE)
      ) %>%
      animation_opts(
        frame = 500,
        easing = 'linear',
        redraw = FALSE
      ) %>%
      layout(
        showlegend = FALSE,
        paper_bgcolor = '#5875D5',
        plot_bgcolor = '#5875D5',
        yaxis = list(title = "", color = '#ffffff', tickangle = -45),
        xaxis = list(title = xaxis_label, color = '#ffffff'),
        font = list(
          color = '#ffffff',
          family = "TTSupermolotNeue-Bold",
          size = 16)
      )
  })
}
    
## To be copied in the UI
# mod_welcome_ui("welcome_ui_1")
    
## To be copied in the server
# callModule(mod_welcome_server, "welcome_ui_1")
 
