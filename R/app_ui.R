#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import fresh
#' @import bs4Dash
#' @noRd
app_ui <- function(request) {
  # create tribble for box global config
  box_config <- tibble::tribble(
    ~background, ~labelStatus,
    "danger", "warning",
    "purple", "success",
    "success", "primary",
    "warning", "danger",
    "fuchsia", "info"
  )
  
  # box factory function
  box_factory <- function(background, labelStatus) {
    box(
      title = "Cyberpunk Box",
      collapsible = TRUE,
      background = background,
      height = "200px",
      label = boxLabel(1, labelStatus)
    )
  }
  
  # pmap magic
  boxes <- purrr::pmap(box_config, box_factory)
  
  my_theme <- create_theme(
    bs4dash_vars(
      navbar_light_color = "#bec5cb",
      navbar_light_active_color = "#FFF",
      navbar_light_hover_color = "#FFF"
    ),
    bs4dash_yiq(
      contrasted_threshold = 10,
      text_dark = "#FFF", 
      text_light = "#272c30"
    ),
    bs4dash_layout(
      main_bg = "#353c42"
    ),
    bs4dash_sidebar_light(
      bg = "#272c30", 
      color = "#bec5cb",
      hover_color = "#FFF",
      submenu_bg = "#272c30", 
      submenu_color = "#FFF", 
      submenu_hover_color = "#FFF"
    ),
    bs4dash_status(
      primary = "#5E81AC", danger = "#BF616A", light = "#272c30"
    ),
    bs4dash_color(
      gray_900 = "#FFF"
    ),
    bs4dash_font(
      family_base = 'TTSupermolotNeue-Bold'
    )
  )
  
  # my_theme <- bs_theme(
  #   bg = "#0090f9", 
  #   fg = "#f0f0e6", 
  #   primary = "#71cc65", 
  #   base_font = font_face(
  #     family = 'TTSupermolotNeue-Bold',
  #     src = 'TTSupermolotNeue-Bold.eot',
  #     weight = 'normal',
  #     style = 'normal',
  #     display = 'swap'
  #   )
  # )
  
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here
    dashboardPage(
      freshTheme = my_theme,
      header = dashboardHeader(
        leftUi = dropdownMenu(
          type = "messages",
          badgeStatus = "success",
          messageItem(
            from = "Support Team",
            message = "This is the content of a message.",
            time = "5 mins"
          ),
          messageItem(
            from = "Support Team",
            message = "This is the content of another message.",
            time = "2 hours"
          )
        )
      ),
      sidebar = dashboardSidebar(),
      body = dashboardBody(boxes),
      controlbar = dashboardControlbar(),
      title = "Fresh theming"
    )
    # fluidPage(
    #   #theme = my_theme,
    #   h1("hotshots.dashboard"),
    #   p("this is some text")
    # )
    
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'hotshots.dashboard'
    ),
    #gfonts::use_font("roboto", "inst/app/www/css/roboto.css")
    #gfonts::use_font("henny-penny", "inst/app/www/css/henny-penny.css")
    #gfonts::use_font("ubuntu", "inst/app/www/css/ubuntu.css")
    tags$link(rel = "stylesheet", type = "text/css", href = "www/css/TTSupermolotNeue.css"),
    tags$style(HTML("body {font-family: 'TTSupermolotNeue-Bold', sans-serif;}" ))
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}