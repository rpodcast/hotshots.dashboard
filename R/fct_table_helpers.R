#' @import dplyr
#' @import reactable
#' @noRd
create_leaderboard <- function(df_position, df_overall) {

  df_position <- df_position %>%
    select(grand_prix_fct, direction, player_name, position, points, n_top3, n_races)
  
  # define reactable object
  res <- reactable(
    df_overall,
    filterable = FALSE,
    resizable = TRUE,
    showPageSizeOptions = FALSE,
    selection = 'single',
    onClick = 'select',
    highlight = TRUE,
    columns = list(
      position_emo = colDef(
        name = "Position",
        cell = function(value, index) {
          emo_val <- dplyr::slice(df_overall, index) %>% dplyr::pull(position_emo)
          emo::ji(emo_val)
        }
      ),
      player_name = colDef(
        name = "Player",
        minWidth = 100,
        cell = function(value, index) {
          # grab appropriate record from country flag data frame
          country_select <- dplyr::slice(df_overall, index) %>% dplyr::pull(country)
          country_url <- dplyr::slice(df_overall, index) %>% dplyr::pull(url)
          div(
            img(class = "flag", src = country_url),
            value
          )
        }),
      country = colDef(show = FALSE),
      url = colDef(show = FALSE),
      total_points = colDef(name = "Total Points")
    ),
    details = function(index) {
      player_selected <- dplyr::slice(df_overall, index) %>% dplyr::pull(player_name)
      gp_df <- filter(df_position, player_name == !!player_selected) %>%
        select(., -player_name, -n_races)
      htmltools::div(
        style = "padding: 12px",
        reactable(
          gp_df, 
          theme = reactableTheme(
            color = "hsl(233, 9%, 87%)",
            backgroundColor = "hsl(233, 9%, 19%)",
            borderColor = "hsl(233, 9%, 22%)",
            stripedColor = "hsl(233, 12%, 22%)",
            highlightColor = "hsl(233, 12%, 24%)",
            inputStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
            selectStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
            pageButtonHoverStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
            pageButtonActiveStyle = list(backgroundColor = "hsl(233, 9%, 28%)")
          ),
          outlined = TRUE)
      )
    },
    theme = reactableTheme(
      color = "hsl(233, 9%, 87%)",
      backgroundColor = "hsl(233, 9%, 19%)",
      borderColor = "hsl(233, 9%, 22%)",
      stripedColor = "hsl(233, 12%, 22%)",
      highlightColor = "hsl(233, 12%, 24%)",
      inputStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
      selectStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
      pageButtonHoverStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
      pageButtonActiveStyle = list(backgroundColor = "hsl(233, 9%, 28%)")
    )
  )
}

#' @import dplyr
#' @import reactable
#' @noRd
create_gp_table <- function(df_position, player_selected = NULL) {
  if (is.null(player_selected)) {
    gp_df <- df_position %>%
      select(., -n_races)
  } else {
    gp_df <- filter(df_position, player_name == !!player_selected) %>%
      select(., -player_name, -n_races)
  }
  
  res <- reactable(
    gp_df, 
    theme = reactableTheme(
      color = "hsl(233, 9%, 87%)",
      backgroundColor = "hsl(233, 9%, 19%)",
      borderColor = "hsl(233, 9%, 22%)",
      stripedColor = "hsl(233, 12%, 22%)",
      highlightColor = "hsl(233, 12%, 24%)",
      inputStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
      selectStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
      pageButtonHoverStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
      pageButtonActiveStyle = list(backgroundColor = "hsl(233, 9%, 28%)")
    ),
    outlined = TRUE)
  
  return(res)
}
  
  

#' @import dplyr
#' @import reactable
#' @noRd
create_track_table <- function(raw_df) {
  
  # get all available tracks from built in set
  region_df <- tibble::enframe(hotshot_data$tracks) %>%
    tidyr::unnest(cols = value) %>%
    rename(region = name, track = value)
  
  gp_df <- tibble::enframe(hotshot_data$grand_prix) %>%
    tidyr::unnest(cols = value) %>%
    rename(grand_prix = name, track = value)
  
  # obtain tracks used thus far
  current_tracks <- raw_df %>%
    select(grand_prix, track) %>%
    distinct() %>%
    left_join(region_df, by = "track")
  
  res <- reactable(
    current_tracks,
    filterable = TRUE,
    resizable = TRUE,
    showPageSizeOptions = FALSE,
    selection = 'single',
    onClick = 'select',
    highlight = TRUE,
    columns = list(
      grand_prix = colDef(name = "Grand Prix"),
      region = colDef(name = "Region"),
      track = colDef(name = "Track")
    ),
    theme = reactableTheme(
      color = "hsl(233, 9%, 87%)",
      backgroundColor = "hsl(233, 9%, 19%)",
      borderColor = "hsl(233, 9%, 22%)",
      stripedColor = "hsl(233, 12%, 22%)",
      highlightColor = "hsl(233, 12%, 24%)",
      inputStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
      selectStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
      pageButtonHoverStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
      pageButtonActiveStyle = list(backgroundColor = "hsl(233, 9%, 28%)")
    )
  )
  
  return(res)
}

