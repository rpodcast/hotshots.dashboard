#' @import dplyr
#' @noRd
gen_tidy_race_data <- function(df) {
  grand_prix_order <- names(hotshot_data$grand_prix)
  track_order <- purrr::simplify(hotshot_data$grand_prix)
  names(track_order) <- NULL
  
  df2 <- df %>%
    mutate(player_time = stringi::stri_replace_last(player_time, replacement = ".", regex = "\\:"),
           race_time = as.numeric(as.difftime(player_time, format="%M:%OS", units="secs")),
           grand_prix_fct = factor(grand_prix, levels = grand_prix_order),
           track_fct = factor(track, levels = track_order))
  return(df2)
}

#' @import dplyr
#' @noRd
gen_summary_gp_data <- function(df2) {
  df_summary1 <- df2 %>%
    group_by(grand_prix_fct, direction, player_name) %>%
    summarize(race_points_total = sum(points, na.rm = TRUE),
              n_races = n(),
              n_first = sum(position == 1),
              n_second = sum(position == 2),
              n_third = sum(position == 3),
              n_top3 = sum(position <= 3)) %>%
    arrange(grand_prix_fct, desc(direction), desc(race_points_total), desc(n_first), desc(n_second), desc(n_third)) %>%
    ungroup() %>%
    group_by(grand_prix_fct, direction) %>%
    mutate(position = row_number()) %>%
    left_join(hotshot_points, by = "position") %>%
    ungroup()
  
  return(df_summary1)
}

gen_summary_overall <- function(df_summary1) {
  df_summary2 <- df_summary1 %>%
    group_by(grand_prix_fct, player_name) %>%
    summarize(total_gp_points = sum(points, na.rm = TRUE),
              n_races = sum(n_races),
              n_top3 = sum(n_top3),
              n_first_race = sum(n_first),
              n_first_gp = sum(position == 1),
              n_second_gp = sum(position == 2),
              n_third_gp = sum(position == 3)) %>%
    ungroup() %>%
    arrange(grand_prix_fct, desc(total_gp_points), desc(n_first_gp), desc(n_second_gp), desc(n_third_gp)) %>%
    group_by(player_name) %>%
    summarize(total_points = sum(total_gp_points),
              n_races = sum(n_races),
              n_top3 = sum(n_top3),
              n_first_race = sum(n_first_race),
              n_first_gp = sum(n_first_gp),
              n_second_gp = sum(n_second_gp),
              n_third_gp = sum(n_third_gp)) %>%
    ungroup() %>%
    arrange(desc(total_points), desc(n_first_gp), desc(n_second_gp), desc(n_third_gp))
  
  return(df_summary2)
}