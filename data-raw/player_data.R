## code to prepare `player_data` dataset goes here

player_data <- tibble::tribble(
  ~player_name, ~full_name, ~country, ~avatar,
  "Wimpy", "Martin Wimpress", "gb", "Wimpy.jpg",
  "FrenchguyCH", "Yannick Mauray", "ch", "FrenchguyCH.jpg",
  "rpodcast", "Eric Nantz", "us", "rpodcast.jpg",
  "bigcalm", "", "gb", "bigcalm.png",
  "TwoD", "", "se", "TwoD.png",
  "madhens", "Monica Ayhens-Madon", "us", "madhens.jpg",
  "TheUnwiseGeek", "", "us", "TheUnwiseGeek.png",
  "popey", "Alan Pope", "gb", "popey.jpg",
  "bigpod", "", "si", "bigpod.png"
)


usethis::use_data(player_data, overwrite = TRUE)
