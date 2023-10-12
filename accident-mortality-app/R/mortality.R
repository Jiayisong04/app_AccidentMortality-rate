library(tidyverse)

calc_perc = function(data) {
  data |>
    replace_na(list(Conflict = 0, Terrorism = 0)) |>
    mutate(
      total = (`Road accidents` + Suicide + Homicide + Drowning + Fire +
                 `Heat-related` + `Natural disasters` + Conflict + Terrorism)
    )
}




