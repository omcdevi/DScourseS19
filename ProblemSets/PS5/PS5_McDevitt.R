library(rvest)
library(dplyr)
library(stringr)
library(ggplot2)
webpage <- read_html("https://www.metacritic.com/browse/albums/score/metascore/all?sort=desc")
alldata <- html_nodes(webpage, '.product_wrap')
data.text <- html_text(alldata)
head(data.text)
#single columns example
metascore <- html_nodes(webpage, '.positive')
meta_text <- html_text(metascore)
head(meta_text)
userscore <- html_nodes(webpage, '.textscore')
user_text <- html_text(userscore)
head(user_text)
album <- html_nodes(webpage, '.first_product a')
album_text <- html_text(album)
head(album_text)
#dataframe example
x <- head(meta_text, -2)
scores <- data.frame(user_text,x)
head(scores)

#NBA Stats API
library(nbastatR)
#Code copied from nbastatR to get combine data
.parse_out_set <-
  memoise::memoise(function(data, set_column = "setSpot15CornerLeftCollege") {
    df_set <-
      data %>%
      select(one_of(set_column)) %>%
      distinct()

    all_data <-
      df_set %>% pull() %>%
      future_map_dfr(function(x) {
        if (x %>% is.na()) {
          return(tibble(UQ(set_column) := x))
        }
        names_set <-
          c(
            set_column,
            glue::glue("{set_column}Made"),
            glue::glue("{set_column}Attempted"),
            glue::glue("{set_column}Pct")
          )
        values <-
          x %>% str_split("\\-") %>% flatten_chr() %>% as.numeric()

        tibble(
          X1 = x,
          X2 = values[1],
          X3 = values[2],
          X4 = X2 / X3
        ) %>%
          purrr::set_names(c(names_set))
      })

    data %>%
      left_join(all_data)

  })

.get_shot_pct <-
  memoise::memoise(function(x) {
  shots <-
    x %>%
    str_split('\\-') %>%
    unlist() %>%
    as.numeric()

  shot.pct <-
    shots[1] / shots[2]

  return(shot.pct)

})
.get_year_draft_combine <-
  memoise::memoise(function(combine_year = 2014,
           return_message = T) {
    if (combine_year < 2000) {
      stopifnot("Sorry data starts in the 2000-2001 season")
    }

    if (return_message) {
      glue::glue("Acquiring {combine_year} NBA Draft Combine Data") %>% cat(fill = T)
    }
    slugSeason <- generate_season_slug(season = combine_year)
    url <-
      glue::glue(
        "https://stats.nba.com/stats/draftcombinestats?LeagueID=00&SeasonYear={slugSeason}"
      ) %>%
      as.character()


    json <-
      url %>%
      curl() %>%
      readr::read_lines() %>%
      fromJSON(simplifyDataFrame = T)


    data <-
      json$resultSets$rowSet %>%
      data.frame(stringsAsFactors = F) %>%
      tbl_df()

    headers <-
      json$resultSets$headers %>% flatten_chr()

    actual_names <-  headers %>% resolve_nba_names()

    data <-
      data %>%
      purrr::set_names(actual_names)

    num_names <-
      actual_names[actual_names %>% str_detect("pct|Inches|^id[A-Z]|time|weight|reps")]

    data <-
      data %>%
      mutate_at(num_names,
                funs(. %>% as.character() %>% readr::parse_number())) %>%
      dplyr::rename(slugPosition = groupPosition)

    if (actual_names[actual_names %>% str_detect("set")] %>% length() > 0) {
      data <-
        actual_names[actual_names %>% str_detect("set")] %>%
        future_map(function(set) {
          .parse_out_set(data = data, set_column = set)
        }) %>%
        suppressMessages()

      data <-
        data %>%
        purrr::reduce(left_join) %>%
        suppressMessages()
    }

    data <-
      data %>%
      mutate(yearCombine = combine_year) %>%
      select(yearCombine, everything()) %>%
      remove_na_columns()

    data
  })
#Analyze some data
head(draftcombine.2017)
lm(lengthHandInches ~ setSpot15TopKeyNBAPct, draftcombine.2017)
lm(lengthHandInches ~ setSpot15TopKeyNBAPct + heightWOShoesInches, draftcombine.2017)
