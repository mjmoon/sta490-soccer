# devtools::install_github("mverseanalysis/mverse", ref = "release")
library(log4r)
library(mverse)
library(tidyverse)
if(!dir.exists("data"))
  dir.create("data", showWarnings = FALSE)
log_file <- "soccer.log"
save_to <- "data/soccer.csv"
if(!file.exists(log_file))
  file.create(log_file, showWarnings = FALSE)
log_to_console <- console_appender(layout = default_log_layout())
log_to_file <- file_appender(
  log_file, append = TRUE, layout = default_log_layout())
my_logger <- log4r::logger(
  threshold = "INFO", appenders = list(log_to_console, log_to_file))
log_info <- function(msg, logger = my_logger) {
  log4r::info(logger, msg)
}
log_info("===============Data preparation===============")
variables <- c('player', 'playerShort', 'birthday', 'height', 'weight',
               'position', 'club', 'leagueCountry', 'refNum',
               'goals', 'victories', 'defeats', 'yellowCards', 
               'games', 'redCards', 'rater1', 'rater2')
log_info(paste0("Reading variables: ", paste0(variables, collapse = ", "),
                " from the soccer data set."))
# select variables
df0 <- soccer |>
  mutate(player = str_squish(player)) |>
  select(all_of(variables))
log_info(paste0("<", nrow(df0), " rows>"))
log_info("=======Checking for missing values======")
check_missing <- function(df) {
  missing <- df |>
    summarise(across(.fns = ~ sum(is.na(.x))))
  missing <- sort(unlist(missing), decreasing = TRUE)
  if(all(missing == 0)) {
    log_info(paste0("No rows with missing values."))
    return(NULL)
  }
  log_info(paste0(
    "Variables: ", paste0(names(missing)[missing > 0], collapse = ", "),
    " missing values for ", paste0(missing[missing> 0], collapse = ", "),
    " rows respectively."))
  return(names(missing)[missing > 0])
}
df1 <- df0
missing <- check_missing(df1)
log_info("=======Checking player records for missing information======")
check_missing_player <- function(df, var) {
  players <- df |>
    filter(is.na(.data[[var]])) |>
    pull(playerShort) |>
    unique()
  n_matched <- soccer |>
    filter(playerShort %in% players) |>
    filter(!is.na(.data[[var]])) |>
    nrow()
  log_info(paste0("Found ", n_matched, " records with",
                  " `", var, "` info for players with missing records."))
  if(n_matched == 0) {
    df_new <- df |>
      filter(! is.na(.data[[var]]))
    log_info(paste0("Removed ", nrow(df) - nrow(df_new), " rows missing `", 
                    var, "`."))
    log_info(paste0("<", nrow(df_new), " rows>"))
    
  }
  # else
  # fill missing records using the records for the same players
  # 0 found
  missing <- check_missing(df_new)
  return(list(missing, df_new))
}
while(! is.null(missing)) {
  res <- check_missing_player(df1, missing[1])
  missing <- res[[1]]
  df1 <- res[[2]]
}
log_info("=======Randomly select 75 players from England and Germany======")
set.seed(490)
selectedCountry <- c("England", "Germany")
selected_players <- df1 %>%
  filter(leagueCountry %in% selectedCountry) %>%
  group_by(leagueCountry) %>%
  distinct(playerShort) %>%
  slice_sample(n = 75) %>%
  pull(playerShort)
df2 <- df1 %>%
  filter(leagueCountry %in% selectedCountry,
         playerShort %in% selected_players)
write.csv(df2, save_to, row.names = FALSE)
log_info(
  paste0("===============Data preparation completed===============\n",
         "    Original data set contained ", 
         format(nrow(soccer), big.mark = ","), " rows.",
         " Removed ", format(nrow(soccer) - nrow(df1), big.mark = ","),
         " rows with missing data then randomly selected 75 players ", 
         "from England and Germany each resulting in ", 
         format(nrow(df2), big.mark = ","), " dyads in the final set."))
log_info(paste0("=======Data saved to `", save_to, "`======="))
