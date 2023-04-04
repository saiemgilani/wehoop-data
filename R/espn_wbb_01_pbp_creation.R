rm(list = ls())
gcol <- gc()
lib_path <- Sys.getenv("R_LIBS")
if (!requireNamespace('pacman', quietly = TRUE)) {
  install.packages('pacman', lib = Sys.getenv("R_LIBS"), repos = 'http://cran.us.r-project.org')
}
suppressPackageStartupMessages(suppressMessages(library(dplyr, lib.loc = lib_path)))
suppressPackageStartupMessages(suppressMessages(library(magrittr, lib.loc = lib_path)))
suppressPackageStartupMessages(suppressMessages(library(jsonlite, lib.loc = lib_path)))
suppressPackageStartupMessages(suppressMessages(library(purrr, lib.loc = lib_path)))
suppressPackageStartupMessages(suppressMessages(library(progressr, lib.loc = lib_path)))
suppressPackageStartupMessages(suppressMessages(library(data.table, lib.loc = lib_path)))
suppressPackageStartupMessages(suppressMessages(library(qs, lib.loc = lib_path)))
suppressPackageStartupMessages(suppressMessages(library(arrow, lib.loc = lib_path)))
suppressPackageStartupMessages(suppressMessages(library(glue, lib.loc = lib_path)))
suppressPackageStartupMessages(suppressMessages(library(optparse, lib.loc = lib_path)))

option_list = list(
  make_option(c("-s", "--start_year"),
              action = "store",
              default = wehoop:::most_recent_wbb_season(),
              type = 'integer',
              help = "Start year of the seasons to process"),
  make_option(c("-e", "--end_year"),
              action = "store",
              default = wehoop:::most_recent_wbb_season(),
              type = 'integer',
              help = "End year of the seasons to process")
)
opt = parse_args(OptionParser(option_list = option_list))
options(stringsAsFactors = FALSE)
options(scipen = 999)
years_vec <- opt$s:opt$e

# --- compile into play_by_play_{year}.parquet ---------
wbb_pbp_games <- function(y){

  espn_df <- data.frame()
  pbp_list <- list.files(path = glue::glue('wbb/json/final/'))
  sched <- data.table::fread(paste0('wbb/schedules/csv/wbb_schedule_', y, '.csv'))
  pbp_game_ids <- as.integer(gsub('.json','',pbp_list))
  season_pbp_list <- sched %>%
    dplyr::filter(.data$game_id %in% pbp_game_ids) %>%
    dplyr::pull("game_id")

  cli::cli_progress_step(msg = "Compiling {y} ESPN WBB pbps ({length(season_pbp_list)} games)",
                         msg_done = "Compiled {y} ESPN WBB pbps!")

  future::plan("multisession")
  espn_df <- furrr::future_map_dfr(season_pbp_list, function(x){
    resp <- glue::glue('wbb/json/final/{x}.json')
    pbp <- wehoop:::helper_espn_wbb_pbp(resp)
    return(pbp)
  }, .options = furrr::furrr_options(seed = TRUE))

  if (!('coordinate_x' %in% colnames(espn_df)) && length(espn_df) > 1) {
    espn_df <- espn_df %>%
      dplyr::mutate(
        coordinate_x = NA_real_,
        coordinate_y = NA_real_,
        coordinate_x_raw = NA_real_,
        coordinate_y_raw = NA_real_
      )
  }

  cli::cli_progress_step(msg = "Updating {y} ESPN WBB PBP GitHub Release",
                         msg_done = "Updated {y} ESPN WBB PBP GitHub Release!")

  ifelse(!dir.exists(file.path("wbb/pbp")), dir.create(file.path("wbb/pbp")), FALSE)
  ifelse(!dir.exists(file.path("wbb/pbp/csv")), dir.create(file.path("wbb/pbp/csv")), FALSE)
  if (nrow(espn_df) > 1) {
    espn_df <- espn_df %>%
      dplyr::arrange(dplyr::desc(.data$game_date)) %>%
      wehoop:::make_wehoop_data("ESPN WBB Play-by-Play from wehoop data repository", Sys.time())

    data.table::fwrite(espn_df, file = paste0("wbb/pbp/csv/play_by_play_", y, ".csv.gz"))

    ifelse(!dir.exists(file.path("wbb/pbp/rds")), dir.create(file.path("wbb/pbp/rds")), FALSE)
    saveRDS(espn_df, glue::glue("wbb/pbp/rds/play_by_play_{y}.rds"))

    ifelse(!dir.exists(file.path("wbb/pbp/parquet")), dir.create(file.path("wbb/pbp/parquet")), FALSE)
    arrow::write_parquet(espn_df, glue::glue("wbb/pbp/parquet/play_by_play_{y}.parquet"))

    sportsdataversedata::sportsdataverse_save(
      data_frame = espn_df,
      file_name =  glue::glue("play_by_play_{y}"),
      sportsdataverse_type = "play-by-play data",
      release_tag = "espn_womens_college_basketball_pbp",
      file_types = c("rds", "csv", "parquet"),
      .token = Sys.getenv("GITHUB_PAT")
    )
  }

  sched <- arrow::read_parquet(paste0('wbb/schedules/parquet/wbb_schedule_', y, '.parquet'))
  sched <- sched %>%
    dplyr::mutate(dplyr::across(dplyr::any_of(c(
      "id",
      "game_id",
      "type_id",
      "status_type_id",
      "home_id",
      "home_venue_id",
      "home_conference_id",
      "home_score",
      "away_id",
      "away_venue_id",
      "away_conference_id",
      "away_score",
      "season",
      "season_type",
      "groups_id",
      "tournament_id",
      "venue_id"
    )), ~as.integer(.x))) %>%
    dplyr::mutate(
      status_display_clock = as.character(.data$status_display_clock),
      game_date_time = lubridate::ymd_hm(substr(.data$date, 1, nchar(.data$date) - 1)) %>%
        lubridate::with_tz(tzone = "America/New_York"),
      game_date = as.Date(substr(.data$game_date_time, 1, 10)))

  if (nrow(espn_df) > 0) {

    sched <- sched %>%
      dplyr::mutate(
        PBP = ifelse(.data$game_id %in% unique(espn_df$game_id), TRUE, FALSE))

  } else {

    sched$PBP <- FALSE

  }

  final_sched <- sched %>%
    dplyr::distinct() %>%
    dplyr::arrange(dplyr::desc(.data$date))

  final_sched <- final_sched %>%
    wehoop:::make_wehoop_data("ESPN WBB Schedule from wehoop data repository", Sys.time())

  data.table::fwrite(final_sched, paste0("wbb/schedules/csv/wbb_schedule_", y, ".csv"))
  qs::qsave(final_sched, glue::glue('wbb/schedules/qs/wbb_schedule_{y}.qs'))
  saveRDS(final_sched, glue::glue('wbb/schedules/rds/wbb_schedule_{y}.rds'))
  arrow::write_parquet(final_sched, glue::glue('wbb/schedules/parquet/wbb_schedule_{y}.parquet'))
  rm(sched)
  rm(final_sched)
  rm(espn_df)
  gc()
  return(NULL)
}

tictoc::tic()
all_games <- purrr::map(years_vec, function(y){
  wbb_pbp_games(y)
  return(NULL)
})
tictoc::toc()

cli::cli_progress_step(msg = "Compiling ESPN WBB master schedule",
                       msg_done = "ESPN WBB master schedule compiled and written to disk")

sched_list <- list.files(path = glue::glue('wbb/schedules/parquet/'))
sched_g <-  purrr::map_dfr(sched_list, function(x){
  sched <- arrow::read_parquet(paste0('wbb/schedules/parquet/',x)) %>%
    dplyr::mutate(dplyr::across(dplyr::any_of(c(
      "id",
      "game_id",
      "type_id",
      "status_type_id",
      "home_id",
      "home_venue_id",
      "home_conference_id",
      "home_score",
      "away_id",
      "away_venue_id",
      "away_conference_id",
      "away_score",
      "season",
      "season_type",
      "groups_id",
      "tournament_id",
      "venue_id"
    )), ~as.integer(.x))) %>%
    dplyr::mutate(
      status_display_clock = as.character(.data$status_display_clock)
    )
  return(sched)
})

sched_g <- sched_g %>%
  wehoop:::make_wehoop_data("ESPN WBB Schedule from wehoop data repository", Sys.time())

# data.table::fwrite(sched_g %>% dplyr::arrange(desc(.data$date)), 'wbb_schedule_master.csv')
data.table::fwrite(sched_g %>%
                     dplyr::filter(.data$PBP == TRUE) %>%
                     dplyr::arrange(dplyr::desc(.data$date)), 'wbb/wbb_games_in_data_repo.csv')

arrow::write_parquet(sched_g %>%
                       dplyr::arrange(dplyr::desc(.data$date)), glue::glue('wbb_schedule_master.parquet'))

arrow::write_parquet(sched_g %>%
                       dplyr::filter(.data$PBP == TRUE) %>%
                       dplyr::arrange(dplyr::desc(.data$date)), 'wbb/wbb_games_in_data_repo.parquet')

cli::cli_progress_message("")


rm(sched_g)
rm(sched_list)
rm(years_vec)
gcol <- gc()