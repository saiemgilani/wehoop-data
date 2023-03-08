rm(list = ls())
gc()

lib_path <- Sys.getenv("R_LIBS")

if (!requireNamespace("pacman", quietly = TRUE)){
  install.packages("pacman",lib=lib_path, repos="http://cran.us.r-project.org")
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

option_list <- list(
  make_option(c("-s", "--start_year"), action = "store", default = wehoop:::most_recent_wbb_season(),
    type = "integer", help = "Start year of the seasons to process"),
  make_option(c("-e", "--end_year"), action = "store", default = wehoop:::most_recent_wbb_season(),
    type = "integer", help = "End year of the seasons to process")
)
opt <- parse_args(OptionParser(option_list = option_list))
options(stringsAsFactors = FALSE)
options(scipen = 999)
years_vec <- opt$s:opt$e
# --- compile into player_box_{year}.parquet ---------

wbb_player_box_games <- function(y) {

  player_box_g <- data.frame()
  player_box_list <- list.files(path = glue::glue("wbb/json/final/"))
  sched <- data.table::fread(paste0("wbb/schedules/csv/wbb_schedule_", y, ".csv"))
  player_box_game_ids <- as.integer(gsub(".json", "", player_box_list))
  player_box_list <- sched %>%
    dplyr::filter(.data$game_id %in% player_box_game_ids) %>%
    dplyr::pull("game_id")
  cli::cli_process_start("Starting wbb player_box parse for {y}!")

  future::plan("multisession")
  player_box_g <- furrr::future_map_dfr(player_box_list, function(x) {

    player_box_score <- data.frame()
    players_box_score_df <- data.frame()

    tryCatch(
      expr = {
        resp <- glue::glue("wbb/json/final/{x}.json")
        game_json <- resp %>%
          jsonlite::fromJSON(flatten = TRUE)


        gameId <- as.integer(game_json[["header"]][["id"]])
        season <- game_json[["header"]][["season"]][["year"]]
        season_type <- game_json[["header"]][["season"]][["type"]]
        game_date <- as.Date(substr(game_json[["header"]][["competitions"]][["date"]], 0, 10))
        boxScoreAvailable <- game_json[["header"]][["competitions"]][["boxscoreAvailable"]]

        boxScoreSource <- game_json[["header"]][["competitions"]][["boxscoreSource"]]
        players_box_score_df <- game_json[["boxscore"]][["players"]] %>%
          jsonlite::toJSON() %>%
          jsonlite::fromJSON(flatten = TRUE) %>%
          as.data.frame()

        if (boxScoreAvailable == TRUE &&
            length(players_box_score_df[["statistics"]][[1]][["athletes"]][[1]]) > 1) {
          players_df <- players_box_score_df %>%
            tidyr::unnest("statistics") %>%
            tidyr::unnest("athletes")
          if (length(players_box_score_df[["statistics"]]) > 1 &&
              length(players_df$stats[[1]] > 0)) {
            players_df <- jsonlite::fromJSON(jsonlite::toJSON(game_json[["boxscore"]][["players"]]), flatten=TRUE) %>%
              tidyr::unnest("statistics") %>%
              tidyr::unnest("athletes")
            stat_cols <- players_df$names[[1]]
            stat_cols <- players_df$keys[[1]]
            stats <- players_df$stats

            stats_df <- as.data.frame(do.call(rbind,stats))
            colnames(stats_df) <- stat_cols
            suppressWarnings(
              stats_df <- stats_df %>%
                tidyr::separate("fieldGoalsMade-fieldGoalsAttempted",
                                into = c("fieldGoalsMade", "fieldGoalsAttempted"),
                                sep = "-") %>%
                tidyr::separate("freeThrowsMade-freeThrowsAttempted",
                                into = c("freeThrowsMade", "freeThrowsAttempted"),
                                sep = "-") %>%
                tidyr::separate("threePointFieldGoalsMade-threePointFieldGoalsAttempted",
                                into = c("threePointFieldGoalsMade", "threePointFieldGoalsAttempted"),
                                sep = "-") %>%
                dplyr::mutate(dplyr::across(dplyr::any_of(c(
                  "minutes",
                  "fieldGoalPct",
                  "freeThrowPct",
                  "threePointFieldGoalPct"
                )), ~as.numeric(.x))) %>%
                dplyr::mutate(dplyr::across(dplyr::any_of(c(
                  "assists",
                  "blocks",
                  "defensiveRebounds",
                  "fieldGoalsMade",
                  "fieldGoalsAttempted",
                  "flagrantFouls",
                  "fouls",
                  "freeThrowsMade",
                  "freeThrowsAttempted",
                  "offensiveRebounds",
                  "steals",
                  "teamTurnovers",
                  "technicalFouls",
                  "threePointFieldGoalsMade",
                  "threePointFieldGoalsAttempted",
                  "rebounds",
                  "totalTechnicalFouls",
                  "totalTurnovers",
                  "turnovers",
                  "points"
                )), ~as.integer(.x)))
            )
            players_df <- players_df %>%
              dplyr::filter(!.data$didNotPlay) %>%
              dplyr::select(dplyr::any_of(c(
                "starter",
                "ejected",
                "didNotPlay",
                "active",
                "athlete.displayName",
                "athlete.jersey",
                "athlete.id",
                "athlete.shortName",
                "athlete.headshot.href",
                "athlete.position.name",
                "athlete.position.abbreviation",
                "team.shortDisplayName",
                "team.name",
                "team.logo",
                "team.id",
                "team.abbreviation",
                "team.color",
                "team.alternateColor"
              )))

            players_df <- dplyr::bind_cols(stats_df,players_df) %>%
              dplyr::select(dplyr::any_of(c(
                "athlete.displayName",
                "team.shortDisplayName")),
                tidyr::everything()) %>%
              janitor::clean_names() %>%
              dplyr::mutate(
                game_id = gameId,
                season = season,
                season_type = season_type,
                game_date = game_date)

            player_box_score <- players_df %>%
              dplyr::select(dplyr::any_of(c(
                "game_id",
                "season",
                "season_type",
                "game_date",
                "athlete_id",
                "athlete_display_name",
                "team_id",
                "team_name",
                "team_short_display_name",
                "minutes",
                "field_goals_made",
                "field_goals_attempted",
                "three_point_field_goals_made",
                "three_point_field_goals_attempted",
                "free_throws_made",
                "free_throws_attempted",
                "offensive_rebounds",
                "defensive_rebounds",
                "rebounds",
                "assists",
                "steals",
                "blocks",
                "turnovers",
                "fouls",
                "points",
                "starter",
                "ejected",
                "did_not_play",
                "active",
                "athlete_jersey",
                "athlete_short_name",
                "athlete_headshot_href",
                "athlete_position_name",
                "athlete_position_abbreviation",
                "team_logo",
                "team_abbreviation",
                "team_color",
                "team_alternate_color"))) %>%
              wehoop:::make_wehoop_data("ESPN WBB Player Box Information from ESPN.com", Sys.time())
          }
        }
      },
      error = function(e) {
        message(glue::glue("{Sys.time()}: Invalid arguments or no player box data available {gameId}!"))
      },
      warning = function(w) {
      },
      finally = {
      }
    )
    return(player_box_score)
  }, .options = furrr::furrr_options(seed = TRUE))

  if (nrow(player_box_g) > 1) {
    player_box_g <- player_box_g %>%
      wehoop:::make_wehoop_data("ESPN WBB Player Boxscore Information from wehoop data repository", Sys.time())

    ifelse(!dir.exists(file.path("wbb/player_box")), dir.create(file.path("wbb/player_box")), FALSE)

    ifelse(!dir.exists(file.path("wbb/player_box/csv")), dir.create(file.path("wbb/player_box/csv")), FALSE)
    data.table::fwrite(player_box_g, file = paste0("wbb/player_box/csv/player_box_", y, ".csv.gz"))

    ifelse(!dir.exists(file.path("wbb/player_box/qs")), dir.create(file.path("wbb/player_box/qs")), FALSE)
    qs::qsave(player_box_g, glue::glue("wbb/player_box/qs/player_box_{y}.qs"))

    ifelse(!dir.exists(file.path("wbb/player_box/rds")), dir.create(file.path("wbb/player_box/rds")), FALSE)
    saveRDS(player_box_g, glue::glue("wbb/player_box/rds/player_box_{y}.rds"))

    ifelse(!dir.exists(file.path("wbb/player_box/parquet")), dir.create(file.path("wbb/player_box/parquet")), FALSE)
    arrow::write_parquet(player_box_g, glue::glue("wbb/player_box/parquet/player_box_{y}.parquet"))
  }
  sched <- data.table::fread(paste0("wbb/schedules/csv/wbb_schedule_", y, ".csv"))
  sched <- sched %>%
    dplyr::mutate(
      game_id = as.integer(.data$id),
      status_display_clock = as.character(.data$status_display_clock)
    )
  if(nrow(player_box_g)>0) {
    sched <- sched %>%
      dplyr::mutate(
        player_box = ifelse(.data$game_id %in% unique(player_box_g$game_id), TRUE, FALSE)
      )
  } else {
    sched$player_box <- FALSE
  }

  final_sched <- dplyr::distinct(sched) %>% dplyr::arrange(desc(.data$date))
  final_sched <- final_sched %>%
    wehoop:::make_wehoop_data("ESPN WBB Schedule Information from wehoop data repository", Sys.time())
  data.table::fwrite(final_sched, paste0("wbb/schedules/csv/wbb_schedule_", y, ".csv"))
  qs::qsave(final_sched, glue::glue("wbb/schedules/qs/wbb_schedule_{y}.qs"))
  saveRDS(final_sched, glue::glue("wbb/schedules/rds/wbb_schedule_{y}.rds"))
  arrow::write_parquet(final_sched, glue::glue("wbb/schedules/parquet/wbb_schedule_{y}.parquet"))
  rm(sched)
  rm(final_sched)
  rm(player_box_g)
  rm(player_box_list)
  gc()
  cli::cli_process_done(msg_done = "Finished wbb player_box parse for {y}!")
  return(NULL)
}

all_games <- purrr::map(years_vec, function(y){
  wbb_player_box_games(y)
})

sched_list <- list.files(path = glue::glue("wbb/schedules/csv/"))
sched_g <-  purrr::map_dfr(sched_list, function(x) {
  sched <- data.table::fread(paste0("wbb/schedules/csv/",x)) %>%
    dplyr::mutate(
      status_display_clock = as.character(.data$status_display_clock)
    )
  return(sched)
})

sched_g <- sched_g %>%
  wehoop:::make_wehoop_data("ESPN WBB Schedule Information from wehoop data repository",Sys.time())

# data.table::fwrite(sched_g %>% dplyr::arrange(desc(.data$date)), "wbb_schedule_master.csv")
data.table::fwrite(sched_g %>% dplyr::filter(.data$PBP == TRUE) %>% dplyr::arrange(desc(.data$date)), "wbb/wbb_games_in_data_repo.csv")
qs::qsave(sched_g %>% dplyr::arrange(desc(.data$date)), "wbb_schedule_master.qs")
qs::qsave(sched_g %>% dplyr::filter(.data$PBP == TRUE) %>% dplyr::arrange(desc(.data$date)), "wbb/wbb_games_in_data_repo.qs")
arrow::write_parquet(sched_g %>% dplyr::arrange(desc(.data$date)),glue::glue("wbb_schedule_master.parquet"))
arrow::write_parquet(sched_g %>% dplyr::filter(.data$PBP == TRUE) %>% dplyr::arrange(desc(.data$date)), "wbb/wbb_games_in_data_repo.parquet")

rm(all_games)
rm(sched_g)
rm(sched_list)
rm(years_vec)
gc()