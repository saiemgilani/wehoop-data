rm(list = ls())
gc()

lib_path <- Sys.getenv("R_LIBS")

if (!requireNamespace("pacman", quietly = TRUE)){
  install.packages("pacman",lib=lib_path, repos="http://cran.us.r-project.org")
}

suppressPackageStartupMessages(suppressMessages(library(dplyr, lib.loc=lib_path)))
suppressPackageStartupMessages(suppressMessages(library(magrittr, lib.loc=lib_path)))
suppressPackageStartupMessages(suppressMessages(library(jsonlite, lib.loc=lib_path)))
suppressPackageStartupMessages(suppressMessages(library(purrr, lib.loc=lib_path)))
suppressPackageStartupMessages(suppressMessages(library(progressr, lib.loc=lib_path)))
suppressPackageStartupMessages(suppressMessages(library(data.table, lib.loc=lib_path)))
suppressPackageStartupMessages(suppressMessages(library(qs, lib.loc=lib_path)))
suppressPackageStartupMessages(suppressMessages(library(arrow, lib.loc=lib_path)))
suppressPackageStartupMessages(suppressMessages(library(glue, lib.loc=lib_path)))
suppressPackageStartupMessages(suppressMessages(library(optparse, lib.loc=lib_path)))

option_list = list(
  make_option(c("-s", "--start_year"), action="store", default=wehoop:::most_recent_wbb_season(), type="integer", help="Start year of the seasons to process"),
  make_option(c("-e", "--end_year"), action="store", default=wehoop:::most_recent_wbb_season(), type="integer", help="End year of the seasons to process")
)
opt = parse_args(OptionParser(option_list=option_list))
options(stringsAsFactors = FALSE)
options(scipen = 999)
years_vec <- opt$s:opt$e
# --- compile into team_box_{year}.parquet ---------

wbb_team_box_games <- function(y) {
  cli::cli_process_start("Starting wbb team_box parse for {y}!")
  team_box_g <- data.frame()
  team_box_list <- list.files(path = glue::glue("wbb/json/final/"))
  sched <- data.table::fread(paste0("wbb/schedules/csv/wbb_schedule_", y, ".csv"))
  team_box_game_ids <- as.integer(gsub(".json","",team_box_list))
  team_box_list <- sched %>%
    dplyr::filter(.data$game_id %in% team_box_game_ids) %>%
    dplyr::pull("game_id")

  future::plan("multisession")
  team_box_g <- furrr::future_map_dfr(team_box_list, function(x) {
    game_json <- glue::glue("wbb/json/final/{x}.json") %>%
        jsonlite::fromJSON()

    team_box_score <- data.frame()
    teams_box_score_df <- data.frame()
    gameId <- as.integer(game_json[["gameId"]])
    box_score_available <- game_json[["header"]][["competitions"]][["boxscoreAvailable"]]
    
    tryCatch(
      expr = {
        if (box_score_available == TRUE) {
          teams_box_score_df <- game_json[["boxscore"]][["teams"]] %>%
            jsonlite::toJSON() %>%
            jsonlite::fromJSON(flatten = TRUE)
          if (length(teams_box_score_df[["statistics"]][[1]]) > 0) {
            # Teams info columns and values
            teams_df <- game_json[["header"]][["competitions"]][["competitors"]][[1]]

            homeAway1 <- teams_df[["homeAway"]][1]
            homeAway1_team.id <- as.integer(teams_df[["id"]][1])
            homeAway1_team.score <- as.integer(teams_df[["score"]][1])
            homeAway1_team.winner <- teams_df[["winner"]][1]

            homeAway2 <- teams_df[["homeAway"]][2]
            homeAway2_team.id <- as.integer(teams_df[["id"]][2])
            homeAway2_team.score <- as.integer(teams_df[["score"]][2])
            homeAway2_team.winner <- teams_df[["winner"]][2]

            # Pivoting the table values for each team from long to wide
            statistics_df_1 <- teams_box_score_df[["statistics"]][[1]] %>%
              tibble::tibble() %>%
              dplyr::select("name", "displayValue") %>%
              tidyr::spread("name", "displayValue")

            statistics_df_2 <- teams_box_score_df[["statistics"]][[2]] %>%
              tibble::tibble() %>%
              dplyr::select("name", "displayValue") %>%
              tidyr::spread("name", "displayValue")

            # Assigning values to the correct data frame rows - 1
            statistics_df_1$team.homeAway <- ifelse(
              as.integer(teams_box_score_df[["team.id"]][1]) == as.integer(homeAway1_team.id),
              homeAway1,
              homeAway2
            )
            statistics_df_1$team.score <- ifelse(
              as.integer(teams_box_score_df[["team.id"]][1]) == as.integer(homeAway1_team.id),
              as.integer(homeAway1_team.score),
              as.integer(homeAway2_team.score)
            )
            statistics_df_1$team.winner <- ifelse(
              as.integer(teams_box_score_df[["team.id"]][1]) == as.integer(homeAway1_team.id),
              homeAway1_team.winner,
              homeAway2_team.winner
            )
            statistics_df_1$team.id <- as.integer(teams_box_score_df[["team.id"]][[1]])
            statistics_df_1$team.uid <- teams_box_score_df[["team.uid"]][[1]]
            statistics_df_1$team.slug <- teams_box_score_df[["team.slug"]][[1]]
            statistics_df_1$team.location <- teams_box_score_df[["team.location"]][[1]]
            statistics_df_1$team.name <- teams_box_score_df[["team.name"]][[1]]
            statistics_df_1$team.abbreviation <- teams_box_score_df[["team.abbreviation"]][[1]]
            statistics_df_1$team.displayName <- teams_box_score_df[["team.displayName"]][[1]]
            statistics_df_1$team.shortDisplayName <- teams_box_score_df[["team.shortDisplayName"]][[1]]
            statistics_df_1$team.color <- teams_box_score_df[["team.color"]][[1]]
            statistics_df_1$team.alternateColor <- teams_box_score_df[["team.alternateColor"]][[1]]
            statistics_df_1$team.logo <- teams_box_score_df[["team.logo"]][[1]]
            statistics_df_1$opponent.team.id <- as.integer(teams_box_score_df[["team.id"]][[2]])
            statistics_df_1$opponent.team.uid <- teams_box_score_df[["team.uid"]][[2]]
            statistics_df_1$opponent.team.slug <- teams_box_score_df[["team.slug"]][[2]]
            statistics_df_1$opponent.team.location <- teams_box_score_df[["team.location"]][[2]]
            statistics_df_1$opponent.team.name <- teams_box_score_df[["team.name"]][[2]]
            statistics_df_1$opponent.team.abbreviation <- teams_box_score_df[["team.abbreviation"]][[2]]
            statistics_df_1$opponent.team.displayName <- teams_box_score_df[["team.displayName"]][[2]]
            statistics_df_1$opponent.team.shortDisplayName <- teams_box_score_df[["team.shortDisplayName"]][[2]]
            statistics_df_1$opponent.team.color <- teams_box_score_df[["team.color"]][[2]]
            statistics_df_1$opponent.team.alternateColor <- teams_box_score_df[["team.alternateColor"]][[2]]
            statistics_df_1$opponent.team.logo <- teams_box_score_df[["team.logo"]][[2]]

            # Assigning values to the correct data frame rows - 2
            statistics_df_2$team.homeAway <- ifelse(
              as.integer(teams_box_score_df[["team.id"]][2]) == as.integer(homeAway2_team.id),
              homeAway2,
              homeAway1
            )
            statistics_df_2$team.score <- ifelse(
              as.integer(teams_box_score_df[["team.id"]][2]) == as.integer(homeAway2_team.id),
              as.integer(homeAway2_team.score),
              as.integer(homeAway1_team.score)
            )
            statistics_df_2$team.winner <- ifelse(
              as.integer(teams_box_score_df[["team.id"]][2]) == as.integer(homeAway2_team.id),
              homeAway2_team.winner,
              homeAway1_team.winner
            )
            statistics_df_2$team.id <- as.integer(teams_box_score_df[["team.id"]][[2]])
            statistics_df_2$team.uid <- teams_box_score_df[["team.uid"]][[2]]
            statistics_df_2$team.slug <- teams_box_score_df[["team.slug"]][[2]]
            statistics_df_2$team.location <- teams_box_score_df[["team.location"]][[2]]
            statistics_df_2$team.name <- teams_box_score_df[["team.name"]][[2]]
            statistics_df_2$team.abbreviation <- teams_box_score_df[["team.abbreviation"]][[2]]
            statistics_df_2$team.displayName <- teams_box_score_df[["team.displayName"]][[2]]
            statistics_df_2$team.shortDisplayName <- teams_box_score_df[["team.shortDisplayName"]][[2]]
            statistics_df_2$team.color <- teams_box_score_df[["team.color"]][[2]]
            statistics_df_2$team.alternateColor <- teams_box_score_df[["team.alternateColor"]][[2]]
            statistics_df_2$team.logo <- teams_box_score_df[["team.logo"]][[2]]
            statistics_df_2$opponent.team.id <- as.integer(teams_box_score_df[["team.id"]][[1]])
            statistics_df_2$opponent.team.uid <- teams_box_score_df[["team.uid"]][[1]]
            statistics_df_2$opponent.team.slug <- teams_box_score_df[["team.slug"]][[1]]
            statistics_df_2$opponent.team.location <- teams_box_score_df[["team.location"]][[1]]
            statistics_df_2$opponent.team.name <- teams_box_score_df[["team.name"]][[1]]
            statistics_df_2$opponent.team.abbreviation <- teams_box_score_df[["team.abbreviation"]][[1]]
            statistics_df_2$opponent.team.displayName <- teams_box_score_df[["team.displayName"]][[1]]
            statistics_df_2$opponent.team.shortDisplayName <- teams_box_score_df[["team.shortDisplayName"]][[1]]
            statistics_df_2$opponent.team.color <- teams_box_score_df[["team.color"]][[1]]
            statistics_df_2$opponent.team.alternateColor <- teams_box_score_df[["team.alternateColor"]][[1]]
            statistics_df_2$opponent.team.logo <- teams_box_score_df[["team.logo"]][[1]]

            complete_statistics_df <- statistics_df_1 %>%
              dplyr::bind_rows(statistics_df_2)

            # Assigning game/season level data to team box score and converting types
            complete_statistics_df$season <- game_json[["header"]][["season"]][["year"]]
            complete_statistics_df$season_type <- game_json[["header"]][["season"]][["type"]]
            complete_statistics_df$game_date <- as.Date(substr(game_json[["header"]][["competitions"]][["date"]], 0, 10))
            complete_statistics_df$game_id <- as.integer(x)

            suppressWarnings(
              complete_statistics_df <- complete_statistics_df %>%
                tidyr::separate("fieldGoalsMade-fieldGoalsAttempted",
                                into = c("fieldGoalsMade", "fieldGoalsAttempted"),
                                sep = "-") %>%
                tidyr::separate("freeThrowsMade-freeThrowsAttempted",
                                into = c("freeThrowsMade", "freeThrowsAttempted"),
                                sep = "-") %>%
                tidyr::separate("threePointFieldGoalsMade-threePointFieldGoalsAttempted",
                                into = c("threePointFieldGoalsMade", "threePointFieldGoalsAttempted"),
                                sep = "-") %>%
                dplyr::mutate(dplyr::across(c(
                  "fieldGoalPct",
                  "freeThrowPct",
                  "threePointFieldGoalPct"
                ), ~as.numeric(.x))) %>%
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
                  "totalRebounds",
                  "totalTechnicalFouls",
                  "totalTurnovers",
                  "turnovers",
                )), ~as.integer(.x)))
            )
            team_box_score <- complete_statistics_df %>%
              janitor::clean_names() %>%
              dplyr::select(
                "game_id",
                "season",
                "season_type",
                "game_date",
                "team_id",
                "team_uid",
                "team_slug",
                "team_location",
                "team_name",
                "team_abbreviation",
                "team_display_name",
                "team_short_display_name",
                "team_color",
                "team_alternate_color",
                "team_logo",
                "team_home_away",
                "team_score",
                "team_winner",
                tidyr::everything()) %>%
              wehoop:::make_wehoop_data("ESPN WBB Team Box Information from ESPN.com", Sys.time())
          }
        }
      },
      error = function(e) {
        message(glue::glue("{Sys.time()}: Invalid arguments or no team box data available {x}!"))
      },
      warning = function(w) {
      },
      finally = {
      }
    )
    drop <- c("statistics")
    team_box_score <- team_box_score[, !(names(team_box_score) %in% drop)]
    return(team_box_score)
  }, .options = furrr::furrr_options(seed = TRUE))

  if (nrow(team_box_g) > 0) {
    team_box_g <- team_box_g %>%
      wehoop:::make_wehoop_data("ESPN WBB Team Boxscore Information from wehoop data repository", Sys.time())

    ifelse(!dir.exists(file.path("wbb/team_box")), dir.create(file.path("wbb/team_box")), FALSE)
    ifelse(!dir.exists(file.path("wbb/team_box/csv")), dir.create(file.path("wbb/team_box/csv")), FALSE)
    data.table::fwrite(team_box_g, file = paste0("wbb/team_box/csv/team_box_", y, ".csv.gz"))

    ifelse(!dir.exists(file.path("wbb/team_box/qs")), dir.create(file.path("wbb/team_box/qs")), FALSE)
    qs::qsave(team_box_g, glue::glue("wbb/team_box/qs/team_box_{y}.qs"))

    ifelse(!dir.exists(file.path("wbb/team_box/rds")), dir.create(file.path("wbb/team_box/rds")), FALSE)
    saveRDS(team_box_g, glue::glue("wbb/team_box/rds/team_box_{y}.rds"))

    ifelse(!dir.exists(file.path("wbb/team_box/parquet")), dir.create(file.path("wbb/team_box/parquet")), FALSE)
    arrow::write_parquet(team_box_g, glue::glue("wbb/team_box/parquet/team_box_{y}.parquet"))
  }

  sched <- sched %>%
    dplyr::mutate(
      game_id = as.integer(.data$id),
      status_display_clock = as.character(.data$status_display_clock)
    )
  if (nrow(team_box_g) > 0) {
    sched <- sched %>%
      dplyr::mutate(
        team_box = ifelse(.data$game_id %in% unique(team_box_g$game_id), TRUE, FALSE)
      )
  } else {
    sched$team_box <- FALSE
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
  rm(team_box_g)
  rm(team_box_list)
  cli::cli_process_done(msg_done = "Finished wbb team_box parse for {y}!")
  return(NULL)
}


all_games <- purrr::map(years_vec, function(y) {
  wbb_team_box_games(y)
  return(NULL)
})
gc()
sched_list <- list.files(path = glue::glue("wbb/schedules/csv/"))
sched_g <-  purrr::map_dfr(sched_list, function(x) {
  sched <- data.table::fread(paste0("wbb/schedules/csv/", x)) %>%
    dplyr::mutate(
      status_display_clock = as.character(.data$status_display_clock)
    )
  return(sched)
})


sched_g <- sched_g %>%
  wehoop:::make_wehoop_data("ESPN WBB Schedule Information from wehoop data repository", Sys.time())

data.table::fwrite(sched_g %>% dplyr::arrange(desc(.data$date)), "wbb_schedule_master.csv")
data.table::fwrite(sched_g %>% dplyr::filter(.data$PBP == TRUE) %>% dplyr::arrange(desc(.data$date)), "wbb/wbb_games_in_data_repo.csv")
qs::qsave(sched_g %>% dplyr::arrange(desc(.data$date)), "wbb_schedule_master.qs")
qs::qsave(sched_g %>% dplyr::filter(.data$PBP == TRUE) %>% dplyr::arrange(desc(.data$date)), "wbb/wbb_games_in_data_repo.qs")
arrow::write_parquet(sched_g %>% dplyr::arrange(desc(.data$date)), glue::glue("wbb_schedule_master.parquet"))
arrow::write_parquet(sched_g %>% dplyr::filter(.data$PBP == TRUE) %>% dplyr::arrange(desc(.data$date)), "wbb/wbb_games_in_data_repo.parquet")


rm(all_games)
rm(sched_g)
rm(sched_list)
rm(years_vec)
gc()