rm(list = ls())
gc()

lib_path <- Sys.getenv("R_LIBS")

if (!requireNamespace('pacman', quietly = TRUE)){
  install.packages('pacman',lib=Sys.getenv("R_LIBS"), repos='http://cran.us.r-project.org')
}

suppressPackageStartupMessages(suppressMessages(library(dplyr, lib.loc=lib_path)))
suppressPackageStartupMessages(suppressMessages(library(tidyr, lib.loc=lib_path)))
suppressPackageStartupMessages(suppressMessages(library(magrittr, lib.loc=lib_path)))
suppressPackageStartupMessages(suppressMessages(library(janitor, lib.loc=lib_path)))
suppressPackageStartupMessages(suppressMessages(library(jsonlite, lib.loc=lib_path)))
suppressPackageStartupMessages(suppressMessages(library(purrr, lib.loc=lib_path)))
suppressPackageStartupMessages(suppressMessages(library(progressr, lib.loc=lib_path)))
suppressPackageStartupMessages(suppressMessages(library(arrow, lib.loc=lib_path)))
suppressPackageStartupMessages(suppressMessages(library(glue, lib.loc=lib_path)))
suppressPackageStartupMessages(suppressMessages(library(wehoop, lib.loc=lib_path)))
suppressPackageStartupMessages(suppressMessages(library(future, lib.loc=lib_path)))
suppressPackageStartupMessages(suppressMessages(library(furrr, lib.loc=lib_path)))
suppressPackageStartupMessages(suppressMessages(library(stringr, lib.loc=lib_path)))
suppressPackageStartupMessages(suppressMessages(library(tibble, lib.loc=lib_path)))
suppressPackageStartupMessages(suppressMessages(library(zoo, lib.loc=lib_path)))

options(stringsAsFactors = FALSE)
options(scipen = 999)
years_vec <- 2017:wehoop::most_recent_wnba_season()
proxies <- data.table::fread("../proxylist.csv")
select_proxy <- function(proxies) {
  proxy <- sample(proxies$ip, 1)
  proxy_selected <- proxies %>%
    dplyr::filter(.data$ip == proxy)
  my_proxy <- httr::use_proxy(url = proxy_selected$ip,
                              port = proxy_selected$port,
                              username = proxy_selected$login,
                              password = proxy_selected$password)
  return(my_proxy)
}
# --- wnba_stats_pbp_with_lineups ---------

proxy <- select_proxy(proxies)

player_logs <- wehoop::wnba_leaguegamelog(season = 2017, player_or_team = "P", proxy = proxy)   # change season here

print(player_logs)