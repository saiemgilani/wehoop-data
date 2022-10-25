#!/bin/bash
while getopts s:e:r: flag
do
    case "${flag}" in
        s) START_YEAR=${OPTARG};;
        e) END_YEAR=${OPTARG};;
        r) RESCRAPE=${OPTARG};;
    esac
done
git pull
python3 scrape_wbb_schedules.py -s $START_YEAR -e $END_YEAR
python3 scrape_wbb_json.py -s $START_YEAR -e $END_YEAR
git pull
git add .
Rscript R/espn_wbb_01_pbp_creation.R -s $START_YEAR -e $END_YEAR
Rscript R/espn_wbb_02_team_box_creation.R -s $START_YEAR -e $END_YEAR
Rscript R/espn_wbb_03_player_box_creation.R -s $START_YEAR -e $END_YEAR
git pull
git add wbb/* wbb_schedule_master.parquet