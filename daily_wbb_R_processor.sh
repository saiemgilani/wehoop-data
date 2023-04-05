#!/bin/bash
while getopts s:e:r: flag
do
    case "${flag}" in
        s) START_YEAR=${OPTARG};;
        e) END_YEAR=${OPTARG};;
        r) RESCRAPE=${OPTARG};;
    esac
done
for i in $(seq "${START_YEAR}" "${END_YEAR}")
do
    echo "$i"
    git pull  >> /dev/null
    Rscript R/espn_wbb_01_pbp_creation.R -s $i -e $i
    Rscript R/espn_wbb_02_team_box_creation.R -s $i -e $i
    Rscript R/espn_wbb_03_player_box_creation.R -s $i -e $i
    git pull  >> /dev/null
    git add wbb/* wbb_schedule_master.csv wbb_schedule_master.parquet >> /dev/null
    git pull  >> /dev/null
    git commit -m "WBB Play-by-Play and Schedules update (Start: $i End: $i)" >> /dev/null || echo "No changes to commit"
    git pull  >> /dev/null
    git push  >> /dev/null
done