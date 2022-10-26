#!/bin/bash
while getopts s:e:r: flag
do
    case "${flag}" in
        s) START_YEAR=${OPTARG};;
        e) END_YEAR=${OPTARG};;
        r) RESCRAPE=${OPTARG};;
    esac
done
bash daily_wnba_scraper.sh -s $START_YEAR -e $END_YEAR
bash daily_wbb_scraper.sh -s $START_YEAR -e $END_YEAR