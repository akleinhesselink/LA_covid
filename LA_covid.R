# Download new data, scrape, run stats: 

rm(list = ls())

source('code/process_BASA_shapes.R')
source('code/download_updates.R')
source('code/scrape_cases.R')  # for showing old data sources 
source('code/fix_place_names.R')
source('code/calc_stats.R')
