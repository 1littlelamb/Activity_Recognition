# Version 2 of Loading Data, this time as a Dataframe

pacman::p_load(rio, dplyr, tidyr, tidyverse, RMariaDB)
source('functions/truncate_to_min_activity.R')

directory <- c(pa = "./wisdm-dataset/raw/phone/accel/", pg = "./wisdm-dataset/raw/phone/gyro/",
               wa = "./wisdm-dataset/raw/watch/accel/", wg = "./wisdm-dataset/raw/watch/gyro/") 

files_pa <- directory['pa'] %>% list.files(pattern = "*.txt") %>% paste0(directory['pa'], .)
files_pg <- directory['pg'] %>% list.files(pattern = "*.txt") %>% paste0(directory['pg'], .)
files_wa <- directory['wa'] %>% list.files(pattern = "*.txt") %>% paste0(directory['wa'], .)
files_wg <- directory['wg'] %>% list.files(pattern = "*.txt") %>% paste0(directory['wg'], .)

pa <- lapply(files_pa, function(x) import(x))
pg <- lapply(files_pg, function(x) import(x))
wa <- lapply(files_wa, function(x) import(x))
wg <- lapply(files_wg, function(x) import(x))

# Creating a normal length for all activities across dataframes
combination <- list(pa=pa,pg=pg,wa=wa,wg=wg)
new_combination <- truncate_to_min_activity(combination)

pa_mod <- bind_rows(new_combination$pa)
pg_mod <- bind_rows(new_combination$pg)
wa_mod <- bind_rows(new_combination$wa)
wg_mod <- bind_rows(new_combination$wg)

colnames(pa_mod) <- c("User", "Activity", "Time", "PAX", "PAY", "PAZ")
colnames(pg_mod) <- c("User", "Activity", "Time", "PGX", "PGY", "PGZ")
colnames(wa_mod) <- c("User", "Activity", "Time", "WAX", "WAY", "WAZ")
colnames(wg_mod) <- c("User", "Activity", "Time", "WGX", "WGY", "WGZ")

df <- cbind(pa_mod, pg_mod[,c(4,5,6)], wa_mod[,c(4,5,6)], wg_mod[,c(4,5,6)])

df$PAZ <- as.double(sub(";$", "", df$PAZ))
df$PGZ <- as.double(sub(";$", "", df$PGZ))
df$WAZ <- as.double(sub(";$", "", df$WAZ))
df$WGZ <- as.double(sub(";$", "", df$WGZ))

df$Time <- df$Time / 1e9

saveRDS(df, 'wisdm_dataset_df.rds')