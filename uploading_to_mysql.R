# 3rd Iteration of the Data Loading Process
# This was part of an investigation into using MySQL as suggested by a Computer Science 
# friend of mine.

pacman::p_load(RMariaDB, rio)

data <- import('rds_datasets/wisdm_dataset_df.rds')

# Connecting and uploading to MySQL
localuserpass <- 'password'

wisdmDb <- dbConnect(RMariaDB::MariaDB(),
                     user = 'felix',
                     password = localuserpass,
                     dbname = 'wisdm',
                     host = 'localhost')

dbWriteTable(wisdmDb, value = data, name = 'Data', row.names = F, overwrite = T)



dbDisconnect(wisdmDb)

