# author - Taiyun Kim

################################################################################
# MS_db_update 
# 
# Backup the current database (CSV file) to another location named by current date/time of creation
# Then write a new update database by user's request.
################################################################################
MS_db_update <- function(new_allFeatures) {
  # today <- Sys.Date()
  now <- strftime(Sys.time(), format = "%Y-%m-%d %H.%M.%S")
  dir.create(paste(getwd(), "backup_db", now, sep="/"))
  
  # copy current allFeatures.csv
  file.copy(from = paste(getwd(), "/allFeatures.csv", sep = ""), to = paste(getwd(), "backup_db",  now, sep = "/"))
 
  # write the updated allFeatures.csv to set as new db.
  write.csv(new_allFeatures, paste(getwd(), "/allFeatures.csv", sep = ""))
}