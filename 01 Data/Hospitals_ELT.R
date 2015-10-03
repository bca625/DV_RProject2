setwd("~/Assignments/UT Austin/Data Vis/DV_RProject2/01 Data") ###

file_path <- "HospitalPayments.csv"

hospitals <- read.csv(file_path, stringsAsFactors = FALSE)

# Replace "." (i.e., period) with "_" in the column names.
names(hospitals) <- gsub("\\.+", "_", names(hospitals))

str(hospitals) # Uncomment this and  run just the lines to here to get column types to use for getting the list of measures.

measures <- c("Provider_ID", "ZIP_Code", "Phone_number", "Payment", "Lower_estimate", "Higher_estimate")
#measures <- NA # Do this if there are no measures.

# Get rid of special characters in each column.
# Google ASCII Table to understand the following:
for(n in names(hospitals)) {
  hospitals[n] <- data.frame(lapply(hospitals[n], gsub, pattern="[^ -~]",replacement= ""))
}

dimensions <- setdiff(names(hospitals), measures)
if( length(measures) > 1 || ! is.na(dimensions)) {
  for(d in dimensions) {
    # Get rid of " and ' in dimensions.
    hospitals[d] <- data.frame(lapply(hospitals[d], gsub, pattern="[\"']",replacement= ""))
    # Change & to and in dimensions.
    hospitals[d] <- data.frame(lapply(hospitals[d], gsub, pattern="&",replacement= " and "))
    # Change : to ; in dimensions.
    hospitals[d] <- data.frame(lapply(hospitals[d], gsub, pattern=":",replacement= ";"))
  }
}

library(lubridate)
# Fix date columns, this needs to be done by hand because | needs to be correct.
#                                                        \_/
hospitals$Measure_start_date <- gsub(" [0-9]+:.*", "", gsub(" UTC", "", mdy(as.character(hospitals$Measure_start_date), tz="UTC")))
hospitals$Measure_End_Date  <- gsub(" [0-9]+:.*", "", gsub(" UTC", "", mdy(as.character(hospitals$Measure_End_Date),  tz="UTC")))

# The following is an example of dealing with special cases like making state abbreviations be all upper case.
# hospitals["State"] <- data.frame(lapply(hospitals["State"], toupper))

# Get rid of all characters in measures except for numbers, the - sign, and period.dimensions
if( length(measures) > 1 || ! is.na(measures)) {
  for(m in measures) {
    hospitals[m] <- data.frame(lapply(hospitals[m], gsub, pattern="[^--.0-9]",replacement= ""))
  }
}

write.csv(hospitals, paste(gsub(".csv", "", file_path), ".reformatted.csv", sep=""), row.names=FALSE, na = "")

tableName <- gsub(" +", "_", gsub("[^A-z, 0-9, ]", "", gsub(".csv", "", file_path)))
sql <- paste("CREATE TABLE", tableName, "(\n-- Change table_name to the table name you want.\n")
if( length(measures) > 1 || ! is.na(dimensions)) {
  for(d in dimensions) {
    sql <- paste(sql, paste(d, "varchar2(4000),\n"))
  }
}
if( length(measures) > 1 || ! is.na(measures)) {
  for(m in measures) {
    if(m != tail(measures, n=1)) sql <- paste(sql, paste(m, "number(38,4),\n"))
    else sql <- paste(sql, paste(m, "number(38,4)\n"))
  }
}
sql <- paste(sql, ");")
cat(sql)
