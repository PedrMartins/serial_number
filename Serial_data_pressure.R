pkg <- c("serial", "lubridate", "dplyr","stringr", "googlesheets4")

pkg <- pkg[!pkg%in%installed.packages()]
install.packages (pkg)

library(serial)
library(lubridate)
library(dplyr)
library(stringr)
library(googlesheets4)




# Authenticate (run only once)
gs4_auth()  # This opens browser for auth

# Create or link to a sheet
sheet <- gs4_create("Pressure Data Log"
                     , timeZone =)

# Define serial connection
port <- serialConnection(
  name = "myport",
  port = "ttyUSB0",     # Replace with your actual port
  mode = "115200,n,8,1"
)
open(port)
#nBytesInQueue(port)
#close (port)



# Function to parse a serial line into a dataframe row
parse_log_data <- function(raw_text) {
  # Remove ANSI escape sequences (like \033[1;33m)
  clean_text <- gsub("\\033\\[[0-9;]*m", "", raw_text)

  # Split by "Timestamp:" to isolate data entries
  entries <- unlist(strsplit(clean_text, "Timestamp:"))

  # The first element is log text before first timestamp, discard it
  entries <- entries[nchar(entries) > 0][-1]

  # Initialize list to collect parsed data
  data_list <- list()

  for (entry in entries) {
    parts <- unlist(strsplit(entry, ";"))
    rec <- list()

    # Parse timestamp (convert from seconds to POSIX)
    ts_raw <- as.numeric(trimws(parts[1]))
    if (!is.na(ts_raw)) {
      dt <- as_datetime(ts_raw)
      rec$Timestamp <- dt
    }

    # Parse remaining parts
    for (part in parts[-1]) {
      if (grepl("Temperature", part)) {
        rec$Temperature <- as.numeric(str_extract(part, "[0-9.]+"))
      } else if (grepl("Humidity", part)) {
        rec$Humidity <- as.numeric(str_extract(part, "[0-9.]+"))
      } else if (grepl("Pressure", part)) {
        rec$Pressure <- as.numeric(str_extract(part, "[0-9.]+"))
      } else if (grepl("Latitude", part)) {
        rec$Latitude <- as.numeric(str_extract(part, "-?[0-9.]+"))
      } else if (grepl("Longitude", part)) {
        rec$Longitude <- as.numeric(str_extract(part, "-?[0-9.]+"))
      }
    }

    data_list[[length(data_list) + 1]] <- rec
  }

  # Combine into data frame
  df <- do.call(rbind, lapply(data_list, as.data.frame))
  return(df)
}


start_time <- Sys.time() #+ as.difftime(20, units = "mins")
end_time <- start_time + as.difftime(12, units = "weeks")  # Limite de 24 horas
# Initialize dataframe
data_log <- data.frame()

while (dim (data_log)[1] < 50) {

    line <- read.serialConnection(port)
    parsed <- parse_log_data(line)
    new_data <- as.data.frame( parsed)
    data_log <- rbind(data_log, new_data)
    sheet_append(sheet, parsed)


    Sys.sleep(25)

}

close (port)
cat (paste ("work done \nstart in", start_time,"\nfinished in",end_time, sep=" " ))

q (save="no")
