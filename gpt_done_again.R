pkg <- c("serial", "lubridate", "dplyr")

pkg <- pkg[!pkg%in%installed.packages()]
install.packages (pkg)

library(serial)
library(lubridate)
library(dplyr)

# Function to convert timestamp to POSIXct (UNIX time to human-readable)
convert_timestamp <- function(ts) {
  as.POSIXct(as.numeric(ts), origin = "1970-01-01", tz = "UTC")
}

# Define serial connection
port <- serialConnection(
  name = "myport",
  port = "ttyUSB0",     # Replace with your actual port
  mode = "115200,n,8,1"
)
open(port)
nBytesInQueue(port)
close (port)



# Initialize an empty data frame
data <- data.frame()

start_time <- Sys.time()
end_time <- start_time + as.difftime(12, units = "hours")  # Limite de 24 horas


# Start reading loop
while (Sys.time() < end_time) {
  tryCatch({
    line <- read.serialConnection(port)
    if (nchar(line) > 0) {
      # Remove leading/trailing spaces
      line <- trimws(line)

      # Parse key-value pairs
      parts <- unlist(strsplit(line, ";"))
      measurements <- list()
      timestamp <- NULL

      for (part in parts) {
        part <- trimws(part)
        if (grepl("^Timestamp:", part)) {
          timestamp <- sub("Timestamp:\\s*", "", part)
        } else if (grepl(":", part)) {
          kv <- unlist(strsplit(part, ":"))
          key <- trimws(kv[1])
          value <- trimws(kv[2])
          measurements[[key]] <- value
        }
      }

      if (!is.null(timestamp)) {
        datetime <- convert_timestamp(timestamp)
        entry <- data.frame(
          Date = as.Date(datetime),
          Time = format(datetime, "%H:%M:%S"),
          t(measurements),
          stringsAsFactors = FALSE
        )
        data <- rbind(data, entry)
        print(entry)
      }
    }
  }, error = function(e) {
    cat("Error:", e$message, "\n")
  })
}

# Close port on exit (in real use you might use on.exit() or an interrupt handler)
close(port)
