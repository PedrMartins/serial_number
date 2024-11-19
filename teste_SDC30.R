install.packages("serial")
library(serial)
library(readr)
library (lubridate)



tty <- serialConnection(name = "test",
                        port = "ttyUSB0")
open (tty)

nBytesInQueue(tty)



parse_measurements <- function(segment) {
  list(CO2 = as.numeric(sub(".*CO2: ([0-9.]+) ppm.*", "\\1", segment)),
      Temperature = as.numeric(sub(".*Temperature: ([0-9.]+) oC.*", "\\1", segment)),
      Humidity = as.numeric(sub(".*Humidity: ([0-9.]+) % RH.*", "\\1", segment))
  )
}

data_df <- data.frame(CO2 = numeric(),
                      Temperature = numeric(),
                      Humidity = numeric(),
                      stringsAsFactors = FALSE)




#Guardar o tempo inicial
start_time <- Sys.time()
end_time <- start_time + as.difftime(12, units = "hours")  # Limite de 24 horas

# Loop de leitura por 24 horas
while (Sys.time() < end_time) {
  # Ler e parsear os dados da conexão serial
  raw_data <- read.serialConnection(tty)  # Ajuste `size` conforme necessário


  # Extrair e armazenar cada leitura no data frame
  parsed_data <- lapply(raw_data, parse_measurements)
  new_data <- as.data.frame(do.call(rbind, parsed_data))  # Converter para data.frame
  data_df <- rbind(data_df, new_data)  # Adicionar novas leituras ao data frame principal

  # Pausar o loop para evitar leituras excessivas (por exemplo, 10 segundos entre leituras)
  Sys.sleep(240)
}



head (data_df)
data_dioxC<- data.frame (unlist (data_df[,1]))
head (data_dioxC)
colnames (data_dioxC) <- c( "co2")
time_series <- seq.POSIXt(from = start_time,
                          by = "60 secs",
                          length.out = nrow(data_dioxC))


data_dioxC$timeH <- time_series
length(data_dioxC [,1])

boxplot(data_dioxC [,1])
par(bty ="n", bg = "grey99", las =1,
    family="serif")


plot (data_dioxC$co2~data_dioxC$timeH,
      type="p", xlab= "hour",
      ylab = expression(CO[2]~"(ppm)"),
      pch="*"
)


plot (data_dioxC$co2~data_dioxC$timeH,
      type="p", xlab= "hour",
      ylab = "ºC",
      pch="*"
)

dev.off()

isOpen(tty)
#close(tty)
