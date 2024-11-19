install.packages("serial")
library(serial)
library(readr)
library (lubridate)



tty <- serialConnection(name = "test",
                        port = "ttyUSB0")
open (tty)
close (tty)
nBytesInQueue(tty)
listPorts()

raw_data <- read.serialConnection(tty)
head (raw_data)
data_segment <- strsplit(raw_data, "Encoded data:") [[1]]
data_segment <- data_segment[data_segment != ""]

parse_measurements <- function(segment) {
  list(
    Light = as.numeric(sub(".*Light: ([0-9.]+) lux.*", "\\1", segment)),
    Infrared = as.numeric(sub(".*Infrared: ([0-9.]+) lux.*", "\\1", segment)),
    UV = as.numeric(sub(".*UV: ([0-9.]+);.*", "\\1", segment)),
    UVIndex = as.numeric(sub(".*UVIndex: ([0-9.]+);.*", "\\1", segment)),
    CO2 = as.numeric(sub(".*CO2: ([0-9.]+) ppm.*", "\\1", segment)),
    Temperature = as.numeric(sub(".*Temperature: ([0-9.]+) oC.*", "\\1", segment)),
    Humidity = as.numeric(sub(".*Humidity: ([0-9.]+) % RH.*", "\\1", segment))
  )
}

data_df <- data.frame(Light = numeric(),
                      Infrared = numeric(),
                      UV = numeric(),
                      CO2 = numeric(),
                      Temperature = numeric(),
                      Humidity = numeric(),
                      stringsAsFactors = FALSE)




#Guardar o tempo inicial
start_time <- Sys.time()
end_time <- start_time + as.difftime(30, units = "mins")  # Limite de 24 horas

# Loop de leitura por 24 horas
while (Sys.time() < end_time) {
  # Ler e parsear os dados da conexão serial
  raw_data <- read.serialConnection(tty)  # Ajuste `size` conforme necessário
  data_segments <- strsplit(raw_data, "Encoded data: ")[[1]]
  data_segments <- data_segments[data_segments != ""]  # Remover entradas vazias

  # Extrair e armazenar cada leitura no data frame
  parsed_data <- lapply(data_segments, parse_measurements)
  new_data <- as.data.frame(do.call(rbind, parsed_data))  # Converter para data.frame
  data_df <- rbind(data_df, new_data)  # Adicionar novas leituras ao data frame principal

  # Pausar o loop para evitar leituras excessivas (por exemplo, 10 segundos entre leituras)
 Sys.sleep(60)
}


data_dioxC<- data.frame (na.omit(unlist (data_df[,5])))
colnames (data_dioxC) <- c( "co2_ppm")
time_series <- seq.POSIXt(from = start_time,
                          by = "1 secs", length.out = nrow(data_dioxC))


data_dioxC$timeH <- time_series


boxplot(data_dioxC)
par(bty ="l", bg = "grey99", las =1,
    family="serif")


plot (data_dioxC$co2_ppm~data_dioxC$timeH,
      type="p", xlab= "hour",
      ylab = expression(CO[2]),
      pch="*"
      )
dev.off()

isOpen(tty)
#close(tty)



