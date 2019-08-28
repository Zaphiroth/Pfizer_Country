library(readr)
guess_encoding("Background/Raw data final.csv")

raw_data_final <- read_csv("Background/Raw data final.csv", locale = locale(encoding = "GB18030")) %>%
  data.frame()
colnames(raw_data_final) <- tolower(colnames(raw_data_final))
save(raw_data_final, file = "./data/raw_data_final.RData")

raw_data_forecast <- read_csv("Background/Raw data forecast.csv", locale = locale(encoding = "GB18030")) %>%
  data.frame()
colnames(raw_data_forecast) <- tolower(colnames(raw_data_forecast))
save(raw_data_forecast, file = "./data/raw_data_forecast.RData")
