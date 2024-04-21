
library(rsdmx)
library(dplyr)

url <- paste("https://sdmx.oecd.org/public/rest/data",
             "OECD.ELS.SAE,DSD_LFS@DF_LFS_INDIC,1.1",
             "all?dimensionAtObservation=AllDimensions",
             sep = "/")
sdmx_data <- rsdmx::readSDMX(url)
data <- as.data.frame(sdmx_data)

oecd_lfp <- data %>%
  filter(MEASURE == "LF_RATE") %>%
  filter(TIME_PERIOD %in% 2010:2012) %>%
  tibble()

save(oecd_lfp, file = "data/oecd_lfp.rda", compress = "bzip2")
