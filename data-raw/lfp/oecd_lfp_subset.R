
library(rsdmx)
library(dplyr, warn.conflicts = FALSE)
library(command)

cmd_assign(.out = "../data/oecd_lfp_subset.rda")

url <- paste("https://sdmx.oecd.org/public/rest/data",
             "OECD.ELS.SAE,DSD_LFS@DF_LFS_INDIC,1.1",
             "all?dimensionAtObservation=AllDimensions",
             sep = "/")
sdmx_data <- rsdmx::readSDMX(url)
data <- as.data.frame(sdmx_data)

oecd_lfp_subset <- data |>
  filter(MEASURE == "LF_RATE") |>
  filter(TIME_PERIOD %in% 2010:2012) |>
  tibble()

save(oecd_lfp_subset, file = .out, compress = "bzip2")
