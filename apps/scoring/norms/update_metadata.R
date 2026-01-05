library(googlesheets4)
library(readr)

ss <- "13eZuteIwVslQBnHRkh-9VeyhzbrHEGPOH2VoEYTdgGA"

norms_index <- read_sheet(ss, "norms")
write_csv(norms_index, "metadata/norms_index.csv")

label <- read_sheet(ss, "labels")
write_csv(label, "metadata/labels.csv")

measures <- read_sheet(ss, "measures")
write_csv(measures, "metadata/measures.csv")

instruments <- read_sheet(ss, "instruments")
write_csv(instruments, "metadata/instruments.csv")
