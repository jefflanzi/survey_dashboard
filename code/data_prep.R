#this file is designed to be run once to help prepare the data for import by the dashboard

sourceFile <- "survey_461795_R_syntax_file.R"
source(paste0("data/prep/", sourceFile), chdir = T)
saveRDS(data, "data/survey_completes.RDS")

#create template for qtypes.csv
library(dplyr)
meta <- read.table("data/meta_data.txt", header = T, sep = "\t", 
                   quote = "", stringsAsFactors = F, na.strings = "")
meta <- select(meta, which(!apply(meta, 2, function(x) all(is.na(x)))))

qtypes <- filter(meta, class == "Q") %>%
  mutate(shortname = name) %>%
  select(name, shortname, type.scale, text)

write.csv(qtypes, "data/qtypes.csv")
