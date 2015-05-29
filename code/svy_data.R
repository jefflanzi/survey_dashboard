## IMPORT DATA FILES
library(dplyr)
source("code/svy_functions.R")

# import data from pre-prepared RDS
# This RDS is created by running the limesurvey R script
# then merging with client supplied segmentation data
data <- readRDS("data/survey_completes.RDS")

# remove NA columns e.g. for rank questions which don't rank all options
data <- select(data, which(!apply(data, 2, function(x) all(is.na(x)))))

# clean variable names
names(data) <- gsub("\\.$", "", names(data))

# import survey meta structure
meta <- read.table("data/meta_data.txt", header = T, sep = "\t",
                   quote = "", stringsAsFactors = F, na.strings = "")
meta <- select(meta, which(!apply(meta, 2, function(x) all(is.na(x)))))

# classify qtypes - required for svy_table subfunction selection
q_types <- read.csv("data/qtypes.csv", stringsAsFactors = F)

## PREPARE DATA

#get survey name
survey_title <- filter(meta, name == "surveyls_title")[["text"]]

# add overall column for overall segmentation
data$overall <- rep("overall", nrow(data))
# this needs to be deprecated and replaced with a better handling method

# remove html from meta text
# long term should use the html to format text in dashboard
meta$text <- gsub("<.*?>", "", meta$text)

# create data helper objects
qoverview <- filter(meta, class == "Q")
qstr <- q_str(meta)
qstr <- lapply(qstr, qstr_sq_labels)


# define segmentation options
segments <- c("none", q_types[q_types$segment == 1, "shortname"])

#define colors
bbred <- "#DA291C"
bbyellow <- "#FFD100"

# positive likert value strings as regex for grep
likert_pos <- paste0(c("Extremely", "Very", "More", "Most", "Agree", "5", "6", "7"), collapse = "|")
