## IMPORT DATA FILES
library(data.table)
library(dplyr)
# import survey data
data <- readRDS("data/ycod client completes with sales.RDS")
data <- select(data, which(!apply(data, 2, function(x) all(is.na(x)))))

# import survey meta structure
meta <- read.table("data/survey_meta_data.txt", header = T, sep = "\t", 
                   quote = "", stringsAsFactors = F, na.strings = "")
meta <- select(meta, which(!apply(meta, 2, function(x) all(is.na(x)))))

# classify qtypes - required for svy_table subfunction selection
q_types <- read.csv("data/qtypes.csv", stringsAsFactors = F)

# positive likert value strings for grep
likert_pos <- c("7", "6", "5", "p1", "p2")

## PREPARE DATA

#get survey name
survey_title <- filter(meta, name == "surveyls_title")[["text"]]

# clean variable names
setnames(data, gsub("\\.$", "", names(data)))

# add overall column for overall segmentation
data$overall <- rep("overall", nrow(data))

# remove html from meta text
# long term should use the html to format text in dashboard
meta$text <- gsub("<.*?>", "", meta$text)

# create data helper objects
qoverview <- filter(meta, class == "Q")
qstr <- q_str(meta)
qstr <- lapply(qstr, qstr_sq_labels)


# define segmentation options
segments <- c("none", q_types[q_types$segment == 1, "shortname"])

# convert single choice answers to factors
scqids <- qoverview[qoverview$type.scale == "L", "name"]
for (i in seq_along(scqids)) {
        qid <- scqids[i]
        qcol <- q_cols(data, qid)
        #   set(data, j = qcols, as.factor(qcol))
        data[[qcol]]  <- as.factor(data[[qcol]])
        q <- qstr[[which(names(qstr) == qid)]]   
        matches <- match(levels(data[[qcol]]), q[q$class == "A", "name"])
        levels(data[[qcol]]) <- q[q$class == "A", "text"][matches]  
}
rm(scqids, qid, qcol, q, matches)

#define colors
bbred <- "#DA291C"
bbyellow <- "#FFD100"
