#import a limesurvey survey strcuture in tabular form (.txt.)
read_meta <- function(metafile) {
   read.table(metafile, header = T, sep = "\t", quote = "", stringsAsFactors = F)
}
   
#identify all subquestion column indexes in raw data that make up a single question
q_cols <- function(x = data, qid) {
        qcols <- grep(paste0("^", qid, "$|", "^", qid, "[._]"), names(x))
        other <- qcols[grep("other", names(x)[qcols])]
        if (length(other) > 0) {qcols <- qcols[!qcols==other]}
        qcols[!apply(x[qcols], 2, function(x) all(is.na(x)))]  
}


#generate list of question structures from survey structure
q_str <- function(x = svy.str) {    
        #remove groups
        x <- x[!x$class == "G", ]
        
        #replace html text
#         text <- x$text
#         text <- gsub("<br />", "", text)
#         text <- gsub("<br />", " ", text)
#         text <- gsub("<i>|<i/>", "", text)
        
        #identify primary question rows
        rows <- which(x$class == "Q")
        
        #parse questions into list of data frames
        rowlist <- list(NULL)
        for(i in 1:length(rows)) {
                if(i == length(rows)) {
                        rowlist[[i]] <- rows[i]:nrow(x)
                }
                else {rowlist[[i]] <- rows[i]:(rows[i+1]-1)}                
        }
        names(rowlist) <- as.character(x[rows, "name"])
        lapply(rowlist, function(y){x[y, ]})
}

#reshape question data to long form
qmelt <- function(data, qid, segment = "overall") {
        require(reshape2)
        qcols <- q_cols(data, qid)
        scols <- q_cols(data, segment)
        mdata <- melt(data, id.vars = scols, measure.vars = qcols, na.rm = T, variable.name = "sq")
        names(mdata) <- gsub(segment, "segment", names(mdata))
        return(mdata)
}

#qmelt with multiple choice segmentation
mc_segment <- function(data, qid, segment = "overall") {      
        #get column indexes
        require(reshape2)
        qcols <- q_cols(data, qid)
        scols <- q_cols(data, segment)
        idcol <- q_cols(data, "id")

        #melt component data sets
        qmelt <- melt(data[, c(idcol, qcols)], id.vars = idcol, na.rm = T, variable.name = "sq")
        smelt <- melt(data[, c(idcol, scols)], id.vars = idcol, na.rm = T, variable.name = "segment")

        #keep only selected segment values
        smelt <- smelt[smelt$value == 1, 1:2]

        #label segmentation factor
        s <- qstr[[which(names(qstr) == segment)]]
        sqmatch <- match(levels(smelt$segment), s[s$class == "SQ", "name"])
        levels(smelt$segment) <- as.character(s[s$class == "SQ", "text"][sqmatch])

        #merge and dcast
        merge(smelt, qmelt, by = "id")[, -1]
}

#label attributes to match column names
sq_labels <- function(x) {
        qid <- x[x$class == "Q", "name"]
        sq <- x[x$class == "SQ", "name"]
        sqids <- paste0(qid, ".", sq)
        x[x$class == "SQ", "name"] <- sqids
        
        return(x)
}

#remove html tags
remove_html <- function(x) {
        gsub("<.*?>", "", x)
}

#write a list of dataframes to an excel workbook
write_workbook <- function(x, destfile) {
        require(xlsx)
        wb <- createWorkbook()
        for(i in 1:length(x)) {
                sheet <- createSheet(wb, sheetName = names(x)[i])
                addDataFrame(as.data.frame(x[[i]]), sheet)
        }
        
        saveWorkbook(wb, destfile)
}