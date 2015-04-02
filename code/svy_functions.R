# identify all subquestion column indexes in raw data that make up a single question
q_cols <- function(x = data, qids) {  
        qcols <- grep(paste0("^", qids, "($|[._])", collapse = "|"), names(data))
        other <- qcols[grep("other", qcols)]
        if (length(other) > 0) {qcols <- qcols[-other]}
        return(qcols)
}

# generate list of question structures from survey structure
q_str <- function(x = meta) {    
        #remove groups
        x <- x[!x$class == "G", ]   
        
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
        require(dplyr)
        
        qdata <- select(data, q_cols(data, c("id", qid)))
        sdata <- select(data, q_cols(data, c("id", segment)))

        #melt component data sets
        qmelt <- melt(qdata, id.vars = "id", na.rm = T, variable.name = "sq")
        smelt <- melt(sdata, id.vars = "id", na.rm = T, variable.name = "segment")

        #keep only selected segment values        
        smelt <- filter(smelt, value == 1) %>% select(-value)

        #merge
        inner_join(qmelt, smelt, by = "id") %>% select(-id)        
}

#label attributes to match column names
sq_labels <- function(sqids) {      
        if(!is.factor(sqids)) {sqids <- as.factor(sqids)}        
        sqmatch <- match(levels(sqids), meta[meta$class == "SQ", "name"])
        levels(sqids) <- as.character(meta[meta$class == "SQ", "text"][sqmatch])
        return(sqids)
}

# relabel sqs in qstr to match data column labels
qstr_sq_labels <- function(x) {
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

#paste to clipboard (windows only)
paste_table <- function(x) {write.table(x, file = "clipboard", sep = "\t")}
