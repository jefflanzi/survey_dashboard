#table wrapper function
svy_table <- function(x, qid, segment = "overall", series = NULL, freq = F) {
   
   require(reshape2)
   
   if (segment == "none") segment <- "overall"
   if (segment == "overall") {
      stype <- "overall"
   } else stype <- q_types[q_types$qid == segment, "qtype"]
   
   #melt question data   
   if (stype == "multiple_choice") {
      mdata <- mc_segment(data, qid, segment)
   } else mdata <- qmelt(data, qid, segment)
   
   #determine qtype
   qtype <- q_types[q_types$qid == qid, "qtype"]
   
   #label attributes if appropriate
   q <- qstr[[which(names(qstr) == qid)]]
   
   if(qtype == "ranking") {
      mdata$value <- as.factor(mdata$value)
      sqmatch <- match(levels(mdata$value), q[q$class == "A", "name"])
      levels(mdata$value) <- as.character(q[q$class == "A", "text"][sqmatch])
   } else if (!(qtype %in% c("single_choice", "free_text"))) {      
      sqmatch <- match(levels(mdata$sq), q[q$class == "SQ", "name"])
      levels(mdata$sq) <- as.character(q[q$class == "SQ", "text"][sqmatch])
   }
   
   #get tabulated data
   qtable <- switch(qtype,
                    single_choice = single_choice(mdata, qid, segment, series),
                    ranking = ranking(mdata, qid, segment, series),
                    likert_avg = likert_avg(mdata, qid, segment, series),
                    likert_sum = likert_sum(mdata, qid, segment, series),
                    multiple_choice = multiple_choice(mdata, qid, segment, series, freq),
                    nps = nps(mdata, qid, segment, series),
                    free_text = free_text(mdata, qid, segment, series),
                    stop("qtype not supported")
   )
   
   #sort table by first numerical column
   if(qtype != "free_text") { 
      
      #reorder attributes and convert attributes to ordered factor   
      qtable[,1] <- reorder(qtable[,1], qtable[,2], order = T)
      
      #sort table
      qtable <- qtable[order(-qtable[,2]), ]
   }
   
   return(qtable)
}



#tabulate data from single choice questions
single_choice <- function(mdata, qid, segment = "overall", series = NULL) {
   
   qtable <- dcast(mdata, value ~ segment, length)
   names(qtable) <- gsub("value", "sq", names(qtable))
   
   #reformat frequency to percentages
   numcols <- which(sapply(qtable, is.numeric))
   if(length(numcols) > 1) {
      qtable[, numcols] <- sapply(qtable[, numcols], function(x) x/sum(x))
   } else qtable[, numcols]  <- qtable[, numcols]/sum(qtable[, numcols])
   
   return(qtable)
}

#tabulate data from ranking questions
ranking <- function(mdata, qid, segment = "overall", series = NULL) {
   
   qtable <- dcast(mdata, value ~ segment, length)
   names(qtable) <- gsub("value", "sq", names(qtable))
   
   #reformat frequency to percentages
   numcols <- which(sapply(qtable, is.numeric))
   if(length(numcols) > 1) {
      qtable[, numcols] <- sapply(qtable[, numcols], function(x) x/sum(x)*5)
   } else qtable[, numcols]  <- qtable[, numcols]/sum(qtable[, numcols])*5
   
   return(qtable)
}

# likert using mean of scale values
likert_avg <- function(mdata, qid, segment = "overall", series = NULL) {

        # tabulate data
        qtable <- dcast(mdata, sq ~ segment, mean)        
        return(qtable)        
}

# likert using sum of predefined positive values
likert_sum <- function(mdata, qid, segment = "overall", series = NULL) {

        #aggregation function for dcast
        l_sum <- function(x) {
                length(grep(paste(likert_pos, collapse="|"), x))/length(x)
        }
        
        #tabulate data
        qtable <- dcast(mdata, sq ~ segment, l_sum)
        
        return(qtable)
}

# tabulate data from multiple choice questions
multiple_choice <- function(mdata, qid, segment = "overall", series = NULL, freq = F) {
        
        #tabulate data
        if(freq == T) {
                qtable <- dcast(mdata, sq ~ segment, function(x) sum(x == 1))
                return(qtable)
        }
        
        qtable <- dcast(mdata, sq ~ segment, function(x) sum(x)/length(x))   
        
        return(qtable)
}

free_text <- function(mdata, qid = NULL, segment = "overall", series = NULL) {
        require(tm)
        
        #transform the text
        words <- tolower(mdata$value)
        words <- removePunctuation(words)
        words <- removeWords(words, stopwords("english"))
        words <- stripWhitespace(words)
        
        #create a corpus
        c <- Corpus(VectorSource(words))
        
        #matrix of individual words
        tdm <- TermDocumentMatrix(c)
        m <- as.matrix(tdm)
        
        #word counts
        data.frame(sort(rowSums(m), decreasing = T))        
}
        

#calculate NPS score
nps <- function(mdata, qid, segment = "overall", series = NULL) {
   
   #    nps_score <- function(x) {
   #       x <- as.numeric(x)
   #       (sum(x >= 9)/length(x)) - (sum(x <= 6)/length(x))
   #    }
   
   dcast(mdata, value ~ segment, length)
}

