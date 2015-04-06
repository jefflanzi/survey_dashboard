##load required packages
library(magrittr)
library(dplyr)
library(tidyr)

# wrapper function for selecting table type depending on q_types as defined in qtypes.csv
svy_table <- function(data, qid, segment = "overall", freq = FALSE) {

        #gather question and segment data in long form
        qdata <- qmelt(data, qid, segment)
                
        #tabulate data
        qtype <- q_types[q_types$qid == qid, "qtype"]
        qtable <- switch(qtype,
                         free_text = free_text(qdata),
                         single_choice = single_choice(qdata, qid, segment, freq),
                         #multiple_choice = single_choice(...),
                         likert_sum = likert_sum(qdata, qid, segment, freq),
                         likert_avg = likert_avg(qdata, qid, segment, freq),
                         #nps = single_choice(...),
                         stop("invalid question type")
        )
        
        return(qtable)
}
       
# raw frequencies/percentages table
single_choice <- function(qdata, qid, segment = "overall", freq = FALSE) {
        
        # summarise data from long form        
        qtable <- qdata %>%         
                group_by(segment, answer) %>%
                summarise(value = length(answer))
        
        # calculate percentages if pct == TRUE
        if (freq == FALSE) {
                qtable %<>%
                        group_by(segment) %>%
                        mutate(value = round(value/sum(value), 3))
        }
        
        spread(qtable, segment, value, drop = F)
        
}

# average rating
likert_avg <- function(qdata, qid, segment = "overall", freq = FALSE) {
        
        qtable <- qdata %>%
                group_by(segment, sq) %>%
                summarise(value = round(mean(as.numeric(answer)), 2)) %>%
                spread(segment, value, drop = F)
        
}

# percentage of agreement
likert_sum <- function(qdata, qid, segment = "overall", freq = FALSE) {
        
        qtable <- qdata %>% 
                group_by(segment, sq) %>%
                summarise(value = round(sum(grepl(likert_pos, answer))/length(sq), 3)) %>%
                spread(segment, value, drop = F)
        
}

# generate word counts from raw text
free_text <- function(qdata) {
        library(tm)
        
        #transform the text
        words <- tolower(qdata$answer)
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