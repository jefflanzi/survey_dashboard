# load packages
library(ggplot2)
library(wordcloud)

# plotting wrapper function
svy_plot <- function(data, qid, segment = "value", freq = FALSE) {
        
        #tabulate question data
        qtable <- svy_table(data, qid, segment, spread = FALSE)
        
        #identify the question type
        qtype <- q_types[q_types$qid == qid, "qtype"]
        
        if (qtype == "word_cloud") {
                word_cloud(qtable)
        } else if (qtype == "free_text") {
                text_plot()
        } else if (length(unique(qtable$segment)) == 1) {
                simple_bar(qtable)
        } else line_plot(qtable) 
}

# simple bar plot
simple_bar <- function(qtable) {
        require(ggplot2)   
        ggplot(qtable, aes(x = sq, y = value)) +
                geom_bar(stat = "identity") +
                geom_text(aes(label = round(value, 2)), hjust = 1.5, color = "white", fontface = "bold") +
                coord_flip() +
                theme_bw() +
                theme(text = element_text(size = 16)) +
                xlab(NULL) + ylab(NULL)
}

line_plot <- function(qtable) {
        #create plot   
        ggplot(qtable, aes(x = sq, y = value, color = segment)) +
                geom_point(shape = 19, size = 4) +
                geom_line(aes(x = as.numeric(sq), y = value)) +
                coord_flip() +
                theme_bw()
}

#word cloud
word_cloud <- function(qtable) {
        
        #define colors
        bbred <- "#DA291C"
        bbyellow <- "#FFD100"
        
        #add color vector
        qtable$colors <- c(rep(bbred, 3), 
                           rep(bbyellow, 7), 
                           rep("#000000", 15), 
                           rep("#999999", (nrow(qtable)-25)))
        
        wordcloud(rownames(qtable), qtable[,1], min.freq = 1, scale = c(5, .5), max.words = 100,
                  rot.per = 0, random.order = F, random.color = F, 
                  colors = qtable$colors, ordered.colors = T, fixed.asp = F)
}

text_plot <- function() {
        stop("No graphical output available for uncoded text, please use table view")        
}
        
        