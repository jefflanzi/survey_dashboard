library(ggplot2)
#plotting wrapper function
svy_plot <- function(x, qid, segment = "overall", freq = FALSE) {
   
   #tabulate question data
   qtable <- svy_table(x, qid, segment)
   
   #identify the question type
   qtype <- q_types[q_types$qid == qid, "qtype"]
   
   if (qtype == "free_text") {
      word_cloud(qtable)
   } else if (sum(sapply(qtable, is.numeric)) == 1) {
      simple_bar(qtable)
   } else line_plot(qtable) 
}


line_plot <- function(qtable) {
   require(reshape2)
   require(ggplot2)
   #prepare data for ggplot
   gdata <- melt(qtable, id.vars = "sq")
   
   #create plot   
   ggplot(gdata, aes(x = sq, y = value, color = variable)) +
      geom_point(shape = 19, size = 4) +
      geom_line(aes(x = as.numeric(sq), y = value)) +
      coord_flip() +
      theme_bw()
}

#single_bar
simple_bar <- function(qtable) {
   require(ggplot2)   
   ggplot(qtable, aes(x = sq, y = overall)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = round(overall, 2)), hjust = 1.5, color = "white", fontface = "bold") +
      coord_flip() +
      theme_bw() +
      theme(text = element_text(size = 16)) +
      xlab(NULL) + ylab(NULL)
}

#word cloud
word_cloud <- function(qtable) {
        library(wordcloud)
        
        #define colors
        bbred <- "#DA291C"
        bbyellow <- "#FFD100"
        
        #check to see if there are at least 25 words (temporary solution)
        #if(nrow(qtable < 25)) stop("Not enough words")
        
        #add color vector
        qtable$colors <- c(rep(bbred, 3), 
                           rep(bbyellow, 7), 
                           rep("#000000", 15), 
                           rep("#999999", (nrow(qtable)-25)))
        
        wordcloud(rownames(qtable), qtable[,1], min.freq = 1, scale = c(5, .5), max.words = 100,
          rot.per = 0, random.order = F, random.color = F, 
          colors = qtable$colors, ordered.colors = T, fixed.asp = F)
}   

