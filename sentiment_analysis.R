# Sentiment analysis
# install packages
options(java.parameters = "- Xmx1024m")

install.packages("schrute")
library(schrute)
install.packages('dplyr')
library(dplyr)
install.packages('tidytext')
library(tidytext)
install.packages('ggplot2')
library(ggplot2)

install.packages('ggplot2')
library(ggplot2)
install.packages('tibble')
library(tibble)
gc()
# Load in data
data <- schrute::theoffice
head(data)

#https://medium.com/@randerson112358/simple-short-and-easy-sentiment-analysis-34834082e6fa
# Sentiment practice
install.packages('RSentiment')
library(RSentiment)



total_sent <- calculate_total_presence_sentiment(data$text[data$season == '01'])
test <- calculate_score(c("this is good", "this is bad"))


# creed test
total_creed <- calculate_total_presence_sentiment(data$text[data$character == 'Creed'])

# All lines
count(data, character == 'Creed')[2,2]

# Sarcastic lines
as.numeric(total_creed[2,1])

# Percentage of sarcastic lines
creed_sarc <- round(as.numeric(total_creed[2,1])/count(data, character == 'Creed')[2,2]*100)

# Michael sarcastic
total_michael <- calculate_total_presence_sentiment(data$text[data$character == 'Michael'])

# All lines
count(data, character == 'Michael')[2,2]

# Sarcastic lines
as.numeric(total_michael[2,1])

# Percentage of sarcastic lines
michael_sarc <- round(as.numeric(total_michael[2,1])/count(data, character == 'Michael')[2,2]*100)
michael_sarc


# Jim sarcastic
total_jim <- calculate_total_presence_sentiment(data$text[data$character == 'Jim'])
jim_sarc <- round(as.numeric(total_michael[2,1])/count(data, character == 'Jim') [2,2]*100)


# Pam sarcastic
total_pam <- calculate_total_presence_sentiment(data$text[data$character == 'Pam'])
pam_sarc <- round(as.numeric(total_michael[2,1])/count(data, character == 'Pam')[2,2]*100)

# Kelly
total_kelly <- calculate_total_presence_sentiment(data$text[data$character == 'Kelly'])
kelly_sarc <- round(as.numeric(total_kelly[2,1])/count(data, character == 'Kelly')[2,2]*100)

# Dwight
total_dwight <- calculate_total_presence_sentiment(data$text[data$character == 'Dwight'])
dwight_sarc <- round(as.numeric(total_dwight[2,1])/count(data, character == 'Dwight')[2,2]*100)

# Oscar
total_oscar <- calculate_total_presence_sentiment(data$text[data$character == 'Oscar'])
oscar_sarc <- round(as.numeric(total_oscar[2,1])/count(data, character == 'Oscar')[2,2]*100)

# Ryan
total_ryan <- calculate_total_presence_sentiment(data$text[data$character == 'Ryan'])
ryan_sarc <- round(as.numeric(total_ryan[2,1])/count(data, character == 'Ryan')[2,2]*100)

####################################################
# Darryl
total_darr <- calculate_total_presence_sentiment(data$text[data$character == 'Darryl'])
darr_sarc <- round(as.numeric(total_darr[2,1])/count(data, character == 'Darryl')[2,2]*100)

# Angela
total_ang <- calculate_total_presence_sentiment(data$text[data$character == 'Angela'])
ang_sarc <- round(as.numeric(total_ang[2,1])/count(data, character == 'Angela')[2,2]*100)

#Andy
total_andy <- calculate_total_presence_sentiment(data$text[data$character == 'Andy'])
andy_sarc <- round(as.numeric(total_andy[2,1])/count(data, character == 'Andy')[2,2]*100)


####################################################
# Make df
ch <- c('Creed', 'Michael', 'Jim', 'Pam', 'Kelly', 'Dwight','Oscar', 'Ryan', 'Darryl', 'Angela', 'Andy')
p <- c(creed_sarc, michael_sarc, jim_sarc, pam_sarc, kelly_sarc, 
       dwight_sarc, oscar_sarc, ryan_sarc, darr_sarc, ang_sarc, andy_sarc)

testdf <- data.frame(ch)
testdf$p <- p
testdf

# Visualize data
#plot(testdf$ch, testdf$p)

#counts <-  table(testdf$p)
#barplot(testdf$p)


testdf <- as_tibble(testdf)
testdf$p <- as.numeric(testdf$p)


ggplot(testdf, aes(..count.., y = ch)) +
     geom_bar()

testdf$p <- as.numeric(testdf$p)

install.packages('ggpubr')
library(ggpubr)
install.packages('ggplot2')
library('ggplot2')


ggplot(testdf, aes(x=reorder(ch, -p), y=p, fill = ch)) +
     geom_col(show.legend = FALSE) +
     xlab('Characters') + 
     ylab('Percentage (%)') +
     ggtitle('Sarcasm of The Office (US) by Character') +
     ylim(0, 100) +
     geom_text(aes(label = p, vjust = -0.5, , fontface = "bold.italic")) +
     theme_minimal() +
     font("title", size = 14, face = "bold.italic")




# changing formatting of labels
#https://rdrr.io/cran/ggpubr/man/font.html

#sentiment analysis on main characters
# percentage of total statements that are sarcastic

#https://pages.github.com/
#git blogging
# https://github.com/barryclark/jekyll-now