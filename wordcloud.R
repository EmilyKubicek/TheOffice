# install packages
install.packages("schrute")
library(schrute)
install.packages('dplyr')
library(dplyr)
install.packages('tidytext')
library(tidytext)
install.packages('ggplot2')
library(ggplot2)

# Word cloud packages
install.packages("wordcloud")
install.packages("RColorBrewer")
library(wordcloud)
library(RColorBrewer)


# Read in data
data <- schrute::theoffice

# Explore data
head(data)

data[,0:5]

glimpse(data)

filter(data, season == '01' & character == 'Michael')

# Tokenize data
# Notice how the tibble grew
t_data <- data %>%
     unnest_tokens(word, text)


# Remove stop words
tt_data <- anti_join(t_data, stop_words, by = "word")


# Word count plot
###############################################################
# Check most common words for one character
count(tt_data[tt_data$character == 'Michael',], word, sort = TRUE)

tt_michael <- tt_data[tt_data$character == 'Michael',]

tt_michael %>%
     count(word, sort = TRUE) %>%
     filter(n > 200) %>%
     mutate (word = reorder(word, n)) %>%
     ggplot(aes(word, n)) +
     geom_col() +
     xlab(NULL) +
     coord_flip() +
     theme_minimal()

# Get rid of 'yeah' and 'hey'
tt_data2 <- tt_data[tt_data$word != "yeah" & tt_data$word != "uh" &
                    tt_data$word != "um" & tt_data$word != "hey" &
                            tt_data$word != "guys" & tt_data$word != "gonna",]

# Try individual character again with the above removed
tt_michael2 <- tt_data2[tt_data2$character == "Michael",]

tt_michael2 %>%
     count(word, sort = TRUE) %>%
     filter(n > 200) %>%
     mutate (word = reorder(word, n)) %>%
     ggplot(aes(word, n)) +
     geom_col() +
     xlab(NULL) +
     coord_flip() +
     theme_minimal()


tt_kelly <- tt_data2[tt_data2$character == "Kelly",]
tt_kelly %>%
        count(word, sort = TRUE) %>%
        filter(n > 16) %>%
        mutate (word = reorder(word, n)) %>%
        ggplot(aes(word, n)) +
        ggtitle("Kelly Kapoor") +
        geom_col() +
        xlab("Word") +
        ylab("Count") +
        coord_flip() +
        theme_minimal()

# Make word cloud
###############################################################
# make word cloud for whole show
# make count df
tt_data_wordfreq <- count(tt_data2, word, sort = TRUE)


wordcloud(words = tt_data_wordfreq$word, freq = tt_data_wordfreq$n,
          min.freq = 1,  max.words=100, scale=c(3.5,.01), random.order=FALSE, rot.per=0.25, 
          colors=brewer.pal(8, "Dark2"))

# Michael
unique(tt_data2$character)
table(tt_data2[tt_data2$character > 20,])

tt_michael2 <- tt_data2[tt_data2$character == "Michael",]
michael <- count(tt_michael2, word, sort = TRUE)
michael

michael_wc <- wordcloud(words = michael$word, freq = michael$n,
                     min.freq = 1, max.words = 50, scale = c(2.8, .5), random.order = FALSE,
                     rot.per = 0, fixed.asp = FALSE,
                     colors = brewer.pal(8, "Dark2"))

# Kelly
tt_kelly <- tt_data2[tt_data2$character == "Kelly",]
kelly <- count(tt_kelly, word, sort = TRUE)


kelly_wc <- wordcloud(words = kelly$word, freq = kelly$n,
                        min.freq = 1, max.words = 50, scale = c(2.8, .5), random.order = FALSE,
                        rot.per = 0, fixed.asp = FALSE,
                        colors = brewer.pal(8, "Dark2"))
kelly_wc

# Jim
tt_jim <- tt_data2[tt_data2$character == "Jim",]
jim <- count(tt_jim, word, sort = TRUE)


jim_wc <- wordcloud(words = jim$word, freq = jim$n,
                      min.freq = 1, max.words = 50, scale = c(2.8, .5), random.order = FALSE,
                      rot.per = 0, fixed.asp = FALSE,
                      colors = brewer.pal(8, "Dark2"))
jim_wc

# Pam
tt_pam <- tt_data2[tt_data2$character == "Pam",]
pam <- count(tt_pam, word, sort = TRUE)


pam_wc <- wordcloud(words = pam$word, freq = pam$n,
                    min.freq = 1, max.words = 50, scale = c(2.8, .5), random.order = FALSE,
                    rot.per = 0, fixed.asp = FALSE,
                    colors = brewer.pal(8, "Dark2"))


# Dwight
tt_dwight <- tt_data2[tt_data2$character == "Dwight",]
dwight <- count(tt_dwight, word, sort = TRUE)


dwight_wc <- wordcloud(words = dwight$word, freq = dwight$n,
                    min.freq = 1, max.words = 50, scale = c(2.8, .5), random.order = FALSE,
                    rot.per = 0, fixed.asp = FALSE,
                    colors = brewer.pal(8, "Dark2"))
# Andy
tt_andy <- tt_data2[tt_data2$character == "Andy",]
andy <- count(tt_andy, word, sort = TRUE)


andy_wc <- wordcloud(words = andy$word, freq = andy$n,
                    min.freq = 1, max.words = 50, scale = c(2.9, .3), random.order = FALSE,
                    rot.per = 0, fixed.asp = FALSE,
                    colors = brewer.pal(8, "Dark2"))
# Angela
tt_ang <- tt_data2[tt_data2$character == "Angela",]
ang <- count(tt_ang, word, sort = TRUE)


ang_wc <- wordcloud(words = ang$word, freq = ang$n,
                    min.freq = 1, max.words = 50, scale = c(2.8, .5), random.order = FALSE,
                    rot.per = 0, fixed.asp = FALSE,
                    colors = brewer.pal(8, "Dark2"))


# Oscar
tt_oscar <- tt_data2[tt_data2$character == "Oscar",]
oscar <- count(tt_oscar, word, sort = TRUE)


oscar_wc <- wordcloud(words = oscar$word, freq = oscar$n,
                    min.freq = 1, max.words = 50, scale = c(2.8, .5), random.order = FALSE,
                    rot.per = 0, fixed.asp = FALSE,
                    colors = brewer.pal(8, "Dark2"))

# Darryl
tt_darr <- tt_data2[tt_data2$character == "Darryl",]
darr <- count(tt_darr, word, sort = TRUE)


darr_wc <- wordcloud(words = darr$word, freq = darr$n,
                    min.freq = 1, max.words = 50, scale = c(2.8, .5), random.order = FALSE,
                    rot.per = 0, fixed.asp = FALSE,
                    colors = brewer.pal(8, "Dark2"))

# Ryan
tt_ryan <- tt_data2[tt_data2$character == "Ryan",]
ryan <- count(tt_ryan, word, sort = TRUE)


ryan_wc <- wordcloud(words = ryan$word, freq = ryan$n,
                    min.freq = 1, max.words = 50, scale = c(2.8, .5), random.order = FALSE,
                    rot.per = 0, fixed.asp = FALSE,
                    colors = brewer.pal(8, "Dark2"))

# Toby
tt_toby <- tt_data2[tt_data2$character == "Toby",]
toby <- count(tt_toby, word, sort = TRUE)


toby_wc <- wordcloud(words = toby$word, freq = toby$n,
                    min.freq = 1, max.words = 50, scale = c(2.8, .5), random.order = FALSE,
                    rot.per = 0, fixed.asp = FALSE,
                    colors = brewer.pal(8, "Dark2"))

# Creed
tt_creed <- tt_data2[tt_data2$character == "Creed",]
creed <- count(tt_creed, word, sort = TRUE)


creed_wc <- wordcloud(words = creed$word, freq = creed$n,
                    min.freq = 1, max.words = 50, scale = c(2.8, .5), random.order = FALSE,
                    rot.per = 0, fixed.asp = FALSE,
                    colors = brewer.pal(8, "Dark2"))
jim_wc


# How many lines does everyone have
###############################################################
# Character lines by originial data (before tokenization)
View(count(data, character, sort = TRUE)[1:30,])



# That's what she said plot
###############################################################
# by character
twss <- data[grepl("that's what she said", data$text, ignore.case = TRUE),]
twss %>%
        count(character, sort = TRUE) %>%
        mutate (character = reorder(character, n)) %>%
        ggplot(aes(character, n)) +
        ggtitle("That's what she said") +
        geom_col() +
        xlab(NULL) +
        coord_flip() +
        theme_minimal()


# word count for each character (cloud?) - with picture
# That's what she said graph

# How many times Jim says pams name
# How many times Pam says Jims name
# word cloud for each character
# counts of that's what she said by character
