library(tm)
library(tidyverse)
#cite this: http://www.sthda.com/english/wiki/text-mining-and-word-cloud-fundamentals-in-r-5-simple-steps-you-should-know

####
#Word Frequencies on the description, name, title 



TedRaw <- read.csv(file="/cloud/project/FinalProject/ted_main.csv", header=TRUE, sep=",")


############################################
#Work on finding frequency of the description column
############################################
description_corpus <- Corpus(VectorSource(TedRaw$description))

# Convert the text to lower case
description_corpus <- tm_map(description_corpus, content_transformer(tolower))
# Remove numbers
description_corpus <- tm_map(description_corpus, removeNumbers)
# Remove english common stopwords
description_corpus <- tm_map(description_corpus, removeWords, stopwords("english"))
# Remove punctuation
description_corpus <- tm_map(description_corpus, removePunctuation)

dtm_description <- TermDocumentMatrix(description_corpus)
m_description <- as.matrix(dtm_description)
v_description <- sort(rowSums(m_description),decreasing=TRUE)
d_description <- data.frame(word = names(v_description),freq=v_description)
head(d_description, 50)

############################################
#Work on finding frequency of the name column
############################################
name_corpus <- Corpus(VectorSource(TedRaw$name))

# Convert the text to lower case
name_corpus <- tm_map(name_corpus, content_transformer(tolower))
# Remove numbers
name_corpus <- tm_map(name_corpus, removeNumbers)
# Remove english common stopwords
name_corpus <- tm_map(name_corpus, removeWords, stopwords("english"))
# Remove punctuation
name_corpus <- tm_map(name_corpus, removePunctuation)

dtm_name <- TermDocumentMatrix(name_corpus)
m_name <- as.matrix(dtm_name)
v_name <- sort(rowSums(m_name),decreasing=TRUE)
d_name <- data.frame(word = names(v_name),freq=v_name)
head(d_name, 50)



############################################
#Work on finding frequency of the title column
############################################
title_corpus <- Corpus(VectorSource(TedRaw$title))

# Convert the text to lower case
title_corpus <- tm_map(title_corpus, content_transformer(tolower))
# Remove numbers
title_corpus <- tm_map(title_corpus, removeNumbers)
# Remove english common stopwords
title_corpus <- tm_map(title_corpus, removeWords, stopwords("english"))
# Remove punctuation
title_corpus <- tm_map(title_corpus, removePunctuation)

dtm_title <- TermDocumentMatrix(title_corpus)
m_title <- as.matrix(dtm_title)
v_title <- sort(rowSums(m_title),decreasing=TRUE)
d_title <- data.frame(word = names(v_title),freq=v_title)
head(d_title, 50)