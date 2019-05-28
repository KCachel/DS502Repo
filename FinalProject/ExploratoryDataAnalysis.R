library(ggplot2) # Data visualisation
library(reshape2)
library(corrplot)
library(dplyr) 
library(ggthemes)
#library(stringr) # String manipulation
#library(anytime)
#library(data.table)


TedRaw <- read.csv(file="ted_main.csv", header=TRUE, sep=",")

###############################################
#EDA for nums: comments, duration, languages
###############################################

#Number of Comments
#Histogram Graph
ggplot(data=TedRaw, aes(x=comments)) +
  geom_histogram(binwidth=50, fill = '#99d8c9')+
  geom_vline(aes(xintercept = mean(comments)),col='red',
             size=.5, linetype="dotted")+
  geom_vline(aes(xintercept = median(comments)),col='black',
             size=.5, linetype="dotted")+
  labs(x = "Number of Comments", y = "Count of TED Talks",
       title = "Histogram of Comments")+
  scale_x_continuous(breaks = pretty(TedRaw$comments, n = 22))

#Non-hist version and data manipulation
comments <- TedRaw %>%
  group_by(comments) %>%
  tally(name ="numTalks")

#Number of Comments bar graph
ggplot(data=comments, aes(x=comments, y=numTalks)) +
  #geom_point(stat = "identity", aes(color = '#fa9fb5'))+
  geom_bar(stat = "identity", fill = '#fa9fb5')+
  geom_vline(aes(xintercept = mean(TedRaw$comments)),col='red',
             size=.5, linetype="dotted")+
  geom_vline(aes(xintercept = median(TedRaw$comments)),col='black',
             size=.5, linetype="dotted")+
  labs(x = "Number of comments", y = "Count of TED Talks",
       title = "Talks by number of comments")+
  scale_x_continuous(breaks = pretty(TedRaw$comments, n = 20))

#Duration of Talk
duration <- TedRaw %>%
  group_by(duration) %>%
  tally(name ="numTalks") 

#Duration bar graph
ggplot(data=duration, aes(x=duration, y=numTalks)) +
  geom_point(stat = "identity", aes(color = '#fa9fb5'))+
  #geom_bar(stat = "identity", fill = '#fa9fb5')+
  geom_vline(aes(xintercept = mean(TedRaw$duration)),col='red',
             size=.5, linetype="dotted")+
  geom_vline(aes(xintercept = median(TedRaw$duration)),col='black',
             size=.5, linetype="dotted")+
  labs(x = "Duration (s)", y = "Count of TED Talks",
       title = "Talks by length")+
  scale_x_continuous(breaks = pretty(TedRaw$duration, n = 20))


#histogram
ggplot(data=TedRaw, aes(x=duration)) +
  geom_histogram(binwidth=50, fill = '#fa9fb5')+
  geom_vline(aes(xintercept = mean(duration)),col='red',
             size=.5, linetype="dotted")+
  geom_vline(aes(xintercept = median(duration)),col='black',
             size=.5, linetype="dotted")+
  labs(x = "Length of Talk (s)", y = "Count of TED Talks",
       title = "Histogram of Talk Length (s)")+
  scale_x_continuous(breaks = pretty(TedRaw$duration, n = 22))



languages <- TedRaw %>%
  group_by(languages) %>%
  tally(name ="numTalks") 


#Number of Languages
ggplot(data=languages, aes(x=languages, y=numTalks)) +
  geom_point(stat = "identity", aes(color = '#fa9fb5'))+
  #geom_bar(stat = "identity", fill = '#fa9fb5')+
  geom_vline(aes(xintercept = mean(TedRaw$languages)),col='red',
             size=.5, linetype="dotted")+
  geom_vline(aes(xintercept = median(TedRaw$languages)),col='black',
             size=.5, linetype="dotted")+
  labs(x = "Number of available languages", y = "Count of TED Talks",
       title = "Talks available in translated languages")+
  scale_x_continuous(breaks = pretty(TedRaw$languages, n = 20))



