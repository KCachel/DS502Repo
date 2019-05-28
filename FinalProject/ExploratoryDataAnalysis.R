library(ggplot2) # Data visualisation
library(reshape2)
library(corrplot)
library(dplyr) 
library(ggthemes)
#library(stringr) # String manipulation
library(anytime)
#library(data.table)
library(lubridate)

TedRaw <- read.csv(file="/cloud/project/FinalProject/ted_main.csv", header=TRUE, sep=",")

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


###############################################
#EDA for factors: Occupation, event
###############################################


##speaker occupation
speaker_occupation <- TedRaw %>%
  group_by(speaker_occupation) %>%
  tally(name ="numTalks") 

speaker_occupationtop <- arrange(speaker_occupation, desc(numTalks))
speaker_occupationtop <- head(speaker_occupationtop,15)

ggplot(data=speaker_occupationtop, aes(x=speaker_occupation, y=numTalks)) +
  #geom_point(stat = "identity", aes(color = '#2b8cbe'))+
  geom_bar(stat = "identity", fill = '#2b8cbe')+
  geom_text(aes(label = numTalks), vjust = 1.6, color = "white", size = 3)+
  labs(x = "Speaker Occupation", y = "Count of TED Talks",
       title = "Talks by 15 most popular speaker occupations")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#event

event <- TedRaw %>%
  group_by(event) %>%
  tally(name ="numTalks") 

eventtop <- arrange(event, desc(numTalks))
eventtop <- head(eventtop,20)

ggplot(data=eventtop, aes(x=event, y=numTalks)) +
  #geom_point(stat = "identity", aes(color = '#2b8cbe'))+
  geom_bar(stat = "identity", fill = '#c51b8a')+
  geom_text(aes(label = numTalks), vjust = 1.6, color = "white", size = 3)+
  labs(x = "Event", y = "Count of TED Talks",
       title = "Talks by 20 most popular Events")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


###############################################
#EDA for time data:
###############################################
#film_date
film_date <- anydate(TedRaw$film_date)
TedRaw$film_date_clean <- film_date


film_date_clean <- TedRaw %>%
  group_by(film_date_clean) %>%
  tally(name ="numTalks") 

ggplot(data = film_date_clean, aes(x = film_date_clean, y = numTalks))+
  geom_line(color = "#feb24c")+
  labs(x = "Date", y = "Count of TED Talks",
       title = "Talks by film date")

##########################
#published_date

TedRaw$published_date_clean <- anydate(TedRaw$published_date)

published_date_clean <- TedRaw %>%
  group_by(published_date_clean) %>%
  tally(name ="numTalks") 

ggplot(data = published_date_clean, aes(x = published_date_clean, y = numTalks))+
  geom_line(color = "#feb24c")+
  labs(x = "Date", y = "Count of TED Talks",
       title = "Talks by published date")

#day of week

TedRaw$published_date_wday <- wday(TedRaw$published_date_clean, label = TRUE)

published_date_wday <- TedRaw %>%
  group_by(published_date_wday) %>%
  tally(name ="numTalks") 

ggplot(data=published_date_wday, aes(x=published_date_wday, y=numTalks)) +
  #geom_point(stat = "identity", aes(color = '#2b8cbe'))+
  geom_bar(stat = "identity", fill = '#2b8cbe')+
  geom_text(aes(label = numTalks), vjust = 1.6, color = "white", size = 3)+
  labs(x = "Day of week", y = "Count of TED Talks",
       title = "Talks by published date (day of week)")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#month

TedRaw$published_date_month <- month(TedRaw$published_date_clean, label = TRUE)

published_date_month <- TedRaw %>%
  group_by(published_date_month) %>%
  tally(name ="numTalks") 

ggplot(data=published_date_month, aes(x=published_date_month, y=numTalks)) +
  #geom_point(stat = "identity", aes(color = '#2b8cbe'))+
  geom_bar(stat = "identity", fill = '#2b8cbe')+
  geom_text(aes(label = numTalks), vjust = 1.6, color = "white", size = 3)+
  labs(x = "Day of week", y = "Count of TED Talks",
       title = "Talks by published date (month)")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

