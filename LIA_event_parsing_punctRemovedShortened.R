setwd("C:/Users/Axel/Desktop/Psych Stuff/Faulkner") #set wd
###load in packages
library(tidyverse) 
library(tidytext)
library(stringr)
library(strex)
library(xlsx)

all_events <- read.csv("events.csv") #load events data
LIA_events <- all_events[all_events$SourceTextCode=="LA",] #subset to just ones from Light in August
remove(all_events) #remove from environment to save space
LIA_events <- LIA_events[order(LIA_events$OrderWithinPage),] #reorder by page order (i.e., order within the text itself)

###create begin and end word columns from snippet###
for (i in 1:nrow(LIA_events)) {
  LIA_events$First.8.10.words.of.event[i] <- str_replace_all(LIA_events$First.8.10.words.of.event[i], ' aint ', " ain't ") #convert '\n' into new line
  LIA_events$First.8.10.words.of.event[i] <- str_replace_all(LIA_events$First.8.10.words.of.event[i], ' Aint ', " Ain't ") #convert '\n' into new line
  LIA_events$First.8.10.words.of.event[i] <- str_replace_all(LIA_events$First.8.10.words.of.event[i], 'dont', "don't") #convert '\n' into new line
  LIA_events$First.8.10.words.of.event[i] <- str_replace_all(LIA_events$First.8.10.words.of.event[i], 'Dont', "Don't") #convert '\n' into new line
  LIA_events$First.8.10.words.of.event[i] <- str_replace_all(LIA_events$First.8.10.words.of.event[i], 'Mrs', "Mrs.") #convert '\n' into new line
}
LIA_events$begin_word <- word(LIA_events$First.8.10.words.of.event, 1,3) #pull first 3 words to identify chunk

LIA_events$end_word <- "" #create end word chunk identifier
i <- 0 
for (i in 1:nrow(LIA_events)) {
  LIA_events$end_word[i] <- LIA_events$begin_word[i+1]
} #set the end word of row[i] to the begin word of the next row
LIA_events$end_word[nrow(LIA_events)] <- "THE END" #last row is missing, so set to 'THE END'
###


###
LIA <- as.data.frame(read_file("William Faulkner - Light in August-Vintage International_Random House (1990).txt")) #load in Light in August text as dataframe
colnames(LIA) <- "text"
#clean up the text
LIA$text <- sub(".*\n4\n","",LIA$text) #remove edition information
LIA$text <- str_replace_all(LIA$text, "", "") #remove unknown characters
LIA$text <- str_replace_all(LIA$text, '\n[0-9]+\n', '') #remove page numbers
LIA$text <- str_replace_all(LIA$text, '\nfWilliam Faulkner  LIGHT IN AUGUST\n', '') #remove header
LIA$text <- str_replace_all(LIA$text, '\fWilliam Faulkner  LIGHT IN AUGUST', '') #remove broken header
LIA$text <- str_replace_all(LIA$text, '\n', ' ') #convert '\n' into new line
###

LIA2 <- LIA
capFix2 <- data.frame(str_locate_all(LIA2$text, ' [:upper:]{2,}'))
for (i in 1:nrow(capFix2)) {
  LIA2$text <- str_replace(LIA2$text, substr(LIA2$text, capFix2$start[i], capFix2$end[i]), str_to_title(substr(LIA2$text, capFix2$start[i], capFix2$end[i])))
} #remove every instance of all uppercase words
###

capFix <- data.frame(str_locate_all(LIA$text, ' [:upper:]{2,}'))
for (i in 1:nrow(capFix)) {
  #LIA$text <- str_replace(LIA$text, substr(LIA$text, capFix$start[i], capFix$end[i]), str_to_title(substr(LIA$text, capFix$start[i], capFix$end[i])))
  LIA$text <- str_replace(str_replace_all(LIA$text, '[[:punct:]]', ""), substr(LIA$text, capFix$start[i], capFix$end[i]), str_to_title(substr(LIA$text, capFix$start[i], capFix$end[i])))
} #remove every instance of all uppercase words
capFix <- data.frame(str_locate_all(LIA$text, ' [:upper:]{2,}'))
for (i in 1:nrow(capFix)) {
  #LIA$text <- str_replace(LIA$text, substr(LIA$text, capFix$start[i], capFix$end[i]), str_to_title(substr(LIA$text, capFix$start[i], capFix$end[i])))
  LIA$text <- str_replace(str_replace_all(LIA$text, '[[:punct:]]', ""), substr(LIA$text, capFix$start[i], capFix$end[i]), str_to_title(substr(LIA$text, capFix$start[i], capFix$end[i])))
} #remove every instance of all uppercase words
str_extract_all(LIA$text, ' [:upper:]{2,}') #verify no more all caps words
#LIA$text
###

###
LIA_events$begin_word <- str_replace_all(LIA_events$begin_word, '[[:punct:]]', "") #remove punctuation from begin_word ##input "%" to fill in spot for missing punctuation points
LIA_events$end_word <- str_replace_all(LIA_events$end_word, '[[:punct:]]', "") #and end_word

LIA_events$excerpt <- ""
temp_locations <- data.frame(0,0)
colnames(temp_locations) <- c("start", "end")
temp_locations$start <- as.numeric(temp_locations$start)
temp_locations$end <- as.numeric(temp_locations$end)
for (i in 1:nrow(LIA_events)) {
  if (i==1) {
    temp_locations <- as.data.frame(str_locate(LIA$text, paste('(', LIA_events$begin_word[i], ')', '.\\s*(.*?)\\s*(?=', LIA_events$end_word[i],')', sep=""))) #save indices of this excerpt
    #LIA$text <- str_sub(LIA$text, temp_locations$end[i], -1)
  } else {
    temp_locations <- rbind(temp_locations, as.data.frame(str_locate(LIA$text, paste('(', LIA_events$begin_word[i], ')', '.\\s*(.*?)\\s*(?=', LIA_events$end_word[i],')', sep="")))) #save indices of this excerpt
    #LIA$text <- str_sub(LIA$text, temp_locations$end[i], -1)
  }
  if (is.na(temp_locations$start[i])) {
    LIA_events$excerpt[i] <- NA
  } else if (((temp_locations$end[i] - temp_locations$start[i]) > 7000)) {
    LIA_events$excerpt[i] <- NA
    temp_locations$start[i] <- NA
    #temp_locations$end[i] <- NA
  }
}
sum(is.na(temp_locations$start) == TRUE)
sum(is.na(temp_locations$end) == TRUE)

for (i in 1:nrow(temp_locations)) {
  if (i > 1) {
    if (!is.na(temp_locations$end[i]) & !is.na(temp_locations$end[i-1])) {
      if (temp_locations$end[i] < temp_locations$end[i-1]) {
        temp_locations$end[i] <- NA
      }
    }
    if (!is.na(temp_locations$start[i]) & !is.na(temp_locations$start[i-1])) {
      if (temp_locations$start[i] < temp_locations$start[i-1]) {
        temp_locations$start[i] <- temp_locations$end[i-1] + 1
      }
    }
    if (is.na(temp_locations$start[i]) & is.numeric(temp_locations$end[i]) & is.numeric(temp_locations$start[i-1])) {
      temp_locations$start[i] <- temp_locations$end[i-1] + 1
      #temp_locations[i,1] <- as.data.frame(str_locate_nth(LIA$text, paste('(', LIA_events$begin_word[i], ')'), 2))[,1]
    }
  }
}
sum(is.na(temp_locations$start) == TRUE)
sum(is.na(temp_locations$end) == TRUE)

substr(LIA$text, 1, 500)
substr(LIA2$text, 1, 500)

for (i in 1:nrow(LIA_events)) {
  if (!is.na(temp_locations$start[i]) & !is.na(temp_locations$end[i])) {
    if (temp_locations$end[i] - temp_locations$start[i] > 32767) {
      LIA_events$excerpt[i] <- NA
      temp_locations$end[i] <- NA
    }
    LIA_events$excerpt[i] <- str_sub(LIA$text, temp_locations$start[i], temp_locations$end[i])
  }
}
write.xlsx2(LIA_events, "LIA_events.xlsx")
###


str_locate_all(LIA2$text, '[:punct:]')









for (i in 1:nrow(temp_locations)) {
  if (i == nrow(temp_locations)) {
    
  } else if (is.na(temp_locations$start[i]) & is.numeric(temp_locations$end[i]) & is.numeric(temp_locations$start[i+1])) {
    if (temp_locations$end[i] + 1 == temp_locations$start[i+1]) {
      #temp_locations[i,] <- as.data.frame(str_locate_nth(LIA$text, paste('(', LIA_events$begin_word[i], ')', '.\\s*(.*?)\\s*(?=', LIA_events$end_word[i],')', sep=""), 2))
      temp_locations[i,1] <- as.data.frame(str_locate_nth(LIA$text, paste('(', LIA_events$begin_word[i], ')'), 2))[,1]
    }
  }
}
  
  
  if (is.na(temp_locations$start[i])) {
    LIA_events$excerpt[i] <- NA
  } else if (((temp_locations$end[i] - temp_locations$start[i]) > 7000)) {
    LIA_events$excerpt[i] <- NA
    temp_locations$start[i] <- NA
    #temp_locations$end[i] <- NA
  }


sum(is.na(temp_locations$start) == TRUE)
sum(is.na(temp_locations$end) == TRUE)

else {
  if (is.na(temp_locations$start[i-1]) & is.numeric(temp_locations$end[i-1])) {
    if (is.numeric(temp_locations$start[i])) {
      temp_locations[(i-1),] <- as.data.frame(str_locate_nth(LIA$text, paste('(', LIA_events$begin_word[i-1], ')', '.\\s*(.*?)\\s*(?=', LIA_events$end_word[i-1],')', sep=""), 2))
    }
  }
else if (is.numeric(temp_locations$start[i]) & is.numeric(temp_locations$end[i-1])) {
  if (temp_locations$start[i] < temp_locations$end[i-1]) {
    temp_locations$start[i] <- NA
  }
}

for (i in 1:nrow(LIA_events)) {
  #new for loop here
  LIA_events$excerpt[i] <- str_extract(LIA$text, paste('(', LIA_events$begin_word[i], ')', '.\\s*(.*?)\\s*(?=', LIA_events$end_word[i],')', sep=""))
  if (is.na(LIA_events$excerpt[i]) == FALSE) {
    LIA_events$excerpt[i] <- str_extract(LIA$text, paste('(', LIA_events$begin_word[i], ')', '.\\s*(.*?)\\s*(?=', LIA_events$end_word[i],')', sep=""))
  } else {
    LIA_events$begin_word[i] <- word(LIA_events$begin_word[i], 1,2)
    LIA_events$end_word[i] <- word(LIA_events$end_word[i], 1,2)
    LIA_events$excerpt[i] <- str_extract(LIA$text, paste('(', LIA_events$begin_word[i], ')', '.\\s*(.*?)\\s*(?=', LIA_events$end_word[i],')', sep=""))
  }
}#create sentence parsing from begin and end words (essentially pull the text in between these words)
LIA_events <- cbind(LIA_events, temp_locations) #bind temp_locations to LIA_events dataframe
for (i in 1:nrow(LIA_events)) {
  LIA_events$excerpt[i] <- str_sub(LIA2$text, LIA_events$start[i], LIA_events$end[i])
}#now replace the excerpts with ones that actually have punctuation
LIA_events$excerpt[1] #check that it worked
sum(is.na(LIA_events$excerpt)) #check number of failures
LIA_events[is.na(LIA_events$excerpt) == TRUE,]
str_length(LIA_events$excerpt)

LIA_events$end_word[14]
#####


temp <- as.list(str_extract_all(LIA$text, LIA_events$end_word[14]))
str_sub(LIA$text, LIA_events$start[2], -1) #search only after the current begin word location
#need to be able to search for second appearnace of a substring

test <- "testthisis a test string and another test (oops) string"
str_locate(test, '(test).*?(?=string)')
as.data.frame(str_locate_nth(test, 'test', 2))[,1]


str_locate(test, "(?= a).*")
str_sub(test, start, -1)
LIA$text <- str_sub(LIA$text, temp_locations$start[i], -1)




str_length(LIA_events$excerpt)
LIA_events$end_word[8]




sum(nchar(LIA$text))
LIA_events$excerpt <- str_sub(LIA2$text, LIA_events$start[1], LIA_events$end[1])

str_extract(str_replace_all(LIA2$text, '[[:punct:]]', ""), LIA_events$excerpt[2])
str_extract(LIA2$text, LIA_events$excerpt[2])

str_sub(LIA$text, temp_locations$end[i], -1)

temp_locations <- data.frame(0,0)
colnames(temp_locations) <- c("start", "end")
temp_locations <- as.data.frame(str_locate(LIA$text, LIA_events$excerpt[1]))
nchar(str_extract(LIA$text, paste('(', LIA_events$begin_word[1], ')', '.\\s*(.*?)\\s*(?=', LIA_events$end_word[1],')', sep="")))
str_locate(LIA$text, paste('(', LIA_events$begin_word[1], ')', '.\\s*(.*?)\\s*(?=', LIA_events$end_word[1],')', sep=""))
temp_locations <- as.data.frame(str_locate(LIA$text, paste('(', LIA_events$begin_word[1], ')', '.\\s*(.*?)\\s*(?=', LIA_events$end_word[1],')', sep="")))
temp_locations <- rbind(temp_locations, as.data.frame(str_locate(LIA$text, paste('(', LIA_events$begin_word[2], ')', '.\\s*(.*?)\\s*(?=', LIA_events$end_word[2],')', sep=""))))


str_which(LIA$text, LIA_events$begin_word[1])
str_replace_all(LIA2$text, '[[:punct:]]', '%')
substr(LIA$text, 1, 500)
substr(LIA2$text, 1, 500)

LIA_events$excerpt[1]
LIA_events[is.na(LIA_events$excerpt) == TRUE,]
###
nrow(LIA_events[nchar(LIA_events$excerpt) > (32767*10),])
#let's save it (commas in the text make csv a bad choice!)
write.csv(LIA_events, "LIA_events.csv")
write.xlsx2(LIA_events, "LIA_events.xlsx")


#provided code
LIA_tidy_string <- LIA2 %>%
  mutate(text = gsub("[‘’]", "'", text)) %>%
  unnest_tokens(text, text) %>%
  summarize(text = str_c(text, collapse = " "))

LIA_events_tidy_begin <- LIA_events %>%
  unnest_tokens(begin_word, begin_word) %>%
  mutate(begin_word = gsub("[‘’]", "'", begin_word)) %>%
  group_by(Nid) %>%
  summarize(begin_word = str_c(begin_word, collapse = " ")) %>%
  ungroup()

LIA_events_tidy_end <- LIA_events %>%
  unnest_tokens(end_word, end_word) %>%
  mutate(end_word = gsub("[‘’]", "'", end_word)) %>%
  group_by(Nid) %>%
  summarize(end_word = str_c(end_word, collapse = " ")) %>%
  ungroup()

LIA_events_tidy <- LIA_events %>%
  select(-begin_word,-end_word) %>%
  left_join(LIA_events_tidy_begin) %>%
  left_join(LIA_events_tidy_end)

LIA_event_sentences <- LIA_events_tidy %>%
  mutate(event_sentence = str_match(
    LIA_tidy_string$text,
    paste(
      "\\s*",
      LIA_events_tidy$begin_word,
      "(.*?)\\s*",
      LIA_events_tidy$end_word
    )
  )) %>%
  mutate(event_sentence = paste(begin_word, event_sentence[, 2]))


#see which did better
sum(is.na(LIA_events$excerpt))
sum(nchar(LIA_event_sentences$event_sentence) < 25)



write.csv(LIA_event_sentences, "LIA_event_sentences.csv")
#install.packages("xlsx")
library(xlsx)
write.xlsx(LIA_event_sentences, "LIA_event_sentences.xlsx")
