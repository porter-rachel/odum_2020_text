
##### AUTHORS:     Rachel Porter
##### DATE:        13 October 2020
##### PURPOSE:     Intro to Text Analysis - Day 1
##### COURSE:      Odum -- Short Course -- Text Analysis

#install.packages("")
library(rvest)
library(readtext)
library(quanteda)
library(skmeans)
library(dplyr)
library(tidytext)
library(tibble)
library(ggplot2)

#### TEXT COLLECTION, SKIP DOWN TO NEXT SECTION FOR PREPROCESSING ####

## Specify the root of the URL we will be pulling from 
## In order to scroll over multiple pages, we will need to 
## take this root url and apply changes to it 

root <- "https://obamawhitehouse.archives.gov/"

## Creating an empty dataframe for text
master <- data.frame()

## Increase the timeout wait time 
timeout(40000)

## Using a for loop, scrolling over 100 pages to find links 
## to text on White House press releases 
for (p in 1:50){
  
  ## Taking the root and modifying it to iterative over each page
  obama <- read_html(paste(root, "briefing-room/speeches-and-remarks?term_node_tid_depth=31&page=", 
                           p, sep = ""))
  
  ## Using CSS selector, picking the content we want from each page
  ## In this case, we want the links to the speeches
  obama_nodes <- html_nodes(obama, css="#content-start")
  
  ## Pull out the links to speeches from the previous object 
  links <- html_attr(html_nodes(obama_nodes, "a"), "href")
  
  ## Loop inception! Now we're going to be looping over the links 
  ## we just pulled in the previous set in ordet to get the text
  for(i in 1:10){
    
    ## Same deal as above, creating a URL using the links 
    link_app <- read_html(paste(root, links[i], sep = ""))
    
    ## Pulling out the stuff we want ie. the text 
    link_nodes <- html_nodes(link_app, css="p")
    
    ## Extracting the text from the node
    text <- html_text(link_nodes)
    
    ## Pieceing all the text together into one document 
    text <- paste(text, collapse = "")
    
    ## Pulling out the date, we will use this as `metadata`
    date_nodes <- html_nodes(link_app, css="#press_article_date_created")
    
    ## Pulling the text from the node 
    date <- html_text(date_nodes)
    
    ## Placing the text and date into a new matrix, which we will 
    ## append to the master so that the final result will be a complete 
    ## data set of text and dates!
    temp <- matrix(data = NA, nrow = 1, ncol = 2)
    temp[,1] <- date
    temp[,2] <- text
    master <- rbind(master, temp)
  }
}

## Some final housekeeping
colnames(master) <- c("date", "text")
master[] <- lapply(master, as.character)

## Let's save this in case we need to load it back later 
save(master, file = "obama_speeches.Rdata")

#### TEXT PREPROCESSING, START HERE FOR CLEANING AND PREPARING TEXT ####

load("~/Dropbox/Text_Class/odum_text_2019_fall/obama_speeches.Rdata")

## Inspecting our 'master' dataframe, we have around 1000
## observations, however in order to use this text we need 
## to clean it first, let's check out one piece of text to 
## see what may need to be cleaned up! 

str(master)
master[1,2]

## We see punctuation, captialization, and some weird tags 
## These will all need to go! Let's start by removing those 
## pesky word tags. To do this we'll rely on regular expressions. 
## These are ways to manipulate letter or number patterns within 
## text in R. For instance take the line of text below 

example <- "The quick \nbr\nown fox jumped \rover the lazy, foxy dog"

## Let's say we wanted to get rid of any instance of an animal
## being mentioned. To do this we can specify that the patterns 
## 'fox' and 'dog' be replaced with '' --- simply white space 
## To do this, we'll use the gsub command, which is essentially
## substituting the pattern for something else...

example <- gsub( "\\bfox\\b", "", example)
example <- gsub( "\\bdog\\b", "", example)

## The "\b" here is ensuring that the substitute is only occuring
## for an exact word patter, note here that the word 'foxy' was
## not susbsittuted out! How about those other tags? Those are 
## line breaks and need to be stripped as well!

example <- gsub("[\r\n]", "", example)

example

####################### Practice ###############################

## Try using gusbs on the master dataframe to get rid of any 
## uhelpful patterns that are cluttering the text!
## phrases separated by a paragraph should use different gsubs!






#################################################################

## Let's check our example text again

master[1,2]

## Looking good!
## In order to go further we'll need to convert 
## our dataframe into a corups using the quanteda package 

speeches <- corpus(master, text_field = "text")

## We'll eliminate stop words, stemming, and removing 
## punctuation. This can all be done in a single command 
## while compiling a document-term matrix (think back to
## the example from the slides)

speeches_dfm <- dfm(speeches)

## But what exactly is dfm doing?

?dfm

## Brice Acree explains this very well so this is lifted 
## straight from him: 

#############################################################

## We need to think closely about a lot of the decisions that 
## dfm() is making on our behalf.
## In other words, we can create a DTM without lowering all
## characters, without removing numbers, et cetera. We need
## to note, however, that these decisions are important in 
## two huge ways:
##  1. This can dramatically affect the results we achieve
##  2. This can make out data massive and hard to use.

# Let's assume, though, that we want to ignore some words,
## common english 'stopwords', that don't often contribute
## to what we're aiming for. Let's look at the stopwords
## included in quanteda:
stopwords('english')

## Let's assign these to a vector
eng.stopwords <- stopwords('english')

## And we can pass this to the remove argument in
## dfm(), so that these do not get included in the DTM.
example_dfm <- dfm(speeches, 
            remove=eng.stopwords)

keep.stopword <- eng.stopwords[-c(which(eng.stopwords=="her"))]

keep.stopword <- c("america",keep.stopword)

## And we can see the summary
summary(example_dfm)
## or the first bit:
head(example_dfm)
## Note that there are fewer columns in that matrix...

## We can also add new stopwords, if we wanted. If for some reason
## we wanted to get God out of public life...
eng.stopwords <- c("Washington", eng.stopwords)

## And again...
example_dfm <- dfm(speeches, remove=eng.stopwords)

## And we can see the first few rows
head(example_dfm)

#############################################################

## Alright, back to our text, let's go ahead and remove all 
## english stop words, stem our words, and remove punctuation

speeches_dfm <- dfm(speeches, stem = T,
                    remove = stopwords('english'), remove_punct = T)

head(speeches_dfm)

## We can alsp examine top features and threshold level
tops <- topfeatures(speeches_dfm, 100)

## We can also plot this information
top20 <- as.data.frame(tops)
top20 <- top20 %>% rownames_to_column("words")
top20 <- top20[1:20,]

ggplot(top20, aes(x=top20$word, y=top20$tops, fill=top20$word)) +
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylab("Number of Times Word Appears in Obama Speeches")+
  xlab("")+
  guides(fill=FALSE)

## Documents can also be sorted into clusters based on word similarity
clust_mod = skmeans(speeches_dfm, k=15, 
                    method='pclust', 
                    control=list(verbose=TRUE))

## The number of clusters, words included, etc. will all drastically 
## alter document cluster membership 
## Let's examine the clusters for the first 10 texts
df <- data.frame(cluster = clust_mod$cluster[1:15], text = master$text[1:15])
#master$text[1:10]
clust_mod$cluster[1:15]

####################### Practice ###############################

## Modifying the above code, change the words included / excluded
## to experiment with how cluster change with variation in the 
## document composition








#################################################################

## From Chris Bail:

## It is common in quantitative text analysis to identify 
## unusual words that might set one document apart from the others 
## (this will become particularly important when we get to more advanced 
## forms of pattern recognition in text later on). The metric most commonly 
## used to identify these unusual words is “Term Frequency Inverse Document 
## Frequency” (tf-idf). 

obama_tfidf <- master %>%
    select(date, text) %>%
      unnest_tokens("word", text) %>%
        anti_join(stop_words) %>%
          count(word, date) %>%
            bind_tf_idf(word, date, n)

## Now let’s see what the most unusual words are:

top_tfidf <- obama_tfidf %>%
  arrange(desc(tf_idf))

top_tfidf$word[1]
top_tfidf$word[1:10]
