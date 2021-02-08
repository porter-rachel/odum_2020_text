library(jsonlite)
library(dplyr)

NYTIMES_KEY <- "GaHxO48m5mA8ttBX0zKbZNAhGh4FxGAM"

baseurl <- paste('http://api.nytimes.com/svc/search/v2/articlesearch.json?',
                 '&fq=source:',"(The New York Times)",'AND type_of_material:',"(News)",
                 'AND persons:',"(Trump, Donald J)",
                 '&begin_date=','20160522&end_date=','20161107&api-key=',NYTIMES_KEY,sep="")

initialQuery <- fromJSON(baseurl)
maxPages <- round((initialQuery$response$meta$hits[1] / 10)-1) 

maxPages = ifelse(maxPages >= 5, 5, maxPages)

donald_text <- vector("list",length=maxPages)

for(i in 0:maxPages){
  nytSearch <- fromJSON(paste0(baseurl, "&page=", i), flatten = TRUE) %>% data.frame() 
  donald_text[[i+1]] <- nytSearch 
  Sys.sleep(5) #I was getting errors more often when I waited only 1 second between calls. 5 seconds seems to work better.
}

articles <- rbind_pages(donald_text)

colnames(articles) <- str_replace(colnames(articles),
                               pattern='response.',replace='')
colnames(articles) <- str_replace(colnames(articles),
                               pattern='docs.',replace='')
colnames(articles)


##thanks to Heather Geiger for posting her code