apikey <- 'ZtuAPLi6BGVFawVY3NczmXdtBMbo8GGl'

base_url <- "http://api.nytimes.com/svc/search/v2/articlesearch.json"

# install.packages("httr")
library(httr)
install.packages("tidyverse")
install.packages("httr")

library(tidyverse)

r <- GET(base_url, query=list(q="inequality","api-key"=apikey))
r

content(r, 'text')
writeLines(content(r, 'text'), con = file("nyt.json"))

json <- content(r, 'parsed')
class(json); names(json)

json$status

json$response

json$response$meta

#Check the data

r <- GET(base_url, query=list(q="Taiwan",
                              "api-key"=apikey,
                              "begin_date"=20200101,
                              "end_date"=20200519))
json <- content(r, 'parsed')
json$response$meta

#Count how many times the keyword mentioned in articles

nyt_count <- function(q, date1, date2){
  r <- GET(base_url, query=list(q=q,
                                "api-key"=apikey,
                                "begin_date"=date1,
                                "end_date"=date2))
  json <- content(r, "parsed")
  return(json$response$meta$hits)
}

nyt_count(q="Taiwan", date1=20200101, date2=20200519)

#Using for loop to scrape the data

nyt_years_count <- function(q, yearinit, yearend){
  # sequence of years to loop over
  years <- seq(yearinit, yearend)
  counts <- rep(NA, length(years))
  # loop over periods
  for (i in 1:length(years)){
    # information message to track progress
    message(years[i])
    # retrieve count
    counts[i] <- nyt_count(q=q, date1=paste0(years[i], "0101"),
                           date2=paste0(years[i], "1231"))
  }
  return(counts)
}

Taiwan_count_y <- nyt_years_count(q="Taiwan", yearinit=2000, yearend=2020)

#Make a slightly change of code

nyt_count <- function(q, date1, date2){
  r <- GET(base_url, query=list(q=q,
                                "api-key"=apikey,
                                "begin_date"=date1,
                                "end_date"=date2))
  json <- content(r, "parsed")
  ## if there is no response
  while (r$status_code!=200){
    Sys.sleep(2) # wait a couple of seconds
    # try again:
    r <- GET(base_url, query=list(q=q,
                                  "api-key"=apikey,
                                  "begin_date"=date1,
                                  "end_date"=date2))
    json <- content(r, "parsed")
  }
  df <- data.frame()
  df <- data.frame(date = as.Date(dates[-length(dates)], format="%Y%m%d"), count = counts)
  return(json$response$meta$hits)
}

counts <- nyt_years_count(q="Taiwan", yearinit=2000, yearend=2020)
Taiwan_count_y <- rbind(counts)

#plot

plot(2000:2020, Taiwan_count_y, type="l", main="Mentions of Taiwan on the NYT, by year",
     xlab="Year", ylab="Article count")

ggplot(fortify(counts),aes(x = 2000:2020, y = counts))+
  geom_area(fill = "#00a497")+
  ggtitle("Mentions about Taiwan")+
  labs(subtitle = "Posts mentioned about Taiwan count by year from 2000 to 2020")+
  theme_pubclean()

#Count by months

nyt_dates_count <- function(q, init, end, by){
  # sequence of dates to loop over
  dates <- seq(from=init, to=end, by=by)
  dates <- format(dates, "%Y%m%d") # changing format to match NYT API format
  counts <- rep(NA, length(dates)-1)
  # loop over periods
  for (i in 1:(length(dates)-1)){ ## note the -1 here
    # information message to track progress
    message(dates[i])
    # retrieve count
    counts[i] <- nyt_count(q=q, date1=dates[i],
                           date2=dates[i+1])
  }
  # improving this as well so that it returns a data frame
  df <- data.frame(date = as.Date(dates[-length(dates)], format="%Y%m%d"), count = counts)
  return(df)
}

counts_n <- nyt_dates_count(q="Taiwan", init = as.Date("2015/01/01"), end = as.Date("2020/04/30"), by="month")
counts_y <- nyt_dates_count(q="Taiwan", init = as.Date("2000/01/01"), end = as.Date("2021/05/20"), by = "year")
counts_ct <- nyt_dates_count(q="Taiwan+China", init = as.Date("2000/01/01"), end = as.Date("2021/05/20"), by = "year")
counts_c <- nyt_dates_count(q="China", init = as.Date("2000/01/01"), end = as.Date("2021/05/20"), by = "year")

#TidyData
#Combine three data frame into one data set

counts_y$name="Taiwan"
counts_ct$name="Taiwan+China"
counts_c$name="China"

cdata <- rbind(counts_y, counts_ct, counts_c)

#plot

Taiwan_mention_m <- plot(counts_n$date, counts_n$count, type="l", main="Mentions of 'Taiwan' in the NYT, by month",
                         xlab="Month", ylab="Article count")

Taiwan_mention_y <- plot(counts_y$date, counts_y$count, type="l", main="Mentions of 'Taiwan' in the NYT, by year",
                         xlab="year", ylab="Article count")

#Using ggplot to create a better viz

library(ggplot2)
library(plotly)

Taiwan_mention_m <- ggplot(counts_n,aes(x = date, y = count))+
  geom_area(fill = "#00a497") +
  ggtitle("Frequency of posts about Taiwan since 2015")+
  labs(subtitle = "Posts related to Taiwan count by month in past 5 years.")+
  theme_pubclean()

Taiwan_mention_y <- ggplot(counts_y,aes(x = date, y = count))+
  geom_area(fill = "#00a497", alpha = 0.5) +
  ggtitle("Posts mentioned Taiwan since 2000")+
  labs(subtitle = "Posts related to Taiwan count by year in past 20 years.")+
  theme_pubclean()

Search_compare <- ggplot(cdata, aes(x = date, y = count, fill = name))+
  geom_area()+
  ggtitle("Frequency of posts mentioned Taiwan")+
  labs(subtitle = "Comparing to search result with/ without China")+
  guides(fill=guide_legend(title="search query"))+
  theme_pubclean()

Taiwan_2015c <- ggplotly(Taiwan_mention_m)
Taiwan_mention_y <- ggplotly(Taiwan_mention_y)
search_c <- ggplotly(Search_compare)

#save to html
library(htmlwidgets)  

saveWidget(Taiwan_2015c, file = "Taiwan_2015c.html")
saveWidget(Taiwan_mention_y, file = "Taiwan_ym.html")
saveWidget(search_c, file = "search_c.html")

#####################################################

####################################################
####  Text analysis ####

nytime_test = function (keyword,year) {
  searchQ = URLencode(keyword)
  url = paste('http://api.nytimes.com/svc/search/v2/articlesearch.json?q=',
              searchQ,
              '&begin_date=',year,'0101&end_date=',
              year,'1231&api-key=',api,sep="")
  
  #get the total number of search results
  initialsearch = fromJSON(url,flatten = T)
  maxPages = round((initialsearch$response$meta$hits / 10)-1)
  
  #try with the max page limit at 10
  maxPages = ifelse(maxPages >= 10, 10, maxPages)
  
  #creat a empty data frame(set the form of data frame)
  df = data.frame(id=as.numeric(),
                  created_time=character(),
                  snippet=character(),
                  headline=character())
  
  #save search results into data frame
  for(i in 0:maxPages){
    #get the search results of each page
    nytSearch = fromJSON(paste0(url, "&page=", i), flatten = T)
    message("Retrieving page ", i)
    temp = data.frame(id=1:nrow(nytSearch$response$docs),
                      created_time = nytSearch$response$docs$pub_date,
                      snippet = nytSearch$response$docs$snippet,
                      headline = nytSearch$response$docs$headline.main)
    df=rbind(df,temp)
    Sys.sleep(5) #sleep for 5 second
  }
  return(df)
}

#Use the function to create data

tw = nytime_test('Taiwan',2020)

write.csv(tw, "NYT news_Taiwan_2020.csv")

xi = nytime('xi jinping',2020)

write.csv(xi, "NYT news_Xi Jinping_2020.csv")

###############################################
#### Text Clean 

####  function - preprocessing of text   ####

#load library
if (!require("tm")) install.packages("tm")
library(tm)


#preprocessing function for text
preprocessing = function (doc){
  doc = gsub("[^[:alnum:]]"," ",doc)
  #create corpus
  corpus = Corpus(VectorSource(doc))
  #Removal of punctuation
  corpus = tm_map(corpus, removePunctuation)
  #Removal of numbers
  corpus = tm_map(corpus, removeNumbers)
  #Conversion to lower case
  corpus = tm_map(corpus, content_transformer(tolower)) 
  #customize my stopwords
  mystopword = "Here’s what you need to know to start your day"
  #Removal of stopwords
  corpus = tm_map(corpus, removeWords, c(stopwords("english"),mystopword))
  #retun result
  return(corpus)
}

#text cleaning
xi.clean = preprocessing(xi$snippet)
tw.clean = preprocessing(tw$snippet)

#Using wordcloud to visualise the result
if (!require("wordcloud")) install.packages("wordcloud")
library(wordcloud)
library(RColorBrewer)

par(mfrow=c(1,2)) # 1x2 panel plot
par(mar=c(1, 2, 1, 2)) # Set the plot margin
par(bg="black") # set background color as black
par(col.main="white") # set title color as white

wordcloud(xi.clean, scale=c(3,.5),min.freq=3, max.words=Inf, random.order=F, 
          colors = brewer.pal(8, "Set3"))   
title("News report of Xi Jinping")


wordcloud(tw.clean, scale=c(3,.5),min.freq=3, max.words=Inf, random.order=F, 
          colors = brewer.pal(8, "Set3"))   
title("News report of Taiwan")

dev.off()

#########################################################

#Operations on Term-Document Matrices
xi.dtm = DocumentTermMatrix(xi.clean)
#we can find those terms that occur at least 3 times
termXI <- findFreqTerms(xi.dtm, 3)

tw.dtm = DocumentTermMatrix(tw.clean)
termTW <- findFreqTerms(tw.dtm, 3)

##### Visualise the frequency of the words mentioned in articles ####

#plot frequency
par(mfrow=c(1,2))
freq = sort(colSums(as.matrix(xi.dtm)), decreasing=TRUE) 
barplot(freq[1:9],col=rev(brewer.pal(9, "PuRd")), horiz=TRUE)
title("News about Xi")
#for tw data
freq = sort(colSums(as.matrix(tw.dtm)), decreasing=TRUE) 
barplot(freq[1:9], col=rev(brewer.pal(9, "GnBu")), horiz=TRUE)
title("News about Taiwan")

dev.off()
####################################################

