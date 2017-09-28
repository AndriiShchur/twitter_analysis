
library(twitteR)
api_key<- '---'
api_secret <- '---'
access_token <- '---'
access_token_secret <- '---'
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)


pos <- read_csv("---",col_names = FALSE)
neg <- read_csv("---",col_names = FALSE)

pos.words<-as.list(pos$X1)
neg.words<-as.list(neg$X1)

score.sentiment = function(tweets, pos.words, neg.words)
  
{
  
  require(plyr)
  require(stringr)
  
  scores = laply(tweets, function(tweet, pos.words, neg.words) {
    
    tweet = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ",  tweet)
    tweet = gsub('https://','',tweet) # removes https://
    tweet = gsub('http://','',tweet) # removes http://
    tweet = gsub('[^[:graph:]]', ' ',tweet) ## removes graphic characters like emoticons 
    tweet = gsub('[[:punct:]]', ' ', tweet) # removes punctuation 
    tweet = gsub('[[:cntrl:]]', '', tweet) # removes control characters
    tweet = gsub('\\d+', '', tweet) # removes numbers
    tweet = gsub('  ', ' ', tweet)
    tweet = gsub('   ', ' ', tweet)
    tweet = tolower(tweet) # makes all letters lowercase
    
    word.list = str_split(tweet, '\\s+') # splits the tweets by word in a list
    
    words = unlist(word.list) # turns the list into vector
    
    pos.matches = match(words, pos.words) ## returns matching 
    #values for words from list 
    neg.matches = match(words, neg.words)
    
    pos.matches = !is.na(pos.matches) ## converts matching values to true of false
    neg.matches = !is.na(neg.matches)
    
    score = sum(pos.matches) - sum(neg.matches) # true and false are 
    #treated as 1 and 0 so they can be added
    
    return(score)
    
    
  }, pos.words, neg.words )
  
  scores.df = data.frame(score=scores, text=tweets)
  return(scores.df)
  
}


tweeter = searchTwitter('Ukraine',n=1000,since='2014-06-07',lang='en')

tweeter_df<-twListToDF(tweeter)
Tweets.text = tweeter_df$text
#Tweets.text = laply(tweeter,function(t)t$getText())
analysis = score.sentiment(Tweets.text, pos.words, neg.words)
#analysis$time<-as.POSIXct(tweeter_df$created)

tweeter_df$score<-analysis$score
hist(analysis$score)
plot(x=analysis$time,y=analysis$score,type='l')






