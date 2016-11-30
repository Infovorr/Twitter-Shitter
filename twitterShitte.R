#initialize the script
library(twitteR)
library(ngram)

options(httr_oauth_cache=T)

#set up permissions
api <- ""
apiSecret <- ""
token <- ""
tokenSecret <- ""
setup_twitter_oauth(api, apiSecret, token, tokenSecret)

#farm tweets
reactionary <- getUser('')
friendList <- reactionary$getFriends()
friend.tweets <- c()
#friend.profiles <- c()
for(i in 1:length(friendList)) {
	print(paste("Scraping", friendList[[i]]$getScreenName()))
	friend.tweets <- paste(friend.tweets, paste(twListToDF(userTimeline(friendList[[i]], n=3200))$text, collapse = " "), collapse = " ")
	#friend.profiles <- paste(friend.profiles, paste(friendList[[i]]$getDescription()))
	#print(paste("Got", nchar(friend.tweets), "characters so far..."))
	Sys.sleep(300)
}

#write tweets to text file
tweets <- gsub("http[^[:space:]]*", "", friend.tweets)
tweets <- gsub('\\\\n', "", tweets, perl=TRUE)
tweets <- gsub('\\n', "", tweets, perl=TRUE)
tweets <- gsub("([\\])", " ", tweets)
tweets <- gsub("([\"])", " ", tweets)
tweets <- gsub(" , ", " ", tweets)
tweets <- iconv(tweets, "latin1", "ASCII", sub="")
write(tweets, file="reactionary.txt")

#determine number of tweets for the day and the maximum time between each tweet
numOfTweets <- sample(6:9, 1)
maxTime <- round((480 / numOfTweets), 0)

for(i in 1:numOfTweets) {
	#compile ngrams from farmed tweets
	reactionaryShit <- file("reactionary.txt")
	reactionary.in <- paste(readLines(reactionaryShit), collapse = " ") 
	reactionary.ngram <- ngram(reactionary.in, 2)
	reactionary.babble <- babble(reactionary.ngram)

	#start tweeting!
	pastSentences <- c()
	sentences <- c()
	sentence.starts <- as.vector(gregexpr("[?.!] +[A-Z]", reactionary.babble)[[1]])
	for(i in 1:(length(sentence.starts) - 1)) {
		this.sentence <- substr(reactionary.babble, sentence.starts[i]+2, sentence.starts[i+1])
		if(!is.na(nchar(this.sentence)) && (nchar(this.sentence) <= 140)) {
			sentences <- c(sentences, this.sentence)
			print(this.sentence)
			print(nchar(sentences[1]))
		}
	}
	if((nchar(sentences[1]) <= 140) && !(sentences[1] %in% pastSentences)) {
		setup_twitter_oauth(api, apiSecret, token, tokenSecret)
		tweet(sentences[1])
		pastSentences <- c(pastSentences, sentences[1])
	}
	#sentences <- c()
	Sys.sleep(sample(1380:(maxTime*60), 1))
}