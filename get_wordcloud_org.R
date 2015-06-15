setwd("~../../Dropbox/R_Projects/AGA_Attendees/PDT_2015")

require(tm)
require(wordcloud)
require(Rstem)

load('PDT_2015.RData')

junk <- c('dept', 'department', 'inc', 'llp', 'llc', 'company', 'and', 'the')

corpus <- Corpus(VectorSource(final$company))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeWords, 'english')
corpus <- tm_map(corpus, removeWords, junk)

tdm <- TermDocumentMatrix(corpus)
m <- as.matrix(tdm)
d <- data.frame(freq = sort(rowSums(m), decreasing = TRUE))

# Stem words
d$stem <- wordStem(row.names(d), language = "english")

# and put words to column, otherwise they would be lost when aggregating
d$word <- row.names(d)

# aggregate freqeuncy by word stem and
# keep first words..
agg_freq <- aggregate(freq ~ stem, data = d, sum)
agg_word <- aggregate(word ~ stem, data = d, function(x) x[1])

d <- cbind(freq = agg_freq[, 2], agg_word)

# sort by frequency
d <- d[order(d$freq, decreasing = T), ]

# print wordcloud:
set.seed(995)
png('wordcloud_org.png',width = 900, height = 900, res = 72*3)
wordcloud(d$word, d$freq, scale=c(3,0.5), max.words = 150,
          random.order=FALSE, rot.per=0.20,  
          use.r.layout=FALSE, colors=brewer.pal(8, 'Dark2'))
dev.off()
shell.exec('wordcloud_org.png')

