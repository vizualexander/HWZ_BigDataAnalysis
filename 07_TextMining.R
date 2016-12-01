##################################################################################################
# Beispiel text Mining mit R
#
# https://rstudio-pubs-static.s3.amazonaws.com/31867_8236987cf0a8444e962ccd2aec46d9c3.html
#
##################################################################################################

Needed <- c("tm", "SnowballCC", "RColorBrewer", "ggplot2", "wordcloud", "biclust", "cluster", "igraph", "fpc")   
install.packages(Needed, dependencies=TRUE)   

install.packages("Rcampdf", repos = "http://datacube.wu.ac.at/", type = "source") 


#####################################################################################################
#### Loading Data, txt Files
#####################################################################################################

# Texte einlesen, lokal mit Angabe Verzeichnis:
cname <- file.path("C:/Temp", "txt_files")   
cname   
dir(cname) 


# Bibliothek für Text Mining
library(tm)   
docs <- Corpus(DirSource(cname))   
summary(docs) 
#Dokument 2 anzeigen
inspect(docs[2])


#####################################################################################################
#### Preprocessing
#####################################################################################################

docs <- tm_map(docs, removePunctuation)  

for(j in seq(docs))   
{   
  docs[[j]] <- gsub("/", " ", docs[[j]])   
  docs[[j]] <- gsub("@", " ", docs[[j]])   
  docs[[j]] <- gsub("\\|", " ", docs[[j]])   
} 

# Removing numbers
docs <- tm_map(docs, removeNumbers) 
# Converting to lowercase
docs <- tm_map(docs, tolower)  
#Removing “stopwords” (common words) that usually have no analytic value
# For a list of the stopwords, see: 
# stopwords("german") 
# length(stopwords("german"))
docs <- tm_map(docs, removeWords, stopwords("german"))   

#Removing particular words
docs <- tm_map(docs, removeWords, c("Unnötig", "Mussnichtsein"))   

# Combining words that should stay together --> not needed in this example
#for (j in seq(docs))
#{
#  docs[[j]] <- gsub("qualitative research", "QDA", docs[[j]])
#  docs[[j]] <- gsub("qualitative studies", "QDA", docs[[j]])
#  docs[[j]] <- gsub("qualitative analysis", "QDA", docs[[j]])
#  docs[[j]] <- gsub("research methods", "research_methods", docs[[j]])
#}


# Removing common word endings (e.g., “ing”, “es”, “s”) --> not needed in this example
#library(SnowballC)   
# docs <- tm_map(docs, stemDocument) 


# Stripping unnecesary whitespace from your documents:
docs <- tm_map(docs, stripWhitespace)

# preprocessed documents as text documents
docs <- tm_map(docs, PlainTextDocument) 

#####################################################################################################
#### Stage the Data
#####################################################################################################
 
# Document Term Matrix
dtm <- DocumentTermMatrix(docs)   
dtm  

# Transponse MAtrix
tdm <- TermDocumentMatrix(docs)   
tdm


#####################################################################################################
#### Explore your data
#####################################################################################################

# Organize terms by their frequency
freq <- colSums(as.matrix(dtm))   
length(freq) 

ord <- order(freq) 

# export the matrix to Excel --> not needed in this example  
#m <- as.matrix(dtm)   
#dim(m)   
#write.csv(m, file="dtm.csv") 


#  Start by removing sparse terms:   
dtms <- removeSparseTerms(dtm, 0.1) # This makes a matrix that is 10% empty space, maximum.   
inspect(dtms) 

# Word Frequency
freq[head(ord)]
freq[tail(ord)] 
head(table(freq), 20)  
tail(table(freq), 20)


freq <- colSums(as.matrix(dtms))   
freq



freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)   
head(freq, 14) 



findFreqTerms(dtm, lowfreq=50)   # Change "50" to whatever is most appropriate for your text data.
findFreqTerms(dtm, lowfreq=400)


wf <- data.frame(word=names(freq), freq=freq)   
head(wf) 


#####################################################################################################
#### Plot Word Frequencies
#####################################################################################################
library(ggplot2)   
p <- ggplot(subset(wf, freq>300), aes(word, freq))    
p <- p + geom_bar(stat="identity")   
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))   
p   


#####################################################################################################
#### Plot Word Frequencies
#####################################################################################################
library(wordcloud)  
set.seed(142)   
wordcloud(names(freq), freq, min.freq=25)


# Plot the 100 most frequently used words.
set.seed(142)   
wordcloud(names(freq), freq, max.words=100)

# colored
set.seed(142)   
wordcloud(names(freq), freq, min.freq=20, scale=c(5, .1), colors=brewer.pal(6, "Dark2")) 

# Plot the 100 most frequently occurring words.
set.seed(142)   
dark2 <- brewer.pal(6, "Dark2")   
wordcloud(names(freq), freq, max.words=100, rot.per=0.2, colors=dark2)   



#####################################################################################################
#### Clustering by Term Similarity
#####################################################################################################
dtmss <- removeSparseTerms(dtm, 0.15) # This makes a matrix that is only 15% empty space, maximum.   
inspect(dtmss)   



#####################################################################################################
#### Hierarchal Clustering
#####################################################################################################
library(cluster)   
d <- dist(t(dtmss), method="euclidian")   
fit <- hclust(d=d, method="ward.D")   
fit  





# Cluster Dendogram
plot.new()
plot(fit, hang=-1)
groups <- cutree(fit, k=5)   # "k=" defines the number of clusters you are using   
rect.hclust(fit, k=5, border="red") # draw dendogram with red borders around the 5 clusters   



# K-means clustering
library(fpc)   
d <- dist(t(dtmss), method="euclidian")   
kfit <- kmeans(d, 2)   
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0) 


