library(tm)
library(topicmodels)

setwd("~/Documents/MSDS/dataViz/Slogans")

slogans = read.csv("SlogansClean.csv", header=TRUE)
#write.csv(slogans, file = "SlogansClean.csv", row.names = FALSE)

#get lemma
adorn <- function(text) {
  require(httr)
  require(XML)
  url <- "http://devadorner.northwestern.edu/maserver/partofspeechtagger"
  response <- GET(url,query=list(text=text, media="xml", 
                                 xmlOutputType="outputPlainXML",
                                 corpusConfig="ncf", # Nineteenth Century Fiction
                                 includeInputText="false", outputReg="true"))
  doc <- content(response,type="text/xml")
  words <- doc["//adornedWord"]
  xmlToDataFrame(doc,nodes=words)
}

first = VCorpus(DataframeSource(slogans["Slogan"]))
sloganCorpus2 = lapply(first,function(x) adorn(as.character(x)))

lemma = slogans["Slogan"]
for (i in as.numeric(names(sloganCorpus2))){
  lemma$lemma[i] = paste(sloganCorpus2[i][[1]][4][[1]], collapse = ' ')
}
lemma$lemma <- gsub('\\|', ' ', lemma$lemma)
lemma$lemma[146] = "compassionate conservative"
lemma$lemma[113] = "nixon be one the"
lemma$lemma[144] = "lead for the new millennium"
lemma$lemma[179] = "new possibility . real lead ."
lemma$lemma[95] = "a time for great"
lemma$lemma[123] = "return integrity to the white home"
lemma$lemma[18] = "a home divide against itself can stand"
lemma$lemma[15] = "America for the American"
lemma$lemma[4] = "Hurray , Hurray , the country be rise ' vote for clay and Frelinghuysen !"

sloganCorpus = VCorpus(DataframeSource(lemma["lemma"]))

slogan.clean = tm_map(sloganCorpus, stripWhitespace)
slogan.clean = tm_map(slogan.clean, removeNumbers)
slogan.clean = tm_map(slogan.clean, removePunctuation)
slogan.clean = tm_map(slogan.clean, content_transformer(tolower))
#slogan.clean = tm_map(slogan.clean, removeWords, stopwords("english"))
#slogan.clean = tm_map(slogan.clean, stemDocument)  
slogan.clean.tf = DocumentTermMatrix(slogan.clean, control = list(weighting = weightTfIdf))

# frequent terms
findFreqTerms(slogan.clean.tf)

m <- as.matrix(slogan.clean.tf)
frequency <- colSums(m)
frequency <- sort(frequency, decreasing=TRUE)

# word cloud
library(wordcloud)
words <- names(frequency)
wordcloud(words[1:75], frequency[1:75])

# remove empty documents
row.sums = apply(slogan.clean.tf, 1, sum)
slogan = sloganCorpus[row.sums > 0]
slogan.clean.tf = slogan.clean.tf[row.sums > 0,]

row.sums[row.sums==0]

# topic modeling (do after names are removed)
#topic.model = LDA(slogan.clean.tf, 4)
#terms(topic.model, 5)[,1:4]

#cosine similarity
library(lsa)
dtmat = as.matrix(slogan.clean.tf)
row.names(dtmat) <- slogans$Slogan

distance = cosine(t(dtmat))

#remove slogans with no similarity
distance = distance[ rowSums(distance)!=1, ]
distance = distance[ ,colSums(distance)!=0]

#document clustering - heirarchically
distance = dist(distance)
hclusters = hclust(distance)
#plot(hclusters)

# try clustering documents into 10 clusters using kmeans
kmeans.clusters = kmeans(slogan.clean.tf, 50)
clustered.kmeans = split(slogans, kmeans.clusters$cluster)

# inspect a couple clusters
inspect(clustered.kmeans[[1]])
inspect(clustered.kmeans[[2]])

#plot in D3
library(networkD3)
radialNetwork(as.radialNetwork(hclusters))
dendroNetwork(hclusters, treeOrientation = "vertical")
diagonalNetwork(as.radialNetwork(hclusters), fontFamily = "Helvetica")

saveNetwork(diagonalNetwork(as.radialNetwork(hclusters)), "diagonal.html", selfcontained = TRUE)

###############################
#aggregate by president
result <- aggregate(Slogan~Candidate,paste,collapse=",",data=slogans)
sloganCorpus = VCorpus(DataframeSource(result["Slogan"]))
slogan.clean = tm_map(sloganCorpus, stripWhitespace)
slogan.clean = tm_map(slogan.clean, removeNumbers)
slogan.clean = tm_map(slogan.clean, removePunctuation)
slogan.clean = tm_map(slogan.clean, content_transformer(tolower))
slogan.clean = tm_map(slogan.clean, removeWords, stopwords("english"))
slogan.clean = tm_map(slogan.clean, stemDocument)  
slogan.clean.tf = DocumentTermMatrix(slogan.clean, control = list(weighting = weightTf))
#distances = dist(slogan.clean.tf)
#distances = as.matrix(distances)
#row.names(distances) <- result$Candidate
distances = dist(distances)
hclusters = hclust(distances)
plot(hclusters)

