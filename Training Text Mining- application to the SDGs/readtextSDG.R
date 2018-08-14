# create a corpus with the library tm
library(tm)

# create a corpus frame
docs <- Corpus(DirSource("./Text_SDGs"))
getTransformations()

# create toSpace content transformer
toSpace <- content_transformer(function(x, pattern) {return (gsub(pattern, " ", x))})

# see list of functions to clean the corpus
getTransformations()

# replace "-" / ":" ...  by " " using tm_map
docs <- tm_map(docs, toSpace, "-")
docs <- tm_map(docs, toSpace, ":")
docs <- tm_map(docs, toSpace, " -")
# replace all punctuation marks by " "
docs <- tm_map(docs, removePunctuation)

# transform to lower case (need to wrap in content_transformer)
docs <- tm_map(docs,content_transformer(tolower))

# strip digits (no need for content_transformer)
docs <- tm_map(docs, removeNumbers)

# remove stopwords using the standard list in tm
docs <- tm_map(docs, removeWords, stopwords("english"))

# compile meaningless words in myStopwords such as 'can', 'whose'
myStopwords <- c(stopwords("english"), "can","whose","however","may","part",
                 "also","one","first","three","four","will","within","first",
                 "still","always","thus","show","us","small","take","suggest","must",
                 "certain","allow","better","without","seem","great","face","lead",
                 "find","rather","of","and","in","end","per","least","all","as","at",
                 "year","less","low","well","particular","best")

# remove stopwords from corpus
docs <- tm_map(docs, removeWords, myStopwords)

# change words if necessary (NOT RUN)
# myCorpus <- tm_map(myCorpus, gsub, pattern = "miners", replacement = "mining")

# strip whitespace
docs <- tm_map(docs, stripWhitespace)

# inspect SDG1
writeLines(as.character(docs[[1]]))
# inspect SDG2
writeLines(as.character(docs[[2]]))

# transformation to corpora
docs <- tm_map(docs,stemDocument)

# coerce to a document-term matrix
dtm <- DocumentTermMatrix(docs)
# inspect the document-term matrix
dtm
inspect(dtm[1:5,1:5])

#
freq <- colSums(as.matrix(dtm))
# length should be total number of terms
length(freq)

# create sort order (descending)
ord <- order(freq,decreasing=TRUE)

# list the most and least frequently occurring terms
# inspect most frequently occurring terms
freq[head(ord)]
#inspect least frequently occurring terms
freq[tail(ord)] 

# find the terms with a frequence of >= 50 citations in the corpus
findFreqTerms(dtm,lowfreq=50)
# find the terms with a frequence of >= 50 citations in the corpus
findFreqTerms(dtm,lowfreq=20)

# find which terls are associated with the term "soil", with a correlation >= 0.3
findAssocs(dtm,"soil",0.3)
# find which terls are associated with the term "farmer" with a correlation >= 0.2
findAssocs(dtm,"farmer",0.2)
# find which terls are associated with the term "biodiversity" with a correlation >= 0.3
findAssocs(dtm, "biodivers", 0.3)

# prepare a data.frame
wf=data.frame(term=names(freq),occurrences=freq)

# show the number occurrence of terms with freq >=10
library(ggplot2)
p <- ggplot(subset(wf, freq>=10), aes(term, occurrences))+ 
  geom_bar(stat="identity",fill = "blue")+
  theme_classic()+ theme(axis.text.x=element_text(angle=45, hjust=1))
p

### clustering
tdm <- TermDocumentMatrix(docs, control = list(wordLengths = c(1, Inf)))
a <- as.matrix(tdm)
b <- sort(rowSums(a),decreasing=TRUE)
c <- data.frame(word = names(b),freq=b)
head(c, 10)
# remove sparse terms
dtmr2 <- removeSparseTerms(tdm, sparse = 0.80)
dtmr2 <- as.matrix(dtmr2)
distMatrix <- dist(scale(dtmr2))
# cluster terms
fit <- hclust(distMatrix, method = "ward.D")
# plot the cluster
plot(fit,cex=0.4)
rect.hclust(fit, k = 6) # cut tree into 6 clusters 

#wordcloud
library(wordcloud)
#setting the same seed each time ensures consistent look across clouds
set.seed(42)
#limit words by specifying min frequency
wordcloud(names(freq),freq, min.freq=15)

#add color
wordcloud(names(freq),freq,min.freq=15,colors=brewer.pal(6,"Dark2"))

#
library(wordcloud2)
wordcloud2(data=head(wf,100),rotateRatio = 0.4, shape = 'triangle', ellipticity = 0.65)
wordcloud2(data=head(wf,50), shape = 'triangle')


####  
## Find topics and their terms using the library topicmodels
library(topicmodels)

## reprensation of the topics / terms using "Latent Dirichlet Allocation" algorithm
# find 5 topics (k=5)
ap_lda <- LDA(dtm,method="Gibbs", k = 5)
# find the 5 first terms of every topic
term <- terms(ap_lda, 5) 
term <- apply(term, MARGIN = 2, paste, collapse = ", ")
term

##  term - topic probabilities
library(tidytext)
abstract_topics <- tidy(ap_lda, matrix = "beta")
abstract_topics

library(ggplot2)
library(dplyr)

# term - topic probabilities
ap_top_terms <- abstract_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
ap_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

top_terms <- abstract_topics %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
top_terms

## show the terms that are most common within each topic 
top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()


## another representation of the association terms 
tdm <- TermDocumentMatrix(docs, control = list(wordLengths = c(1, Inf)))
a <- as.matrix(tdm)
b <- sort(rowSums(a),decreasing=TRUE)
c <- data.frame(word = names(b),freq=b)
head(c, 10)

## Freqency terms and Association
idx <- which(dimnames(tdm)$Terms == "ecolog")
inspect(tdm[idx + (0:5), 1:2])

# terms association for terms with frequencey >=25
findAssocs(dtm, terms = "biodivers", corlimit = 0.8)

(freq.terms <- findFreqTerms(dtm, lowfreq = 25))
plot(dtm, term = freq.terms, corThreshold = 0.15, weighting = F, 
     attrs=list(node=list(width=20, fontsize=24, fontcolor="blue", color="red")))

plot.new()
# can change to terms with frequencey >=2O
(freq.terms <- findFreqTerms(dtm, lowfreq = 20))
plot(dtm, term = freq.terms, corThreshold = 0.15, weighting = F, 
     attrs=list(node=list(width=20, fontsize=24, fontcolor="blue", color="red")))


###### prepare the list of SGDs
listSDGs<-list.files("./Text_SDGs")
listSDGs<-data.frame(listSDGs)
listSDGs
row.names(listSDGs) <- listSDGs$listSDGs

## network analysis and network representation
library(ggraph)
library(ggforce)
library(igraph)
library(tnet)

reseau<-data.frame(as.matrix(dtm))
onep<-projecting_tm(reseau, method="Newman")

closeness_w(onep, directed=NULL, gconly=TRUE, precomp.dist=NULL, alpha=1)
clustering_local_w(onep)

### transform to igraph object
onep.i<-tnet_igraph(onep, type="weighted one-mode tnet")
show(onep.i)
summary(onep.i)

# plot
plot.new()
plot(onep.i, layout=layout.fruchterman.reingold)

V(onep.i)$name<-rownames(listSDGs)
rownames(listSDGs)
listSDGs$listSDGs

#  V(onep.i)$name<-list.files("./Abstracts_T")
# V(onep.i)$name
# E(onep.i)$weight
plot.new()
# plot.igraph(onep.i,vertex.label=V(onep.i)$name,
#             layout=layout.fruchterman.reingold, edge.color="black",
#             vertex.color="orange",edge.width=E(onep.i)$weight)

plot.new()
# hist(degree(onep.i))
dg <- decompose.graph(onep.i)
length(dg)
plot(dg[[1]],directed=T)


mc <- maximal.cliques(onep.i)
length(mc)
sapply(mc, length)


### centrality
alpha.centrality(onep.i)
evcent(onep.i)
degree(onep.i)
clusters(onep.i)
closeness(onep.i, mode="in")
closeness_w(onep)

betweenness_w(onep)
betweenness(onep.i)
edge.betweenness(onep.i)


### modularity
### Community structure via greedy optimization of modularity
##  http://kateto.net/netscix2016
# see also Community detection
net.sym <- as.undirected(onep.i, mode= "collapse",
                         edge.attr.comb=list(weight="sum", "ignore"))
plot.new()
plot(net.sym)

fastgreedy.community(onep.i)
fc <- cluster_fast_greedy(onep.i)
membership(fc)
sizes(fc)
plot.new()
dendPlot(fc,mode="hclust")

plot.new()
V(onep.i)$color <- fc$membership+1
g <- set_graph_attr(onep.i, "layout", layout.fruchterman.reingold(onep.i))
plot.new()

plot(g, vertex.label.dist=1.0,
     edge.width=E(onep.i)$weight, edge.color="black")
plot(g,  edge.color="black")


# Finding community structure by multi-level optimization of modularity
# com <- cluster_louvain(onep.i)
# membership(com)
# dendPlot(com)

ceb <- cluster_fast_greedy(onep.i)
dendPlot(ceb)

plot(ceb, onep.i,cex=0.1)



V(onep.i)$color <- com$membership+1
g <- set_graph_attr(onep.i, "layout", layout.fruchterman.reingold(onep.i))
plot(g, vertex.label.dist=1.5,
     edge.color="black")

# node.size= c(10,10,10)
# vertex.size=node.size*1
plot(g,  edge.color="black")

comp.df <- as.data.frame(get.edgelist(onep.i))

clp <- cluster_label_prop(onep.i)
plot(clp, onep.i)

cfg <- cluster_fast_greedy(as.undirected(onep.i))


plot(cfg, as.undirected(onep.i),
     rescale = FALSE, ylim=c(-4,4),xlim=c(-4,4), asp = 0,
     vertex.size=20,
     cex=0.2,
     shape="csquare")

