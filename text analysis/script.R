rm(list = ls())
library(slam)
library(tm)
library(SnowballC)
library(igraph)
library(igraphdata)

# get file path to the documents folder
cname = file.path(".", "documents")
cname

#create corpus
docs = Corpus(DirSource(cname))
summary(docs)

# hyphen1 to space
toSpace = content_transformer(function(x, pattern) gsub(pattern, " ", x))
docs = tm_map(docs, toSpace, "-")

# hyphen2 to space
toSpace = content_transformer(function(x, pattern) gsub(pattern, " ", x))
docs = tm_map(docs, toSpace, "–")

#hyphen3 to space
toSpace = content_transformer(function(x, pattern) gsub(pattern, " ", x))
docs = tm_map(docs, toSpace, "—")


#tokenization 
docs = tm_map(docs, removeNumbers)
docs = tm_map(docs, removePunctuation)
docs = tm_map(docs, content_transformer(tolower))
docs = tm_map(docs, removeWords, stopwords("english"))

#Stemming
docs = tm_map(docs, stemDocument, language = "english")

#remove miscellaneous words
toSpace = content_transformer(function(x, pattern) gsub(pattern, " ", x))
docs = tm_map(docs, toSpace, "can")
docs = tm_map(docs, toSpace, "media")
docs = tm_map(docs, toSpace, "social")
docs = tm_map(docs, toSpace, "platform")
docs = tm_map(docs, toSpace, "use")
docs = tm_map(docs, toSpace, "user")
docs = tm_map(docs, toSpace, "also")
docs = tm_map(docs, toSpace, "like")

#remove leading whitespace
docs = tm_map(docs, stripWhitespace)

#sample document
writeLines(as.character(docs[11]))

#create Document-Term Matrix
dtm = DocumentTermMatrix(docs)
inspect(dtm)
dim(dtm)

# remove sparse terms
dtm = removeSparseTerms(dtm, 0.3)
dim(dtm)
View(dtm)
#inspect(dtm[1:5, 1:15])

# save as csv
dtm = as.matrix(dtm)
write.csv(dtm, "dtm.csv")

# hierarchical clustering
distmatrix = proxy::dist(dtm, method = "cosine")
fit = hclust(distmatrix, method = "ward.D")
#View(fit)
plot(fit, hang = -1)
rect.hclust(fit, k = 3, border = 2:6)

fit$height

#create topic labels vector
topics = c("market", "market", "market", "market", "market", 
           "mental", "mental", "mental", "mental", "mental", 
           "security", "security", "security", "security", "security")

groups = cutree(fit, k=3)

# table of labels vs cluster
table = table(groups = topics, clusters = groups)

# create matrix and reorder columns
cluster.matrix = as.data.frame.matrix(table)

cluster.matrix = cluster.matrix[c(2, 3, 1)]
cluster.matrix

# cluster accuracy calculated using every cluster
cluster.acc = (4+4+4)/(15)
cluster.acc










# DOCUMENT










# single-mode document network
# convert dtm to binary matrix
dtm.bin = as.matrix((dtm > 0) + 0)

# multiply binary matrix by its transpose
# this shows the connections between each document
by.abs.matrix = dtm.bin %*% t(dtm.bin)

# diagonal 0
diag(by.abs.matrix) = 0

#create abstracts graph
by.abs = graph_from_adjacency_matrix(by.abs.matrix, mode = "undirected", weighted = TRUE)

set.seed(33550166)
plot(by.abs)

degree(by.abs)
format(betweenness(by.abs), digits = 2)
format(closeness(by.abs), digits = 2)
format(eigen_centrality(by.abs)$vector, digits = 2)

# convert betweenness centrality for each document to numeric
abs.betw = as.numeric(betweenness(by.abs))

# create a vector of betweenness
v = abs.betw

# create a vector of colors for significant documents (high betweenness)
abs.colors = ifelse(v > 1, "aquamarine", "white")

set.seed(33550166)
plot(by.abs, 
     layout = layout_nicely(by.abs),
     vertex.color = abs.colors,
     main = "Abstracts/Documents Network")
abs.plot.labels = c("Significant", "Less significant")
abs.plot.colors = c("aquamarine", 'white')
legend("topright", abs.plot.labels, fill = abs.plot.colors)

set.seed(33550166)
plot(cluster_optimal(by.abs), by.abs)







# TOKEN











# convert dtm to binary matrix
dtm.bin = as.matrix((dtm > 0) + 0)

# token matrix showing connections between each token
by.token.matrix = t(dtm.bin) %*% dtm.bin

#diagonal 0
diag(by.token.matrix) = 0

# create token graph
by.token = graph_from_adjacency_matrix(by.token.matrix, mode = "undirected", weighted = TRUE)
set.seed(33550166)
plot(by.token)

degree(by.token)
format(betweenness(by.token), digits = 2)
format(closeness(by.token), digits = 2)
format(eigen_centrality(by.token)$vector, digits = 2)

# make the edge weights the power for 2 and divide by 1000
# to show the differences between edge weights
edges = (2^(E(by.token)$weight))/1000
edges

edge.colors = ifelse(edges > 2, "red", "gray")

set.seed(33550166)
plot(by.token,
     layout = layout_nicely(by.token), # set layout
     edge.width = edges,               # set edge width
     edge.color = edge.colors,         # set edge colors
     main = "Tokens/Words Network")

# vectors of labels and its corresponding color
token.plot.labels = c("Significant", "Less significant")
token.plot.colors = c("red", "gray")

legend("topright", token.plot.labels, fill = token.plot.colors, pch = c(3, 3))

# apply clustering to the nodes
token.cluster = cluster_optimal(by.token)

set.seed(33550166)
plot(token.cluster,
     by.token,
     layout = layout_nicely(by.token),
     main = "Tokens/Words Network")










# BIPARTITE








# create a copy of dtm
dtm1 = as.data.frame(dtm)
dtm1$ABS = rownames(dtm1)

# create empty data frame
dtm2 = data.frame()
for(i in 1:nrow(dtm1)){
  for(j in 1:(ncol(dtm1) - 1)){
    to.use = cbind(dtm1[i,j], dtm1[i, ncol(dtm1)], colnames(dtm1[j])) # weight, abs, token
    dtm2 = rbind(dtm2, to.use)
  }
}

colnames(dtm2) = c("weight", "abs", "token") #label columns
dtm.bip = dtm2[dtm2$weight != 0,] #delete 0 weights
dtm.bip = dtm.bip[, c(2,3,1)] #rearrange

# create bipartite elements for the graph
bipartite.g = graph.data.frame(dtm.bip, directed = FALSE)
bipartite.mapping(bipartite.g)
V(bipartite.g)$type = bipartite_mapping(bipartite.g)$type
V(bipartite.g)$color = ifelse(V(bipartite.g)$type, "lightgreen", "pink")
V(bipartite.g)$shape = ifelse(V(bipartite.g)$type, "circle", "square")
E(bipartite.g)$color = "lightgray"

set.seed(33550166)
plot(bipartite.g,
     main = "Bipartite Document and Term Network")

bip.labels = c("tokens/words", "abstracts/documents")
bip.colors = c("lightgreen", "pink")

# Adding a legend for different shapes
legend("topright", legend = bip.labels,
       col = bip.colors, pch = c(21, 22),
       pt.bg = bip.colors, pt.cex = 2, cex = 0.8, bty = "n", ncol = 1)


# centrality measures
degree(bipartite.g)
format(betweenness(bipartite.g), digits = 2)
format(closeness(bipartite.g), digits = 2)
format(eigen_centrality(bipartite.g)$vector, digits = 2)

#create a cluster object for the bipartite network
bip.cluster = cluster_optimal(bipartite.g)

set.seed(33550166)
plot(bip.cluster,
     bipartite.g,
     layout = layout_nicely(bipartite.g),
     main = "Bipartite Document and Term Network")

# convert betweenness centrality for each node to numeric
bip.between = as.numeric(betweenness(bipartite.g))

# list of edge weights
bip.edges = as.numeric(E(bipartite.g)$weight)
bip.edge.colors = ifelse(bip.edges>15, "tomato", "gray")

set.seed(33550166)
plot(bipartite.g,
     layout = layout_nicely(bipartite.g),
     edge.width = bip.edges,
     edge.color = bip.edge.colors,
     main = "Bipartite Document and Term Network")

# Adding a legend for different shapes
legend("topright", legend = bip.labels,
       col = bip.colors, pch = c(21, 22),
       pt.bg = bip.colors, pt.cex = 2, cex = 0.8, bty = "n", ncol = 1)

## remove multiplotting
#dev.off()
