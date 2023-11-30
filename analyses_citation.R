library(bibliometrix)
library(reshape2)
library(ggplot2)
#library(ggdist)
library(magrittr)
library(dplyr)
library(RCy3)

# combining searches in scopus search in article title, abstract and keywords
# (dendroprovenance) OR (dendroprovenancing) OR (dendro-provenance) OR (tree-ring AND provenance) OR (dendrochronology AND provenance)
#dendroprovenance_combined_scopus <- convert2df(file="data/scopus_combined_words.bib", dbsource="scopus",format="bibtex")
dendroprovenance_combined_scopus <- convert2df(file="data/scopus_combined_words.csv", dbsource="scopus",format="csv")
dendroprovenance_combined_wos <- convert2df(file="data/wos_combined_words.ciw", dbsource="wos",format="endnote")
# combine sources
dendroprovenance_combined <- mergeDbSources(dendroprovenance_combined_wos, dendroprovenance_combined_scopus)
# remove duplicates based on SR and/or DI
#dendroprovenance_combined <- dendroprovenance_combined[!((duplicated(dendroprovenance_combined$SR) + duplicated(dendroprovenance_combined$DI)) == 2),]
dendroprovenance_combined <- dendroprovenance_combined[!duplicated(dendroprovenance_combined$SR, fromLast = TRUE),]

# exporting for easy loading
saveRDS(dendroprovenance_combined, "data/dendroprovance_lit.rds")
dendroprovenance_combined <- readRDS("data/dendroprovance_lit.rds")


# combined keywords dendroprovenance or dendroprovenancing or dendro-provenance or (dendrochronology and provenance) or (wood and provenance and (archaeology or heritage or painting or building)) or (tree-ring and provenance and (archaeology or heritage or painting or building)) 
dendroprovenance_combined_scopus_results <- biblioAnalysis(dendroprovenance_combined_scopus)
sink("export/dendroprovenance_combined_scopus_results.txt")
summary(dendroprovenance_combined_scopus_results, k=10, pause=F, width=130) 
sink(file = NULL)
plot(x=dendroprovenance_combined_scopus_results, k=10, pause=F)


# combined keywords dendroprovenance or dendroprovenancing or dendro-provenance or (dendrochronology and provenance) or (wood and provenance and (archaeology or heritage or painting or building)) or (tree-ring and provenance and (archaeology or heritage or painting or building)) 
dendroprovenance_combined_wos_results <- biblioAnalysis(dendroprovenance_combined_wos)
sink("export/dendroprovenance_combined_wos_results.txt")
summary(dendroprovenance_combined_wos_results, k=10, pause=F, width=130) 
sink(file = NULL)
plot(x=dendroprovenance_combined_wos_results, k=10, pause=F)

# combined analyses
dendroprovenance_combined_results <- biblioAnalysis(dendroprovenance_combined)
sink("export/dendroprovenance_combined_results.txt")
summary(dendroprovenance_combined_results, k=10, pause=F, width=130) 
sink(file = NULL)
plot(x=dendroprovenance_combined_results, k=10, pause=F)

ggplot(dendroprovenance_combined, aes(x=PY, fill = DT)) + geom_histogram(binwidth = 1) + 
  xlab("Publication years") + scale_fill_discrete(name = "Publication type")
ggsave("export/publication_year_type.png", width = 10, height = 8)

melt(dendroprovenance_combined$PY) %>% 
  ggplot(aes(x=value)) + geom_histogram(binwidth = 1) + xlab("Publication year") +
  ylab("Number of publications")
ggsave("export/publication_year.png", width = 8, height = 6)

melt(dendroprovenance_combined$PY) %>% 
  ggplot(aes(x=value)) + xlab("Publication year") +  ylab("Number of publications") +
  geom_histogram(binwidth = 1) + geom_density(aes(y = ..count..), alpha=0.5)
ggsave("export/publication_year_density.png", width = 8, height = 6)


ggplot(melt(dendroprovenance_combined_results$CountryCollaboration), 
       aes(y=Country, x = value, fill=variable)) + geom_bar(stat = "identity") +
  scale_fill_discrete(name = "Publication type", labels = c("Single Country Paper", "Multi Country Paper"))
ggsave("export/publication_country.png", width = 10, height = 8)

melt(dendroprovenance_combined_results$Authors) %>%
  filter(value>=6) %>% 
  ggplot(aes(y=AU, x = value)) + geom_bar(stat = "identity") + ylab("Author") + xlab("Count")
ggsave("export/author_papers_6.png", width = 10, height = 8)


CR <- citations(dendroprovenance_combined, field = "article", sep = ";")
cbind(CR$Cited[1:25])




# network
NetMatrix <- biblioNetwork(dendroprovenance_combined, analysis = "co-citation", network = "references", sep = ";")
co_citation_net=networkPlot(NetMatrix, n = 25, Title = "Co-Citation Network", type = "fruchterman", size.cex=TRUE, size=20, remove.multiple=FALSE, labelsize=1,edgesize = 10, edges.min=5)
# export to Cytoscape
createNetworkFromIgraph(co_citation_net$graph, title = "Co-citation network")
setVisualStyle("default black")
setLayoutProperties(edge_attribute = 'similarity', iterations = 1000)
layoutNetwork("kamada-kawai")

# journal co-citation network
dendroprovenance_combined <- metaTagExtraction(dendroprovenance_combined,"CR_SO",sep=";")
NetMatrix <- biblioNetwork(dendroprovenance_combined, analysis = "co-citation", network = "sources", sep = ";")
co_citation_journal_net=networkPlot(NetMatrix, n = 30, Title = "Co-Citation Network", type = "auto", size.cex=TRUE, size=15, remove.multiple=FALSE, labelsize=1,edgesize = 10, edges.min=5)
# export to Cytoscape
createNetworkFromIgraph(co_citation_journal_net$graph, title = "Co-citation network (journal)")




# Historiograph - Direct citation linkages
histResults <- histNetwork(dendroprovenance_combined[dendroprovenance_combined$CR!="",], sep = ";")
options(width = 130)
net_hist <- histPlot(histResults, n=200, size = 5, labelsize = 4)
createNetworkFromIgraph(net_hist$net, title = "Historiograph_200")
ggsave("export/historiograph_n200.png", width = 14, height = 10)
net_hist <- histPlot(histResults, n=65, size = 5, labelsize = 4)
ggsave("export/historiograph_n60.png", width = 14, height = 10)
createNetworkFromIgraph(net_hist$net, title = "Historiograph_60")


# keyword co-occurences
NetMatrix <- biblioNetwork(dendroprovenance_combined, analysis = "co-occurrences", network = "keywords", sep = ";")
net_keyword_co_scopus <- networkPlot(NetMatrix, normalize="association", n = 35, Title = "Keyword Co-occurrences", type = "fruchterman", size.cex=TRUE, size=20, remove.multiple=F, edgesize = 10, labelsize=5,label.cex=TRUE,label.n=30,edges.min=2)

# Co-word Analysis through Correspondence Analysis
suppressWarnings(
  CS <- conceptualStructure(dendroprovenance_combined, method="MCA", field="ID", minDegree=5, clust=5, stemming=FALSE, labelsize=15,documents=50)
)

# Author collaboration network
NetMatrix <- biblioNetwork(dendroprovenance_combined, analysis = "collaboration",  network = "authors", sep = ";")
net_authors_50 <- networkPlot(NetMatrix,  n = 50, Title = "Author collaboration",type = "auto", size=5,size.cex=T,edgesize = 3,labelsize=1)
net_authors_25 <- networkPlot(NetMatrix,  n = 25, Title = "Author collaboration",type = "auto", size=5,size.cex=T,edgesize = 3,labelsize=1)

#Edu collaboration network
NetMatrix <- biblioNetwork(dendroprovenance_combined, analysis = "collaboration",  network = "universities", sep = ";")
net_edu_collab_50 <- networkPlot(NetMatrix,  n = 50, Title = "Edu collaboration",type = "auto", size=4,size.cex=F,edgesize = 3,labelsize=1)
net_edu_collab_25 <- networkPlot(NetMatrix,  n = 25, Title = "Edu collaboration",type = "auto", size=4,size.cex=F,edgesize = 3,labelsize=1)

