library(bibliometrix)
library(reshape2)
library(ggplot2)
#library(ggdist)
library(magrittr)
library(dplyr)
library(RCy3)
library(wordcloud)
library(stringr)
library(tidyr)

# combining searches in scopus search in article title, abstract and keywords
# (dendroprovenance) OR (dendroprovenancing) OR (dendro-provenance) OR (dendro-provenancing) OR (tree-ring AND provenance AND archaeology) OR (tree-ring AND provenancing AND archaeology) OR (dendrochronology AND provenance AND archaeology) OR (dendrochronology AND provenancing and archaeology) OR (tree-ring AND provenance and painting) OR (dendrochronology AND provenance AND painting) OR (dendrochronology AND provenancing and painting) OR (tree-ring AND provenance AND heritage)  OR (dendrochronology AND provenance AND heritage) OR (dendrochronology AND provenancing AND heritage)


#dendroprovenance_combined_scopus <- convert2df(file="data/scopus_combined_words.bib", dbsource="scopus",format="bibtex")
dendroprovenance_combined_scopus <- convert2df(file="data/scopus_combined_words.csv", dbsource="scopus",format="csv")
dendroprovenance_combined_wos <- convert2df(file="data/wos_combined_words.ciw", dbsource="wos",format="endnote")
#dendroprovenance_theses_wos <- convert2df(file="data/wos_combined_words_dissertations.ciw", dbsource="wos",format="endnote")
# combine sources
dendroprovenance_combined <- mergeDbSources(dendroprovenance_combined_wos, dendroprovenance_combined_scopus)
# remove duplicates based on SR and/or DI
#dendroprovenance_combined <- dendroprovenance_combined[!((duplicated(dendroprovenance_combined$SR) + duplicated(dendroprovenance_combined$DI)) == 2),]
dendroprovenance_combined <- dendroprovenance_combined[!duplicated(dendroprovenance_combined$SR, fromLast = TRUE),]

# exporting for easy loading
saveRDS(dendroprovenance_combined, "data/dendroprovance_lit.rds")
dendroprovenance_combined <- readRDS("data/dendroprovance_lit.rds")

# correct spelling of Domínguez-Delmás
dendroprovenance_combined$AU <- str_replace(dendroprovenance_combined$AU, "Dominguez-Delmas M.", "Domínguez-Delmás M.")
dendroprovenance_combined$AU <- str_replace(dendroprovenance_combined$AU, "Wazny T.", "Ważny T.")




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
  ggplot(aes(y=AU, x = value)) + geom_bar(stat = "identity") + 
  ylab("Author") + xlab("Count") + scale_x_continuous(breaks = c(5,10))
ggsave("export/author_papers_6.png", width = 10, height = 8)


CR <- citations(dendroprovenance_combined, field = "article", sep = ";")
cbind(CR$Cited[1:25])




# network
NetMatrix <- biblioNetwork(dendroprovenance_combined, analysis = "co-citation", network = "references", sep = ";")
co_citation_net=networkPlot(NetMatrix, n = 25, Title = "Co-Citation Network", type = "fruchterman", size.cex=TRUE, size=20, remove.multiple=FALSE, labelsize=1,edgesize = 10, edges.min=5)
# export to Cytoscape
createNetworkFromIgraph(co_citation_net$graph, title = "Co-citation network")
setVisualStyle("default black")
layoutNetwork("kamada-kawai")
exportImage("export/co-citation-network.png")


# journal co-citation network
dendroprovenance_combined <- metaTagExtraction(dendroprovenance_combined,"CR_SO",sep=";")
NetMatrix <- biblioNetwork(dendroprovenance_combined, analysis = "co-citation", network = "sources", sep = ";")
co_citation_journal_net=networkPlot(NetMatrix, n = 30, Title = "Co-Citation Network", type = "auto", size.cex=TRUE, size=15, remove.multiple=FALSE, labelsize=1,edgesize = 10, edges.min=5)
# export to Cytoscape
createNetworkFromIgraph(co_citation_journal_net$graph, title = "Co-citation network (journal)")
createVisualStyle("journal-co-citation")
layoutNetwork("kamada-kawai")
setVisualStyle("default black")
exportImage("export/co-citation-network_journal.png")


# Historiograph - Direct citation linkages
histResults <- histNetwork(dendroprovenance_combined[dendroprovenance_combined$CR!="",], sep = ";")
options(width = 130)
net_hist_200 <- histPlot(histResults, n=200, size = 3, labelsize = 3)
# removing logo Bibliometrix, because of the ugly placement
net_hist_200$g$layers[[4]] <- NULL
plot(net_hist_200$g)
ggsave("export/historiograph_n200.png", width = 14, height = 10)
createNetworkFromIgraph(net_hist_200$net, title = "Historiograph_200")

net_hist_100 <- histPlot(histResults, n=100, size = 3, labelsize = 3)
# removing logo Bibliometrix, because of the ugly placement
net_hist_100$g$layers[[4]] <- NULL
plot(net_hist_100$g)
ggsave("export/historiograph_n100.png", width = 14, height = 10)
createNetworkFromIgraph(net_hist_100$net, title = "Historiograph_100")

net_hist_75 <- histPlot(histResults, n=75, size = 3, labelsize = 3)
# removing logo Bibliometrix, because of the ugly placement
net_hist_75$g$layers[[4]] <- NULL
plot(net_hist_75$g)
ggsave("export/historiograph_n100.png", width = 14, height = 10)
createNetworkFromIgraph(net_hist_75$net, title = "Historiograph_75")


net_hist_50 <- histPlot(histResults, n=50, size = 3, labelsize = 3)
# removing logo Bibliometrix, because of the ugly placement
net_hist_50$g$layers[[4]] <- NULL
plot(net_hist_50$g)
ggsave("export/historiograph_n50.png", width = 14, height = 10)
createNetworkFromIgraph(net_hist_50$net, title = "Historiograph_50")


# keyword co-occurences
NetMatrix_keywords <- biblioNetwork(dendroprovenance_combined, analysis = "co-occurrences", network = "keywords", sep = ";")
net_keyword_co_occurrence <- networkPlot(NetMatrix_keywords, normalize="association", n = 35, Title = "Keyword Co-occurrences", type = "fruchterman", size.cex=TRUE, size=20, remove.multiple=F, edgesize = 10, labelsize=5,label.cex=TRUE,label.n=30,edges.min=2)


keyword_count <- as.data.frame(table(tolower(str_trim(unlist(strsplit(dendroprovenance_combined$DE, ";"))))))
colnames(keyword_count) <- c("keyword", "freq")
png("export/wordcloud_keywords.png", width=12,height=8, units='in', res=600)
wordcloud(keyword_count$keyword, keyword_count$freq, min.freq = 2,  random.order=FALSE, scale=c(4, .2), rot.per=.15, colors=brewer.pal(8,"Dark2"))
dev.off()


keyword_count %>% 
  arrange(desc(freq)) %>% 
  filter(freq > 5)

author_keyword <- dendroprovenance_combined %>% 
  select(AU, DE) %>%
  mutate(AU = strsplit(AU, ";")) %>% 
  unnest(AU) %>% 
  mutate(DE = strsplit(DE, ";")) %>%
  unnest(DE)
author_keyword$AU <- tolower(str_trim(author_keyword$AU))
author_keyword$DE <- tolower(str_trim(author_keyword$DE))
author_keyword <- melt(table(author_keyword))

author_keyword %>% 
  filter(DE =="dendroprovenancing") %>% 
  filter(value > 0) %>% 
  arrange(desc(value))

author_keyword %>% 
  filter(DE =="dendroprovenance") %>% 
  filter(value > 0) %>% 
  arrange(desc(value))

author_keyword %>% 
  filter(value > 2) %>% 
  arrange(desc(value))


# Co-word Analysis through Correspondence Analysis
suppressWarnings(
  CS <- conceptualStructure(dendroprovenance_combined, method="MCA", field="ID", minDegree=5, clust=5, stemming=FALSE, labelsize=15,documents=50)
)

# Author collaboration network
NetMatrix <- biblioNetwork(dendroprovenance_combined, analysis = "collaboration",  network = "authors", sep = ";")
net_authors_50 <- networkPlot(NetMatrix,  n = 50, Title = "Author collaboration",type = "auto", size=5,size.cex=T,edgesize = 3,labelsize=1)

#net_authors_25 <- networkPlot(NetMatrix,  n = 25, Title = "Author collaboration",type = "auto", size=5,size.cex=T,edgesize = 3,labelsize=1)

#Edu collaboration network
NetMatrix <- biblioNetwork(dendroprovenance_combined, analysis = "collaboration",  network = "universities", sep = ";")
net_edu_collab_50 <- networkPlot(NetMatrix,  n = 50, Title = "Edu collaboration",type = "auto", size=4,size.cex=F,edgesize = 3,labelsize=1)
#net_edu_collab_25 <- networkPlot(NetMatrix,  n = 25, Title = "Edu collaboration",type = "auto", size=4,size.cex=F,edgesize = 3,labelsize=1)

