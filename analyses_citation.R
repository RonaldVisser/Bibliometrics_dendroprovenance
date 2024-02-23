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

dendroprovenance_combined_scopus <- convert2df(file="data/scopus_combined_words.csv", dbsource="scopus",format="csv")
dendroprovenance_combined_wos <- convert2df(file="data/wos_combined_words.ciw", dbsource="wos",format="endnote")
# combine sources
dendroprovenance_combined <- mergeDbSources(dendroprovenance_combined_wos, dendroprovenance_combined_scopus)
# remove duplicates based on SR 
dendroprovenance_combined <- dendroprovenance_combined[!duplicated(dendroprovenance_combined$SR, fromLast = TRUE),]

# exporting for easy loading
saveRDS(dendroprovenance_combined, "data/dendroprovance_lit.rds")
dendroprovenance_combined <- readRDS("data/dendroprovance_lit.rds")

# correct spelling of Domínguez-Delmás and Ważny
dendroprovenance_combined$AU <- str_replace(dendroprovenance_combined$AU, "Dominguez-Delmas M.", "Domínguez-Delmás M.")
dendroprovenance_combined$AU <- str_replace(dendroprovenance_combined$AU, "Dominguez-Delmás M.", "Domínguez-Delmás M.")
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


# keywords related to the Roman period and Compared to medieval
sum(stringr::str_detect(dendroprovenance_combined$DE, "ROMAN|ROMAN ARCHAEOLOGY|ROMAN PERIOD"), na.rm = TRUE)
dendroprovenance_combined$AU[stringr::str_detect(dendroprovenance_combined$DE, "ROMAN|ROMAN ARCHAEOLOGY|ROMAN PERIOD")]
sum(stringr::str_detect(dendroprovenance_combined$DE, "MIDDLE AGES|MEDIEVAL|MODERN|BALTIC"), na.rm = TRUE)
dendroprovenance_combined$AU[stringr::str_detect(dendroprovenance_combined$DE, "MIDDLE AGES|MEDIEVAL|MODERN|BALTIC")]

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

# plot publication year with trend visualised (excluding 2024)
melt(dendroprovenance_combined$PY) %>% 
  filter(value<2024) %>% 
  group_by(value) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(x=value,y=n)) + geom_col() + 
    xlab("Publication year") +  ylab("Number of publications") +
    geom_smooth(level = 0.68) + coord_cartesian(ylim = c(0,21)) + scale_y_continuous(expand = c(0, 0))
ggsave("export/publication_year_trend.png", width = 8, height = 6)

dendroprovenance_combined %>%
  mutate(OpenAccess = is.na(OA)) %>% 
  group_by(PY, OpenAccess) %>%
  tally() %>%
  spread(PY, n) %>% 
  melt() %>% 
  ggplot(aes(x=variable, y=value, fill=OpenAccess)) + geom_col() +
  labs(x="Publication year", y="Number of publications", fill="Open Access") +
  scale_fill_manual("Open Access", values = c("brown1", "darkolivegreen3"), labels = c("No", "Yes")) +
  theme(axis.text.x=element_text(angle = -90, hjust = 0))
ggsave("export/publication_year_openaccess.png", width = 8, height = 6)

ggplot(melt(dendroprovenance_combined_results$CountryCollaboration), 
       aes(y=str_to_title(Country), x = value, fill=variable)) + geom_bar(stat = "identity") +
  scale_fill_discrete(name = "Publication type", labels = c("Single Country Paper", "Multi Country Paper")) +
  labs(x="Number of publications", y="Country")
ggsave("export/publication_country.png", width = 10, height = 8)

melt(dendroprovenance_combined_results$Authors) %>%
  filter(value>=10) %>% 
  ggplot(aes(y=str_to_title(AU), x = value)) + geom_bar(stat = "identity") + 
  ylab("Author") + xlab("Number of papers") + scale_x_continuous(breaks = c(5,10))
ggsave("export/author_papers_10.png", width = 5, height = 4)


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
#createVisualStyle("journal-co-citation")
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
createNetworkFromIgraph(net_authors_50$graph, title = "Author collaboration (n=50)")
layoutNetwork("kamada-kawai")
setVisualStyle("default black")
exportImage("export/author_collaboration_network.png")


#net_authors_25 <- networkPlot(NetMatrix,  n = 25, Title = "Author collaboration",type = "auto", size=5,size.cex=T,edgesize = 3,labelsize=1)

#Edu collaboration network
NetMatrix <- biblioNetwork(dendroprovenance_combined, analysis = "collaboration",  network = "universities", sep = ";")
net_edu_collab_50 <- networkPlot(NetMatrix,  n = 50, Title = "Edu collaboration",type = "auto", size=4,size.cex=F,edgesize = 3,labelsize=1)
#net_edu_collab_25 <- networkPlot(NetMatrix,  n = 25, Title = "Edu collaboration",type = "auto", size=4,size.cex=F,edgesize = 3,labelsize=1)

# sankey plots
# these are plotly plots and we need kaleido to export:
# install.packages('reticulate')
#reticulate::install_miniconda()
#reticulate::conda_install('r-reticulate', 'python-kaleido')
#reticulate::conda_install('r-reticulate', 'plotly', channel = 'plotly')
reticulate::use_miniconda('r-reticulate')

fig <- threeFieldsPlot(dendroprovenance_combined, fields = c("DE", "AU", "AU_CO"))
plotly::save_image(fig, "export/keywords_author_country_sankey.svg")

fig <- threeFieldsPlot(dendroprovenance_combined, fields = c("PU", "OA", "AU_CO"))
plotly::save_image(fig, "export/publishers_openaccess_country_sankey.svg")

fig <- threeFieldsPlot(dendroprovenance_combined, fields = c("DT", "DE", "AU_CO"))
plotly::save_image(fig, "export/publication_keywords_country_sankey.svg")

fig <- threeFieldsPlot(dendroprovenance_combined, fields = c("ID", "DE", "AU_CO"))
plotly::save_image(fig, "export/ScopusWosKeyword_keywords_country_sankey.svg")

threefield <- dendroprovenance_combined
threefield$TC <- as.character(threefield$TC)
threefield$PY <- as.character(threefield$PY)
fig <- threeFieldsPlot(threefield, fields = c("PY", "TC", "DE"))
plotly::save_image(fig, "export/year_citation_count_keyword_sankey.svg")
fig <- threeFieldsPlot(threefield, fields = c("PY", "TC", "AU_CO"))
plotly::save_image(fig, "export/year_citation_count_country_sankey.svg")
threeFieldsPlot(threefield, fields = c("AU_CO", "PY", "OA"))

dendroprovenance_combined %>% 
  ggplot(aes(x=PY, y=TC)) + geom_point()

