---
bibliography: references.bib
---

# Bibliometrics_dendroprovenance

This repository consists of a bibliometric analyses of literature related to dendroprovenance. This will be published as:

For the analyses the R-package Bibliometrix [@aria2017] was used in R in combination with reshape [@wickham2007], ggplot [@wickham2016], dplyr [@wickham2023], RCy3 [@gustavsen2019], wordcloud [@fellows2022], stringr [@wickham2023] and tidyr [@wickham2023a].

The data was obtained from Scopus (<https://www.scopus.com/>) and Web of Science (WoS: <https://clarivate.com/products/scientific-and-academic-research/research-discovery-and-workflow-solutions/webofscience-platform/>) on February 22, 2024 and transformed and combined using Bibliometrix and dplyr.

If you use this software please cite this with:

```{r}
cffr::cff_to_bibtex("CITATION.cff")
```

# References
