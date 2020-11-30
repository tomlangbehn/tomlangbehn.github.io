library(tidyverse)
library(bib2df)

#!> read bib text files
bib <- bib2df::bib2df("my_pubs.bib") %>% arrange(YEAR,AUTHOR)


yamllist <- list()
yamllist2 <- list()

for (i in 1:nrow(bib)) {
  no_au <- length(bib$AUTHOR[[i]])
  
  authorlist <- bib$AUTHOR[[i]]
  for (j in 1:no_au) {
    lastname <- bib$AUTHOR[[i]][j] %>%
      str_remove(., "[,]") %>%
      str_remove(., "[.]") %>%
      word(1)
    
    initital <- bib$AUTHOR[[i]][j] %>%
      str_remove(., "[,]") %>%
      str_remove(., "[.]") %>%
      str_split(" ") %>%
      unlist() %>%
      .[-1] %>%
      str_sub(., 1, 1) %>%
      paste(., sep = "", collapse = "")
    authorlist[j] <- paste(lastname, initital)
  }
  
  typ <- paste0("type: ",paste(bib$CATEGORY[[i]]))
  col <- paste0("collection: ", "publications")
  au <- paste0("author: ", paste0(paste(authorlist[-no_au], collapse = ", ")), " & ", last(authorlist))
  yr <- paste0("year: ", paste(bib$YEAR[[i]]))
  tit <- paste0("title: ", paste(bib$TITLE[[i]]))
  jou <- paste0("journal: ", paste(bib$JOURNAL[[i]]))
  vol <- paste0("volume: ", paste(bib$VOLUME[[i]]))
  pge <- paste0("pages: ", paste(bib$PAGES[[i]]))
  doi <- paste0("doi: ", paste(bib$DOI[[i]]))
  url <- paste0("url: ", paste(bib$URL[[i]]))
  name <- paste0("article", i, sep = "")

  tmp <- list(typ = typ, col=col, au = au, yr = yr, tit = tit, jou = jou, vol = vol, pge = pge, doi = doi, url = url)
  yamllist[[name]] <- tmp
  

  filename <- paste0(word(authorlist[1]), "_", bib$YEAR[[i]], "_", name, ".md")
  abst <- paste0(bib$ABSTRACT[[i]])
  tmp2 <- list(abst = abst, filename=filename)
  yamllist2[[name]] <- tmp2
}


for (i in 1:length(yamllist)) {
  fileConn<-file(yamllist2[[i]]$filename)
  writeLines(c("---",
               paste0(unlist(yamllist[i])),
               "---",
               paste0(yamllist2[[i]]$abst)), 
             fileConn)
  close(fileConn)
}




# for (i in 1:length(yamllist)) {
#   cat(paste0("---", sep = "\n"),
#       paste0(unlist(yamllist[i]), sep = "\n"), 
#       paste0("---", sep = "\n"), 
#       paste0("\n", yamllist2[[i]]$abst, sep = "\n"), file = yamllist2[[i]]$filename)
# }
