library(tidyverse)
library(bib2df)

#!> read bib text files
bib <- bib2df::bib2df("my_pubs.bib") %>% arrange(YEAR,AUTHOR)


yamllist <- list()


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
    
    filename <- paste0(word(authorlist[1]), "_", bib$YEAR[[i]], "_", name, ".md")
    
    
    
fileConn <- file("test.html")

writeLines(
  c(
    "<table style='width:100%'>",
    "<tr>",
      "<td width =80%> January </td>",
      "<td width =20%>",
    "<span class='__dimensions_badge_embed__' data-doi='10.1001/jama.2016.9797' data-style='large_rectangle'></span><script async src='https://badge.dimensions.ai/badge.js' charset='utf-8'></script>"
      "</td>",
      "</tr>",
     "</table>"
  ),
  fileConn
)

close(fileConn)
    
    
    
    <table style="width:100%">
      <tr>
      <td width =80%>January</td>
      <td width =20%>$100</td>
      </tr>
      </table>
    
    
    
    
  }
}
  












   typ <- paste0("type: ", "'", paste(bib$CATEGORY[[i]]),"'")
  col <- paste0("collection: ", "publications")
  au <- paste0("author: ", paste0(paste(authorlist[-no_au], collapse = ", ")), " & ", last(authorlist))
  yr <- paste0("year: '", paste(bib$YEAR[[i]]), "'")
  tit <- paste0("title: ", paste(bib$TITLE[[i]]))
  jou <- paste0("journal: ", paste(bib$JOURNAL[[i]]))
  vol <- paste0("volume: ", paste(bib$VOLUME[[i]]))
  pge <- paste0("pages: ", paste(bib$PAGES[[i]]))
  doi <- paste0("doi: ", paste(bib$DOI[[i]]))
  url <- paste0("url: ", paste(bib$URL[[i]]))
  name <- paste0("article", i, sep = "")
  
  tmp <- list(typ = typ, col=col, au = au, yr = yr, tit = tit, jou = jou, vol = vol, pge = pge, doi = doi, url = url)
  yamllist[[name]] <- tmp
  


