library(tidyverse)


library(bib2df)




bib <- bib2df::bib2df("my_pubs.bib") 

yamllist <- list()
for (i in 1:nrow(bib)) { 
  au <- paste0("author: ", paste(bib$AUTHOR[[i]], collapse =", "))
  co <- paste0("comments: false")
  yr <- paste0("date: '", paste(bib$YEAR[[i]]),"'")
  date <- paste0(bib$YEAR[[i]])
  slu <- paste0("slug: ", paste(bib$BIBTEXKEY[[i]]))
  tit <- paste0("title: ", paste(bib$TITLE[[i]]))
  pub <- paste("kind:", "  published", sep = "\n")
  name <- paste0("article", i, sep = '')
  # abst <- paste0(bib$ABSTRACT[[i]])
  tmp <- list(au=au, co =co, yr=yr,slu=slu, tit=tit , pub=pub)
  yamllist[[name]] <- tmp
}


yamllist2 <- list()
for (i in 1:nrow(bib)) { 
  au <- paste0("author: ", paste(bib$AUTHOR[[i]], collapse =", "))
  co <- paste0("comments: false")
  yr <- paste0("date: '", paste(bib$YEAR[[i]]),"'")
  date <- paste0(bib$YEAR[[i]])
  slu <- paste0("slug: ", paste(bib$BIBTEXKEY[[i]]))
  tit <- paste0("title: ", paste(bib$TITLE[[i]]))
  pub <- paste("kind:", "  published", sep = "\n")
  name <- paste0("article", i, sep = '')
  abst <- paste0(bib$ABSTRACT[[i]])
  tmp2 <- list(au=au, co =co, yr=yr,slu=slu, tit=tit , pub=pub, abst = abst, date = date)
  yamllist2[[name]] <- tmp2
}



for (i in 1:length(yamllist)) {
  cat(paste0("---", sep = "\n"),
      paste0(unlist(yamllist[i]), sep = "\n"), 
      paste0("---", sep = "\n"), 
      paste0("\n", yamllist2[[i]]$abst, sep = "\n"), file = paste0(yamllist2[[i]]$date, "-01-01-", names(yamllist)[i], ".md"))
}
n_entries = nrow(df)



for (i in 1){
  
  tmp <- df %>% slice(i)
  tmp$author  
  
 test <- paste0("---")  

 
}
names(df)


