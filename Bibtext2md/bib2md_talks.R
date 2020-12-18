library(tidyverse)
library(bib2df)




# !> read bib text files
bib <- bib2df::bib2df("my_talks.bib") %>% arrange(YEAR, AUTHOR)



tex2txt <- function(x) {
  x <- x %>%
    str_replace_all(., "\\{\\\\o\\}", "&oslash;") %>%
    str_replace_all(., "\\{\\\\O\\}", "&Oslash;") %>%
    str_replace_all(., "\\{\\\\'\\{o\\}\\}", "&oacute;") %>%
    str_replace_all(., "\\{\\\\'\\{O\\}\\}", "&Oacute;") %>%
    str_replace_all(., "\\{\\\\\\\"\\{o\\}\\}", "&ouml;") %>%
    str_replace_all(., "\\{\\\\\\\"\\{O\\}\\}", "&Ouml;") %>%
    str_replace_all(., "\\{\\\\\\\"\\{e\\}\\}", "&euml;") %>%
    str_replace_all(., "\\{\\\\\\\"\\{u\\}\\}", "&uuml;") %>%
    str_replace_all(., "\\{\\\\\\\"\\{U\\}\\}", "&Uuml;") %>%
    str_replace_all(., "'", "&apos;")
}

bib$AUTHOR[[2]] %>% str_replace_all(., "\\{\\\\\\\"\\{u\\}\\}", "&uuml;")

spec2italics <- function(x) {
  x <- x %>%
    str_replace_all(., "Stratiotes aloides", "<i>Stratiotes aloides</i>") %>%
    str_replace_all(., "Clupea harengus", "<i>Clupea harengus</i>") %>%
    str_replace_all(., "Hydrellia tarsata", "<i>Hydrellia tarsata</i>") %>%
    str_replace_all(., "Periphylla periphylla", "<i>Periphylla periphylla</i>")
}


bold_author <- function(x) {
x <- x %>% str_replace_all(., "Langbehn T", "Langbehn TJ") %>%   
       str_replace_all(., "Langbehn TJ", "<strong>Langbehn TJ</strong>") 
 return(x)
}
  

yamllist <- list()
yamllist2 <- list()

for (i in 1:nrow(bib)) {
  no_au <- length(bib$AUTHOR[[i]])

  # !> reformat author names to just include lastname followed by initials
  authorlist <- bib$AUTHOR[[i]]
  for (j in 1:no_au) {
    lastname <- bib$AUTHOR[[i]][j] %>%
      str_remove(., "[,]") %>%
      str_remove(., "[.]") %>%
      word(1)

    initial <- bib$AUTHOR[[i]][j] %>%
      tex2txt() %>%
      str_remove(., "[,]") %>%
      str_remove(., "[.]") %>%
      str_split(" ") %>%
      unlist() %>%
      .[-1] %>%
      str_split(";") %>%
      unlist() %>%
      .[1]

    if (str_starts(initial, "&", negate = FALSE)) {
      initial <- paste0(initial, ";")
    } else {
      initial <- initial %>%
        str_sub(., 1, 1) %>%
        paste(., sep = "", collapse = "")
    }

    authorlist[j] <- paste(lastname, initial)
  }

  typ <- paste0("type: ", "talk")
  col <- paste0("collection: ", "publications")
  au <- paste0("author: ", paste0(paste(authorlist[-no_au], collapse = ", ")), " & ", last(authorlist)) %>% tex2txt() %>% bold_author()
  yr <- paste0("year: ", paste(bib$YEAR[[i]]))
  tit <- paste0("title: ", paste0("'", tex2txt(bib$TITLE[[i]]), "'")) %>% spec2italics()
  jou <- paste0("journal: ", paste0("'", bib$JOURNAL[[i]], "'"))
  vol <- paste0("volume: ", paste(bib$VOLUME[[i]]))
  pge <- paste0("pages: ", bib$PAGES[[i]] %>% str_replace(., "--", "-"))
  name <- paste0("talk", i, sep = "")


  tmp <- list(typ = typ, col = col, au = au, yr = yr, tit = tit, jou = jou, vol = vol, pge = pge)
  yamllist[[name]] <- tmp


  filename <- paste0(name, "_", word(authorlist[1]), "_", bib$YEAR[[i]], ".md")
  abst <- "empty"
  tmp2 <- list(abst = abst, filename = filename)
  yamllist2[[name]] <- tmp2
}


for (i in 1:length(yamllist)) {
  fileConn <- file(here::here("_publications", yamllist2[[i]]$filename))
  writeLines(
    c(
      "---",
      paste0(unlist(yamllist[i])),
      "---"
    ),
    fileConn
  )
  close(fileConn)
}




# for (i in 1:length(yamllist)) {
#   cat(paste0("---", sep = "\n"),
#       paste0(unlist(yamllist[i]), sep = "\n"),
#       paste0("---", sep = "\n"),
#       paste0("\n", yamllist2[[i]]$abst, sep = "\n"), file = yamllist2[[i]]$filename)
# }
