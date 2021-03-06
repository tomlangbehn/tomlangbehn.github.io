library(tidyverse)
library(bib2df)




# !> read bib text files
bib <- bib2df::bib2df("my_pubs.bib") %>% arrange(YEAR, AUTHOR)
pdf_filenames <- here::here("files", "papers") %>% list.files(".pdf")




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
  


if(authorlist[1] == "Langbehn T" | authorlist[1] == "Langbehn TJ"){
  authorlist[1] <- paste0("<b>", authorlist[1], "</b>")
} else

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

  typ <- paste0("type: ", paste(bib$CATEGORY[[i]]))
  col <- paste0("collection: ", "publications")
  au <- paste0("author: ", paste0(paste(authorlist[-no_au], collapse = ", ")), " & ", last(authorlist)) %>% tex2txt() %>% bold_author()
  yr <- paste0("year: ", paste(bib$YEAR[[i]]))
  tit <- paste0("title: ", paste0("'", tex2txt(bib$TITLE[[i]]), "'")) %>% spec2italics()
  jou <- paste0("journal: ", paste0("'", bib$JOURNAL[[i]], "'"))
  vol <- paste0("volume: ", paste(bib$VOLUME[[i]]))
  pge <- paste0("pages: ", bib$PAGES[[i]] %>% str_replace(., "--", "-"))

  if (is.na(bib$URL[[i]])) {
    doi <- paste0("doi: ")
  } else {
    doi <- paste0("doi: ", paste(bib$DOI[[i]]))
  }

  if (is.na(bib$URL[[i]])) {
    external_url <- paste0("external_url: ")
  } else {
    external_url <- paste0("external_url: ", bib$URL[[i]] %>% str_split(" ") %>% unlist() %>% last())
  }

  name <- paste0("article", i, sep = "")

  pdf_file <- paste0(name, "_", word(authorlist[1]), "_", bib$YEAR[[i]], ".pdf")

  if (!file.exists(here::here("files", "papers", pdf_file))) {
    pdf <- pdf_filenames %>%
      .[str_starts(., word(authorlist)[1])] %>%
      .[str_which(., as.character(bib$YEAR[[i]]))] %>%
      .[str_which(., word(bib$TITLE[[i]], 1, 2))]
    file.rename(here::here("files", "papers", pdf), here::here("files", "papers", pdf_file))
  }


  pdf_file <- paste0("filename: ", "'", "/files/papers/", pdf_file, "'")



  tmp <- list(typ = typ, col = col, au = au, yr = yr, tit = tit, jou = jou, vol = vol, pge = pge, doi = doi, external_url = external_url, pdf_file = pdf_file)

  yamllist[[name]] <- tmp


  filename <- paste0(name, "_", word(authorlist[1]), "_", bib$YEAR[[i]], ".md")
  abst <- paste0(bib$ABSTRACT[[i]])
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
