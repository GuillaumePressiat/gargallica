library(tidyverse)
library(rvest)
i = 1
# fonction pour en faire 50 via l'api bnf (50 max)
# http://api.bnf.fr/api-gallica-de-recherche
# http://api.bnf.fr/api-gallica-de-recherche#chapitre2
#

# indiquer la question (la requête CQL visible dans l'URL query = () )
# Il faut recopier la question posée sur gallica.bnf.fr

question <- 'text%20adj%20%22jacques%20bonhomme%22%20and%20(dc.type%20all%20%22fascicule%22)'

page <- function(i)xml2::read_xml(paste0('http://gallica.bnf.fr/SRU?operation=searchRetrieve&version=1.2&query=(', question,')&collapsing=false&maximumRecords=50&startRecord=', i))


# Première 50 réponses (initialiser la structure xml avec un premier coup)
tot <- page(1)
# récupérer le nombre total de réponses
te <- xml2::as_list(tot)
nmax <- as.integer(unlist(te$searchRetrieveResponse$numberOfRecords))
# nmax <- 7853

# Boucle sur la suite, 50 par 50
# Ajouter au document xml tot les réponses des autres pages
for (j in seq(51, nmax, by = 50)){
  temp <- page(j)
  for (l in xml2::xml_children(temp)){
    xml2::xml_add_child(tot, l)
  }
}

xml2::write_xml(tot, 'data/jacques_bonhomme_gallica.xml')

# zip(zipfile = 'data/jacques_bonhomme_gallica.xml.zip',
#     files = 'data/jacques_bonhomme_gallica.xml')


xml_to_df <- function(doc, ns = xml_ns(doc)) {
  library(xml2)
  library(purrr)
  split_by <- function(.x, .f, ...) {
    vals <- map(.x, .f, ...)
    split(.x, simplify_all(transpose(vals)))
  }
  node_to_df <- function(node) {
    # Filter the attributes for ones that aren't namespaces
    # x <- list(.index = 0, .name = xml_name(node, ns))
    x <- list(.name = xml_name(node, ns))
    # Attributes as column headers, and their values in the first row
    attrs <- xml_attrs(node)
    if (length(attrs) > 0) {attrs <- attrs[!grepl("xmlns", names(attrs))]}
    if (length(attrs) > 0) {x <- c(x, attrs)}
    # Build data frame manually, to avoid as.data.frame's good intentions
    children <- xml_children(node)
    if (length(children) >= 1) {
      x <- 
        children %>%
        # Recurse here
        map(node_to_df) %>%
        split_by(".name") %>%
        map(bind_rows) %>%
        map(list) %>%
        {c(x, .)}
      attr(x, "row.names") <- 1L
      class(x) <- c("tbl_df", "data.frame")
    } else {
      x$.value <- xml_text(node)
    }
    x
  }
  node_to_df(doc)
}

# u <- xml_to_df(xml2::xml_find_all(tot, ".//srw:records"))
x = 1:3
parse_gallica <- function(x){
  xml2::xml_find_all(tot, ".//srw:recordData")[x] %>% 
    xml_to_df() %>% 
    select(-.name) %>% 
    .$`oai_dc:dc` %>% 
    .[[1]] %>% 
    mutate(recordId = 1:nrow(.)) %>% 
#    tidyr::unnest() %>% 
    tidyr::gather(var, val, - recordId) %>% 
    group_by(recordId, var) %>% 
    mutate(value = purrr::map(val, '.value') %>% purrr::flatten_chr() %>% paste0( collapse = " -- ")) %>% 
    select(recordId, var, value) %>% 
    ungroup() %>% 
    mutate(var = stringr::str_remove(var, 'dc:')) %>% 
    tidyr::spread(var, value) %>% 
    select(-.name)
}

tot <- xml2::read_xml('data/jacques_bonhomme_gallica.xml')

tot_df <- 1:nmax %>% 
  parse_gallica %>% 
  bind_rows()

readr::write_delim(tot_df, 'data/jacques_bonhomme_gallica.csv', delim = ";", na = "")


tot_df <- readr::read_delim('data/jacques_bonhomme_gallica.csv', delim = ";")

parse_extraRecordData <- function(x){
xml2::xml_find_all(tot, ".//srw:extraRecordData")[x] %>% 
  xml_to_df() %>% 
  select(-.name) %>% 
  #select(`d1:nqamoyen`) %>% 
  select(`nqamoyen`) %>% 
  unnest() %>% 
  mutate(recordId = 1:nrow(.)) %>% 
  #mutate(nqamoyen = stringr::str_remove(.value, 'd1:')) %>%
  mutate(nqamoyen = .value) %>%
  select(recordId, nqamoyen)
}

extraRecordData <- 1:nmax %>% 
  parse_extraRecordData

extraRecordData <- extraRecordData %>% mutate(nqamoyen = as.numeric(nqamoyen))

readr::write_delim(extraRecordData, 'data/jacques_bonhomme_nqamoyen.csv', delim = ";", na = "")
extraRecordData <- readr::read_delim('data/jacques_bonhomme_nqamoyen.csv', delim = ";", na = "")



works <- tot_df %>% 
  filter(grepl('public', rights), as.integer(stringr::str_extract(date, '[0-9]{4}')) < 1946 ) %>% 
  inner_join(extraRecordData %>% filter(nqamoyen > 80), by = "recordId")
  # inner_join(extraRecordData %>% filter(nqamoyen > 85), by = "recordId")

i = 6953
# Fonction pour récupérer le texte brut (OCR document par document)
texteBrut <- function(i){
  tem <- works %>% 
    filter(recordId == i)
  contenu <-  tem %>% 
    pull(identifier) %>%
    paste0(., '.texteBrut') %>%
    read_html() %>%
    html_nodes('p') %>%
    html_text() %>%
    paste0(collapse = "\n") %>%
    stringr::str_split("En savoir plus sur l'OCR", n = 2, simplify = TRUE)
  readr::write_csv(data_frame(identifier = tem$identifier, entete = contenu[1,1],
                              texteBrut = contenu[1,2],
                              recordId = tem$recordId), paste0('data/docs_ocr/', tem$recordId, '.csv'))

}

texteBrut(6953)

deja_ok <- list.files('data/docs_ocr/') %>% 
  stringr::str_remove('\\.csv') %>% 
  as.integer

todo <- unique(works$recordId)  %>% 
  .[!(. %in% deja_ok)]
todo <- todo %>% sort()

todo %>% purrr::map(purrr::possibly(texteBrut, otherwise = NULL))

dat <- list.files('data/docs_ocr/') %>% 
  purrr::map(function(f)readr::read_csv(paste0('data/docs_ocr/', f),
             col_types = cols(
               identifier = col_character(),
               entete = col_character(),
               texteBrut = col_character(),
               recordId = col_integer()
             ))) %>% 
  bind_rows()

dat_xml <- lapply(purrr::transpose(dat),
                  function(x) {
                    as_xml_document(list(textes = lapply(x, as.list)))
                  })
doc <- xml_new_document()
for(i in 1:length(dat_xml)){
  if(i == 1){
    doc <- doc %>% xml_add_child(dat_xml[[i]])
  } else {
    doc <- doc %>% xml_add_sibling(dat_xml[[i]])
  }
} 

# doc
# Créer un seul xml contenant tous les textes bruts
xml2::write_xml(doc, file="data/jacques_bonhomme_texteBrut.xml")



# Compter les occurences de l'expression dans chaque document
dat_ <- dat %>% 
  mutate(nb_occ = stringr::str_count(texteBrut, regex('Jacques.{1,3}Bonhomme', ignore_case = TRUE))) %>% 
  filter(nb_occ > 0)

count(dat_, is.na(texteBrut))
dat_ %>% filter(is.na(texteBrut)) %>% select(recordId)
dat_ %>% filter(nb_occ == 0) %>% select(recordId)
count(dat_, nb_occ) %>% View

dat_ %>% select(recordId, identifier, nb_occ) %>% 
  readr::write_csv('~/Desktop/gallica/Nombre_doccurences.csv')

dat__ <- dat_ %>% 
  select(recordId, identifier, texteBrut, nb_occ) %>% 
  mutate(texteBrut = stringr::str_replace_all(texteBrut, regex('Jacques.{1,3}Bonhomme?', 
                                                               ignore_case = TRUE), 
                                              'jacquesbonhomme')) %>% 
  arrange(as.integer(recordId))

# %>% 
#   
View(dat__ %>% select(recordId))

dat__ %>% filter(nb_occ > 30) %>% select(recordId)


# avec tidytext on récupère tous les mots autour des occurences de jacquesbonhomme, 200 mots avant, 200 mots après
fenetrage <- function(dat__, fenetre = 200, f = '1.csv'){
  dat_t <- dat__ %>%
    ungroup() %>% 
    group_by(recordId, identifier, nb_occ) %>% 
    tidytext::unnest_tokens(words, texteBrut) %>% 
    ungroup() %>% 
    filter(nchar(words) > 1) %>% 
    filter(! stringr::str_detect(words, '^[0-9\\.,]+$')) %>% 
    group_by(recordId, identifier, nb_occ) %>% 
    mutate(nw = row_number()) %>% 
    mutate(occurence = grepl('jacquesbonhomme', words, ignore.case = TRUE) * nw,
           occurence = ifelse(occurence == 0, NA, occurence)) %>% 
    mutate(occ_deb = min(occurence, na.rm = TRUE),
           occ_fin = max(occurence, na.rm = TRUE)) %>% 
    mutate(occurence_up = occurence, 
           occurence_down = occurence) %>% 
    tidyr::fill(occurence_down, .direction = "down") %>% 
    tidyr::fill(occurence_up, .direction = "up") %>% 
    filter((nw >= occurence_up - fenetre & nw <= occurence_up + fenetre)|
             (nw >= occurence_down - fenetre & nw <= occurence_down + fenetre)) %>% 
    group_by(recordId, identifier, nb_occ) %>% 
    summarise(citation = paste0(words, collapse = " "))
    readr::write_csv(dat_t, paste0("data/fenetre200/", f))
  return(dat_t)
}

# C'est exigeant pour le processeur et la mémoire vive, on le fait par morceau : 
tot_fenetre <- bind_rows(
  fenetrage(dat__ %>% filter(row_number() >=     1, row_number() <= 1000), f = '1.csv'),
  fenetrage(dat__ %>% filter(row_number() >=  1001, row_number() <= 2000), f = '2.csv'),
  fenetrage(dat__ %>% filter(row_number() >=  2001, row_number() <= 3000), f = '3.csv'),
  fenetrage(dat__ %>% filter(row_number() >=  3001, row_number() <= 4000), f = '4.csv'),
  fenetrage(dat__ %>% filter(row_number() >=  4001, row_number() <= 5000), f = '5.csv'),
  fenetrage(dat__ %>% filter(row_number() >=  5001, row_number() <= 5300), f = '6.csv'),
  fenetrage(dat__ %>% filter(row_number() >=  5301, row_number() <= 5700), f = '7.csv'),
  fenetrage(dat__ %>% filter(row_number() >=  5701, row_number() <= 5800), f = '8.csv'),
  fenetrage(dat__ %>% filter(row_number() >=  5801, row_number() <= 6000), f = '9.csv'))

gc()

dat_total_200 <- list.files('data/fenetre200/') %>% 
  purrr::map(function(f){readr::read_csv(paste0('data/fenetre200/', f), 
                                         col_types = cols(
    recordId = col_integer(),
    identifier = col_character(),
    nb_occ = col_integer(),
    citation = col_character()
  ))}) %>% 
  bind_rows()



# readr::read_csv('~/Desktop/gallica/fenetre200/1.csv')
dat_xml <- lapply(purrr::transpose(dat_total_200),
                  function(x) {
                    as_xml_document(list(textes = lapply(x, as.list)))
                  })
doc <- xml_new_document()
for(i in 1:length(dat_xml)){
  if(i == 1){
    doc <- doc %>% xml_add_child(dat_xml[[i]])
  } else {
    doc <- doc %>% xml_add_sibling(dat_xml[[i]])
  }
} 

# doc
xml2::write_xml(doc, file="data/fenetre200/jacques_bonhomme_autour_200.xml")

ww <- dat_total_200 %>% 
  group_by(recordId) %>% 
  tidytext::unnest_tokens(word, citation) %>% 
  ungroup() %>% 
  filter(! word %in% c(tm::stopwords('fr'), 'plus', 'comme', 'tout',
                       'là', 'toujours', 'encore', 'tous', 'dont',
                       'si', 'où', 'fait', "qu'il", "d'un", "dit", "peut",
                       "c'est", "bien", "d'une", "sous", "deux", "fr", 
                       "jacquesbonhomme", "n'est", "faire", "mm", "toutes", "être", 
                       "qu'on", "francs")) %>% 
  group_by(recordId) %>% 
  summarise(citation = paste0(word, collapse = " ")) %>% 
  ungroup() %>% 
  group_by(recordId) %>% 
  tidytext::unnest_tokens(word, citation, token = "ngrams", n = 2, to_lower = FALSE) %>% 
  tidyr::separate(word, c("word1", "word2"), sep = " ") %>% 
  count(word1, word2) %>% 
  ungroup() %>% 
  mutate(f = n / sum(n))

library(ggraph)
library(igraph)
ww %>% 
  filter(n > 30,
         ! word1 %in% tm::stopwords('fr'),
         ! word2 %in% tm::stopwords('fr')) %>% 
  graph_from_data_frame() -> g

a <- grid::arrow(type = "closed", length = unit(.05, "inches"))

ggraph(g, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = T,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 3) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1, size = 2) +
  theme_void()

ww <- dat_total_200 %>% 
  tidytext::unnest_tokens(word, citation, to_lower = FALSE) %>% 
  filter(! word %in% c(tm::stopwords('fr'), 'plus', 'comme', 'tout',
                       'là', 'toujours', 'encore', 'tous', 'dont',
                       'si', 'où', 'fait', "qu'il", "d'un", "dit", "peut",
                       "c'est", "bien", "d'une", "sous", "deux", "fr", 
                       "jacquesbonhomme", "n'est", "faire", "mm", "toutes", "être", 
                       "qu'on", "francs")) %>% 
  count(word) %>% 
  ungroup() %>% 
  mutate(p = n / sum(n)) %>% 
  (function(df){
    wordcloud::wordcloud(words = df$word, freq = df$p, 
                         min.freq = 0.0006, rot.per = 0)})
ww
