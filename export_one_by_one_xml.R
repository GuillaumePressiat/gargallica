
dat_total_200 <- list.files('data/fenetre200/') %>% 
  purrr::map(function(f){readr::read_csv(paste0('data/fenetre200/', f), 
                                         col_types = cols(
                                           recordId = col_integer(),
                                           identifier = col_character(),
                                           nb_occ = col_integer(),
                                           citation = col_character()
                                         ))}) %>% 
  bind_rows()


www <- dat_total_200 %>% select(recordId, citation)


i = unique(www$recordId)[1]
for(i in unique(www$recordId)){
    dat_xml <- lapply(purrr::transpose(www %>% filter(recordId == i) %>% 
                        select(citation) %>% 
                          mutate(citation = paste0('\n', citation, '\n'))),
                    function(x) {
                      as_xml_document(list(textes = x))
                    })
    doc <- xml_new_document()
    doc <- doc %>% xml_add_child(dat_xml[[1]])
    xml2::write_xml(doc, paste0('data/xml_txm/', i, '.xml' ))
}

