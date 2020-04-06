# https://medium.com/@seeksanusername/astuce1-r%C3%A9cup%C3%A9rer-de-la-hd-sur-gallica-bef0a6cc7f89

ark <- "bpt6k37947d"
get_hd_image <- function(ark){
  url_ <- paste0("gallica.bnf.fr/iiif/ark:/12148/", ark, "/f1/full/full/0/native.jpg")
  httr::GET(url_, 
            httr::write_disk(paste0('img/', ark, '.jpg')))
}

get_hd_image(ark)
