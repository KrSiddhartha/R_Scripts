lat_long = function(addr){
  require(stringr)
  require(RCurl)
  require(htmltidy)
  require(XML)
  
  # addr = "Anjor society,baner road,pune,maharashtra"
  addr = gsub(" ", "+", addr)
  if(grepl("\\s{1,}", addr)){
    addr = gsub(",", ",+", addr)
  }
  
  url = "https://www.google.com/maps/search/"
  url = paste0(url, addr)
  
  doc.raw <- getURL(url, httpheader = c("User-Agent" = "R(2.10.0)"))
  doc <- tidy_html(doc.raw)
  html <- htmlTreeParse(doc, useInternal = TRUE, asTree = T)
  
  val = xmlToList(html)
  val_text = tryCatch({val[["head"]][[11]][["content"]]},
                      error = function(e){
                        NA
                      })
  st_st = as.numeric(str_locate(val_text, "center="))
  val_text = substr(val_text, (st_st[2]+1), str_length(val_text))
  st_st = as.numeric(str_locate(val_text, "&"))
  val_text = substr(val_text, 1, (st_st[1]-1))
  val_text = as.character(str_split(val_text, "%2C", simplify = T))
  
  return(val_text)
}

# lat_long("Gandhi Engineering College, Bhubaneshwar, Orissa")
