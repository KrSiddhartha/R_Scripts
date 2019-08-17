lat_long_addr = function(addr){
  require(rvest)
  require(stringr)
  
  search_start = as.character(str_split(addr, " ", simplify = T))[1]
  addr = gsub(" ", "+", addr)
  if(grepl("\\s{1,}", addr)){
    addr = gsub(",", ",+", addr)
  }
  url = "https://www.google.com/maps/search/"
  url = paste0(url, addr)
  
  js_parse <- read_html(url) %>% html_nodes('head') %>% html_nodes('script') %>% html_text()
  
  txt_txt = str_extract(js_parse[[8]][[1]], pattern = paste0("http:.+", search_start,".+[0-9]{6}"))
  txt_txt = substr(txt_txt, 1, 200)
  txt_txt = substr(txt_txt, as.numeric(str_locate(txt_txt, "\\\\u003[A-Za-z]"))[1], str_length(txt_txt))
  txt_txt = substr(txt_txt, 1, as.numeric(str_locate(txt_txt, "[0-9]{6}"))[2])
  full_addr = gsub("\\+", " ", txt_txt)
  
  lat_long = str_extract(js_parse[[8]][[1]], pattern = paste0(search_start,".+[0-9]{6}[^(?<!\\)]"))
  lat_long = substr(lat_long, (str_length(lat_long)-34), str_length(lat_long))
  lat_long = gsub("[^0-9,.]", "", lat_long)
  lat_long = gsub("^,{1,}", "", lat_long)
  
  c("Full_Address"=full_addr, "Latitude_Longitude"=lat_long)
}

# geo_code = lat_long_addr("Gandhi Engineering College, Bhubaneshwar, Orissa")
# geo_code



