# Scraping Metros
## @knitr variablesXY

library(rvest) 
metroURL <- "https://en.wikipedia.org/wiki/List_of_Copenhagen_Metro_stations"
metro_scrap <- metroURL %>% 
  read_html %>%
  html_nodes(xpath='//th') %>% html_nodes("a") %>% html_attr("href") %>% na.omit() %>% 
  paste0("https://en.wikipedia.org", .) #create link, we only need first 22 lists

# Using purrr instead of for-loops
library(purrr)

# Extract the geo locations and names of the metro stations
names <- map_df(metro_scrap[1:22], ~ tibble(names = read_html(.) %>% html_nodes("#firstHeading") %>% html_text())) %>% distinct()
geo <- map_df(metro_scrap[1:22], ~ tibble(coor = read_html(.) %>% html_nodes(".geo") %>% html_text())) %>% 
  distinct()

Numextract_coord <- function(string){
  as.data.frame(as.numeric(unlist(regmatches(string, gregexpr("[[:digit:]]+\\.*[[:digit:]]*", string)))))
}

df <- Numextract_coord(geo$coor)
lng <- df %>% dplyr::filter(row_number() %% 2 == 0) ## Select even rows
lat <- df %>% dplyr::filter(row_number() %% 2 == 1) ## Select odd rows

metro_df <- cbind(names, lat,lng)
colnames(metro_df) <- c("metro","lat", "long")
metro_df
