# https://components.one/datasets/billboard-200/
# https://www.dropbox.com/s/ahog97hcatpiddk/billboard-200.db?dl=1

library(RSQLite)
library(tidyverse)
library(jsonlite)
library(lubridate)
library(ggthemes)



# Get Billboard 200 data --------------------------------------------------


curl::curl_download("https://www.dropbox.com/s/ahog97hcatpiddk/billboard-200.db?dl=1",
                    destfile = "bb200.db")
sqlite.driver <- dbDriver("SQLite")
db <- dbConnect(sqlite.driver,
                dbname = "bb200.db")
albums <- dbReadTable(db,"albums") %>%
  filter(!is.na(artist) &
           artist != "" &
           !(artist %in% c("Soundtrack", "Various Artists", "Original Cast")) &
           !is.na(date)) %>%
  mutate(search = map_chr(.$artist, URLencode, reserved = TRUE))
#acoustic_features <- dbReadTable(db,"acoustic_features")
dbDisconnect(db)

artists <- albums %>%
  group_by(artist, search) %>%
  tally() %>%
  ungroup() %>% 
  arrange(desc(n))


# get_artist_mbid <- function(search) {
#   url <- paste0("https://musicbrainz.org/ws/2/artist/?query=artist:", search, "&limit=1&fmt=json")
#   json <- fromJSON(url, flatten = TRUE) %>% as_tibble() %>% flatten()
#   json$search <- search
#   return(json)
# }
# artists_result <- map_dfr(artists$search, possibly(get_artist_mbid, otherwise = NULL))
# missed_artists <- artists %>% 
#   anti_join(artists_result, by = "search")
# more_results <- map_dfr(missed_artists$search, possibly(get_artist_mbid, otherwise = NULL))
# artists_result <- bind_rows(artists_result, 
#                             more_results)
# 
# saveRDS(artists_result, "artist_result.RDS")
artists_result <- readRDS("artist_result.RDS")

artists_cleaned <- artists_result %>% 
  select(id = artists.id,
         search, 
         type = artists.type,
         name = artists.name,
         tags = artists.tags,
         start_year = `artists.life-span.begin`) %>% 
  mutate(start_year = as.integer(substr(start_year, 1, 4)))

albums_cleaned <- albums %>% 
  rename(album_index = id) %>% 
  left_join(artists_cleaned %>% select(id, search, start_year, type), by = "search") %>%
  mutate(date = as.Date(date),
         chart_year = year(date),
         age = chart_year - start_year) %>% 
  ## Bad data
  filter(date < "2019-01-01" & ((age >= 0 & type == "Group") | (age >= 10 & type == "Person")))


write_csv(albums_cleaned, "albums.csv")



# Last FM tags ------------------------------------------------------------

## Insert your own user name
api_user <- "diezzz_htc"
## Insert your API key
api_key <- Sys.getenv("LASTFM_API")


get_tags <- function(search) {
  api_artist_tags <- paste0(
    "https://ws.audioscrobbler.com/2.0/",
    "?method=artist.gettoptags", 
    "&artist=", URLencode(search),
    #"&mbid=", artist_mbid ,
    "&api_key=", api_key,
    "&format=json"
  )
  
  error <- fromJSON(api_artist_tags)$error
  tags <- fromJSON(api_artist_tags)$toptags$tag
  
  if (!is.null(error) || length(tags) == 0) {
    return(NULL)
  } else {
    api_response <- fromJSON(api_artist_tags) %>%
      .$toptags %>%
      .$tag %>%
      filter(name != "seen live") %>%
      head(5) %>%
      mutate(search = search,
             name = tolower(name)) %>%
      select(-url)
    return(api_response)
  }
}


lastfm_tags <- read_csv("lastfm_tags.csv")

tags_temp <- artists %>% 
  select(search) %>% 
  anti_join(lastfm_tags, by = "search") %>% 
  as_vector() %>% 
  map_dfr(get_tags)

lastfm_tags <- bind_rows(lastfm_tags, tags_temp)
remove(tags_temp)
write_csv(lastfm_tags, "lastfm_tags.csv")

top_tags <- lastfm_tags %>% 
  group_by(name) %>% 
  tally %>% 
  ungroup() %>% 
  filter(n >= 25)
  
lastfm_tags %>% 
  semi_join(top_tags, by = "name") %>% 
  write_csv("tags.csv")

