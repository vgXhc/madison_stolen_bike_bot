E6Y4qJe31unt2uRW73qEIK6t5ui1GbIRW7Zx55Nhf7c
h2QcHCdaho_WQ5p5zeX3pA1s_L9ifWVRHjSvj2-nflk

EGrf5CFuLiCF-4y6c7vwbaAaU9yFrVQxLLmY-tDEEnE 

library(httr2)

#authorization URI
https://bikeindex.org/documentation/authorize

client <- oauth_client(
  id = "-fE4nXWhieP-h3wVyz_8-fahLbwauy349Ta3WyxAkpE",
  token_url = "https://bikeindex.org/documentation/authorize",
  name = " msn_masto_bot"
)


oauth_client(
  id,
  token_url,
  secret = NULL,
  auth = c("header"),
  auth_params = list(),
  name = hash(id)
)

oauth_flow_refresh(
  client,
  refresh_token = "YdRuF1iaMxrQsvPokr2BUDamzMyL1Zs85u2Hg4Qtngg",
  scope = NULL,
  token_params = list()
)


token <- oauth_flow_auth_code(
  client = client, 
  auth_url = "https://bikeindex.org/documentation/authorize"
)


https://bikeindex.org/documentation/authorize?response_type=code&client_id=-fE4nXWhieP-h3wVyz_8-fahLbwauy349Ta3WyxAkpE&redirect_uri=http%3A%2F%2Flocalhost%3A38161%2F&state=acyMZRlEt1JWs52PYAJ1keVf02OrOWwloq70kUOuswE&code_challenge=XZeaXFTHf_PQY8EhSjTSa8t-s8XfS5FohriiOjaG9as&code_challenge_method=S256
https://bikeindex.org/documentation/authorize?code=w6v3pHM0GDrJ3gm7zTNKwi8yJWTuhZoD4C49AxJRGuY
https://bikeindex.org/oauth/authorize?client_id=-fE4nXWhieP-h3wVyz_8-fahLbwauy349Ta3WyxAkpE&redirect_uri=urn%3Aietf%3Awg%3Aoauth%3A2.0%3Aoob&response_type=code&scope=public


client

req <- request("https://bikeindex.org:443/api/v3/search")

?page=1&per_page=25&location=Madison&distance=10&stolenness=proximity&access_token=FDBn9l-Rnv-PdBe2TyXCLQrH4QL5v7oWUupcVbzRFVI

#find bikes stolen within 10 miles of Madison, WI

stolen_bikes <- req |> 
  req_url_query(
    location = "Madison WI",
    distance = "10",
    stolenness = "proximity"
  ) %>% 
  req_perform() |> 
  resp_body_json()

lubridate::as_datetime(stolen_bikes$bikes[[1]]$date_stolen)
library(jsonlite)
library(tidyverse)

jsonlite::fromJSON(stolen_bikes)

stolen_bikes %>% 
  unnest(bikes)


# unnest() is designed to work with lists of data frames
df <- tibble(
  x = 1:3,
  y = list(
    NULL,
    tibble(a = 1, b = 2),
    tibble(a = 1:3, b = 3:1, c = 4)
  )
)
# unnest() recycles input rows for each row of the list-column
# and adds a column for each column
df %>% unnest(y)

# input rows with 0 rows in the list-column will usually disappear,
# but you can keep them (generating NAs) with keep_empty = TRUE:
df %>% unnest(y, keep_empty = TRUE)

# Multiple columns ----------------------------------------------------------
# You can unnest multiple columns simultaneously
df <- tibble(
  x = 1:2,
  y = list(
    tibble(a = 1, b = 2),
    tibble(a = 3:4, b = 5:6)
  ),
  z = list(
    tibble(c = 1, d = 2),
    tibble(c = 3:4, d = 5:6)
  )
)
df %>% unnest(c(y, z))

# Compare with unnesting one column at a time, which generates
# the Cartesian product
df %>%
  unnest(y) %>%
  unnest(z)

cat_names <- pluck(stolen_bikes, "bikes", 1) %>% unlist()

get_colors <- function(x){
  stolen_bikes %>% 
    pluck("bikes", row_n, "frame_colors", x)
}


parse_bike_data <- function(row_n) {
  stolen_bikes %>% 
  bikes <- pluck("bikes", row_n) %>% 
    keep(names(.) %in% c("date_stolen",
    "frame_model",
    "manufacturer_name",
    "url",
    "thumb", 
    "title"))
  

  no_of_colors <- stolen_bikes %>% 
    pluck("bikes", row_n, "frame_colors") %>% 
    length()
  colors <- map(1:no_of_colors, get_colors)
  
  c(bikes, colors[1])
  
}

parse_bike_data(1)

all_bikes <- map_dfr(1:length(stolen_bikes$bikes), parse_bike_data)

all_bikes %>% 
  mutate(date_stolen = as_datetime(date_stolen))
