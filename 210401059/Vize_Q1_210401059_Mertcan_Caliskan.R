

library(httr)

spotify_token <- function(){
  # Spotify API'ye erişim için gerekli bilgiler
  readRenviron("./.Renviron")
  client_id <- Sys.getenv("spotify_id")
  client_secret <- Sys.getenv("spotify_secret")
  
  # Token almak için gerekli endpoint ve parametreler
  token_url <- "https://accounts.spotify.com/api/token"
  grant_type <- "client_credentials"
  
  # Token almak için POST isteği gönderme
  response <- POST(
    token_url,
    authenticate(client_id, client_secret),
    body = list(grant_type = grant_type),
    encode = "form"
  )
  
  # HTTP status kodunu al
  status_code <- status_code(response)
  
  # Response JSON içerisinden token'ı al
  token <- content(response)$access_token
  
  # Bearer token string oluştur
  bearer_token <- paste("Bearer", token, sep = " ")
  
  # Sonuçları liste olarak döndür
  result <- list(status_code = status_code, token = bearer_token)
  return(result)
  
}

# Token al
token <- spotify_token()
print(token)



spotify_search_artist <- function(artist_name) {
  # Spotify API'ye erişim token'ı al
  access_token <- spotify_token()$token
  
  # Spotify API endpoint for searching artists
  endpoint <- "https://api.spotify.com/v1/search"
  
  # Query parameters for the search request
  params <- list(
    q = artist_name,
    type = "artist"
  )
  
  # Set headers for the request with the access token
  headers <- c(
    `Authorization` = access_token
  )
  
  # Make GET request to Spotify API
  response <- GET(endpoint, query = params, add_headers(headers))
  
  # Extract http status code
  status_code <- status_code(response)
  
  # Extract search results from the response
  search_results <- content(response)$artists
  
  # Create a data.frame with artist name and id
  if (!is.null(search_results)) {
    artist_df <- data.frame(
      artist = search_results$items$name,
      id = search_results$items$id
    )
  } else {
    artist_df <- data.frame(artist = character(), id = character(), stringsAsFactors = FALSE)
  }
  
  # Create the final output list
  output <- list(
    status_code = status_code,
    search_results = artist_df
  )
  
  return(output)
}

# Örnek kullanım:
result2 <- spotify_search_artist("Migos")
print(result2)
