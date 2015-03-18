#' upload the R package binary build as an asset to its github release
#' @param repo Repository name 
#' @param username User name
#' @param token oAuth2.0 token
#' @param release_tag the tag where the asset is associated to
#' @param path the folder where package binary tar ball stores
github_release <- function(repo, username, token, release_tag = "linux", path = "."){
  
  auth_head <- add_headers(Authorization = paste("token", token))
  
  #get the assets url
  release_url <- file.path("https://api.github.com/repos", username, repo, "releases/tags", release_tag)      
  req <- GET(release_url, auth_head)
  stop_for_status(req)
  res <- content(req)
  upload_url <- res[["upload_url"]]
  assets <- res[["assets"]]
  
  #delete if it exists already
  if(length(assets) > 0){
    asset_url <- assets[[1]][["url"]]
    res <- DELETE(asset_url, auth_head)
    stop_for_status(res)
  }
  
  
  #upload the new tar ball
  fileToupload <- paste0(repo, ".tar.gz")
  filePath <- file.path(path, fileToupload)
  
  #convert upload url from generic format to the specific
  upload_url <- gsub("[\\{|\\}]", "", upload_url)
  upload_url <- paste0(upload_url, "=", fileToupload)
  #upload tar ball
  res <- POST(upload_url
      , body = upload_file(filePath, type = "application/x-gzip")
      , config = c(auth_head
          , add_headers(Accept = "application/vnd.github.v3+json")
      )
  )
  stop_for_status(res)
  
  
}