#' upload the R package binary build as an asset to its github release
#' @param repo Repository name 
#' @param username User name
#' @param token character oAuth2.0 token. If not supplied, then try o read it from the environment variable 'GITHUB_TOKEN'
#' @param release_tag the tag where the asset is associated to
#' @param path the folder where package binary tar ball stores
#' @param ... other arguments passed to httr APIs (i.e. verbose())
github_release <- function(repo, username, token = Sys.getenv("GITHUB_TOKEN"), release_tag = "linux", path = ".", ...){
  
  if(token=="")
    stop("token is not found!")
  
  auth_head <- add_headers(Authorization = paste("token", token))
  
  #get the assets url
  release_url <- file.path("https://api.github.com/repos", username, repo, "releases/tags", release_tag)      
  req <- GET(release_url, auth_head, ...)
  stop_for_status(req)
  res <- content(req)
  upload_url <- res[["upload_url"]]
  assets <- res[["assets"]]
  
  #delete if it exists already
  if(length(assets) > 0){
    asset_url <- assets[[1]][["url"]]
    res <- DELETE(asset_url, auth_head, ...)
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
            , config = c(auth_head, add_headers(Accept = "application/vnd.github.v3+json"))
            , ...
            )
  stop_for_status(res)
  
  
}