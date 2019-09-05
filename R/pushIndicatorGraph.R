#' pushIndicatorGraph uploads plotly graphs in action plan tool. You need an instance and API key
#' @param plotlyGraph plotly type graph object
#' @param indicatorId identifier of the indicator that the graph represents
#' @return message about success

pushIndicatorGraph <- function (plotlyGraph, indicatorId)
{
  require(plotly)
  require(rjson)
  indicatorId <- toString(indicatorId)
  
  json <- plotly_json(plotlyGraph, jsonedit = FALSE)
  # print(json)
  plotlyData <- fromJSON(json, simplify = FALSE)
  
  for (i in 1:length(plotlyData$data)) {
    plotlyData$data[[i]]$frame <- NULL
  }
  
  apiBaseUrl <- Sys.getenv("APLANS_API_BASE_URL")
  if (substr(apiBaseUrl, nchar(apiBaseUrl), nchar(apiBaseUrl)) != "/") {
    apiBaseUrl <- paste0(apiBaseUrl, "/")
  }
  indicatorUri <- paste0(apiBaseUrl, "indicator/", indicatorId, "/")
  data <- list(data = plotlyData$data, layout = plotlyData$layout)
  body <- list(indicator = indicatorUri, data = data)
  
  pushAplansData(body, "indicator_graph/")
}

#' pushAplansData is a helper function to upload graphs
#' @param data plotly graph adjusted in a correct way
#' @param path the path to the indicators

pushAplansData <- function (data, path)
{
  require(httr)
  require(rjson)
  
  apiBaseUrl <- Sys.getenv("APLANS_API_BASE_URL")
  apiKey <- Sys.getenv("APLANS_API_KEY")
  if (substr(apiBaseUrl, nchar(apiBaseUrl), nchar(apiBaseUrl)) != "/") {
    apiBaseUrl <- paste0(apiBaseUrl, "/")
  }
  
  apiUri <- paste0(apiBaseUrl, path)
  authHeader <- paste("Token", apiKey)
  
  jsonData <- toJSON(data, indent = 4)
  # writeLines(jsonData)
  
  resp <- POST(apiUri, body = jsonData, encode = "raw",
               add_headers('Authorization' = authHeader), content_type_json())
  if (resp$status_code != 201 && resp$status_code != 200) {
    respData <- content(resp)
    if (is.vector(respData)) {
      respData = respData[[1]]$detail
    }
    msg = sprintf("API call failed with HTTP %s:\n%s", resp$status_code,
                  respData)
    writeLines(msg)
    stop_for_status(resp)
  }
  resp
}
