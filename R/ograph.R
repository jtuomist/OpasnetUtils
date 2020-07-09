#' pushIndicatorGraph pushes a plotly graph (as JSON) to a website through a REST interface.
#' @param plotlyGraph a plotly graph
#' @param indicatorId the indentifier of the indicator in the receiving website.
#' @return a status note

pushIndicatorGraph <- function (plotlyGraph, indicatorId)
{
  require(plotly)
  require(httr)
  require(jsonlite)
  indicatorId <- toString(indicatorId)
  json <- plotly_json(plotlyGraph, jsonedit = FALSE)
  plotlyData <- fromJSON(json)
  apiUri <- paste0(Sys.getenv("APLANS_API_BASE_URL"), "indicator_graph/")
  authHeader <- paste("Token", Sys.getenv("APLANS_API_KEY"))
  indicatorUri <- paste0(Sys.getenv("APLANS_API_BASE_URL"), "indicator/", indicatorId, "/")
  data <- list(data = plotlyData$data, layout = plotlyData$layout)
  resp <- POST(apiUri, body = list(indicator = indicatorUri,
                                   data = data), encode = "json", add_headers('Authorization' = authHeader))
  if (resp$status_code != 201 && resp$status_code != 200) {
    respData <- content(resp)
    msg = sprintf("API call failed with HTTP %s:\n%s", resp$status_code,
                  respData)
    print(msg)
    stop_for_status(resp)
  }
  resp
}



ograph <- function( # maaritellaan yleisfunktio piirtamiseen
		ovariable, 
		x, 
		y = character(), 
		type = character(), 
		other = character(),
		fill = NA, 
		...
) {
	if("ovariable" %in% class(ovariable))  {
		if(nrow(ovariable@output) == 0) ovariable <- EvalOutput(ovariable)
		data <- ovariable@output
		title <- ovariable@name
		if(length(y) == 0) y <- paste(title, "Result", sep = "")
	} else {
		data <- ovariable
		title <- character()
		if(length(y) == 0) y <- "Result"
	}
	if(length(type) == 0) {
		if("Iter" %in% colnames(data)) type <- geom_boxplot() else type <- geom_bar(stat = "identity")
	}
	out <- ggplot(data, aes_string(x = x, y = y, fill = fill)) # maaritellaan kuvan sarakkeet
	out <- out + type
	out <- out + theme_grey(base_size=24) # Fontin kokoa suurennetaan
	out <- out + labs(
			title	= title,
			y = paste(unique(data[[paste(title, "Yksikk\u00f6", sep = "")]]), sep = "", collapse = ", ")
	)
	out <- out + theme(axis.text.x = element_text(angle = 90, hjust = 1)) # X-akselin tekstit kaannetaan niin etta mahtuvat
	if(length(other) != 0) out <- out + other
	return(out)
}