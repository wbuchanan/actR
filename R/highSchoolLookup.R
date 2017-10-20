#' @title Finding ACT High School Codes
#' @description Provides a simple function to form/request high school lookup
#' codes from the ACT API
#' @param country Should be the abbreviated character list
#' @param state Should be the two character state/province abbreviation
#' @param city Optional - name of the city where the high school is located
#' @param name Optional - name of the school to lookup
#' @import httr
#' @importFrom jsonlite fromJSON
#' @import
#' @return Returns a data frame containing the response payload
#' @examples \run{
#' # Get all the high schools located in Kentucky
#' kyHighSchools <- ACT::highSchoolLookup("US", "KY")
#'
#' # Display part of the dataframe
#' head(kyHighSchools)
#'
#' }
#' @export highSchoolLookup
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

highSchoolLookup <- function(state, country = "US", city = "", name = "") {

	# Used to obtain a valid API key/token
	referrer <- "http://www.act.org/content/act/en/products-and-services/the-act/registration/high-school-codes-lookup.html"

	# Headers that need to be added to the initial request that generates the
	# API token
	apiHeaders <- c("Content-Type" = "application/x-www-form-urlencoded",
                   "Connection" = "keep-alive",
                   "X-Requested-With" = "XMLHttpRequest",
                   "referrer" = referrer)

	# Object containing the response headers that can be used to get the
	# API token
	apiKey <- httr::GET("http://www.act.org/services/.gettoken.sjson",
                    httr::add_headers(apiHeaders))

	# Headers used as part of the API query
	headers <- list("authorization" = paste("Bearer", httr::content(apiKey, "text")),
                "access-control-allow-origin" = "*",
                "referrer" = referrer,
                "cache-control" = "no-cache",
                "dnt" = 1,
                "origin" = "http://www.act.org",
                "pragma" = "no-cache",
                "accept-encoding" = "gzip, deflate, br",
                "accept-language" = "en-US,en;q=0.9",
                "accept" = "*/*")

	# Places all of the parameters in a list...
	# In a future version we'll validate the values prior to passing them
	# to the actual API call
	params <- list(country = country, state = state, city = city, name = name)

	# Root of the API URL endpoint
	actUrl <- "https://api.act.org/actcodes/1.0.1/code/highschool/websearch?"

	# Makes the request to the API endpoint, retrieves the payload contents,
	# and parses the JSON object into a list object containing a single data frame
	payload <- 	httr::GET(actUrl,
               	httr::set_cookies(as.character(httr::cookies(apiKey))),
               	httr::add_headers(unlist(headers)),
               	query = params) %>%
      			httr::content("text") %>%
      			jsonlite::fromJSON()

	# Returns only the data.frame object from the list obtained above.
	return(payload[["party"]])

} # End of Function definition
