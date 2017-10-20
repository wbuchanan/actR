#' @title Valid Country Codes of HS Lookups
#' @description Function to return all valid country codes used by the ACT API
#' @return Returns a named character vector.  The names are the valid country
#' codes that can be passed to the ACT API and the values are the common nmaes.
#' @importFrom jsonlite fromJSON
#' @examples ACT::validCountries()
#' @export validCountries
validCountries <- function() {

	# Location where the country codes are stored
	values <- "http://www.act.org/content/dam/act/unsecured/documents/system/code-lookup/countries.txt"

	# Parses the JSON payload into a named character vector
	countryVector <- unlist(list(jsonlite::fromJSON(values)$Country))

	# Displays list on screen
	print(countryVector)

	# Returns the value to the user
	return(countryVector)

} # End of Function definition

