#' @title Valid State Codes of HS Lookups
#' @description Function to return all valid state codes used by the ACT API
#' @return Returns a named character vector.  The names are the valid state
#' codes that can be passed to the ACT API and the values are the common nmaes.
#' @importFrom jsonlite fromJSON
#' @export validStates
validStates <- function() {

	# Location where the state, territory, and Armed Forces codes are stored
	values <- "http://www.act.org/content/dam/act/unsecured/documents/system/code-lookup/state-territory-armedforce.txt"

	# Parses the JSON payload into a named character vector
	stateVector <- unlist(list(jsonlite::fromJSON(values)), recursive = FALSE)

	stateVector <- c(stateVector[["State"]],
					 stateVector[["Territories"]],
					 stateVector[["Armed Forces"]])

	# Displays list on screen
	print(stateVector)

	# Returns the value to the user
	return(stateVector)

} # End of Function definition

