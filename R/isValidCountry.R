#' @title Internal function for validating entries
#' @description Function used to test whether a valid country code was passed
#' @return Returns a boolean indicating if the country code passed was valid
#' @param countryCode is the country code value being validated
#'
isValidCountry <- function(countryCode) {
	countries <- validCountries()
	if (!(countryCode %in% names(countries)) && !(countryCode %in% countries)) {
		return(FALSE)
	} else if (countryCode %in% countries) {
		warning(paste("Must use two character country code and not full",
					  "country name...see ACT::validCountries for the named",
					  "character vector with the codes and country names."),
				immediate = TRUE)
		return(FALSE)
	} else if (countryCode %in% names(countries)) {
		return(TRUE)
	} else {
		return(FALSE)
	}
}
