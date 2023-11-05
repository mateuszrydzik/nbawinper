#' NBA Winning Percentage Data Frame, by Conference
#' @description Allows to create a data frame of NBA teams from
#' a chosen conference, with their winning percentage
#' @param conference - character value, either 'eastern' or 'western'
#' @param year - numeric value, a year in which the desired
#' season has ended (e.g. year = 2008 will provide
#' a data frame for 2007-2008 season, year = 2017 for 2016-17 etc.).
#' Please note, that the function only accepts
#' years from 1970.
#' @export
#' @examples
#' df.west_2007 <- confdf('western', 2007)
#' df.east_1999 <- confdf('eastern', 1999)

confdf <- function(conference, year) {
  tab <- get_br_data(conference, year)
  return(tab)
}