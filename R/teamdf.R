#' NBA Winning Percentage Data Frame, by Team
#' @description Allows to create a data frame for specific NBA team,
#' with their winning percentage
#' @param team - character value, a team's abbreviation
#' (e.g. 'BOS' for Boston Celtics)
#' @param year - numeric value, a year in which the desired
#' season has ended (e.g. year = 2008 will provide
#' a data frame for 2007-2008 season, year = 2017 for 2016-17 etc.).
#' @export
#' @examples
#' teamdf('SAS', 2005)

teamdf <- function(team, year = 0) {
    tab <- get_br_team_data(team, year)
    return(tab)
}