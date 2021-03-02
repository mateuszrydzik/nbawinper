#' NBA Winning Percentage Data Frame, by Conference
#' @description Allows to create a data frame of NBA teams from a chosen conference, with their winning percentage
#' @param conference - character value, either 'eastern' or 'western'
#' @param year - numeric value, a year in which the desired season has ended (e.g. year = 2008 will provide
#' a data frame for 2007-2008 season, year = 2017 for 2016-17 etc.). Please note, that the function only accepts
#' years in between 1977 and 2020.
#' @export
#' @examples
#' df.west_2007 <- confdf('western', 2007)
#' df.east_1999 <- confdf('eastern', 1999)
confdf <- function(conference, year){

  if (year >= 2021 || year < 1977){

    stop('Please select a year between 1977 and 2020')
    }

  else{

    web <- paste("https://www.basketball-reference.com/leagues/NBA_",year,".html", sep="")
    page <- xml2::read_html(web)

    if (conference == 'eastern') {

      if (year < 2021 && year >= 2016) {
        tabw <- rvest::html_node(page, xpath = '//*[@id="confs_standings_E"]')
        tab <-  rvest::html_table(tabw, fill = T)
        tab <- dplyr::select(tab, Team = 'Eastern Conference', WinPer = 'W/L%')
        tab$Team <-  stringr::str_replace(tab$Team, "\\*", "")
        tab['Year'] <- year
        tab['Conference'] <- toTitleCase(conference)
      }

      else if (year < 2016 && year >= 1977) {

        tabw <- rvest::html_node(page, xpath = '//*[@id="divs_standings_E"]')
        tab <-  rvest::html_table(tabw, fill = T)
        tab <- dplyr::select(tab, Team = 'Eastern Conference', WinPer = 'W/L%')
        tab$Team <-  stringr::str_replace(tab$Team, "\\*", "")
        tab$WinPer = as.numeric(tab$WinPer)
        tab['Year'] <- year
        tab['Conference'] <- tools::toTitleCase(conference)
        tab <- tab[complete.cases(tab$WinPer),]
        tab <- tab[order(-tab[,2]),]
        rownames(tab) <- 1:nrow(tab)
      }
    }


    else if (conference == 'western') {

      if (year < 2021 && year >= 2016) {

        tabw <- rvest::html_node(page, xpath = '//*[@id="confs_standings_W"]')
        tab <-  rvest::html_table(tabw, fill = T)
        tab <- dplyr::select(tab, Team = 'Western Conference', WinPer = 'W/L%')
        tab$Team <- stringr::str_replace(tab$Team, "\\*", "")
        tab['Year'] <- year
        tab['Conference'] <- tools::toTitleCase(conference)
      }

      else if (year < 2016 && year >= 1977) {

        tabw <- rvest::html_node(page, xpath = '//*[@id="divs_standings_W"]')
        tab <- rvest::html_table(tabw, fill = T)
        tab <- dplyr::select(tab, Team = 'Western Conference', WinPer = 'W/L%')
        tab$Team <-  stringr::str_replace(tab$Team, "\\*", "")
        tab$WinPer = as.numeric(tab$WinPer)
        tab['Year'] <- year
        tab['Conference'] <- tools::toTitleCase(conference)
        tab <- tab[complete.cases(tab$WinPer),]
        tab <- tab[order(-tab[,2]),]
        rownames(tab) <- 1:nrow(tab)
      }
    }
  }

  return(tab)
}
