#' NBA Conferences Winning Percentage Plotter
#' @description Creates a winning percentage bar chart for NBA teams in a chosen conference and year (from 1977 to 2020)
#' @param conference - character value, either 'eastern' or 'western'
#' @param year - numeric value, a year in which the desired season has ended (e.g. year = 2008 will provide
#' a graph for a 2007-2008 season, year = 2017 for 2016-17 etc.). Please note, that the function only accepts
#' years in between 1977 and 2020.
#' @export
#' @examples
#' confplot('eastern', 2007)
#' confplot('western', 1999)

confplot <- function(conference, year){

  if (year >= 2021 || year < 1977)

  {
    stop("Please select a year between 1977 and 2020")
  }

  else

  {
    web <- paste("https://www.basketball-reference.com/leagues/NBA_",year,".html", sep="")
    page <- xml2::read_html(web)

    if (conference == "eastern")

    {
      if (year < 2021 && year >= 2016)

      {
        tabw <- rvest::html_node(page, xpath = '//*[@id="confs_standings_E"]')
        tab <-  rvest::html_table(tabw, fill = T)
        tab <- dplyr::select(tab, Team = 'Eastern Conference', WinPer = 'W/L%')
        tab$Team <-  stringr::str_replace(tab$Team, "\\*", "")
        eightseed <- tab[8,2]
      }

      else if (year < 2016 && year >= 1977)

      {
        tabw <- rvest::html_node(page, xpath = '//*[@id="divs_standings_E"]')
        tab <-  rvest::html_table(tabw, fill = T)
        tab <- dplyr::select(tab, Team = 'Eastern Conference', WinPer = 'W/L%')
        tab$Team <-  stringr::str_replace(tab$Team, "\\*", "")
        tab$WinPer = as.numeric(tab$WinPer)
        tab <- tab[complete.cases(tab$WinPer),]
        tab <- tab[order(-tab[,2]),]
        rownames(tab) <- 1:nrow(tab)

        if (year < 2016 && year >= 1984)

        {
          eightseed <- tab[8,2]
        }

        else if (year < 1984 && year >= 1977)

        {
          eightseed <- tab[6,2]
        }
      }
    }

    else if (conference == "western")

    {
      if (year < 2021 && year >= 2016)

      {
        tabw <- rvest::html_node(page, xpath = '//*[@id="confs_standings_W"]')
        tab <-  rvest:html_table(tabw, fill = T)
        tab <- dplyr::select(tab, Team = 'Western Conference', WinPer = 'W/L%')
        tab$Team <-  stringr::str_replace(tab$Team, "\\*", "")
        eightseed <- tab[8,2]
      }

      else if (year < 2016 && year >= 1977)

      {
        tabw <- rvest::html_node(page, xpath = '//*[@id="divs_standings_W"]')
        tab <-  rvest::html_table(tabw, fill = T)
        tab <- dplyr::select(tab, Team = 'Western Conference', WinPer = 'W/L%')
        tab$Team <-  stringr::str_replace(tab$Team, "\\*", "")
        tab$WinPer = as.numeric(tab$WinPer)
        tab <- tab[complete.cases(tab$WinPer),]
        tab <- tab[order(-tab[,2]),]
        rownames(tab) <- 1:nrow(tab)

        if (year < 2016 && year >= 1984)

        {
          eightseed <- tab[8,2]
        }

        else if (year < 1984 && year >= 1977)

        {
          eightseed <- tab[6,2]
        }
      }
    }

    else
    {
      stop("Please select either 'eastern' or 'weastern' conference.")
    }
  }
  ggplot2::ggplot(tab, ggplot2::aes(x = reorder(Team, WinPer), y = WinPer)) +
    ggplot2::theme_bw() +
    ggplot2::geom_bar(stat = "identity", fill = ifelse(tab$WinPer >= eightseed, "#FFDB6D", "lightgray"), color = "black") +
    ggplot2::coord_flip() +
    ggplot2::labs(y = "Winning percentage", x = "",
         title = paste("Winning percentage of the NBA's", tools::toTitleCase(conference) ,"Conference teams"),
         caption = "Teams with yellow bars were promoted to the playoffs",
         subtitle = paste(year - 1, "-", year, "season")) +
    ggplot2::scale_y_continuous(breaks = seq(0, 1, by = 0.1))
}
