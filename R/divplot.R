#' NBA Divisions Winning Percentage Plotter
#' @description Creates a winning percentage boxplot for NBA teams in a chosen division and selected timespan
#' (between 2005 and 2020)
#' @param division - character value, 'northwest', 'pacific', 'southwest' for the Western Conference,
#' and 'atlantic', 'central', 'southeast' for the Eastern Conference
#' @param first.year - first year, from which the data will be presented on the graph
#' @param last.year - last year, that closes the selected timespan
#' @export
#' @examples
#' divplot('southwest', 2014, 2018)
#' divplot('central', 2009, 2012)

divplot <- function(division, first.year, last.year){

  if (first.year < 2005 || last.year > 2020)

  {
    stop('Please select a timespan between 2005 and 2020.')
  }

  else

  {
    years <- first.year:last.year
    dataframe <- data.frame()
    div_west <- c('northwest', 'pacific', 'southwest')
    div_east <- c('atlantic', 'central', 'southeast')

    for (i in years)

    {
      web <- paste("https://www.basketball-reference.com/leagues/NBA_",i,".html", sep="")
      page <- xml2::read_html(web)


      if (is.element(division, div_west))

      {
        tabw <- rvest::html_node(page, xpath = '//*[@id="divs_standings_W"]')
        tab <- rvest::html_table(tabw, fill = T)
        tab <- dplyr::select(tab, Team = 'Western Conference', WinPer = 'W/L%')
        tab['Year'] <- i
        tab$Team <-  stringr::str_replace(tab$Team, "\\*", "")

        if (division == 'northwest')

        {
          tab <- tab[2:6,]
        }

        else if (division == 'pacific')

        {
          tab <- tab[8:12,]
        }

        else if (division == 'southwest')

        {
          tab <- tab[14:18,]
        }

        tab$WinPer = as.numeric(tab$WinPer)
        tab <- tab[complete.cases(tab$WinPer),]
        tab <- tab[order(-tab[,2]),]
        rownames(tab) <- 1:nrow(tab)
        dataframe <- dplyr::bind_rows(tab, dataframe)
      }

      else if (is.element(division, div_east))

      {
        tabw <- rvest::html_node(page, xpath = '//*[@id="divs_standings_E"]')
        tab <- rvest::html_table(tabw, fill = T)
        tab <- dplyr::select(tab, Team = 'Eastern Conference', WinPer = 'W/L%')
        tab['Year'] <- i
        tab$Team <- stringr::str_replace(tab$Team, "\\*", "")

        if (division == 'atlantic')

        {
          tab <- tab[2:6,]
        }

        else if (division == 'central')

        {
          tab <- tab[8:12,]
        }

        else if (division == 'southeast')

        {
          tab <- tab[14:18,]
        }

        tab$WinPer = as.numeric(tab$WinPer)
        tab <- tab[complete.cases(tab$WinPer),]
        tab <- tab[order(-tab[,2]),]
        rownames(tab) <- 1:nrow(tab)
        dataframe <- dplyr::bind_rows(tab, dataframe)
      }
    }

    ggplot2::ggplot(dataframe, ggplot2::aes(x = reorder(Team, -WinPer), WinPer)) +
      ggplot2::theme_bw() +
      ggplot2::geom_boxplot(fill = 'lightgray', color = 'black') +
      ggplot2::coord_flip() +
      ggplot2::labs(y = 'Winning percentage',
           x = '',
           title = paste("Winning percentage of the NBA's", tools::toTitleCase(division), "Division teams"),
           subtitle = paste('Years ', first.year, '-', last.year)) +
      ggplot2::scale_y_continuous(breaks = seq(0, 1, by = 0.1))
  }
}
