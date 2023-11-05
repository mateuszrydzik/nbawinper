#' NBA Divisions Winning Percentage Plotter
#' @description Creates a winning percentage boxplot for NBA teams
#' in a chosen division and selected timespan
#' (between 2005 and 2020)
#' @param division - character value, 'northwest', 'pacific', 'southwest'
#' for the Western Conference, and 'atlantic', 'central', 'southeast'
#' for the Eastern Conference
#' @param first_year - first year, from which the data will be
#' presented on the graph
#' @param last_year - last year, that closes the selected timespan
#' @export
#' @importFrom rlang .data
#' @examples
#' divplot('southwest', 2014, 2018)
#' divplot('central', 2009, 2012)

divplot <- function(division, first_year, last_year) {

  if (first_year < 2005 || last_year > 2020) {
    stop("Please select a timespan between 2005 and 2020.")
  }

  years <- first_year:last_year
  dataframe <- base::data.frame()
  div_west <- c("northwest", "pacific", "southwest")
  div_east <- c("atlantic", "central", "southeast")

  if (base::is.element(division, div_west)) {
    suffix <- "W"
    conf_name <- "Western Conference"
  } else if (base::is.element(division, div_east)) {
    suffix <- "E"
    conf_name <- "Eastern Conference"
  } else {
    stop("Please select a division: 'northwest', 'pacific', 'southwest'
    for the Western Conference, and 'atlantic', 'central', 'southeast'
    for the Eastern Conference")
  }
  xpath <- base::paste0("//*[@id='divs_standings_", suffix, "']")
  for (i in years) {
    base::Sys.sleep(5)
    web <- base::paste0("https://www.basketball-reference.com/leagues/NBA_",
    i, ".html")
    page <- xml2::read_html(web)

    tab <- rvest::html_node(page, xpath = xpath)
    tab <- rvest::html_table(tab, fill = TRUE)
    tab <- dplyr::select(tab, team = conf_name, winper = "W/L%")
    tab["year"] <- i
    tab$team <-  stringr::str_replace(tab$team, "\\*", "")

    if (base::is.element(division, c("northwest", "atlantic"))) {
      tab <- tab[2:6, ]
    } else if (base::is.element(division, c("pacific", "central"))) {
      tab <- tab[8:12, ]
    } else if (base::is.element(division, c("southwest", "southeast"))) {
      tab <- tab[14:18, ]
    }

    tab <- tab[grepl("\\.", tab$winper), ]
    tab$winper <- base::as.numeric(tab$winper)
    tab <- tab[stats::complete.cases(tab$winper), ]
    tab <- dplyr::arrange(tab, dplyr::desc(.data$winper))
    dataframe <- dplyr::bind_rows(tab, dataframe)
  }

  ggplot2::ggplot(dataframe,
  ggplot2::aes(x = stats::reorder(.data$team, -.data$winper), .data$winper)) +
    ggplot2::theme_bw() +
    ggplot2::geom_boxplot(fill = 'lightgray', color = 'black') +
    ggplot2::coord_flip() +
    ggplot2::labs(y = 'Winning percentage',
          x = '',
          title = paste("Winning percentage of the NBA's",
          tools::toTitleCase(division), "Division teams"),
          subtitle = paste("Years ", first_year, "-", last_year)) +
    ggplot2::scale_y_continuous(breaks = seq(0, 1, by = 0.1))

}
