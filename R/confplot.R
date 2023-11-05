#' NBA Conferences Winning Percentage Plotter
#' @description Creates a winning percentage bar chart for NBA teams
#' in a chosen conference and year (from 1977 to 2020)
#' @param conference - character value, either 'eastern' or 'western'
#' @param year - numeric value, a year in which the desired season has ended
#' (e.g. year = 2008 will provide a graph for a 2007-2008 season,
#' year = 2017 for 2016-17 etc.).
#' Please note, that the function only accept years fr0m 1970.
#' @export
#' @importFrom rlang .data
#' @examples
#' confplot('eastern', 2007)
#' confplot('western', 1999)

confplot <- function(conference, year) {
  tab <- confdf(conference, year)
  ggplot2::ggplot(tab, ggplot2::aes(x = stats::reorder(.data$team, .data$winper),
  y = .data$winper)) +
    ggplot2::theme_bw() +
    ggplot2::geom_bar(stat = "identity", fill = ifelse(tab$playoffs == TRUE,
    "#FFDB6D", "lightgray"), color = "black") +
    ggplot2::coord_flip() +
    ggplot2::labs(y = "Winning percentage", x = "",
         title = paste("Winning percentage of the NBA's",
         tools::toTitleCase(conference), "Conference teams"),
         caption = "Teams with yellow bars were promoted to the playoffs",
         subtitle = paste(year - 1, "-", year, "season")) +
    ggplot2::scale_y_continuous(breaks = seq(0, 1, by = 0.1))
}
