library(dplyr)

get_br_data <- function(conference, year) {
  if (year < 1971) {
    stop("Please select a year after 1970")
    }

  if (conference != "eastern" && conference != "western") {
  stop("Please select a conference: 'eastern' or 'western'")
  }

  #scraping data for selected year
  web <- paste("https://www.basketball-reference.com/leagues/NBA_",
  year, ".html", sep = "")
  page <- xml2::read_html(web)

  # filling variables with proper values for selected conference
  if (conference == "eastern") {
    conf_name <- "Eastern Conference"
    xpath_suffix <- "E"
  } else if (conference == "western") {
    conf_name <- "Western Conference"
    xpath_suffix <- "W"
  }

  if (year >= 2016) {
    xpath_prefix <- "confs"
  } else {
    xpath_prefix <- "divs"
  }

  xpath <- paste0(xpath_prefix, "_standings_", xpath_suffix)
  tabw <- rvest::html_element(page, xpath = paste0("//*[@id='", xpath, "']"))
  tab <-  rvest::html_table(tabw, fill = TRUE)
  tab <- dplyr::select(tab, team = conf_name, winper = "W/L%")

  # tables before 2016 have different structure
  if (year < 2016) {
    tab$winper <- as.numeric(tab$winper)
    tab <- tab[complete.cases(tab$winper), ]
    tab <- tab[order(-tab[, 2]), ]
  }

  tab <- tab %>%
    mutate(playoffs = stringr::str_detect(team, "\\*")) %>%
    mutate(team = stringr::str_replace(team, "\\*", ""))

  return(tab)
}