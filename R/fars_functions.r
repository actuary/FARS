#' Read FARS data
#'
#' This function reads in FARS from the given file path,
#' if it exists.
#'
#' @param filename This is the path to the fars data.
#'
#' @return This function returns on object of class tbl_df
#'     (a 'tibble') containing the data, if the filepath
#'     exists. Errors out if given filepath does not exist.
#'
#' @author John J Smith, \email{fakeemail123@@duvman.ie}
#' @keywords FARS
#'
#' @examples
#' fars_read("data\\accident_2013.csv.bz2")
#' fars_read("data\\accident_2014.csv.bz2")
#' fars_read("data\\accident_2015.csv.bz2")
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' Make FARS file name
#'
#' This function generates a file name for FARs data.
#'
#' @param year This is the year in YYYY format of the FARS
#'     data.
#'
#' @return This function returns a string representing the
#'     FARS data filename for a given yeare
#'
#' @author John J Smith, \email{fakeemail123@@duvman.ie}
#' @keywords FARS
#'
#' @examples
#' make_filename("2013")
#' make_filename("2014")
#' make_filename("2015")
#'
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("daccident_%d.csv.bz2", year)
}
#' Read FARs data for years
#'
#' This function loads in FARS data for multiple years.
#'
#' @param years This is a list/vector of years in YYYY format
#'
#' @return This function returns a list of FARS data for
#'     each year, if the data is found. NULL is returned
#'     for entries that are not found, along with a warning.
#'
#' @author John J Smith, \email{fakeemail123@@duvman.ie}
#' @keywords FARS
#'
#' @examples
#' \dontrun{
#' fars_read_years(c("2013", "2014", "2015"))
#' }
#' @export
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate select
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}
#' Summarise FARs data for years
#'
#' This function reads in and then summarises the FARs data into
#' number of observations by month and year
#'
#' @param years This is a list/vector of years in YYYY format
#'
#' @return This function returns a tibble containing the number
#'     of observations in the given FARs years for each month
#'     in the data. The rows are the months and the columns are
#'     the given years
#'
#' @author John J Smith, \email{fakeemail123@@duvman.ie}
#' @keywords FARS
#'
#' @examples
#' \dontrun{
#' fars_summarize_years(c("2013", "2014", "2015"))
#' }
#' @export
#' @importFrom magrittr %>%
#' @importFrom dplyr tbl_df n group_by bind_rows summarize
#' @importFrom tidyr spread
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}
#' Plot FARS data for given state and year
#'
#' This function reads in the FARs data and plots the given state with each accident
#' represented by a black dot on the plot. User should be careful that the state number
#' exists in the data and that there are accidents for that state, otherwise slight change
#' will give a warning.
#'
#' @param state.num This is the number of a state. Should be valid
#' @param year This is the year being considered in YYYY format
#'
#' @return This function returns NULL
#'
#' @author John J Smith, \email{fakeemail123@@duvman.ie}
#' @keywords FARS
#'
#' @examples
#' \dontrun{
#' fars_map_state(1, 2013)
#' fars_map_state(4, 2013)
#' }
#' @export
#' @importFrom dplyr filter
#' @importFrom graphics points
#' @importFrom maps map
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}


