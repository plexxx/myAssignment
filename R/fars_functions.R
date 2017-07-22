#' Read a file into a data frame tbl.
#'
#' This function reads a given comma separated values (CSV) file from the
#' current working directory and creates a data frame tbl from its contents.
#' @param filename the full file name of the CSV file to read
#' @return a data frame tbl containing the data in the file read
#' @details All messages during reading are being suppressed.
#'
#' The function will throw an error if a file with the given file name does not exist
#' @examples dat <- fars_read("accident_2013.csv.bz2")
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}

#' Create a file name from a given year.
#'
#' This function creates a string that is the file name for the FARS data set
#' of a given year.
#' @param year the number of the year for whose data set you want the file name
#' @return the file name of the FARS data set for the given year
#' @details The function will try to cast the parameter into an integer. Errors
#' may result from casting a variable that cannot be cast into an integer.
#' @examples f <- make_filename(2013)
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' Reads FARS data sets for multiple years and creates a list of tibbles.
#'
#' This function reads FARS data sets for multiple years from the current
#' working directory and creates a list of tibbles. The list contains as many
#' elements as years were given. Each tibble has two columns: \code{MONTH}
#' and \code{year}, where \code{year} is the given year and \code{MONTH}
#' contains the number of the month to which each observation belongs.
#' @param years a vector of integers, one for each year for which you want to
#' load the FARS data
#' @return a list of tibbles, one for each year given
#' @examples fars_data <- fars_read_years(c(2013, 2014))
#' @details This function uses \code{fars_read} and \code{make_filename}. These
#' functions may throw errors, for example, if files with names for the
#' requested FARS datasets do not exists in the current working directory.
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom magrittr %>%
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

#' Counts the number of observations per month for given years.
#'
#' This function takes multiple year numbers, reads the FARS data sets for
#' the given years from the current working directory and counts the number
#' of observations per month for each year. The result is a tibble with a
#' number of columns equal to the number of years given and 12 rows, one for
#' each month. Each cell of the tibble contains the number of observations per
#' month and year.
#' @param  years a vector of year numbers.
#' @return a tibble with 12 rows and a number of columns equal to the number of
#' years given.
#' @examples y <- fars_summarize_years(c(2013, 2014))
#' @inherit fars_read_years details
#' @importFrom dplyr bind_rows
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom tidyr spread
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#' Plots all accidents in a given state during a given year in a map.
#'
#' This function takes the number of a state and the number of a year and plots
#' all accidents from the FARS data set which happened during the given year in
#' the given state. Each accident is represented by a point on a map of the
#' given state.
#' @param state.num the index number of a state
#' @param year the number of a year
#' @return NULL
#' @details This function returns NULL, but as a side effect the state map as
#' described above is plotted.
#'
#' This function throws an error if the given state number does not
#' occur in the FARS data set.
#'
#' Errors may also be thrown by calls to the plotting functions, such as
#' \code{maps::map("state")}. This happens, for example when calling
#' \code{fars_map_state(2, 2013)}.
#'
#' One reason for errors could be missing data in the requested data set. This
#' function implicitly sets missing longitudes to 900 and missing latitudes to
#' 90. These values may be outside the bounds of the state map and therefore
#' cause plotting to throw errors.
#' @inherit fars_read_years details
#' @examples fars_map_state(1, 2013)
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
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
