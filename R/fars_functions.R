#' Read Fatality Analysis Reporting System (FARS) Data
#'
#' This is a function that read data from file referenced by \code{filename}
#' into a tbl_df class object. It will give an error and exist if the file
#' \code{filename} does not exist in the working directory. The source data
#' is from the US National Highway Traffic Safety Administration's Fatality
#' Analysis Reporting System (FARS):
#' \url{https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars}
#'
#' @param filename A character string of file name whose data to be read to
#'    an object of class tbl_df.
#'
#' @return This function returns a table/data frame with data from the
#'    supplied file name.
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @examples
#'    \dontrun{fars_read("accident_2014.csv.bz2")}
#'    \dontrun{fars_read("data/accident_2014.csv.bz2")}
#'
#' @export
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}


#' Construct FARS filename from year input parameter
#'
#' This is a function that takes 4 digit \code{year} as input and
#' construct the FARS filename for the year.
#'
#' @param year A 4 digit integer or string indicating the year
#'
#' @return This function returns a FARS filename corresponding to the
#'    year.
#'
#' @examples
#'    \dontrun{make_filename(2015)}
#'    \dontrun{make_filename("2015")}
#'
#' @export
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' Read multi-year Fatality Analysis Reporting System (FARS) Data
#'
#' This is a function that takes a vector of 4 digit, called \code{years},
#' as input and read FARS data into a list of data frames, one for each year.
#' Each of the year input is converted to FARS filename via \code{make_filename}.
#' Each data frame only contains the year and month. If FARS data file
#' is not available any of the given year, the list entry for that year
#' will be NULL
#'
#' @param years An vector of 4 digit integer or string indicating the year
#'
#' @return This function returns a list of data frame with FARS data for
#'    the years.
#'
#' @importFrom dplyr mutate select
#'
#' @examples
#'    \dontrun{fars_read_years(c(2014,2015))}
#'    \dontrun{fars_read_years((2013:2015))}
#'
#' @export
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

#' Summarize multi-year Fatality Analysis Reporting System (FARS) Data
#'
#' This is a function that takes a vector of 4 digit, called \code{years},
#' as input, read FARS data into a list of data frames, one for each year
#' via \code{fars_read_years}. These multi-year FARS data will be summarized
#' by counting the number of rows that exist for each year and month.
#' The summarized data frame will have months as rows, and years as columns, while
#' the row counts as the intersection of rows and columns.
#'
#' @param years An vector of 4 digit integer or string indicating the year
#'
#' @return This function returns a data frame with FARS data for summarized
#'    by years and months.
#'
#' @importFrom dplyr bind_rows group_by summarize
#' @importFrom magrittr %>%
#' @importFrom tidyr spread
#'
#' @examples
#'    \dontrun{fars_summarize_years(c(2014,2015))}
#'    \dontrun{fars_summarize_years((2013:2015))}
#'
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#' Plot Fatality Analysis Reporting System (FARS) Data By State
#'
#' This is a function that takes an integer representing \code{state.num}
#' and 4 digit \code{year} as input and FARS data into a data frames using
#' \code{make_filename} and \code{fars_read}.
#' The FARS data is filtered by \code{state.num}, and if the filter criteria
#' does not exist in the dataset, the function will throw an error for invalid state.
#' The filtered data will be plotted as a heat map where area with more accidents
#' will be shown in darker spots. The function will give a message "no accident to plot"
#' if there is no accident to report for the state and year.
#'
#' @param state.num An integer of 4 indicating the state number. Valid values
#'    are 1 to 51, except 3
#' @param year A 4 digit integer or string indicating the year
#'
#' @return This function does not have any return value, but it will plot it will
#'    plot a heat map of the accident for the given state and year.
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @note Though the function anticipates giving a message if no accident is reported
#' for a state in a given year, there is a very good chance that this portion of the
#' code will not be trigerred due to a check to ensure the \code{state.num} filter
#' parameter is in the data set.
#'
#' @examples
#'    \dontrun{fars_map_state(1,2015)}
#'
#' @export
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
