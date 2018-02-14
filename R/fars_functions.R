#' Read a csv data file into a table dataframe (tbl_df)
#'
#' This is a function that reads a csv data file into a table dataframe (tbl_df).
#' You are supposed to provide the name of a dataset (csv file) to read as the argument.
#'
#' @param filename A character string giving the name of the dataset to read in
#'
#' @return This function returns a table dataframe (tbl_df) if it is given
#'    a valid name or a notification or a notification "file 'filename' does not exist"
#'
#' @examples \dontrun{
#' filname("somedata.csv") }
#'
#' @importFrom readr dplyr
#'
#' @note if there is no file wit hthe given filename, the function will return an error
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


#' Make a filename in the format used for FARS data files
#'
#' This is a function that takes a year in numeric format as an argument
#' and returns a filename of the corresponding FARS data file for the given year.
#'
#' @param year A year in the numeric format
#'
#' @return This function returns a character string that corresponds to the name of
#' the FARS datafile for the given year
#'
#' @examples
#' make_filename(2015)
#'
#' @note spaces, commas or other characters in the numeric argumet will retur an error
#'
#' @export
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}



#' Read in the data on MONTH variable from multiple FARS datafiles for different years
#'
#' This is a function that takes a year or a list of years in numeric format, reads in
#' the FARS data files for the corresponding years, and returns a list of tables for each
#' year. Each table contains only the data for two variables: the year and months for which
#' FARS data are available in that year
#'
#' @param years a year in numeric format or a list of such years
#'
#' @return This function returns a list, each element of which is a two-column table (tibble)
#' with one column describing the year and the other column describing the month for each observation
#' in the FARS dataset.
#'
#' @examples \dontrun{
#' fars_read_years(2015)
#' fars_read_years(list(2014, 2015)) }
#'
#'
#' @note providing multiple years that are not in the list format as an argument will cause an error.
#' Also, the function will produce an error if the dplyr package is not loaded.
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




#' Summarize month data for multiple years
#'
#' This is a function that takes a year or a list of years in numeric format and
#' generates a summary table that describes the number of observations in FARS datafiles
#' for each month in each year.
#'
#' @param years a year in numeric format or a list of such years
#'
#' @return this function returns a table (tibble) with one column for each given year and
#' up to twelve rows each representing a month.
#'
#' @examples \dontrun{
#' fars_summarize_years(2015)
#' fars_summarize_years(list(2013, 2014, 2015)) }
#'
#' @importFrom dplyr tidyr
#'
#' @note if the list that is provided as an argument contains a year for which the dataset
#' is unavailable, it will produce an error.
#'
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}


#' Plot a map of accidents in a given state for a given year
#'
#' This is a function that takes a state number and a year and plots
#' accidents on the states's map
#'
#' @param state.num a state in numeric format
#' @param year a year in numeric format or a list of such years
#'
#' @return this function returns a map on the graphical device, a message
#' "no accidents to plot", or a message saying that the provided state number is invalid.
#'
#' @examples \dontrun{
#' fars_map_state(6, 2015) }
#'
#' @importFrom maps graphics
#'
#' @note requires the package maps to be loaded, otherwise produces an error.
#' For some states, the function fails with the message "nothing to draw: all regions out of bounds",
#' e.g. for state.num = 2.
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
