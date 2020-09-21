#' get_crimes
#'
#' Fetches crimes from the police.uk API within a given sf polygon.
#'
#' @param polygon an sf polygon within which to request the crimes data
#' @param start_month first month for which to get data for in the format "YYYY-MM". Defaults to most recent month available.
#' @param end_month last month for which to get data for in the format "YYYY-MM". Defaults to most recent month available.
#'
#' @return street level crimes as dataframe
#'
#' @import sf
#' @import httr
#' @import dplyr
#' @import progress
#'
#'
#' @examples
#' camden_poly <- localauth_ldn[localauth_ldn$lad20nm == 'Camden',]
#' get_crimes(camden_poly, start_month = '2020-03', end_month = '2020-07')
#'
#' @export

get_crimes <- function(polygon, start_month='latest', end_month='latest'){
  ## get available dates for validation checks
  req <- httr::GET(url = 'https://data.police.uk/api/crimes-street-dates')
  res <- httr::content(req, simplifyDataFrame=TRUE, flatten=TRUE)
  if(httr::http_error(req)) {
    stop(paste0('Something went wrong. Error message: ',httr::http_status(req)$message))
  } else{
    available_dates <- paste0(as.Date(paste0(res$date, '-01')))
  }


  ## Form list of months to loop over ----

  # Use latest month or run checks if date out of range
  if(start_month=='latest') {
    start_month <-  res[1,1]
    start_date <- as.Date(paste0(start_month, '-01'))
  } else{
    start_date <- as.Date(paste0(start_month, '-01'))
  }

  if(end_month=='latest') {
    end_month <- res[[1,1]]
    end_date <- as.Date(paste0(end_month, '-01'))
  } else {
    end_date <- as.Date(paste0(end_month, '-01'))
  }

  ## run validation checks on input parameters
  if(start_date < min(available_dates) | start_date > max(available_dates) ){
    stop(paste0("Date out of range. Enter months between ", substr(min(available_dates),1,7), " and ", substr(max(available_dates),1,7)))
  }

  if(end_date < min(available_dates) | end_date > max(available_dates) ){
    stop(paste0("Date out of range. Enter months between ", substr(min(available_dates),1,7), " and ", substr(max(available_dates),1,7)))
  }

  if(start_date > end_date)  stop(paste0("Start month cannot be later than the end month"))

  # Form list of months for loop
  dates <- seq.Date(start_date,end_date,by = '1 month')


  ## Convert polygon to points ----

  poly_coords <- polygon %>% # Need to ask Chris if it's okay to pipe
    sf::st_coordinates() %>%
    as.data.frame() %>%
    select(X, Y)

  # Create character list of dates to pass into POST request
  poly_coords_list <- paste0(round(poly_coords$Y,5),',',round(poly_coords$X,5), collapse=':')


  ## Loop over POST requests for each month ----

  df_out <- data.frame(category=(character()),
                             latitude=character(),
                             longitude=character(),
                             month=character(),
                             stringsAsFactors=FALSE)

  if(length(dates) > 2) {
    pb <- progress::progress_bar$new(total = length(dates))
  }
  for (i in seq_along(dates)) {
    if(length(dates) > 2) pb$tick()
    dt <- substr(dates[i],1,7)

    pth <- 'https://data.police.uk/api/crimes-street/all-crime'

    req <- httr::POST(url = pth,body = list(date = dt, poly = poly_coords_list))

    res <- httr::content(req, simplifyDataFrame=TRUE, flatten=TRUE) %>%
      dplyr::select(category,latitude=location.latitude,longitude=location.longitude,month)

    if(nrow(res) ==10000) {
      # warning if the data frame is 10,000 observations long-the max size for a single request.
      warning(paste0('Looks like we hit the 10,000 observation limit for crimes. Data will be incomplete. Use a smaller area.'))
    }
    df_out <- rbind(df_out,res)
  }

  return(df_out)
}
