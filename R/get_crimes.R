#' get_crimes
#'
#' Fetches crimes from the police.uk API within a given sf polygon.
#'
#' @param polygon an sf polygon within which to request the crimes data
#' @param start_month first month for which to get data for. Defaults to most recent month available.
#' @param end_month last month for which to get data for. Defaults to most recent month available.
#'
#' @return street level crimes as dataframe
#'
#' @import sf
#' @import httr
#' @import dplyr
#'
#'
#' @examples
#'
#'
#' @export

get_crimes <- function(polygon, start_month='latest', end_month='latest'){

  ## Form list of months to loop over ----

  # Retrieve latest month from API if necessary
  if(start_month=='latest') {

    req <- httr::POST(url = 'https://data.police.uk/api/crimes-street-dates')

    res <- httr::content(req, simplifyDataFrame=TRUE, flatten=TRUE)

    start_month = res[[1,1]]
  }

  if(end_month=='latest') {

    req <- httr::POST(url = 'https://data.police.uk/api/crimes-street-dates')

    res <- httr::content(req, simplifyDataFrame=TRUE, flatten=TRUE)

    end_month = res[[1,1]]
  }

  # Form list of months for loop
  dates <- seq.Date(as.Date(paste0(start_month,"-01")),as.Date(paste0(end_month,"-01")),by = '-1 month')


  ## Convert polygon to points ----

  poly_coords <- polygon %>% # Need to ask Chris if it's okay to pipe
    sf::st_coordinates() %>%
    as.data.frame() %>%
    select(X, Y)

  # Create chracter list of dates to pass into POST request
  poly_coords_list <- paste0(round(poly_coords$Y,5),',',round(poly_coords$X,5), collapse=':')


  ## Loop over POST requests for each month ----

  df_out <- data.frame(category=(character()),
                             latitude=character(),
                             longitude=character(),
                             month=character(),
                             stringsAsFactors=FALSE)

  for (i in 1:length(dates)) {
    dt <- substr(dates[i],1,7)

    pth <- 'https://data.police.uk/api/crimes-street/all-crime'

    req <- httr::POST(url = pth,body = list(date = dt, poly = poly_coords_list))

    res <- httr::content(req, simplifyDataFrame=TRUE, flatten=TRUE) %>%
      select(category,latitude=location.latitude,longitude=location.longitude,month)

    df_out <- bind_rows(df_out,res)
  }

  return(df_out)
}
