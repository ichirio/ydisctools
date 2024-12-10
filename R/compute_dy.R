#' Compute Day Difference Between Two Dates
#'
#' This function calculates the day difference between a reference date and an event date. If the difference is non-negative, the result is incremented by 1 day.
#'
#' @param refdate A reference date, typically the starting point. Can be a character string or Date object.
#' @param eventdate An event date, typically the comparison date. Can be a character string or Date object.
#' @return The difference in days between the reference date and the event date. If the difference is non-negative, the result is incremented by 1.
#' @examples
#' compute_dy("2023-09-15", "2023-09-20")
#' @export
#' @import lubridate dplyr
compute_dy <- function(refdate, eventdate) {
    interval(as.Date(refdate, format = "%Y-%m-%d"), as.Date(eventdate, format = "%Y-%m-%d")) %>%
    as.numeric(units = "days") %>%
    if_else(. >= 0, . + 1, .)
}
