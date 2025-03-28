#' Convert character date and time to ISO 8601 format
#'
#' This function takes a datetime string and optionally a time string, and converts them to ISO 8601 format.
#' The type parameter controls the order of year, month, and day.
#'
#' @param datetime A character vector representing the date or datetime.
#' @param time A character vector representing the time (optional).
#' @param type A character to specify the format of the date ("DMY", "YMD", "MDY", "YDM").
#'
#' @return A character vector in ISO 8601 format.
#'
#' @import stringr purrr dplyr
#'
#' @export
conv_iso8601 <- function(datetime, time = NA, type = "DMY") {
  if(type == "DMY")      order <- c("Y" = 3, "M" = 2, "D" = 1)
  else if(type == "YMD") order <- c("Y" = 1, "M" = 2, "D" = 3)
  else if(type == "MDY") order <- c("Y" = 3, "M" = 1, "D" = 2)
  else if(type == "YDM") order <- c("Y" = 1, "M" = 3, "D" = 2)
  else                   order <- c("Y" = 3, "M" = 2, "D" = 1)
  priority <- c("Y" = 1, "M" = 2, "D" = 3)

  get_index <- function(parts, num) {
    o <- order[parts]
    p <- priority[parts]
    ym <- sort(order[names(order) %in% c("Y", "M")])
    ym[1:2] <- 1:2

    if(num <= 0) return(NULL)
    else if(num >= 3) return(o)
    else if(num == 1) {
      if(p == 1) return(p)
      else return(NULL)
    }
    else if(num == 2) {
      if(p %in% 1:2) return(ym[parts])
      else return(NULL)
    }
  }

  get_year <- function(year = NA) {
    if(length(year) == 0L) year <- NA
    if(is.na(year)) return("")

    num_year <- suppressWarnings(as.numeric(year))
    if(is.na(num_year)) return(year)

    n <- str_length(year)
    if(n <= 2) return(as.character(if_else(num_year > 50, num_year + 1900, num_year + 2000)))
    else return(year)
  }

  get_month <- function(month = NA) {
    if(length(month) == 0L) month <- NA
    if(is.na(month)) return("")

    num_month <- suppressWarnings(as.numeric(month))
    if(!is.na(num_month)) return(sprintf("%02d", num_month))

    month <- str_sub(month, 1, 3)
    if     (toupper(month) == "JAN") return("01")
    else if(toupper(month) == "FEB") return("02")
    else if(toupper(month) == "MAR") return("03")
    else if(toupper(month) == "APR") return("04")
    else if(toupper(month) == "MAY") return("05")
    else if(toupper(month) == "JUN") return("06")
    else if(toupper(month) == "JUL") return("07")
    else if(toupper(month) == "AUG") return("08")
    else if(toupper(month) == "SEP") return("09")
    else if(toupper(month) == "OCT") return("10")
    else if(toupper(month) == "NOV") return("11")
    else if(toupper(month) == "DEC") return("12")
    else if(toupper(month) == "UNK") return("")
    else if(toupper(month) == "UN")  return("")
    else return(month)
  }

  get_day <- function(day = NA) {
    if(length(day) == 0L) day <- NA
    if(is.na(day)) return("")

    num_day <- suppressWarnings(as.numeric(day))
    if(is.na(num_day)) {
      if(toupper(day) == "UN")  return("")
      else return(day)
    }
    return(sprintf("%02d", num_day))
  }

  get_time <- function(time = NA) {
    if(is.null(time)) time <- NA
    if(is.na(time)) return("")

    time_parts <- str_split(time, ":| |\\.", simplify = TRUE)
    n <- length(time_parts)

    for(i in 1:3) {
      if(n >= i) {
        num <- suppressWarnings(as.numeric(time_parts[i]))
        time_parts[i] <- ifelse(!is.na(num), sprintf("%02d", num), "")
      }

    }
    ot <- ifelse(n >= 4, paste(time_parts[4:n], collapse = " "), "")

    result_time <- catx(":", time_parts[1], time_parts[2], time_parts[3])
    result <- catx(".", result_time, ot)

    return(result)
  }

  if(!all(is.na(time))) {
    datetime <- catx(" ", datetime, time)
  }

  list_date_parts <- str_split(datetime, " |-|/|\\.|,")

  list_date_parts |>
    map_chr(~{
      vec_date_parts <- .[!is.na(.) & . != ""]
      n <- length(vec_date_parts)

      year <- get_year(vec_date_parts[get_index("Y",n)])
      month <- get_month(vec_date_parts[get_index("M",n)])
      day <- get_day(vec_date_parts[get_index("D",n)])

      if(n < 4) time <- ""
      else time <-  get_time(paste(vec_date_parts[4:n], collapse = " "))

      result_date <- catx("-", year, month, day)
      result <- catx("T", result_date, time)

      return(result)
    })
}

#' Convert Date to ISO 8601 Format (Date)
#'
#' This function converts a Date object to an ISO 8601 formatted date string ("YYYY-MM-DD").
#'
#' @param date A Date object or character string.
#' @return A character string in ISO 8601 format ("YYYY-MM-DD") if the input is a Date, otherwise returns the input as a character string.
#' @examples
#' conv_iso8601_num_d(as.Date("2023-09-15"))
#' conv_iso8601_num_d("2023-09-15")
#' @export
#' @import lubridate
conv_iso8601_num_d <- function(date) {
  if (is.Date(date)) return(format(date, "%Y-%m-%d"))
  return(as.character(date))
}

#' Convert POSIXct to ISO 8601 Format (Date and Time without Seconds)
#'
#' This function converts a POSIXct object to an ISO 8601 formatted datetime string ("YYYY-MM-DDTHH:MM"), excluding seconds.
#'
#' @param date A POSIXct object or character string.
#' @return A character string in ISO 8601 format ("YYYY-MM-DDTHH:MM") if the input is a POSIXct object, otherwise returns the input as a character string.
#' @examples
#' conv_iso8601_num_hm(as.POSIXct("2023-09-15 14:30:00"))
#' conv_iso8601_num_hm("2023-09-15T14:30")
#' @export
#' @import lubridate
conv_iso8601_num_hm <- function(date) {
  if (is.POSIXct(date)) return(format(date, "%Y-%m-%dT%H:%M"))
  return(as.character(date))
}

#' Convert POSIXct to ISO 8601 Format (Date and Time with Seconds)
#'
#' This function converts a POSIXct object to an ISO 8601 formatted datetime string ("YYYY-MM-DDTHH:MM:SS"), including seconds.
#'
#' @param date A POSIXct object or character string.
#' @return A character string in ISO 8601 format ("YYYY-MM-DDTHH:MM:SS") if the input is a POSIXct object, otherwise returns the input as a character string.
#' @examples
#' conv_iso8601_num_hms(as.POSIXct("2023-09-15 14:30:45"))
#' conv_iso8601_num_hms("2023-09-15T14:30:45")
#' @export
#' @import lubridate
conv_iso8601_num_hms <- function(date) {
  if (is.POSIXct(date)) return(format(date, "%Y-%m-%dT%H:%M:%S"))
  return(as.character(date))
}

#' Derive POSIXct Datetime from Partial ISO 8601 Datetime String
#'
#' This function converts a partial ISO 8601 datetime string (`DTC`) into a complete `POSIXct` datetime object.
#' Missing components (e.g., month, day, time) are imputed based on the specified `inpute_type`.
#'
#' @param dtc A character vector containing partial ISO 8601 datetime strings (e.g., "2023", "2023-01", "2023-01-01T12").
#' @param inpute_type A character string specifying how to impute missing components.
#'   - `"first"`: Impute the earliest possible values (e.g., "2023" becomes "2023-01-01T00:00:00").
#'   - `"last"`: Impute the latest possible values (e.g., "2023" becomes "2023-12-31T23:59:59").
#'   Default is `"first"`.
#' @param tz A character string specifying the time zone for the resulting `POSIXct` object. Default is an empty string (`""`), which uses the system's default time zone.
#'
#' @return A `POSIXct` vector representing the complete datetime values.
#'   If `dtc` contains invalid datetime strings, an error is raised.
#'
#' @details
#' The function validates the input `dtc` against a regular expression to ensure it conforms to the ISO 8601 format.
#' Missing components are imputed based on the `inpute_type` parameter:
#' - `"first"`: Imputes the earliest possible values (e.g., "2023" becomes "2023-01-01T00:00:00").
#' - `"last"`: Imputes the latest possible values (e.g., "2023" becomes "2023-12-31T23:59:59").
#'
#' @examples
#' # Example 1: Impute earliest possible values
#' derive_dtm_from_dtc(c("2023", "2023-01", "2023-01-01"), inpute_type = "first")
#'
#' # Example 2: Impute latest possible values
#' derive_dtm_from_dtc(c("2023", "2023-01", "2023-01-01"), inpute_type = "last")
#'
#' # Example 3: Handle time zone
#' derive_dtm_from_dtc(c("2023-01-01T12:34:56"), tz = "UTC")
#'
#' @import stringr
#' @export
derive_dtm_from_dtc <- function(dtc, inpute_type ="first", tz = "") {
  # If all results are not TRUE, return an error
  pattern <- "^\\d{4}(-\\d{2}(-\\d{2}(T\\d{2}(:\\d{2}(:\\d{2}(\\.\\d+)?)?)?)?)?)?$"
  if (!all(grepl(pattern, dtc[!is.na(dtc) & dtc != ""]))) {
    stop("Invalid datetime string")
  }

  if(inpute_type == "last") dtc_format <- "XXXX-12-31T23:59:59"
  else dtc_format <- "XXXX-01-01T00:00:00"

  result <- as.POSIXct(
    ifelse(!is.na(dtc) & str_length(dtc) >= 4,
           paste0(dtc, str_sub(dtc_format, str_length(dtc) + 1, -1)),
           NA),
    tz = tz,
    format = "%Y-%m-%dT%H:%M:%OS"
  )

  return(result)
}

#' Derive DTF (Date Imputation Flag) Variable from DTC
#'
#' This function creates the DTF (Date Imputation Flag) variable for ADaM datasets.
#' It determines the level of date imputation (e.g., year, month, or day) based on the length of the input datetime string (`dtc`).
#'
#' @param dtc A character vector containing datetime strings. Each string should follow the ISO 8601 format (e.g., `YYYY`, `YYYY-MM`, `YYYY-MM-DD`).
#' @param null_all_na A logical value indicating whether to return `NULL` if all results are `NA`. Defaults to `TRUE`.
#'
#' @return A character vector indicating the level of date imputation:
#'   - `"M"` for month-level imputation (e.g., `YYYY` -> `YYYY-MM-DD`).
#'   - `"D"` for day-level imputation (e.g., `YYYY-MM` -> `YYYY-MM-DD`).
#'   If `null_all_na` is `TRUE` and all results are `NA`, the function returns `NULL`.
#'
#' @examples
#' # Example with valid datetime strings
#' derive_dtf_from_dtc(c("2023", "2023-05", "2023-05-15"))
#' # Returns: c("D", "M", NA)
#'
#' # Example with non-inputed dates
#' derive_dtf_from_dtc(c("2023-01-01", "2023-05-01", "2023-05-15"))
#' # Returns: NULL (if all results are NA after processing)
#'
#' # Example with non-inputed dates with null_all_na = FALSE
#' derive_dtf_from_dtc(c("2023-01-01", "2023-05-01", "2023-05-15"))
#' # Returns: c(NA, NA, NA)
#'
#' @export
derive_dtf_from_dtc <- function(dtc, null_all_na = TRUE) {
  # If all results are not TRUE, return an error
  pattern <- "^\\d{4}(-\\d{2}(-\\d{2}(T\\d{2}(:\\d{2}(:\\d{2}(\\.\\d+)?)?)?)?)?)?$"
  if (!all(grepl(pattern, dtc[!is.na(dtc) & dtc != ""]))) {
    stop("Invalid datetime string")
  }

  result <- case_match(str_length(dtc),
    4  ~ "M",
    7  ~ "D",
    .default = NA
  )
  return(if(null_all_na & all(is.na(result))) NULL else result)
}

#' Derive TMF (Time Imputation Flag) Variable from DTC
#'
#' This function creates the TMF (Time Imputation Flag) variable for ADaM datasets.
#' It determines the level of time imputation (e.g., hour, minute, or second) based on the length of the input datetime string (`dtc`).
#'
#' @param dtc A character vector containing datetime strings. Each string should follow the ISO 8601 format (e.g., `YYYY-MM-DDTHH`, `YYYY-MM-DDTHH:MM`, `YYYY-MM-DDTHH:MM:SS`).
#' @param null_all_na A logical value indicating whether to return `NULL` if all results are `NA`. Defaults to `TRUE`.
#'
#' @return A character vector indicating the level of time imputation:
#'   - `"H"` for hour-level imputation (e.g., `YYYY-MM-DD` -> `YYYY-MM-DDTHH:MM:SS`).
#'   - `"M"` for minute-level imputation (e.g., `YYYY-MM-DDTHH` -> `YYYY-MM-DDTHH:MM:SS`).
#'   - `"S"` for second-level imputation (e.g., `YYYY-MM-DDTHH:MM` -> `YYYY-MM-DDTHH:MM:SS`).
#'   - `NA` for non-imputation (e.g., `YYYY-MM-DDTHH:MM:SS` -> `YYYY-MM-DDTHH:MM:SS`).
#'   If `null_all_na` is `TRUE` and all results are `NA`, the function returns `NULL`.
#'
#' @examples
#' # Example with valid datetime strings
#' derive_tmf_from_dtc(c("2023", "2023-05-15", "2023-05-15T12:30", "2023-05-15T12:30:45"))
#' # Returns: c("H", "H", "S", NA)
#'
#' # Example with non-imputed times
#' derive_tmf_from_dtc(c("2023-01-01T12:00:00", "2023-05-01T12:30:00", "2023-05-15T12:30:45"))
#' # Returns: NULL (if all results are NA after processing)
#'
#' # Example with non-imputed times and null_all_na = FALSE
#' derive_tmf_from_dtc(c("2023-01-01T12:00:00", "2023-05-01T12:30:00", "2023-05-15T12:30:45"), null_all_na = FALSE)
#' # Returns: c(NA, NA, NA)
#'
#' @export
derive_tmf_from_dtc <- function(dtc, null_all_na = TRUE) {
  # If all results are not TRUE, return an error
  pattern <- "^\\d{4}(-\\d{2}(-\\d{2}(T\\d{2}(:\\d{2}(:\\d{2}(\\.\\d+)?)?)?)?)?)?$"
  if (!all(grepl(pattern, dtc[!is.na(dtc) & dtc != ""]))) {
    stop("Invalid datetime string")
  }

  result <- case_match(str_length(dtc),
                       4   ~ "H",
                       4   ~ "H",
                       10  ~ "H",
                       13  ~ "M",
                       16  ~ "S",
                       .default = NA
  )
  return(if(null_all_na & all(is.na(result))) NULL else result)
}
