holidays_us <-local({
  opm_holidays <- rvest::read_html(
    r"(https://www.opm.gov/policy-data-oversight/pay-leave/federal-holidays/)"
  )
  opm_holidays %>%
    rvest::html_elements(".DataTable.HolidayTable") %>%
    rlang::set_names(stringr::str_extract(
      rvest::html_text(rvest::html_element(., "caption")),
      "\\d+"
    )) %>%
    purrr::map_dfr(rvest::html_table, .id = "Year") %>%
    dplyr::mutate(
      Holiday = stringr::str_replace(Holiday, "’", "'"),
      Year = dplyr::if_else(
        stringr::str_detect(Date, "\\d{4}"),
        stringr::str_extract(Date, "\\d{4}"),
        Year
      ),
      Date = lubridate::mdy(stringr::str_c(
        stringr::str_remove_all(Date, ", \\d{4}| ?\\*"),
        Year,
        sep = " "
      )),
      Year = NULL
    ) %>%
    dplyr::filter(Holiday != "Inauguration Day") %>%
    dplyr::arrange(Date)
})

holidays_il <- local({
  thanksgiving_fri <- holidays_us %>%
    dplyr::filter(Holiday == "Thanksgiving Day") %>%
    dplyr::mutate(
      Holiday = "Thanksgiving Friday",
      Date = Date + 1
    )
  lincolns_bday <- data.frame(
      Holiday = "Lincoln's Birthday",
      Date = lubridate::mdy(stringr::str_c("02-12-", 2011:2030))
    ) %>%
    dplyr::mutate(
      Date = dplyr::case_when(
        weekdays(Date) == "Saturday" ~ Date - 1,
        weekdays(Date) == "Sunday" ~ Date + 1,
        TRUE ~ Date
      )
    )
  election_day <- tidyr::expand_grid(
      y = seq(2022, 2030, by = 2),
      m = 11,
      d = 2:8
    ) %>%
    dplyr::transmute(
      Holiday = "General Election Day",
      Date = lubridate::make_date(y, m, d)
    ) %>%
    dplyr::filter(lubridate::wday(Date) == 3)
  dplyr::bind_rows(holidays_us, thanksgiving_fri, lincolns_bday, election_day) %>%
    dplyr::arrange(Date)
})

holidays_chestnut <- holidays_us %>%
  dplyr::filter(Holiday %in% c(
    "New Year's Day",
    "Birthday of Martin Luther King, Jr.",
    "Memorial Day",
    "Independence Day",
    "Labor Day",
    "Thanksgiving Day",
    "Christmas Day"
  ))

business_days <- local({
  dates_start <- lubridate::ymd("2010-12-31")
  dates_end <- lubridate::ymd("2030-12-31")
  alldates <- seq(from = dates_start, to = dates_end, by = 1)
  wkdays <- alldates[weekdays(alldates) %!in% c("Saturday", "Sunday")]
  map(
    list(
      Chestnut = holidays_chestnut,
      Illinois = holidays_il,
      federal = holidays_us
    ),
    \(hdays) {
      out <- wkdays[wkdays %!in% hdays$Date]
      attributes(out) <- c(
        attributes(out),
        list(start = dates_start, end = dates_end)
      )
      out
    }
  )
})

usethis::use_data(holidays_chestnut, holidays_il, holidays_us, overwrite = TRUE)
usethis::use_data(business_days, internal = TRUE, overwrite = TRUE)
