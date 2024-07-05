
state.terr.abb <- c(datasets::state.abb, "AS", "DC", "GU", "MP", "PR", "VI")
state.terr.name <- c(
  datasets::state.name, "American Samoa", "District of Columbia", "Guam",
  "Northern Mariana Islands", "Puerto Rico", "Virgin Islands"
)
state.terr.data <- tibble::tibble(
  name = c(
    "Alabama", "Alaska", "American Samoa", "Arizona", "Arkansas", "California",
    "Colorado", "Connecticut", "Delaware", "District of Columbia", "Florida",
    "Georgia", "Guam", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas",
    "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan",
    "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada",
    "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina",
    "North Dakota", "Northern Mariana Islands", "Ohio", "Oklahoma", "Oregon",
    "Pennsylvania", "Puerto Rico", "Rhode Island", "South Carolina",
    "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia",
    "Virgin Islands", "Washington", "West Virginia", "Wisconsin", "Wyoming"
  ),
  USPS = c(
    "AL", "AK", "AS", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA", "GU",
    "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN",
    "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "MP", "OH",
    "OK", "OR", "PA", "PR", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "VI",
    "WA", "WV", "WI", "WY"
  ),
  FIPS = c(
    "01", "02", "60", "04", "05", "06", "08", "09", "10", "11", "12", "13", "66",
    "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27",
    "28", "29", "30", "31", "32", "33", "34", "35", "36", "37", "38", "69", "39",
    "40", "41", "42", "72", "44", "45", "46", "47", "48", "49", "50", "51", "78",
    "53", "54", "55", "56"
  ),
  type = c(
    "state", "state", "territory", "state", "state", "state", "state", "state",
    "state", "federal district", "state", "state", "territory", "state", "state",
    "state", "state", "state", "state", "state", "state", "state", "state",
    "state", "state", "state", "state", "state", "state", "state", "state",
    "state", "state", "state", "state", "state", "state", "territory", "state",
    "state", "state", "state", "territory", "state", "state", "state", "state",
    "state", "state", "state", "state", "territory", "state", "state", "state",
    "state"
  ),
)


usethis::use_data(state.terr.abb, state.terr.name, state.terr.data, overwrite = TRUE)
