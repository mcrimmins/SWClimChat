library(httr2)
library(jsonlite)
library(dplyr)
library(purrr)

# ---------------------------------------------------------
# 1. The Phonebook: Find Station IDs by City
# ---------------------------------------------------------
find_station_id_core <- function(state, city_name) {
  payload <- list(state = state, meta = c("name", "sids"))
  
  req <- request("https://data.rcc-acis.org/StnMeta") |>
    req_body_json(payload) |> req_perform()
  
  raw <- resp_body_json(req)
  
  df <- map_df(raw$meta, function(x) {
    data.frame(name = x$name, id = x$sids[[1]])
  })
  
  # matches <- df |> 
  #   filter(grepl(city_name, name, ignore.case = TRUE)) |> 
  #   head(25)
  
  matches <- df |> 
    filter(grepl(city_name, name, ignore.case = TRUE))
  
  return(matches)
}

# --- TEST BLOCK 1 ---
# Expected: A short dataframe showing IDs for Winslow, AZ
# print(find_station_id_core("AZ", "Winslow"))
# print(find_station_id_core("AZ", "Tucson"))


# ---------------------------------------------------------
# Upgraded Time Series Function
# ---------------------------------------------------------
get_timeseries_core <- function(station_id, start_date, end_date, element, normal_departure = FALSE) {
  
  # Always request the absolute value
  elems_list <- list(
    list(name = element, interval = "dly", duration = 1)
  )
  
  # If requested, append a second request for the departure
  if (normal_departure) {
    elems_list[[2]] <- list(name = element, interval = "dly", duration = 1, normal = "departure")
  }
  
  payload <- list(
    sid = station_id, sdate = start_date, edate = end_date,
    elems = elems_list
  )
  
  req <- httr2::request("https://data.rcc-acis.org/StnData") |>
    httr2::req_body_json(payload) |> httr2::req_perform()
  
  raw <- httr2::resp_body_json(req)
  
  # Better Error Handling to guide the LLM
  if (!is.null(raw$error)) {
    if (raw$error == "no data available") {
      stop("ACIS API Error: 'no data available'. This usually means this specific station does not have established 30-year Climate Normals for departures. Tell the user to try a major airport station instead.")
    } else {
      stop(paste("ACIS API Error:", raw$error))
    }
  }
  
  if (is.null(raw$data) || length(raw$data) == 0) {
    stop("No data returned for this station and date range.")
  }
  
  # 1. Map and initial text-cleaning
  df <- purrr::map_df(raw$data, function(row) {
    val1 <- row[[2]]
    if (val1 == "T") val1 <- "0"
    if (val1 %in% c("M", "S", "A")) val1 <- NA # Handle standard ACIS missing flags
    
    res <- data.frame(
      date = as.Date(row[[1]]), 
      value = suppressWarnings(as.numeric(val1))
    )
    
    # If departure was requested, it will be the 3rd item in the row list
    if (normal_departure && length(row) >= 3) {
      val2 <- row[[3]]
      if (val2 == "T") val2 <- "0"
      if (val2 %in% c("M", "S", "A")) val2 <- NA
      res$departure <- suppressWarnings(as.numeric(val2))
    }
    
    return(res)
  }) 
  
  # ==========================================
  # 2. VECTORIZED OUTLIER CLEANING BLOCK
  # ==========================================
  
  # Filter out impossible temperature outliers
  if (element %in% c("maxt", "mint", "avgt")) {
    # ACIS sometimes uses -999 or -99 for missing numerical data
    df$value[df$value <= -50] <- NA 
    df$value[df$value >= 135] <- NA 
    
    # Catch broken '0' values in the dead of summer for maximum temperature
    if (element == "maxt") {
      months <- format(as.Date(df$date), "%m")
      df$value[df$value == 0 & months %in% c("05", "06", "07", "08", "09")] <- NA
    }
  }
  
  # Clean departure extremes
  if (normal_departure && "departure" %in% names(df)) {
    df$departure[df$departure <= -100 | df$departure >= 100] <- NA
  }
  
  # 3. Drop NA values to keep the charts unbroken
  df <- df |> dplyr::filter(!is.na(value))
  
  return(df)
}
# --- TEST BLOCK 2 ---
# Expected: A dataframe of daily max temps for February 2026 in Winslow
# winslow_id <- find_station_id_core("AZ", "Winslow")$id[1]
# print(get_timeseries_core(winslow_id, "2026-02-01", "2026-02-28", "maxt"))


# ---------------------------------------------------------
# Upgraded Regional Summary Function
# ---------------------------------------------------------
get_regional_core <- function(states, start_date, end_date, element, summary_type, normal_departure = FALSE, include_date = FALSE) {
  
  elem_list <- list(name = element, smry_only = 1)
  if (normal_departure) {
    elem_list$normal <- "departure"
  }
  
  if (include_date && grepl("max|min", summary_type)) {
    elem_list$smry <- list(reduce = summary_type, add = "date")
  } else {
    elem_list$smry <- summary_type
  }
  
  payload <- list(
    state = states, sdate = start_date, edate = end_date,
    meta = c("uid", "name", "state"), 
    elems = list(elem_list)
  )
  
  req <- httr2::request("https://data.rcc-acis.org/MultiStnData") |>
    httr2::req_body_json(payload) |> httr2::req_perform()
  
  raw <- httr2::resp_body_json(req)
  
  # Handle regional errors
  if (!is.null(raw$error)) {
    stop(paste("ACIS API Error:", raw$error))
  }
  
  df <- purrr::map_df(raw$data, function(x) {
    smry_obj <- x$smry[[1]]
    
    if (include_date && grepl("max|min", summary_type) && length(smry_obj) >= 2) {
      raw_val <- smry_obj[[1]]
      extreme_date <- smry_obj[[2]]
    } else {
      raw_val <- if (is.list(smry_obj)) smry_obj[[1]] else smry_obj
      extreme_date <- NA
    }
    
    if (raw_val == "T") raw_val <- "0"
    
    res <- data.frame(
      station_id = as.character(x$meta$uid), 
      station = x$meta$name, 
      state = x$meta$state
    )
    
    # Explicitly name the column so the LLM doesn't get confused
    if (normal_departure) {
      res$departure <- suppressWarnings(as.numeric(raw_val))
    } else {
      res$value <- suppressWarnings(as.numeric(raw_val))
    }
    
    if (!is.na(extreme_date)) res$date_of_extreme <- extreme_date
    return(res)
    
  }) |> 
    # Dynamically filter NA based on which column exists
    dplyr::filter(if (normal_departure) !is.na(departure) else !is.na(value)) |> 
    dplyr::arrange(dplyr::desc(if (normal_departure) departure else value)) |> 
    head(15)
  
  return(df)
}
# --- TEST BLOCK 3 ---
# Expected: Top 15 stations in AZ/NM with highest total precip in Feb 2026
# print(get_regional_core("AZ,NM", "2026-02-01", "2026-02-28", "pcpn", "sum"))