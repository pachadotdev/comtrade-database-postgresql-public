pkgs <- c(
  "dplyr",
  "dbplyr",
  "tidyr",
  "janitor",
  "comtradr",
  "purrr",
  "RSelenium"
)

for (pkg in pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

library(dplyr)
library(tidyr)
library(janitor)
library(comtradr)
library(purrr)
library(RSelenium)

# directories ----

dout <- "input"
dout2 <- "input/meta"
dout3 <- "input/data"

try(dir.create(dout, showWarnings = FALSE, recursive = TRUE))
try(dir.create(dout2, showWarnings = FALSE, recursive = TRUE))
try(dir.create(dout3, showWarnings = FALSE, recursive = TRUE))

# country codes ----

fout <- "input/data-availability/year_country_classificacion.rds"

if (file.exists(fout)) {
  message("Country codes file already exists, skipping processing")
  country_codes <- readRDS(fout)
} else {
  message("Processing country codes")

  url_jar <- "https://github.com/SeleniumHQ/selenium/releases/download/selenium-3.9.1/selenium-server-standalone-3.9.1.jar"
  sel_jar <- "selenium-server-standalone-3.9.1.jar"

  if (!file.exists(sel_jar)) {
    download.file(url_jar, sel_jar)
  }

  # run this from bash
  # java -jar selenium-server-standalone-3.9.1.jar

  rmDr <- remoteDriver(port = 4444L, browserName = "firefox")

  rmDr$open(silent = TRUE)

  url <- "https://comtradeplus.un.org/DataAvailability"

  # no login needed for metadata, it's something

  for (year in 1962:2025) {
    new_file <- file.path("input/data-availability", paste0(year, ".csv"))

    if (file.exists(new_file)) {
      message(paste0("File for year ", year, " already exists, skipping download"))
      next
    }

    rmDr$navigate(url)

    Sys.sleep(3) # wait for page to fully load

    message(paste0("Processing year: ", year))
    
    # wait for page to be ready
    Sys.sleep(2)
    
    # find the periods input dropdown
    periods_input <- rmDr$findElement(using = "css", value = "html body div#root div#main.main div.container-fluid div.row div.container div.row div.col div.card div.card-body div.row div.col-md-4 div.form-group div.form-group div div.react-dropdown-select.form-control.css-1le72y.e1gzf2xs0")
    
    # click to activate the field
    periods_input$clickElement()
    Sys.sleep(1)
    
    # clear existing content by using backspace multiple times
    # send enough backspaces to clear "all" (3 characters) plus any other content
    for (i in 1:5) {
      periods_input$sendKeysToElement(list(key = "backspace"))
      Sys.sleep(0.1)
    }
    Sys.sleep(0.5)
    
    # type the year directly into the field
    periods_input$sendKeysToElement(list(as.character(year)))
    
    # wait for dropdown options to appear
    Sys.sleep(1.5)
    
    # click on the year option in the dropdown container
    # use the specific dropdown class and find any clickable item within it
    year_option <- rmDr$findElement(using = "css", value = "div.react-dropdown-select-dropdown.react-dropdown-select-dropdown-position-bottom")
    year_option$clickElement()
    
    Sys.sleep(2)

    # change the "publication date" to 1900-01-01 to ensure we get all available data for that year
    # css path: html body div#root div#main.main div.container-fluid div.row div.container div.row div.col div.card div.card-body div.row div.col-md-4 div.form-group div.row div.col-md-6 div.ant-picker.ant-picker-outlined.css-mncuj7.form-control div.ant-picker-input input
    publication_date_input <- rmDr$findElement(using = "css", value = "div.ant-picker.ant-picker-outlined.css-mncuj7.form-control div.ant-picker-input input")
    publication_date_input$clickElement()
    Sys.sleep(1)
    
    # clear existing date by selecting all and deleting
    for (i in 1:15) {
      publication_date_input$sendKeysToElement(list(key = "backspace"))
      Sys.sleep(0.1)
    }
    Sys.sleep(0.5)
    
    # type the new date
    publication_date_input$sendKeysToElement(list("1900-01-01"))
    Sys.sleep(1)
    
    # press enter to confirm
    publication_date_input$sendKeysToElement(list(key = "enter"))
    Sys.sleep(2)
    
    # find and click the Preview button using more specific selector
    preview_button <- rmDr$findElement(using = "css", value = "div.col-md-12.button-bar.button-section button")
    preview_button$clickElement()
    
    # wait for preview to load
    Sys.sleep(30)
    
    # find and click the Download as Excel button using the specific path
    download_button <- rmDr$findElement(using = "css", value = "button.ant-btn.ant-dropdown-trigger.btn.btn-secondary.secondary-button")
    download_button$clickElement()
    
    # wait for dropdown menu to appear
    Sys.sleep(2)

    # click the "Excel" option in the dropdown
    # use a simpler selector for the menu item
    excel_option <- rmDr$findElement(using = "css", value = "li.ant-dropdown-menu-item")
    excel_option$clickElement()
    
    Sys.sleep(30)

    # wait for download to complete
    message(paste0("Downloaded data for year: ", year))

    # the files are in input/data-availability
    # rename DataAvailability_x_x_xxxx_xx_xx_xx.csv to data-availability/{year}.csv
    downloaded_file <- list.files("input/data-availability", pattern = "DataAvailability_.*\\.csv", full.names = TRUE)
    if (length(downloaded_file) == 0) {
      message(paste0("No downloaded file found for year: ", year))
      next
    }
    
    file.rename(downloaded_file, new_file)
  }

  message("All downloads completed!")

  # close the browser
  rmDr$close()

  # country_codes <- as_tibble(comtradr::country_codes)

  country_codes <- map_df(
    list.files("input/data-availability", pattern = "\\.csv$", full.names = TRUE),
    function(file) {
      message(file)
      
      lines <- readLines(file)
      
      data_lines <- lines[-1]
      
      # Parse each line manually to handle variable column counts
      parsed_data <- map_df(data_lines, function(line) {
        row <- read.csv(text = line, header = FALSE, stringsAsFactors = FALSE)
        
        tibble(
          year = as.integer(row[1, 2]),
          reporter_id = as.character(row[1, 1]),
          reporter = as.character(row[1, ncol(row) - 1]),  # Second-to-last column
          classification = as.character(row[1, 4])
        )
      })
      
      parsed_data
    }
  )

  country_codes <- country_codes %>%
    mutate(reporter_id = as.integer(reporter_id))

  sort(unique(country_codes$reporter_id))

  country_codes$reporter <- NULL

  country_codes <- country_codes %>%
    left_join(
      as_tibble(comtradr::country_codes) %>%
        select(reporter_id = id, reporter = country, reporter_iso3 = iso_3),
      by = "reporter_id"
    )

  country_codes %>%
    filter(is.na(reporter_iso3))

  country_codes %>%
    group_by(year, reporter_id) %>%
    summarise(n = n(), .groups = "drop") %>%
    filter(n > 1) %>%
    distinct(reporter_id)

  # country_codes <- country_codes %>%
  #   filter(reporter_id != 490)

  saveRDS(country_codes, fout)
}

# trade data ----

pmap(
  list(
    y = country_codes$year,
    r = country_codes$reporter_iso3,
    c = country_codes$classification
  ),
  function(y, r, c) {
    message("Downloading data for ", r, "-", y)

    fout <- file.path(dout3, paste0(r, "_", y, ".rds"))

    if (file.exists(fout)) {
      message("File already exists, skipping download")
      return(TRUE)
    }

    d <- try(
      comtradr::ct_get_bulk(
        start_date = y,
        end_date = y,
        reporter = r,
        commodity_classification = c,
        primary_token = Sys.getenv("COMTRADE_PRIMARY")
      )
    )

    if (inherits(d, "try-error")) {
      return(FALSE)
    }

    saveRDS(d, fout)
  }
)

# meta ----

hs_url <- "https://unstats.un.org/unsd/classifications/Econ/download/In%20Text/HSCodeandDescription.xlsx"

hs_xlsx <- file.path(dout2, "HSCodeandDescription.xlsx")

if (!file.exists(hs_xlsx)) {
  message("Downloading HS code file")
  download.file(hs_url, hs_xlsx)
} else {
  message("HS code file already exists, skipping download")
}

sitc_url <- "https://unstats.un.org/unsd/classifications/Econ/Download/In%20Text/SITCCodeandDescription.xlsx"

sitc_xlsx <- file.path(dout2, "SITCCodeandDescription.xlsx")

if (!file.exists(sitc_xlsx)) {
  message("Downloading SITC code file")
  download.file(sitc_url, sitc_xlsx)
} else {
  message("SITC code file already exists, skipping download")
}

bec_url <- "https://unstats.un.org/unsd/classifications/Econ/Download/In%20Text/BECCodeandDescription.xlsx"

bec_xlsx <- file.path(dout2, "BECCodeandDescription.xlsx")

if (!file.exists(bec_xlsx)) {
  message("Downloading BEC code file")
  download.file(bec_url, bec_xlsx)
} else {
  message("BEC code file already exists, skipping download")
}

correlation_url <- "https://unstats.un.org/unsd/classifications/Econ/tables/HS-SITC-BEC%20Correlations_2022.xlsx"

correlation_xlsx <- file.path(dout2, "HS-SITC-BEC_Correlations_2022.xlsx")

if (!file.exists(correlation_xlsx)) {
  message("Downloading correlation file")
  download.file(correlation_url, correlation_xlsx)
} else {
  message("Correlation file already exists, skipping download")
}
