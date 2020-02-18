# 

library(httr)
library(jsonlite)
library(stringr)
library(rvest)
source("./vin_tools.R")

##### data #####

gsa_veh_type <- c(
  "HD",
  "HD Bus",
  "LD Minivan 4x2 (Cargo)",
  "LD Minivan 4x2 (Passenger)",
  "LD Pickup 4x2",
  "LD Pickup 4x4",
  "LD SUV 4x2",
  "LD SUV 4x4",
  "LD Van 4x2 (Cargo)",
  "LD Van 4x2 (Passenger)",
  "LD Van 4x4 (Cargo)",
  "LD Van 4x4 (Passenger)",
  "MD Other",
  "MD Pickup",
  "MD SUV",
  "MD Van (Cargo)",
  "MD Van (Passenger)",
  "Other",
  "SEDAN/ST WAGON",
  "Sedan/St Wgn Compact",
  "Sedan/St Wgn Midsize",
  "Sedan/St Wgn Subcompact"
)


##### functions #####
random_tag <- function(n, prefix = c("G")) {
  if (!prefix %in% c("G", "DOT")) {
    stop("prefix must be one of 'G' or 'DOT")
  }
  if (prefix == "G") {
    prefix <- paste0(prefix, 
                     stringr::str_pad(sample(0:99, n), 2, pad = "0"))
  }
  
  tag <- paste(prefix, 
               stringr::str_pad(sample(1:9999, n, replace = FALSE), 4, pad = "0"),
               sep = "-")
  return(tag)
}

random_vehicle <- function() {
  # queries data from VINGenerator.org
  # http://www.vingenerator.org
  url <- "http://vingenerator.org"
  rvin <- xml2::read_html(url)
  vin <- rvin %>%
    html_node(".number") %>%
    html_children() %>%
    html_attr("value")
  desc <- rvin %>%
    html_node(".description") %>%
    html_text() %>%
    str_remove("VIN Description:") %>%
    str_trim()
  year <- str_extract(desc, "[0-9]+")
  make <- str_split(desc, " ", simplify = TRUE)[[2]]
  model <- trimws(str_remove(desc, paste(year, make)))
  return(data.frame(vin, year, make, model))
}

random_person <- function(n = 1) {
  # queries data from Randomuser.me
  # http://randomuser.me
  url <- "https://randomuser.me/api/"
  url <- paste0(url,
                "?results=", n,
                "&inc=name,email,cell",
                "&format=json",
                "&noinfo")
  user <- GET(url)
}

#####