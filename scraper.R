library(RSelenium)
library(rvest)
library(tidyverse)

# Start Selenium
rD <- rsDriver(browser = "chrome", port = 4567L, verbose = FALSE)
remDr <- rD$client

# Static Archive URLs for Craigslist (Wayback Machine)
archive_urls <- c(
  "https://web.archive.org/web/20140101000000/https://minneapolis.craigslist.org/search/apa",
  "https://web.archive.org/web/20150101000000/https://minneapolis.craigslist.org/search/apa",
  "https://web.archive.org/web/20160101000000/https://minneapolis.craigslist.org/search/apa",
  "https://web.archive.org/web/20170101000000/https://minneapolis.craigslist.org/search/apa",
  "https://web.archive.org/web/20180101000000/https://minneapolis.craigslist.org/search/apa",
  "https://web.archive.org/web/20190101000000/https://minneapolis.craigslist.org/search/apa",
  "https://web.archive.org/web/20200101000000/https://minneapolis.craigslist.org/search/apa",
  "https://web.archive.org/web/20210101000000/https://minneapolis.craigslist.org/search/apa",
  "https://web.archive.org/web/20220101000000/https://minneapolis.craigslist.org/search/apa",
  "https://web.archive.org/web/20230101000000/https://minneapolis.craigslist.org/search/apa",
  "https://web.archive.org/web/20240101000000/https://minneapolis.craigslist.org/search/apa"
)

navigate_and_wait <- function(url) {
  remDr$navigate(url)
  Sys.sleep(5)  # Allow initial load time
  
  # Check for redirection and wait for the final page
  for (i in 1:3) {  # Try up to 3 times
    current_url <- remDr$getCurrentUrl()[[1]]
    if (current_url == url) {
      break
    } else {
      print(paste("Redirect detected. Waiting for final page:", current_url))
      Sys.sleep(5)
    }
  }
}


scrape_rental_listings <- function(page_url) {
  navigate_and_wait(page_url)  # Handle potential redirects
  
  # Extract page content
  page_source <- remDr$getPageSource()[[1]]
  html <- read_html(page_source)
  
  # Extract all rental listings
  listings <- html %>%
    html_nodes(".result-row")
  
  # Filter listings for St. Paul
  st_paul_listings <- listings %>%
    keep(~ {
      neighborhood <- .x %>%
        html_node(".result-hood") %>%
        html_text(trim = TRUE) %>%
        str_to_lower()  # Convert to lowercase
      "st. paul" %in% neighborhood || 
        "saint paul" %in% neighborhood || 
        "st paul" %in% neighborhood
    })
  
  # Extract attributes from filtered listings
  data <- st_paul_listings %>%
    map_df(~ {
      tryCatch({
        title <- .x %>% html_node(".result-title") %>% html_text(trim = TRUE)
        price <- .x %>% html_node(".result-price") %>% html_text(trim = TRUE)
        neighborhood <- .x %>% html_node(".result-hood") %>% html_text(trim = TRUE)
        post_id <- .x %>% html_attr("data-id")
        link <- .x %>% html_node(".result-title") %>% html_attr("href")
        
        # Scrape additional details from individual listing page
        navigate_and_wait(link)
        listing_page <- read_html(remDr$getPageSource()[[1]])
        description <- listing_page %>%
          html_node("#postingbody") %>%
          html_text(trim = TRUE) %>%
          gsub("\n", " ", .)
        details <- listing_page %>%
          html_node(".attrgroup") %>%
          html_text(trim = TRUE)
        
        tibble(
          Post_ID = post_id,
          Title = title,
          Price = price,
          Neighborhood = neighborhood,
          Link = link,
          Description = description,
          Details = details
        )
      }, error = function(e) tibble())
    })
  
  return(data)
}

# Function to handle pagination and scrape all pages
scrape_paginated_links <- function(base_url) {
  all_data <- tibble()
  current_page <- base_url
  
  while (!is.null(current_page)) {
    tryCatch({
      print(paste("Scraping:", current_page))
      page_data <- scrape_rental_listings(current_page)
      all_data <- bind_rows(all_data, page_data)
      
      # Navigate to the "next" page
      remDr$navigate(current_page)
      Sys.sleep(5)
      page_source <- remDr$getPageSource()[[1]]
      html <- read_html(page_source)
      
      next_page <- html %>%
        html_node("a.button.next") %>%
        html_attr("href")
      
      if (!is.null(next_page)) {
        current_page <- paste0("https://web.archive.org", next_page)
      } else {
        print("No next page found.")
        current_page <- NULL
      }
    }, error = function(e) {
      print(paste("Error on page:", current_page))
      print(e)
      current_page <- NULL
    })
  }
  
  return(all_data)
}

# Main Script
all_st_paul_data <- tibble()

for (url in archive_urls) {
  tryCatch({
    print(paste("Processing archive URL:", url))
    st_paul_data <- scrape_paginated_links(url)
    all_st_paul_data <- bind_rows(all_st_paul_data, st_paul_data)
  }, error = function(e) {
    print(paste("Error processing archive:", url))
    print(e)
  })
}

# Save the data to a CSV
write_csv(all_st_paul_data, "st_paul_rentals_2014_2024.csv")

# Stop Selenium
remDr$close()
rD$server$stop()

