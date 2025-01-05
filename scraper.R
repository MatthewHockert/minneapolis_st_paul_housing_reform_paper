# Load required libraries
library(RSelenium)
library(rvest)
library(tidyverse)

# Load archive links from the previous step
archive_links <- read_csv("archive_links.csv")$links

# Step 1: Set up RSelenium with Firefox
rD <- rsDriver(browser = "firefox", port = 4567L, verbose = FALSE)
remDr <- rD$client
remDr$open()

# Step 2: Function to scrape rental listings from an archive link
scrape_rental_listings <- function(url) {
  print(paste("Navigating to:", url))
  remDr$navigate(url)
  Sys.sleep(20)  # Wait for the page to load
  
  # Extract page HTML
  page_source <- remDr$getPageSource()[[1]]
  page_html <- read_html(page_source)
  
  # Extract rental listings
  listings <- page_html %>%
    html_nodes(".result-row") %>%  # CSS selector for Craigslist posts
    map_df(~ {
      tryCatch({
        title <- .x %>% html_node(".result-title") %>% html_text(trim = TRUE)
        price <- .x %>% html_node(".result-price") %>% html_text(trim = TRUE)
        neighborhood <- .x %>% html_node(".result-hood") %>% html_text(trim = TRUE)
        date <- .x %>% html_node(".result-date") %>% html_attr("datetime")
        link <- .x %>% html_node(".result-title") %>% html_attr("href")
        
        # Filter only listings with "St Paul" or variations in the title or neighborhood
        if (str_detect(str_to_lower(title), "st paul|saint paul|st. paul|st.paul") |
            str_detect(str_to_lower(neighborhood), "st paul|saint paul|st. paul|st.paul|hamline - midway|macalester - groveland|highland park|dayton's bluff|saint anthony park|como park|north end|west side|summit hill|summit - university|frogtown|west seventh - fort road|downtown|payne - phalen|greater east side|battle creek - conway - eastview - highwood hills|union park"))
          {
          tibble(
            Title = title,
            Price = price,
            Neighborhood = neighborhood,
            Date = date,
            Link = link
          )
        } else {
          tibble()  # Skip non-St Paul listings
        }
      }, error = function(e) {
        # Return an empty tibble on error
        tibble()
      })
    })
  
  return(listings)
}

# Step 3: Loop through archive links and scrape data
all_rental_data <- tibble()
for (url in archive_links) {
  tryCatch({
    listings <- scrape_rental_listings(url)
    all_rental_data <- bind_rows(all_rental_data, listings)
  }, error = function(e) {
    print(paste("Error scraping:", url))
  })
}

# Deduplicate the scraped data
all_rental_data <- distinct(all_rental_data)

# Step 4: Save the scraped data to a CSV file
write_csv(all_rental_data, "st_paul_rental_listings.csv")

# Step 5: Close the browser and stop RSelenium
remDr$close()
rD$server$stop()
rD$server$stop()

print("Scraping complete! St. Paul rental listings have been saved to 'st_paul_rental_listings.csv'.")