media_from_rmd <- function(rmd_file) {
  
  library(stringr) # %>% str_extract_all str_detect
  library(rvest) # %>% read_html
  library(xml2) # read_html xml_find_all xml_attr
  
  # Read the content of the Rmarkdown file
  
  rmd_content <- readLines(rmd_file, warn = FALSE)
  
  # Combine the lines into a single string
  
  rmd_text <- rmd_content %>% 
    paste(collapse = "\n")
  
  # Extract all links using regular expressions
  
  all_links <- c(
    rmd_text %>% 
      str_extract_all("\\bhttps?://\\S+\\b") %>%
      unlist(),
    rmd_text %>% 
      str_extract_all('url\\(\\s*[\'"]?([^\\)"\']+)') %>% 
      unlist()
  )
  
  # Parse the Rmarkdown content as HTML
  
  rmd_html <- rmd_text %>% 
    read_html()
  
  # Extract links from image tags (img src)
  
  img_links <- rmd_html %>% 
    xml_find_all(".//img") %>% 
    xml_attr("src")
  
  # Extract links from anchor tags (a href)
  
  a_links <- rmd_html %>% 
    xml_find_all(".//a") %>% 
    xml_attr("href")
  
  # Combine all links
  
  all_links <- c(all_links, img_links, a_links)
  
  # Filter the links to get only .gif, .mov, .mp4, .png, .jpg, .jpeg, and .html files
  
  target_extensions <- c(".gif", ".mov", ".mp4", ".png", ".jpg", ".jpeg", ".html")
  
  filtered_links <- all_links %>% 
    str_detect(paste0(target_extensions, collapse = "|")) %>% 
    all_links[.]
  
  # Return the list of unique links
  
  unique_links <- unique(filtered_links)
  
  return(unique_links)
}

# Usage:
# To access and use the function:  copy 'function-media-from-rmd.R' from the 'R Working Directory/Functions - DS" directory to your working directory.

# Execute the following code to load the function into the environment and use it.

# Load the function:
#
# source("function-media-from-rmd.R")

# Use the function:
#
# rmd_file <- "rmd-file-to-be-parsed.Rmd"
#
# media_links <- media_from_rmd(rmd_file)
#
# print(media_links)
#
# Copy and paste the 'media_links' output in the console to a Word document or other text processing application to print the result from your printer.
