#' Create list of all media calls in a xaringan slide deck
#'
#' @param rmd_file
#'
#' @return List of all media calls printed to console
#' @export
#'
#' @examples
#' ds_xaringan_media_calls("unique-is-more-important.Rmd")
ds_xaringan_media_calls <- function(rmd_file) {

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

