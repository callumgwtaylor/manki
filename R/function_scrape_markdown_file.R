#' A Scraping Markdown File Function
#'
#' This function converts a markdown file, into a table of flashcards
#' @param markdown_file path pointing to a markdown file
#' @keywords markdown anki
#' @export
#' @examples
#' scrape_markdown_file()

scrape_markdown_file <- function(markdown_file){
  readr::read_file(markdown_file) %>%
    scrape_markdown()
}
