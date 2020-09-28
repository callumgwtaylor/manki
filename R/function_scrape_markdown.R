#' A Scraping Markdown Function
#'
#' This function converts passed characters from a markdown file, into a table of flashcards
#' @param markdown character string of markdown format, containing flashcard info
#' @keywords markdown anki
#' @export
#' @examples
#' scrape_markdown()

scrape_markdown <- function(markdown){

  flashcards_all <- stringr::str_extract_all(markdown, "(?<=```\\{block2(([:graph:]|[:space:]){0,10})\\})(([:graph:]|[:space:])+?)(?=```)")

  ## I HATE regex - I used this to help: https://stackoverflow.com/questions/8613237/extract-info-inside-all-parenthesis-in-r
  ## Let's explain the regex:
  # Part One: (?<=```\\{block2(([:graph:]|[:space:]){7})\\})
  ### This looks for a string that starts with ```{block..........} Those .'s can be any alphanumeric, punctual, or space. But there can only be a maximum of 10 of them
  #### It's all contained within () so it starts with ( and ends with )
  #### Then we tell it to look for something that starts with, and starts with is ?<=
  #### ``` Doesn't need anything special as aren't a character that needs escape
  #### \\{block means {block2, we need the \\ to escape the {
  ##### This bit (([:graph:]|[:space:]){7}) is a bit more complicated, again we've put it inside it's own brackets
  ##### ([:graph:]|[:space:]){0,10}
  ##### [:graph:]|[:space:] means any character thats alphanumeric/punctual ([:graph:]) OR (|) a space/new line ([:space:])
  ##### {0,10} means the characters between block and } can only be up to 10
  #### Lastly we have \\} to look for }
  # Part Two: (([:graph:]|[:space:])+?)
  ### This looks for all characters both alphanumerical and punctual with [:graph:] OR (using |) spaces/new lines with [:space:].
  ### We contain that stuff from the above line in a brackets
  ### It says these can be of any length by including +?
  ### And again lets contain this in brackets
  # Part Three: (?=```)
  ### This is the simplest bit, it looks for a string that ends with ```
  ### Looking for the end is ?=
  ### Lets put this in brackets too

  # Now we need to identify which ones are which type:
  ## basic
  ## reference
  ## cloze
  ## cloze_reference

  flashcards_all <- flashcards_all %>%
    purrr::map(stringr::str_split, "\\\r\\\n") #This splits our single list for all flashcards onto the page into multiple lists, one for each table.


  flashcards_all <- flashcards_all[[1]] #Then lets remove a layer, so rather than a list of lists of lists, it's just the one list of lists
  flashcards_all_df <- dplyr::tibble(flashcards_all) #Lets turn that into a table, so one column, each row is a separate list


  flashcards_split_filter <- function(n){
    # By having n as an argument, we're able to specify which row of the table we want to look at
    test <- flashcards_all_df[n,1] # This says look at the specified row

    test <- test %>%
      tidyr::unnest(cols = c(flashcards_all)) %>%
      dplyr::filter(flashcards_all != "") # Then we turn the list in that row into a column itself

    if(test[1,1] == "Flashcard | type:basic"){
      flashcards <- test %>%
        dplyr::filter(flashcards_all != "Flashcard | type:basic",
               flashcards_all != "---|---") %>%
        tidyr::separate(flashcards_all, into = c("front", "back"), sep = "\\|")%>%
        dplyr::mutate(flashcard_type = "basic")
      # If the first row in the column shows the type is basic, then we can get rid of the headers and the junk
      # We split using | as our divider, into the columns front and back
      # And add the identifier of flashcard type as basic
    }
    if(test[1,1] == "Flashcard | type:reference | ..."){
      flashcards <- test %>%
        dplyr::filter(flashcards_all != "Flashcard | type:reference | ...",
               flashcards_all != "---|---|---") %>%
        tidyr::separate(flashcards_all, into = c("front", "back", "reference"), sep = "\\|")%>%
        dplyr::mutate(flashcard_type = "reference")
      # If the first row in the column shows the type is reference, then we can get rid of the headers and the junk
      # We split using | as our divider, into the columns front and back and reference
      # And add the identifier of flashcard type as reference
    }
    if(test[1,1] == "Flashcard | type:cloze"){
      flashcards <- test %>%
        dplyr::filter(flashcards_all != "Flashcard | type:cloze",
               flashcards_all != "---|---") %>%
        tidyr::separate(flashcards_all, into = c("front", "spare"), sep = "\\|") %>%
        dplyr::select(front)%>%
        dplyr::mutate(flashcard_type = "cloze")
      # If the first row in the column shows the type is cloze, then we can get rid of the headers and the junk
      # We split using | as our divider, into the columns front and spare, then delete spare
      # And add the identifier of flashcard type as basic
    }
    if(test[1,1] == "Flashcard | type:cloze_reference"){
      flashcards <- test %>%
        dplyr::filter(flashcards_all != "Flashcard | type:cloze_reference",
               flashcards_all != "---|---") %>%
        tidyr::separate(flashcards_all, into = c("front", "reference"), sep = "\\|") %>%
        dplyr::mutate(flashcard_type = "cloze_reference")
      # If the first row in the column shows the type is cloze_reference, then we can get rid of the headers and the junk
      # We split using | as our divider, into the columns front and reference
      # And add the identifier of flashcard type as cloze_reference
    }
    flashcards
    #We then output whatever the function found on this row
  }

  length_cards <- length(flashcards_all_df[[1]]) # This sets the value of however many rows we need to go through (how many blocks of flashcards)


  # This bit goes through each of those rows one by one, creating a table for each, and then sticking those tables together at the end
  # We're using purrr::map to say, for a range of numbers 1:number of rows we need to go for, apply each row number to our function
  flashcards_prepared <- purrr::map(1:length_cards, flashcards_split_filter) %>%
    dplyr::bind_rows()

  flashcards_prepared %>%
    select(front,
           back,
           reference,
           flashcard_type)
}
