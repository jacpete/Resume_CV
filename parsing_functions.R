# Regex to locate links in text
find_link <- regex("
  \\[   # Grab opening square bracket
  .+?   # Find smallest internal text as possible
  \\]   # Closing square bracket
  \\(   # Opening parenthesis
  .+?   # Link text, again as small as possible
  \\)   # Closing parenthesis
  ",
                   comments = TRUE)

# Function that removes links from text and replaces them with superscripts that are 
# referenced in an end-of-document list. 
sanitize_links <- function(text){
  if(PDF_EXPORT){
    str_extract_all(text, find_link) %>% 
      pluck(1) %>% 
      walk(function(link_from_text){
        title <- link_from_text %>% str_extract('\\[.+\\]') %>% str_remove_all('\\[|\\]') 
        link <- link_from_text %>% str_extract('\\(.+\\)') %>% str_remove_all('\\(|\\)')
        
        # add link to links array
        links <<- c(links, link)
        
        # Build replacement text
        new_text <- glue('{title}<sup>{length(links)}</sup>')
        
        # Replace text
        text <<- text %>% str_replace(fixed(link_from_text), new_text)
      })
  }
  text
}

# Take entire positions dataframe and removes the links 
# in descending order so links for the same position are
# right next to eachother in number. 
strip_links_from_cols <- function(data, cols_to_strip){
  for(i in 1:nrow(data)){
    for(col in cols_to_strip){
      data[i, col] <- sanitize_links(data[i, col])
    }
  }
  data
}

# Take a position dataframe and the section id desired
# and prints the section to markdown. 
print_section <- function(position_data, section_id, aside=NULL, asidePos=1, type = c("resume","cv")){
  cv_resume_description <- glue("{type}_description")
  dataFormatted <- position_data %>%
    filter(section == section_id) %>%
    arrange(desc(end)) %>%
    reorderManually() %>%  
    mutate(id = 1:n()) %>%
    pivot_longer(
      starts_with('description'),
      names_to = 'description_num',
      values_to = 'description',
      values_drop_na = FALSE
    ) %>% filter(!is.na(description) | is.na(!!rlang::sym(cv_resume_description))) %>%
    group_by(id) %>%
    mutate(
      descriptions = list(description)
    ) %>%
    ungroup() %>%
    filter(description_num == 'description_1') %>%
    mutate(start = year(start),
           end = ifelse(
             end > today(),
             "Present",
             year(end))
    ) %>%
    mutate(
      timeline = ifelse(
        is.na(start) | start == end,
        end,
        glue('{end} - {start}')
      ),
      description_bullets = map2_chr(!!rlang::sym(cv_resume_description), descriptions, ~if_else(!is.na(.x[[1]]), paste('-', .y, collapse = '\n'), "\n"))
    ) %>%
    strip_links_from_cols(c('title', 'description_bullets')) %>% 
    mutate_all(~ifelse(is.na(.), 'N/A', .)) 
  
  if (is.null(aside)) {
    glue_data(dataFormatted,
              "### {title}",
              "\n\n",
              "{loc}",
              "\n\n",
              "{institution}",
              "\n\n",
              "{timeline}", 
              "\n\n",
              "{description_bullets}",
              "\n\n\n",
    )
  } else {
   prnt <-  map_chr(1:nrow(dataFormatted), ~{
      out <- dataFormatted %>% slice(.x) %>% 
        glue_data(
          "### {title}",
          "\n\n",
          "{loc}",
          "\n\n",
          "{institution}",
          "\n\n",
          "{timeline}", 
          "\n\n",
          "{description_bullets}",
          "\n\n\n",
        )
      
      if (.x == asidePos) {
        out <- glue(
          out, addAside(aside), "\n\n"
        )
        # out <- glue(
        #   out, "\n",
        #   "::: aside\n",
        #   aside, "\n",
        #   ":::\n\n"
        # )
      }
      out
    }) 
   prnt <- paste(prnt, collapse = '')
   cat(prnt)
  }
}

addAside <- function(aside) {
  glue("\n",
       "::: aside\n",
       aside, "\n",
       ":::\n\n"
  )
}


#This function filters the descriptions from the full cv to make it more compact for the resume
filter_descriptions <- function(position_data, type = c("resume","cv")) {
  rowList <- map(1:nrow(position_data), ~{position_data[.x,]})
  descriptionCols <- tibble(column = grep('^description', names(position_data), value = TRUE), 
                            colID = grepl('^description', names(position_data)) %>% which(.), 
                            ID = gsub('description_','',column) %>% as.integer(.))
  
  # .x=rowList[[1]]
  map(rowList, ~{
    # keep <- .x$resume_description[[1]]
    keep <- .x[[paste0(type,"_description")]][[1]]
    makeNA <- descriptionCols$column[!(descriptionCols$ID %in% keep)]
    .x[,makeNA] <- NA
    .x
    
    
    #reorder columns
    .y <- .x
    currentID <- keep
    newID <- order(currentID)
    # z=newID[1]
    if (!is.na(keep)) {
      for (z in newID) {
        # z = which(newID == z)
        .y[,descriptionCols$colID[z]] <- .x[,descriptionCols$colID[currentID[z]]]
        if (descriptionCols$colID[currentID[z]] != descriptionCols$colID[z]) {
          .y[,descriptionCols$colID[currentID[z]]] <- as.character(NA)
        }
      }
    }
    .y
  }) %>% bind_rows()
}    

#Creates asides with headings that match the first page asides
createAside <- function(numBreaks = 0, heading = NULL, body, list = TRUE) {
  brks <- rep('<br>\n', numBreaks) %>% paste0(collapse = "")
  if (!is.null(heading)) {
    head <- glue('<h2 class="extra-aside">{heading}</h2>\n')
  } else {
    head <- ""
  }
  if (list) {
    midBody <- glue('<li class="extra-aside" > {body}</li>') %>% glue_collapse(sep = "\n")
    bdy <- glue('<ul class="extra-aside" > \n{midBody}\n</ul>')
  } else {
    bdy <- glue_collapse(body, sep = "\n")
  }
  out <- glue('{brks}{head}\n{bdy}')
  return(out)
}

#Removes Advisors from positions df
removeAdvisors <- function(df) {
  df %>% mutate(loc = stringr::str_replace(loc, " \n([:graph:]|[:space:])*", "")) 
}

#Reorders positions out of chronological order using `manualOrder` column
reorderManually <- function(df) {
  keepChronological <- df %>% filter(is.na(manualOrder))
  reorder <- df %>% filter(!is.na(manualOrder)) %>% arrange(desc(manualOrder))
  bind_rows(keepChronological,reorder)
}



