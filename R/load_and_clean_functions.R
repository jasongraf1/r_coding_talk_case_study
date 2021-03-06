# -------------------------------------------------------------------------
# File: load_and_clean_functions.R
#
# Description:
# Custom defined functions for data wrangling and analysis of the corpus data
# in the *BE sat/stood* project
# -------------------------------------------------------------------------

# corpus files are sets of results downloaded from the CQPweb corpora. This
# function finds files from the same corpus and combines them
ReadDataset <- function(file, delim = "\t"){
  df <- vroom::vroom(file, delim = delim, escape_double = FALSE, trim_ws = TRUE,
                     col_types = "c") %>%
    janitor::clean_names() %>%
    dplyr::mutate(across(.cols = everything(), as.character))

  # get the speaker sex information
  if(grepl("BNC", file)){
    sex <- stringr::str_extract(file, "(Male|Female)")
    df <- df %>%
      mutate(
        sex = sex,
        corpus = "BNC2014"
      )
  } else {
    df <- df %>%
      mutate(corpus = "glowbe")
  }

  return(df)
}


# strip the "<<< >>>" from the Query_item columns (this comes from the corpus
# interface)
StripBrackets <- function(df){
  df <- df %>%
    dplyr::mutate(
      query_item = stringr::str_remove_all(query_item, "(<<<|>>>)") %>%
        stringr::str_trim(),
      tagged_query_item = stringr::str_remove_all(tagged_query_item, "(<<<|>>>)") %>%
        stringr::str_trim()
    )
  return(df)
}

# Add a concise code for the country in GloWbE data. Easier for plotting
CodeCountry <- function(country) {
  code <- case_when(
    country == "Australia" ~ "AU",
    country == "Bangladesh" ~ "BD",
    country == "Canada" ~ "CA",
    country == "Great Britain" ~ "GB",
    country == "Ghana" ~ "GH",
    country == "Hong Kong" ~ "HK",
    country == "Ireland" ~ "IE",
    country == "India" ~ "IN",
    country == "Jamaica" ~ "JM",
    country == "Kenya" ~ "KE",
    country == "Sri Lanka" ~ "LK",
    country == "Malaysia" ~ "MY",
    country == "Nigeria" ~ "NG",
    country == "New Zealand" ~ "NZ",
    country == "Philippines" ~ "PH",
    country == "Pakistan" ~ "PK",
    country == "Singapore" ~ "SG",
    country == "Tanzania" ~ "TZ",
    country == "South Africa" ~ "ZA",
    country == "United States" ~ "US",
    TRUE ~ "XX"
  )
  return(code)
}

# functions for annotation ------------------------------------------------

# get the verb: sit vs stand
GetVerb <- function(token){
  if(grepl("(sat|sitting|sitten)", token)){
    v <- "sit"
  } else v <- "stand"
  return(v)
}

# get the variant (-ed vs. -ing form) for the token
GetVariant <- function(token){
  form <- stringr::str_split(token, " ", simplify = T) %>%
    last()
  variant <- ifelse(form %in% c("sat", "stood", "sitten"), "ed", "ing")
  return(variant)
}

# make a column with simplified context, e.g. 2 words to the left and 6 words to
# the right of the token
MakeContext <- function(context_before, token, context_after, n1 = 2, n2 = 6){
  before_words <- stringr::str_split(context_before, " ", simplify = T)
  len_b <- length(before_words)
  after_words <- stringr::str_split(context_after, " ", simplify = T)
  text <- c(before_words[(len_b - n1):len_b], token, after_words[1:n2])

  return(paste(text, collapse = " "))
}

# get the following prepositional phrase material after the token
GetPostmodifierPP <- function(context_after){
  first_word <- stringr::str_split(context_after, " ", simplify = T) %>%
    first()
  if(grepl("_(rp|ii|RP|II)", first_word)){
    postm <- "y"
  } else postm <- "n"

  return(postm)
}

# get the following verb phrase material after the token
GetPostmodifierVP <- function(context_after){
  words <- stringr::str_split(context_after, " ") %>%
    unlist()
  first_words <- words[1:4] # pull out the 4 words immediately following verb
  if(any(grepl("_(v.g|V.G)", first_words))){
    postm <- "y"
  } else postm <- "n"

  return(postm)
}

# count the words between sat/stand and a following -ing verb (Rohdengburg)
# calls adjacency avoidance 'horror aequi'
CheckHorrorAequi <- function(context_after, post_vp){
  if(post_vp == "y"){
    words <- stringr::str_split(context_after, " ") %>%
      unlist()
    first_words <- words[1:4]
    horror <- which(grepl("_(v.g|V.G)", first_words))[1]
  } else horror <- NA

  return(horror - 1)
}

# get the subject of the token
GetSubject <- function(token, before){
  if(grepl("^'", token)){
    subj <- stringr::str_split(before, " ") %>%
      unlist() %>%
      last() %>%
      tolower()
  } else if(grepl("am", token)) {
    subj <- "i"
  } else {
    # take the last noun or pronoun in the preceding context as the most likely
    # subject
    before_words <- stringr::str_split(before, " ") %>%
      unlist()
    subj <- before_words[grepl("_[np]\\S+", before_words, ignore.case = T)] %>%
      last() %>%
      tolower()
  }
  return(subj)
}

# get the person of the subject
GetSubjectPerson <- function(subj){
  if(is.na(subj)){
    pers <- "NA"
  } else if(subj %in% c("i", "we")){
    pers  <- "1P"
  } else if(subj == "you"){
    pers <- "2P"
  } else {
    pers <- "3P"
  }
  return(pers)
}

# get the tense and aspect of the token (needs work)
GetTenseAspect <- function(token, before){
  if(grepl("_vb[mzr]", token, ignore.case = T)){
    tense <- "pres"
  } else if(grepl("_vbd", token, ignore.case = T)){
    tense <- "past"
  } else {
    before_words <- stringr::str_split(before, " ") %>%
      unlist()
    last_word <- before_words[grepl("_(vm|vh|to)", before_words, ignore.case = T)] %>%
      last()
    if(grepl("_vm", last_word, ignore.case = T)){
      tense <- "modal"
    } else if(grepl("_vh[0gzi]", last_word, ignore.case = T)){
      tense <- "presperf"
    } else if(grepl("_vhd", last_word, ignore.case = T)){
      tense <- "pastperf"
    } else if(grepl("_to", last_word, ignore.case = T)){
      tense <- "inf"
    } else tense <- "NA"
  }

  return(tense)
}

# Combined function for running all the annotations above in one go
CleanAnnotateData <- function(data){
  data_processed <- data %>%
    StripBrackets() %>%
    mutate(
      variant = map_chr(query_item, GetVariant),
      verb = map_chr(query_item, GetVerb),
      subj_tagged = map2_chr(query_item, tagged_context_before, GetSubject),
      subj = str_remove(subj_tagged, "_.*"),
      subj_person = map_chr(subj, GetSubjectPerson),
      tense_aspect = map2_chr(tagged_query_item, tagged_context_before, GetTenseAspect),
      postmodifier_pp = map_chr(tagged_context_after, GetPostmodifierPP),
      postmodifier_vp = map_chr(tagged_context_after, GetPostmodifierVP),
      dist_to_post_vp = map2_chr(tagged_context_after, postmodifier_vp, CheckHorrorAequi),
      token_simple = pmap_chr(list(context_before, query_item, context_after), MakeContext),
      comment = "" #empty column for manually adding comments later
    ) %>%
    rownames_to_column("token_id") # add rownames for a

  # Cases of passive "BE stood down" are common in AUS and NZ but are not reliable hits.
  # Genuine uses of "BE stood down" are rare, so the false negative rate is likely
  # to be low enough to exclude these here
  data_processed <- data_processed %>%
    dplyr::filter(!(verb == "stand" & grepl("down ", context_after))) %>%
    distinct(token_simple, .keep_all = TRUE) # remove duplicated data

  # add variety information to GloWbE data
  if(data_processed[1,"corpus"] == "glowbe"){
    data_processed <- data_processed %>%
      mutate(country_code = map_chr(country, CodeCountry))
  }

  return(data_processed)
}

# save the dataset
SaveData <- function(data, filename, delim = "\t"){
  filepath <- here("data_processed", filename)
  vroom::vroom_write(data, filepath, delim = delim)
  return(filepath)
}

# load tables of corpus frequencies (for markdown reports)
LoadTable <- function(file, clean = TRUE){
  if(clean){
    tab <- vroom::vroom(file, delim = ",", col_types = cols()) %>%
      mutate(across(c(3, 5, 7, 9),
                    .fns = ~str_trim(format(as.numeric(.x), nsmall = 1))),
             V1 = str_replace(V1, "BNC", "Spoken BNC 2014"))
  } else {
    tab <- vroom::vroom(file, delim = ",", col_types = cols())
    }
  return(tab)
}