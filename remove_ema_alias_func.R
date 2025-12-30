library(readr)
library(dplyr)
library(tidyr)
library(readxl)
library(stringr)

#----- Helper Function to Organize Aliases (trim, squish, lowerCase) -----#
normalize_alias <- function(df) {
  df <- df %>%
    mutate(alias = alias %>% 
             str_trim() %>%
             str_squish() %>%
             str_to_lower()
    )
  return(df)
}


# ----- Match alias column with ID and remove m-path alias
switch_alias_with_id <- function(ema, 
                                 crosswalk_path, 
                                 crosswalk_type = c("csv", "xlsx")) {
  
  crosswalk_type <- match.arg(crosswalk_type)
  
  if (crosswalk_type == "csv") {
    cw <- read.csv(crosswalk_path, stringsAsFactors = FALSE)
  } else if (crosswalk_type == "xlsx") {
    cw <- readxl::read_excel(crosswalk_path, sheet = 1)
  }
  
  names(cw) <- c("alias", "ID")
  
  ema <- normalize_alias(ema)
  cw  <- normalize_alias(cw)
  
  stopifnot(!anyDuplicated(cw$alias))
  
  # ---- join (keep alias for diagnostics)
  joined <- dplyr::left_join(ema, cw, by = "alias")
  
  # ---- summary
  n_total    <- nrow(joined)
  n_matched  <- sum(!is.na(joined$ID))
  unmatched  <- unique(joined$alias[is.na(joined$ID)])
  
  message(
    sprintf(
      "Alias matching summary: %d/%d aliases replaced.",
      n_matched, n_total
    )
  )
  
  if (length(unmatched) > 0) {
    message(
      "Aliases not found in crosswalk: ",
      paste(unmatched, collapse = ", ")
    )
  }
  
  # ---- final cleanup
  ema <- joined %>%
    dplyr::select(-alias) %>%
    dplyr::relocate(ID, .before = 1)
  
  if ("initials" %in% names(ema)) {
    ema <- dplyr::select(ema, -initials)
  }
  
  ema
}
