#' Removes stopwords and common patterns of Chief Complaints
#'
#' Converts a dataframe or tibble into a data.table object,
#' removes stop words and patterns identified by a series of regular expressions.
#'
#' @param tbl the input dataframe
#'
#' @return A transformed tibble dataframe with clean CC and CCDD text fields
#' @export
#'
#' @examples
#' data <- api_data %>%
#'   clean_text()
clean_text <- function(tbl){
  if(!all(c("ChiefComplaintParsed", "DischargeDiagnosis") %in% names(tbl))){
    stop("tbl argument is not a standardized dataset!")
  }
  .stopwords2 <- paste0("\\bOF\\b|\\bOR\\b|\\bWITH\\b|\\bFOR\\b|\\bON\\b|\\bLEFT\\b|\\bRIGHT\\b|",
                        "\\bPT\\b|\\bUNSPECIFIED\\b|\\bSTATUS\\b|\\bIN\\b|\\bHX\\b|//bDAYS\\b|\\bAGO\\b|\\bUNKNOWN",
                        "\\b|\\bTNOTE\\b|\\b2015\\b|\\b2016\\b|\\b2017\\b|\\b2018\\b|\\bEND\\b|\\bNON\\b|\\bSMALL\\b|",
                        "\\bCAUSES\\b|\\bOLD\\b|\\bBY\\b|\\bFROM\\b|\\bLARGE\\b|\\bPOSSIBLE\\b|\\bCAUSE\\b|\\bASSOCIATED",
                        "\\b|\\bFOLLOWING\\b|\\bNOT\\b|\\bPROBLEMS\\b|\\bGENERAL\\b|\\bPROBABLY\\b|\\bSPECIFIED\\b|\\bHIGH",
                        "\\b|\\bFURTHER\\b|\\bAT\\b|\\bFOR\\b|\\bWITHOUT\\b|\\bOTHER\\b|\\bOTHERWISE\\b|\\bSTATE\\b|\\bUSE",
                        "\\b|\\bAS\\b|\\bNO\\b|\\bVIA\\b|\\bBACK\\b|\\bAFTER\\b|\\bLIKE\\b|\\bALL\\b|\\bNEW\\b|\\bBUT\\b|",
                        "\\bBE\\|\\bTO\\b|\\bTHIS\\b|\\bTHAT\\b|\\bNA\\b|\\bIS\\b|\\bHAS\\b|\\bTHE\\b|\\bHAVING\\b|\\b2019\\b|",
                        "\\b2020\\b|\\bTO\\b")

  .pattern1 <- "[A-TV-Z][0-9][0-9AB]\\.?[0-9]{0,2}|PG[0-9]{0,3}"
  .pattern2 <- "[[:cntrl:]]|<BR>|[#?!·.'+)(:=@%]"
  .pattern3 <- "\\b[0-9]{1}\\b|\\b[0-9]{2}\\b|\\bNA\\b|\\|"
  .pattern4 <- "[;/,\\\\*-]|([a-zA-Z])/([\\d])|([\\d])/([a-zA-Z])|([a-zA-Z])/([a-zA-Z])|([\\d])/([\\d])"
  .pattern5 <- "\\b[A-Za-z]{2,20}\\b"
  .pattern6 <- "[[:cntrl:]]|<BR>|[#?!·.'+)(:=@%]"
  .pattern7 <- "\\b[0-9]{1}\\b|\\b[0-9]{2}\\b|\\bNA\\b|\\|"
  .pattern8 <- "[;/,\\\\*-]|([a-zA-Z])/([\\d])|([\\d])/([a-zA-Z])|([a-zA-Z])/([a-zA-Z])|([\\d])/([\\d])"

  tbl %>%
    janitor::clean_names() %>%
    data.table() %>%
    .[, chief_complaint_parsed := str_remove_all(chief_complaint_parsed, .stopwords2)] %>%
    .[, chief_complaint_parsed := str_remove_all(chief_complaint_parsed, .pattern1)] %>%
    .[, chief_complaint_parsed := str_remove_all(chief_complaint_parsed, "CHIEF COMPLAINT|SEE CHIEF")] %>%
    .[, chief_complaint_parsed := str_remove_all(chief_complaint_parsed, .pattern2)] %>%
    .[, discharge_diagnosis := str_remove_all(discharge_diagnosis, .pattern2)] %>%
    .[, chief_complaint_parsed := str_remove_all(chief_complaint_parsed, .pattern3)] %>%
    .[, discharge_diagnosis := str_remove_all(discharge_diagnosis, .pattern3)] %>%
    .[, chief_complaint_parsed := str_remove_all(chief_complaint_parsed, .pattern4)] %>%
    .[, discharge_diagnosis := str_replace_all(discharge_diagnosis, .pattern4, " ")] %>%
    .[, chief_complaint_parsed := str_squish(chief_complaint_parsed)] %>%
    .[, discharge_diagnosis := str_squish(discharge_diagnosis)] %>%
    .[, discharge_diagnosis := str_remove_all(discharge_diagnosis, .pattern5)] %>%
    .[, discharge_diagnosis := ifelse(nchar(discharge_diagnosis) <= 2, "", discharge_diagnosis)] %>%
    .[, discharge_diagnosis := str_replace_na(discharge_diagnosis, replacement = "")] %>%
    .[, chief_complaint_parsed := ifelse(nchar(chief_complaint_parsed) <= 3, "", chief_complaint_parsed)] %>%
    .[, chief_complaint_parsed := str_replace_na(chief_complaint_parsed, "")] %>%
    .[, ccdd2 := str_c(chief_complaint_parsed, discharge_diagnosis, sep = " ")] %>%
    .[, chief_complaint_parsed := vapply(lapply(str_split(chief_complaint_parsed, " "), unique),
                                         paste, character(1L), collapse = " ")] %>%
    .[, ccdd2 := vapply(lapply(str_split(ccdd2, " "), unique), paste, character(1L), collapse = " ")] %>%
    .[, discharge_diagnosis := vapply(lapply(str_split(discharge_diagnosis, " "), unique),
                                      paste, character(1L), collapse = " ")] %>%
    .[, ccdd2 := str_squish(ccdd2)] %>%
    tibble::as_tibble()
}
