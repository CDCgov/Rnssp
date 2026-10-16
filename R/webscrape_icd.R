#' ICD Code Web Scraper
#'
#' Function to web scrape ICD discharge diagnosis code sets from the CMS website.
#' If pulling ICD-10 codes, by default the function will attempt to pull the
#' diagnosis code update file for the current year. Users can specify earlier
#' publication years from the last 6 years if needed. The ICD-9 option will only
#' pull the most recent, final ICD-9 code set publication from 2014.
#' This function will return an error message if the requested diagnosis code
#' update file is not yet available or if the CMS website is  unresponsive.
#' The result is a tibble with 5 fields: version, code, description,
#' published_year, and fy_update_year. Codes are standardized to upper
#' case with punctuation and extra leading/tailing white space removed to
#' enable successful joining.
#'
#' @param icd_version A character value of either "ICD-10" or "ICD-9" to
#'     specify ICD version
#' @param publication_year A numeric integer indicating the publication year of
#'     desired ICD-10 code set. ICD-10 diagnosis code update files are
#'     published on October 1 of each year. Defaults to \code{NULL} to pull the
#'     most recent year's publication. An error will occur if this argument is
#'     used in combination with setting`icd_version` to "ICD-9".
#' @param  quiet logical. If \code{TRUE}, suppress status messages (if any),
#'     and the progress bar.
#'
#' @return A tibble
#'
#' @references
#' \itemize{
#'     \item \href{https://www.cms.gov/medicare/coding-billing/icd-10-codes}{CMS website for published ICD-10 diagnosis code update files}
#'     \item \href{https://www.cms.gov/medicare/coding-billing/icd-10-codes/icd-9-cm-diagnosis-procedure-codes-abbreviated-and-full-code-titles}{CMS website for published ICD-9 diagnosis code files}
#' }
#'
#' @examples
#'
#' # Example 1
#' icd9_2014 <- webscrape_icd(icd_version = "ICD-9")
#' head(icd9_2014)
#'
#' # Example 2
#' icd10_2026 <- webscrape_icd(icd_version = "ICD-10", publication_year = 2026)
#' head(icd10_2026)
#'
#' # Example 3
#' icd10_2025 <- webscrape_icd(icd_version = "ICD-10", publication_year = 2025)
#' head(icd10_2025)
#'
#' @export
#'

webscrape_icd_test <- function(icd_version = "ICD-10", publication_year = NULL, quiet = FALSE) {
  icd_version <- toupper(icd_version)
  
  if (!grepl("ICD-10|ICD-9", icd_version)) {
    cli::cli_abort('ICD version argument {.var icd_version} must be {.var "ICD-9"}
                   or {.var "ICD-10"}')
  }
  
  if (icd_version == "ICD-9" & !is.null(publication_year)) {
    cli::cli_abort("Argument {.var publication_year} only applies for ICD-10")
  }
  
  if (icd_version == "ICD-10" & !is.null(publication_year)) {
    if (publication_year < as.numeric(format(Sys.Date(), "%Y")) - 6) {
      cli::cli_abort("ICD-10 code sets published more than 5 years ago are not supported")
    }
  }
  
  if (icd_version == "ICD-10" & !is.null(publication_year)) {
    if (publication_year > as.numeric(format(Sys.Date(), "%Y"))) {
      cli::cli_abort("Argument {.var publication_year} cannot be greater than the current year.")
    }
  }
  
  if (icd_version == "ICD-10") {
    base_url <- "https://www.cms.gov/files/zip/"
    icd_file <- "-code-descriptions-tabular-order.zip"
    
    current_year <- if (!is.null(publication_year)) {
      publication_year == as.numeric(format(Sys.Date(), "%Y"))
    } else {
      TRUE
    }
    
    if (is.null(publication_year) | current_year) {
      fy_year <- as.numeric(format(Sys.Date(), "%Y")) + 1
      
      cms_url <- paste0(base_url, fy_year, icd_file)
      
      temp_dir <- tempdir()
      temp_file <- tempfile(tmpdir = temp_dir, fileext = ".zip")
      
      download_exit_status <- try(
        download.file(cms_url, temp_file, quiet = quiet)
      )
      
      if (all(class(download_exit_status) == "try-error")) {
        cli::cli_abort(paste("Error in {.fn webscrape_icd}: ICD-10 webscrape failed. Either the", fy_year, "code description file is not yet available or the CMS website is currently unresponsive. Please try a previous year or try again."))
      }
      
      file_name <- unzip(temp_file, list = TRUE) %>%
        filter(grepl("icd10cm_codes_\\d{4}\\.txt", Name)) %>%
        pull(Name)
      
      unzip(temp_file, files = file_name, exdir = temp_dir, overwrite = TRUE)
      
      file_path <- file.path(temp_dir, file_name)
      
      icd_dictionary <- fread(file_path, sep = "\t", header = FALSE) %>%
        setnames(old = "V1", new = "code_combo") %>%
        as_tibble() %>%
        mutate(
          code_combo = str_squish(code_combo),
          code_combo = sub(" ", "_", code_combo)
        ) %>%
        separate(code_combo, c("code", "description"), sep = "_") %>%
        mutate(
          code = str_squish(code),
          description = str_squish(description),
          published_year = fy_year - 1,
          fy_update_year = fy_year,
          version = "ICD-10"
        ) %>%
        relocate(version)
    } else {
      fy_year <- publication_year + 1
      
      cms_url <- paste0(base_url, fy_year, icd_file)
      
      temp_dir <- tempdir()
      temp_file <- tempfile(tmpdir = temp_dir, fileext = ".zip")
      
      download_exit_status <- try(
        download.file(cms_url, temp_file, quiet = quiet)
      )
      
      if (all(class(download_exit_status) == "try-error")) {
        cli::cli_abort(paste("Error in {.fn webscrape_icd}: ICD-10 webscrape failed. Either the", fy_year, "code description file is not available or the CMS website is currently unresponsive. Please try a different year or try again."))
      }
      
      file_name <- unzip(temp_file, list = TRUE) %>%
        filter(grepl("icd10cm_codes_\\d{4}\\.txt", Name)) %>%
        pull(Name)
      
      unzip(temp_file, files = file_name, exdir = temp_dir, overwrite = TRUE)
      
      file_path <- file.path(temp_dir, file_name)
      
      icd_dictionary <- fread(file_path, sep = "\t", header = FALSE) %>%
        setnames(old = "V1", new = "code_combo") %>%
        as_tibble() %>%
        mutate(
          code_combo = str_squish(code_combo),
          code_combo = sub(" ", "_", code_combo)
        ) %>%
        separate(code_combo, c("code", "description"), sep = "_") %>%
        mutate(
          code = str_squish(code),
          description = str_squish(description),
          published_year = fy_year - 1,
          fy_update_year = fy_year,
          version = "ICD-10"
        ) %>%
        relocate(version)
    }
    
    return(icd_dictionary)
  } else {
    cms_url <- "https://www.cms.gov/Medicare/Coding/ICD9ProviderDiagnosticCodes/Downloads/ICD-9-CM-v32-master-descriptions.zip"
    
    temp_dir <- tempdir()
    temp_file <- tempfile(tmpdir = temp_dir, fileext = ".zip")
    
    download_exit_status <- try(
      download.file(cms_url, temp_file, quiet = quiet)
    )
    
    if (all(class(download_exit_status) == "try-error")) {
      cli::cli_abort("Error in {.fn webscrape_icd}: ICD-9 webscrape failed.
                     CMS website is currently unresponsive.")
    }
    
    file_name <- unzip(temp_file, list = TRUE) %>%
      filter(Name == "CMS32_DESC_LONG_DX.txt") %>%
      pull(Name)
    
    unzip(temp_file, files = file_name, exdir = temp_dir, overwrite = TRUE)
    
    file_path <- file.path(temp_dir, file_name)
    
    file_year <- 2014
    
    icd_dictionary <- fread(file_path, sep = "\t", header = FALSE) %>%
      setnames(old = "V1", new = "code_combo") %>%
      as_tibble() %>%
      mutate(
        code_combo = str_squish(code_combo),
        code_combo = sub(" ", "_", code_combo)
      ) %>%
      separate(code_combo, c("code", "description"), sep = "_") %>%
      mutate(
        code = str_squish(code),
        description = str_squish(description),
        published_year = file_year,
        fy_update_year = "Legacy ICD-9 codes",
        version = "ICD-9"
      ) %>%
      relocate(version)
    
    return(icd_dictionary)
  }
}
