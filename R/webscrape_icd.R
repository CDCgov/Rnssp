#' ICD Code Web Scraper
#'
#' Function to web scrape ICD discharge diagnosis code sets from the CDC FTP server
#' (for ICD-10) or CMS website (for ICD-9). If pulling ICD-10 codes, by default the
#' function will search for the most recent year's code set publication by NCHS.
#' Users can specify earlier publication years back to 2019 if needed. The ICD-9
#' option will only web scrape the most recent, final ICD-9 code set
#' publication (2014) from the CMS website. This function will return an error
#' message if the FTP server or CMS website is  unresponsive or if a timeout
#' of 60 seconds is reached. The result is a dataframe with 3 fields: code,
#' description, and set (ICD version concatenated with year). Codes are
#' standardized to upper case with punctuation and extra leading/tailing white
#' space removed to enable successful joining.
#'
#' @param icd_version A character value of either "icd10", "ICD10", "icd9", or
#'     "ICD9" to specify ICD version
#' @param year A numeric integer indicating the year of desired ICD-10 code set.
#'     Defaults to \code{NULL} to pull the most recent year's publication.
#' @param  quiet logical. If \code{TRUE}, suppress status messages (if any),
#'     and the progress bar.
#'
#' @return A dataframe
#'
#' @references
#' \itemize{
#'     \item \href{https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Publications/ICD10CM/}{CDC NCHS FTP Server Location for Published ICD-10 Code Sets}
#'     \item \href{https://www.cms.gov/Medicare/Coding/ICD9ProviderDiagnosticCodes/codes}{CMS Website for Published ICD-9 Code Sets}
#' }
#'
#' @examples
#'
#' # Example 1
#' icd10_2022 <- webscrape_icd(icd_version = "ICD9")
#'
#' # Example 2
#' icd10_2021 <- webscrape_icd(icd_version = "ICD10", year = 2021)
#'
#' # Example 3
#' icd10_2020 <- webscrape_icd(icd_version = "ICD10", year = 2020)
#'
#' @export
#'

webscrape_icd <- function(icd_version = "ICD10", year = NULL, quiet = FALSE) {
  icd_version <- toupper(icd_version)

  if (!grepl("ICD10|ICD9", icd_version)) {
    cli::cli_abort('ICD version argument {.var icd_version} must be {.var "ICD9"}
                   or {.var "ICD10"}')
  }

  if (icd_version == "ICD9" & !is.null(year)) {
    cli::cli_abort("Argument {.var year} only applies for ICD10")
  }

  if (icd_version == "ICD10" & !is.null(year)) {
    if (year <= 2018) {
      cli::cli_abort("ICD-10 code sets prior to 2019 are not supported")
    }
  }

  if (icd_version == "ICD10" & !is.null(year)) {
    if (year > as.numeric(format(Sys.Date(), "%Y")) + 1) {
      cli::cli_abort("Argument {.var year} cannot be greater than the upcoming year.")
    }
  }

  if (icd_version == "ICD10") {
    ftp_url <- "https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Publications/ICD10CM/"

    root_folders <- readLines(ftp_url, warn = FALSE)[3]

    current_year <- if (!is.null(year)) {
      year == as.numeric(format(Sys.Date(), "%Y"))
    } else {
      TRUE
    }

    if (is.null(year) | current_year) {
      path <- str_split(root_folders, "</A><br>") %>%
        magrittr::extract2(1) %>%
        str_extract_all("pub/Health_Statistics/NCHS/Publications/ICD10CM/\\d{4}/") %>%
        compact() %>%
        tibble::enframe() %>%
        mutate(year = as.numeric(str_extract(value, "\\d{4}"))) %>%
        filter(year == max(year)) %>%
        pull(value) %>%
        as.character()

      recent_year <- str_extract(path, "\\d{4}")

      path_files <- readLines(paste0("https://ftp.cdc.gov/", path), warn = FALSE)[3]

      file_list <- unlist(
        str_extract_all(
          path_files, pattern = paste0(
            path, "[a-zA-Z\\d-_ %20]+\\w+(?:\\.(?:xlsx|pdf|zip|txt))?"
          )
        )
      )

      file_match <- grepl(
        "code_descriptions|code%20descriptions|icd10cm_codes_\\d{4}",
        gsub("[ -]", "_", tolower(file_list))
      )

      if (all(file_match == FALSE)) {
        cli::cli_abort(
          paste(
            "The",
            recent_year,
            "code description file is not yet available. Please try a previous year."
          )
        )
      } else {
        file <- paste0("https://ftp.cdc.gov/", file_list[which(file_match)])
        file_ext <- tools::file_ext(file)

        file_idx <- which(file_ext == "zip")

        file_zip <- file[file_idx]

        temp_dir <- tempdir()
        temp_file <- tempfile(tmpdir = temp_dir, fileext = ".zip")

        download_exit_status <- try(
          download.file(file_zip, temp_file, quiet = quiet)
        )

        if (all(class(download_exit_status) == "try-error")) {
          cli::cli_abort("Error in {.fn webscrape_icd}: ICD-10 webscrape failed.
                         FTP server is currently unresponsive.")
        }

        file_name <- unzip(temp_file, list = TRUE) %>%
          filter(
            Name == paste0("Code Descriptions/icd10cm-codes-", file_year, ".txt")
          ) %>%
          pull(Name)

        unzip(temp_file, files = file_name, exdir = temp_dir, overwrite = TRUE)

        file_path <- file.path(temp_dir, file_name)

        icd_dictionary <- fread(file_path, sep = "\t", header = FALSE) %>%
          setnames(old = "V1", new = "code_combo") %>%
          as.data.frame() %>%
          mutate(
            code_combo = str_replace_all(code_combo, "\\s{2,4}", "_"),
            code_combo = str_replace_all(code_combo, "(?<=^[[:alnum:]]{7})\\s{1}", "_")
          ) %>%
          separate(code_combo, c("code", "description"), sep = "_") %>%
          mutate(
            code = str_squish(code),
            description = str_squish(description),
            set = paste("ICD-10", file_year)
          )
      }

    } else {
      file_year <- year
      path <- paste0("pub/Health_Statistics/NCHS/Publications/ICD10CM/", file_year, "/")
      file <- paste0("https://ftp.cdc.gov/", path, "icd10cm_codes_", file_year, ".txt")

      icd_dictionary <- fread(file, sep = "\t", header = FALSE) %>%
        setnames(old = "V1", new = "code_combo") %>%
        as.data.frame() %>%
        mutate(
          code_combo = str_replace_all(code_combo, "\\s{2,4}", "_"),
          code_combo = str_replace_all(code_combo, "(?<=^[[:alnum:]]{7})\\s{1}", "_")
        ) %>%
        separate(code_combo, c("code", "description"), sep = "_") %>%
        mutate(
          code = str_squish(code),
          description = str_squish(description),
          set = paste("ICD-10", file_year)
        )
    }

    return(icd_dictionary)
  } else {
    base_url <- "https://www.cms.gov/Medicare/Coding/ICD9ProviderDiagnosticCodes"
    icd_file <- "Downloads/ICD-9-CM-v32-master-descriptions.zip"
    cms_url <- file.path(base_url, icd_file)

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
      as.data.frame() %>%
      mutate(
        code_combo = str_replace_all(code_combo, "\\s{2,4}", "_"),
        code_combo = str_replace_all(code_combo, "(?<=^[[:alnum:]]{4,7})\\s{1}", "_")
      ) %>%
      separate(code_combo, c("code", "description"), sep = "_") %>%
      mutate(
        code = str_squish(code),
        description = str_squish(description),
        set = paste("ICD-9", file_year)
      )

    return(icd_dictionary)
  }
}

