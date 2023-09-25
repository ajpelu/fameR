#' Read data from all sheets of an uploaded file.
#'
#' This function reads data from all sheets of an uploaded file in ODS or XLSX 
#' format and returns them in a named list.
#'
#' @param upload_path Character string specifying the path to the uploaded file.
#' @param valid_sheets Character vector specifying the names of sheets that are 
#' expected to be in the uploaded file.
#'
#' @return A named list containing data from all sheets in the uploaded file, 
#' with sheet names as list names. 
#'
#' @importFrom readODS list_ods_sheets read_ods
#' @importFrom readxl excel_sheets read_excel
#'
#' @export
readAllsheets <- function(upload_path, valid_sheets) {
  
  my_locale <- readr::locale(encoding = "UTF-8") 
  
  # Get file extension
  extension <- tools::file_ext(upload_path)
  
  # aux function for checking sheets
  check_sheets <- function(sheets, valid_sheets) {
    if (!identical(sheets, valid_sheets)) {
      warning("The uploaded file does not contain the right sheets")
    }
  }
  
  read_sheets_into_list <- function(sheets, reader_function, upload_path, col_types = NULL) {
    purrr::map(
      seq_along(sheets),
      ~{
        Sys.setlocale("LC_CTYPE", "UTF-8")
        if (sheets[.x] == "dicc_taxon") {
          reader_function(
            .x,
            path = upload_path,
            col_names = TRUE,
            col_types = c("numeric", rep("text", 6))  # Specify col_types for the "dicc_taxon" sheet
          )
        } else {
          reader_function(
            .x,
            path = upload_path,
            col_names = TRUE,
            col_types = NULL  # For other sheets, use NULL for col_types
          )
        }
      }) |>
      set_names(sheets)
  }
  
  # Read sheets based on file extension
  # Read sheets based on file extension
  if (extension == "ods") {
    sheets <- readODS::list_ods_sheets(upload_path)
    check_sheets(sheets, valid_sheets)
    col_types <- NULL
    read_sheets_into_list(sheets, readODS::read_ods, upload_path, col_types)
  } else if (extension == "xlsx") {
    sheets <- readxl::excel_sheets(upload_path)
    check_sheets(sheets, valid_sheets)
    col_types <- NULL
    read_sheets_into_list(sheets, readxl::read_excel, upload_path, col_types)
  } else {
    warning("Unsupported file extension")
    NULL
  }
}
