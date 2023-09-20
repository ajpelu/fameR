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
  
  # Get file extension
  extension <- tools::file_ext(upload_path)
  
  # aux function for checking sheets
  check_sheets <- function(sheets, valid_sheets) {
    if (!identical(sheets, valid_sheets)) {
      warning("The uploaded file does not contain the right sheets")
    }
  }
  
  # Aux function for reading sheets into a named list
  read_sheets_into_list <- function(sheets, reader_function, upload_path) {
    purrr::map(seq_along(sheets), reader_function, path = upload_path) |>
      set_names(sheets)
  }
  
  # Read sheets based on file extension
  if (extension == "ods") {
    sheets <- readODS::list_ods_sheets(upload_path)
    check_sheets(sheets, valid_sheets)
    read_sheets_into_list(sheets, readODS::read_ods, upload_path)
  } else if (extension == "xlsx") {
    sheets <- readxl::excel_sheets(upload_path)
    check_sheets(sheets, valid_sheets)
    read_sheets_into_list(sheets, readxl::read_excel, upload_path)
  } else {
    warning("Unsupported file extension")
    NULL
  }
}
