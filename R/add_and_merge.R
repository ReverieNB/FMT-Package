#' Add content to spreadsheets
#'
#' Similar to add_stuff but will also merge columns
#' @param value the value to add
#' @param sheet sheet name
#' @param row_num cell row number
#' @param colIndex minimum column
#' @param max_col maximum column
#' @param style formatting style
#' @export

add_and_merge <- function(value, sheet, row_num, colIndex, max_col, style= column_title){
  sheet_value <- xlsx::createCell(rows[row_num], colIndex = colIndex)
  xlsx::setCellValue(sheet_value[[1,1]], value)
  xlsx::setCellStyle(sheet_value[[1,1]], style)
  xlsx::addMergedRegion(sheet, row_num, row_num, colIndex, max_col) #start and end row must always be the same
}
