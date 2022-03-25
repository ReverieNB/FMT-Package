#' Adjust quarters
#'
#' Used to adjust quarters/periods for MOU lists. Very simple
#' @param value input value between 1 and 4
#' @param move the number of periods back from current
#' @export

move_back <- function(value, move){
  drop <- as.numeric(value)-as.numeric(move)
  temp <- dplyr::case_when(
    drop==0 ~ 4,
    drop==-1 ~ 3,
    drop==-2 ~ 2,
    TRUE ~ drop)

    return(temp)
}
