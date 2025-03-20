#' Friendship network data
#'
#' A dataset with friendship network data of 165 undergaduate students in China, and also contains information extroversion and depression scores
#'
#' @format A list with two elements
#' A data frame with 7,240 rows and 60 columns:
#' \describe{
#'   \item{network}{A 165 x 165 adjacency matrix indicating friendship ties.}
#'   \item{non_network}{A dataframe with 165 rows and 11 columns containing variables on depression from the
#'   Patient Health Questionnaire and extroversion from the mini-IPIP scale.}
#' }
#'
#' @usage data("friend_data")
#'
#' @examples
#' data("friend_data")
#' dim(friend_data$network)
#' head(friend_data$non_network)
#'
#' @source <https://bigsem.org/>
"friend_data"
