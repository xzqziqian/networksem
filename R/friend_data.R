#' Friendship network data
#'
#' A dataset with friendship network data of 165 undergraduate students in China, and also contains information extroversion and depression scores
#'
#' @format A list with two elements
#' \describe{
#'   \item{network}{A list containing a 165 x 165 adjacency matrix indicating friendship ties.}
#'   \item{non_network}{A dataframe with 165 rows and 11 columns containing variables on depression from the
#'   Patient Health Questionnaire and extroversion from the mini-IPIP scale.}
#' }
#'
#' @usage data("friend_data")
#'
#' @examples
#' data("friend_data")
#' length(friend_data$network)
#' head(friend_data$nonnetwork)
#'
#' @source <https://bigsem.org/>
"friend_data"
