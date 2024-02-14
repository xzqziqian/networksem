#' TextSEM
#'
#' Get the sentiment of the text and use that as a variable in SEM
#' @importFrom lavaan lavParseModelString lavaanify sem
#' @importFrom sentimentr sentiment_by
#' @param model The structural equation model.
#' @param data The input data frame.
#' @param text_var A vector of text variable(s).
#' @param text_stats A vector of text sentiment statistic(s). Currently only support one statistic - the overall sentiment.
#' @return A list containing three items: the SEM with added sentiment variables, the input data frame with added text sentiment statistics, and the model estimates.
#' @export
#'
sem.text <- function(model,
                     data,
                     text_var,
                     text_stats=c('OverallSenti'),
                     polarity_dt = lexicon::hash_sentiment_jockers_rinker,
                     valence_shifters_dt = lexicon::hash_valence_shifters,
                     missing = 'ML',
                     fixed.x = FALSE,
                     ...){

  ## parse the model
  model_info <- lavParseModelString(model)
  model_var <- unique(c(model_info$lhs, model_info$rhs))

  ## get the list of text variables in the model
  text_var <- text_var[text_var %in% model_var]
  # print("text_var")
  # print(text_var)

  N <- length(text_var) # Number of text variables
  if (N > 0){
    ## now get the sentiment score of the text
    text_score <- list()
    for(i in 1:N){
      temp <- sentiment_by(text.var=data[, text_var[i]], ...) # Compute sentiment scores
      text_score[[i]] <- temp$ave_sentiment # Select variables
    }
    names(text_score) <- text_var
    # print("text_score")
    # print(as.data.frame(text_score))

    data_new <- cbind(data, as.data.frame(text_score))
    names(data_new) <- c(names(data), paste0(rep(text_var, each = length(text_stats)), '.', text_stats))
    # print("data_new")
    # print(names(data_new))

    model_lavaanify <- lavaanify(model)
    model_user <- model_lavaanify[model_lavaanify$user==1, ]
    # print("model_user")
    # print(model_user)

    model_new <- c()
    for(i in 1:nrow(model_user)){
      row <- model_user[i,]
      # print(row)
      if((row['lhs'] %in% text_var) && (row['rhs'] %in% text_var)){
        model_new <- c(model_new, paste0(rep(paste0(row['lhs'], '.', text_stats), each = length(text_stats)),
                                         ' ', row['op'], ' ', rep(paste0(row['rhs'], '.', text_stats), length(text_stats))))
      } else if(row['lhs'] %in% text_var){
        model_new <- c(model_new, paste0(row['lhs'], '.', text_stats, ' ', row['op'], ' ', row['rhs']))
      } else if(row['rhs'] %in% text_var){
        model_new <- c(model_new, paste0(row['lhs'], ' ', row['op'], ' ', row['rhs'], '.', text_stats))
      } else{
        model_new <- c(model_new, paste0(row['lhs'],  ' ', row['op'], ' ', row['rhs']))
      }
    }
    # print(model_new)
    # model_new <- paste0(model_new, collapse = '\n')
  }
  model_res <- sem(model=model_new, data=data_new,
                   missing = missing, fixed.x = fixed.x, ...)

  return(list(model=model_new, data=data_new, estimates=model_res))
}
