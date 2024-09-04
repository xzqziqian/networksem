# library(readr)
# library(dplyr)
# library(naniar)
# library(caret)
# library(magrittr)
# library(tm)
# library(udpipe)
# library(lavaan)
# library(sentimentr)
# library(lexicon)
#
#
# get_aspect_sentiment <- function(ls){
#   # ls: list of annotation file
#   dep_a2o=c('nsubj','obj','obl','nmod','conj','advcl','xcomp','amod','acl:relcl','advmod','acl','obl:tmod','obl:npmod','iobj')
#   for (s in 1:length(ls)){
#     for (i in 1:dim(ls[[s]])[1]){
#       hid=ls[[s]][i,]$head_token_id
#       if(hid %in% ls[[s]]$token_id & ls[[s]][i,]$dep_rel %in% dep_a2o ){
#         if (!(is.na(ls[[s]][i,]$aspect) & is.na(ls[[s]][ls[[s]]$token_id==hid,]$value))){
#           ls[[s]][i,]$sentiment = ls[[s]][ls[[s]]$token_id==hid,]$value
#         }
#         else if (!(is.na(ls[[s]][i,]$value) & is.na(ls[[s]][ls[[s]]$token_id==hid,]$aspect))){
#           ls[[s]][ls[[s]]$token_id==hid,]$sentiment = ls[[s]][i,]$value
#         }
#       }
#       if(!(is.na(ls[[s]][i,]$value) & is.na(is.na(ls[[s]][i,]$aspect))) & is.na(ls[[s]][i,]$sentiment)){
#         ls[[s]][i,]$sentiment = ls[[s]][i,]$value
#       }
#     }
#   }
#   return(ls)
# }
#
#
# absa <- function(dat, group_var = NULL,
#      udpipe="data/english-ewt-ud-2.5-191206.udpipe",
#      params='data/absa_params.Rds'){
#   udmodel_en <- udpipe_load_model(file = udpipe)
#   params <- readRDS(params)
#
#   # Load pre-defined parameters
#   sentiSET_score <- params$sentiSET%>%select(word,pos,value = mean_all4)%>%mutate(value = round(value,2))
#   degreeAdv <- params$degreeAdv%>%select(!degree)
#   aspect.list <- params$asp_list_firstlevel_new%>%select(word,aspect)%>%arrange(word)
#
#   # Convert data to standard dataframe format
#   if(is.vector(dat)){
#     df <- data.frame(comment = dat)
#   }else if(is.data.frame(dat)){
#     df <- dat
#   }else{
#     stop("Data must be a vector or a dataframe!")
#   }
#   comments <- tolower(df$comment)
#   df['doc_id'] <- as.numeric(row.names(df))
#
#   ## If the data contains group variable
#   if(!is.null(group_var)){
#     if(group_var %in% colnames(df)){
#       group = TRUE
#     } else{
#       stop("Group variable does not exist in the data. Please recheck.")
#     }
#   } else{
#     group = FALSE
#   }
#
#
#   size <- length(comments)
#   annotate <- udpipe_annotate(udmodel_en, x = comments, doc_id = c(1:size))
#   dataframe.annotate <- as.data.frame(annotate)
#
#   negation <- c('none','not','never','neither','nobody','nowhere','no','nothing')
#   #npos = c('adp', 'sconj','x','propn','cconj','num','aux','intj')
#
#   # Aspect class and sentiment score for corresponding word
#   ap_rs <- dataframe.annotate%>%
#     select(doc_id,sentence_id,token_id,token, lemma, pos = upos,head_token_id,dep_rel)%>%
#     mutate(pos = tolower(pos))%>%
#     mutate_at(c('doc_id','sentence_id','token_id','head_token_id'),funs(as.numeric))%>%
#     mutate(word = ifelse(pos == 'adj', token, lemma))%>% #lemmatize the words except adjective
#     left_join(sentiSET_score,by = c('word','pos')) %>% # sign the sentiment value to the sentiment expression
#     left_join(aspect.list, by = 'word') %>%# sign the aspect category to the aspect expression
#     left_join(degreeAdv, by = 'word')%>% # sign weight to the degree adverb/modifier
#     mutate(profid=NULL,neg=ifelse(word %in% negation,1,NA), sentiment = NA, value = ifelse(is.na(weight), value, NA))%>%#create a overall sentiment score variable & negation variable & modifier variable
#     filter_at(vars(value:neg), any_vars(!is.na(.)))
#   # length deduction from 37,489 to 13,393 reduction rate 64%
#
#   list.ap_rs <- split(ap_rs, ap_rs[,1:2], drop=T) #grouping by doc_id and sentence_id, this step each list element contains one sentence info
#
#   for (s in 1:length(list.ap_rs)){ # sentence s
#     for (i in 1:dim(list.ap_rs[[s]])[1]){ # word i
#       if (!is.na(list.ap_rs[[s]][i,]$neg) ){
#         hid=list.ap_rs[[s]][i,]$head_token_id
#         list.ap_rs[[s]][list.ap_rs[[s]]$token_id==hid,]$value = -list.ap_rs[[s]][list.ap_rs[[s]]$token_id==hid,]$value
#       }
#       else if (!is.na(list.ap_rs[[s]][i,]$weight) & list.ap_rs[[s]][i,]$dep_rel == 'advmod'){
#         hid=list.ap_rs[[s]][i,]$head_token_id
#         list.ap_rs[[s]][list.ap_rs[[s]]$token_id==hid,]$value = list.ap_rs[[s]][i,]$weight*list.ap_rs[[s]][list.ap_rs[[s]]$token_id==hid,]$value
#       }
#     }
#   }
#
#   list.ap_rs.final <- get_aspect_sentiment(list.ap_rs)
#   data.ap_rs.final <- as.data.frame(bind_rows(list.ap_rs.final))
#
#   doc_aspect_sentiment <- data.ap_rs.final%>%
#     group_by(doc_id,aspect)%>%
#     summarise(score=sum(sentiment,na.rm=T),)%>%
#     filter(!is.na(aspect))%>%select(doc_id,aspect,A=score)
#
#   doc_aspect_sentiment_wide <- reshape(data.frame(doc_aspect_sentiment),
#                                        timevar='aspect',
#                                        idvar='doc_id',
#                                        direction='wide')
#
#   doc_overall_sentiment <- data.ap_rs.final%>%
#     group_by(doc_id)%>%
#     summarise(OverallSenti = sum(value,na.rm=T))
#
#   final.com.abs <- df%>%
#     left_join(doc_aspect_sentiment_wide, by = 'doc_id')%>%
#     left_join(doc_overall_sentiment, by = 'doc_id')
#
#   if (group){
#       final.pf.abs = final.com.abs%>%
#         group_by_at(group_var)%>%
#         summarise(ncom = n(),
#                   # rate = round(mean(rate, na.rm=T),2),
#                   # easy = round(mean(easy, na.rm=T),2),
#                   A1=round(mean(A.1, na.rm=T),2),
#                   A2=round(mean(A.2, na.rm=T),2),
#                   A3=round(mean(A.3, na.rm=T),2),
#                   OverallSenti=round(mean(OverallSenti, na.rm=T),2))
#       rst <- final.pf.abs
#   } else{
#     rst <- final.com.abs
#   }
#
#   return(rst)
# }
#
#
# # setwd("/Users/lingbotong/Desktop/Research/Ongoing/TextSEM/")
# #
# # ## Input as vectors
# # comment_test=c('I truly love this prof, but the exam is not easy.',
# #                'I hate the class.',
# #                'Do not take the course')
# # absa(comment_test)
# #
# # ## Input as dataframes
# # # columns: c("id, "profid", "cid", "comment")
# # cmt_20 <- read.csv('data/comments_20.csv')
# # absa(cmt_20)
# # absa(cmt_20, group_var = 'profid')
# # absa(cmt_20, group_var = 'something_not_exist')
# #
# # cmt_700 <- read.csv('data/comments_700.csv')
# # absa(cmt_700, group_var = 'profid')
#
# sem.text.absa <- function(model,
#                      data,
#                      text_var,
#                      text_stats=c('OverallSenti'),
#                      polarity_dt = lexicon::hash_sentiment_jockers_rinker,
#                      valence_shifters_dt = lexicon::hash_valence_shifters,
#                      missing = 'ML',
#                      fixed.x = FALSE,
#                      ...){
#
#   ## parse the model
#   model_info <- lavParseModelString(model)
#   model_var <- unique(c(model_info$lhs, model_info$rhs))
#
#   ## get the list of text variables in the model
#   text_var <- text_var[text_var %in% model_var]
#
#   N <- length(text_var) # Number of text variables
#   if (N > 0){
#     ## now get the sentiment score of the text
#     text_score <- list()
#     for(i in 1:N){
#       temp <- sentiment_by(text.var=data[, text_var[i]], ...) # Compute sentiment scores
#       text_score[[i]] <- temp$ave_sentiment # Select variables
#     }
#   names(text_score) <- text_var
#   # print(text_score)
#
#   data_new <- data
#   for(i in 1:N){
#     data_new <- cbind(data_new, text_score[[i]])
#   }
#   names(data_new) <- c(names(data), paste0(rep(text_var, each = length(text_stats)), '.', text_stats))
#
#   model_lavaanify <- lavaanify(model)
#   model_user <- model_lavaanify[model_lavaanify$user==1, ]
#
#   model_new <- c()
#   for(i in 1:nrow(model_user)){
#     row <- model_user[i,]
#     # print(row)
#     if((row['lhs'] %in% text_var) && (row['rhs'] %in% text_var)){
#       model_new <- c(model_new, paste0(rep(paste0(row['lhs'], '.', text_stats), each = length(text_stats)),
#                                        ' ', row['op'], ' ', rep(paste0(row['rhs'], '.', text_stats), length(text_stats))))
#     } else if(row['lhs'] %in% text_var){
#       model_new <- c(model_new, paste0(row['lhs'], '.', text_stats, ' ', row['op'], ' ', row['rhs']))
#     } else if(row['rhs'] %in% text_var){
#       model_new <- c(model_new, paste0(row['lhs'], ' ', row['op'], ' ', row['rhs'], '.', text_stats))
#     } else{
#       model_new <- c(model_new, paste0(row['lhs'],  ' ', row['op'], ' ', row['rhs']))
#     }
#   }
#   # print(model_new)
#   # model_new <- paste0(model_new, collapse = '\n')
# }
#   model_res <- sem(model=model_new, data=data_new,
#                    missing = missing, fixed.x = fixed.x, ...)
#
#   return(list(model=model_new, data=data_new, estimates=model_res))
# }
#
# # mat <- matrix(rnorm(500), nrow = 100)
# # dat <- as.data.frame(x = mat)
# #
# # cmt_700 <- read.csv('data/comments_700.csv')
# # cmt_100a <- cmt_700[1:100,]
# # cmt_100b <- cmt_700[101:200,]
# # row.names(cmt_100a) <- NULL
# # row.names(cmt_100b) <- NULL
# # text <- list(cmt_100a = cmt_100a, cmt_100b = cmt_100b)
# #
# # model <- ' Y =~ V1 + V2
# #            X =~ V3 + V4 + cmt_100b
# #            Y ~ X
# #            cmt_100a ~ X
# #            Y ~ cmt_100a
# #         '
# #
# # model <- ' Y =~ V1 + V2
# #            X =~ V3 + V4 + cmt_100b
# #            Y ~ X
# #            cmt_100a ~ cmt_100b
# #            Y ~ cmt_100a
# #         '
# #
# # res <- sem.text(model = model, data = dat, text = text, text_stats=c('A1', 'A2', 'A3'))
# # summary(res$estimates, fit=TRUE)
