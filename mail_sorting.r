library(tm)
library(ggplot2)
library(Brobdingnag)

data_path = 'dev/r9labs/r/r-data/spam_assassin/'
easyham_path = paste(data_path, 'easy_ham/', sep="")
easyham_docs = dir(easyham_path)
easyham2_path = paste(data_path, 'easy_ham_2/', sep="")
easyham2_docs = dir(easyham2_path)
hardham_path = paste(data_path, 'hard_ham/', sep="")
hardham_docs = dir(hardham_path)
hardham2_path = paste(data_path, 'hard_ham_2/', sep="")
hardham2_docs = dir(hardham2_path)

date_pattern1 = "%a, %d %b %Y %H:%M:%S"
date_pattern2 = "%d %b %Y %H:%M:%S"

read_message = function (path) {  
  con = file(path, open="rt", encoding="latin1")
  text = readLines(con)
  close(con)
  return(text)
}

get_message = function (message_vec) {
  message = message_vec[seq(which(message_vec == "")[1] + 1, length (message_vec), 1)]
  return (paste (message, collapse="\n"))
}

get_from = function (message) {
  from = message[grepl('From:', message)]
  from = strsplit(from, '[":<>]')[[1]]
  from = from[which(from != "" & from != " ")]
  return (from[grepl("@", from)][1])
}

get_subject = function (message_vec) {
  subject = message_vec[grepl("Subject: ", message_vec)]
  if (length(subject) > 0) {
    return (strsplit (subject, "Subject: ")[[1]][2])
  }else{
    return ("")
  }
}

get_date = function (message_vec) {
  date_grep = grepl ("^Date: ", message_vec)
  date_grepl = which (date_grep == TRUE)
  date = message_vec[date_grepl]
  date = strsplit(date, "\\+|\\-|: ")[[1]][2]
  date = gsub("^\\s+|\\s+$", "", date)
  return (strtrim(date, 25))
}

date_converter = function (dates, pattern1, pattern2) {
  pattern1_convert = strptime (dates, pattern1)
  pattern2_convert = strptime (dates, pattern2)
  pattern1_convert[is.na(pattern1_convert)] = pattern2_convert[is.na(pattern1_convert)]
  return (pattern1_convert)
}

parse_email = function (path) {
  full_msg = read_message (path)
  date = get_date (full_msg)
  from = get_from (full_msg)
  subject = get_subject (full_msg)
  msg = get_message (full_msg)
  return (c(date, from, subject, msg, path))
}

easyham_parse = lapply (easyham_docs, function (p) { parse_email (paste (easyham_path, p, sep=""))})
# ham_tdm = get_tdm (all_ham)
# ham_matrix = as.matrix (ham_tdm)
# ham_counts = rowSums (ham_matrix)
# ham_df = data.frame (cbind (names(ham_counts), as.numeric(ham_counts)), stringsAsFactors=FALSE)
# names(ham_df) = c('term', 'frequency')
# ham_df$frequency = as.numeric(ham_df$frequency)
# ham_occurrence = sapply(1:nrow(ham_matrix), function (i) {length(which(ham_matrix[i,]>0))/ncol(ham_matrix)})
# ham_density = ham_df$frequency/sum(ham_df$frequency)
# ham_df = transform(ham_df, density = ham_density, occurrence = ham_occurrence)

easyham_parse_matrix = do.call (rbind, easyham_parse)
allparse_df = data.frame (easyham_parse_matrix, stringsAsFactors=FALSE)
names(allparse_df) = c("Date", "From_Email", "Subject", "Message", "Path")
allparse_df$Date = date_converter (allparse_df$Date, date_pattern1, date_pattern2)
allparse_df$From_Email = tolower(allparse_df$From_Email)
allparse_df$Subject = tolower(allparse_df$Subject)

priority_df = allparse_df[with(allparse_df, order(Date)),]
priority_train = priority_df[1:(round(nrow(priority_df)/2)),]