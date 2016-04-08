library(tm)
library(ggplot2)
library(Brobdingnag)

data_path = 'dev/r9labs/r/r-data/spam_assassin/'
spam_path = paste(data_path, 'spam/', sep="")
spam_docs = dir(spam_path)
spam2_path = paste(data_path, 'spam_2/', sep="")
spam2_docs = dir(spam2_path)
easyham_path = paste(data_path, 'easy_ham/', sep="")
easyham_docs = dir(easyham_path)
easyham2_path = paste(data_path, 'easy_ham_2/', sep="")
easyham2_docs = dir(easyham2_path)
hardham_path = paste(data_path, 'hard_ham/', sep="")
hardham_docs = dir(hardham_path)
hardham2_path = paste(data_path, 'hard_ham_2/', sep="")
hardham2_docs = dir(hardham2_path)
spam_probability = 0.2

get_message = function (path) {
  con = file(path, open="rt", encoding="latin1")
  text = readLines(con)
  msg = text[seq(which(text=="")[1]+1, length(text), 1)]
  close(con)
  return(paste(msg, collapse="\n"))
}

get_tdm = function (doc_vec) {
  doc_corpus = Corpus(VectorSource(doc_vec))
  control = list(stopwords=TRUE, removePunctuation=TRUE, removeNumbers=TRUE, minDocFreq=2)
  doc_dtm = TermDocumentMatrix(doc_corpus, control)
  return (doc_dtm)
}

classify_email = function (message_filename, training_df, prior = 0.5, c=0.000001) {
  message = get_message (message_filename)
  message_tdm = get_tdm (message)
  message_freq = rowSums(as.matrix(message_tdm))
  message_match = intersect (names(message_freq), training_df$term)
  if (length(message_match)<1){
    return (as.brob(prior) * as.brob(c)^(length(message_freq)))
  }else{
    match_probs = training_df$occurrence[match(message_match, training_df$term)]
    return(as.brob(prior) * prod(as.brob(match_probs)) * as.brob(c)^(length(message_freq)-length(message_match)))
  }
}

spam_classifier = function (path) {
  candidates = dir(path)
  pr_spam = sapply (candidates, function (p) {
    classify_email (paste(path, p, sep=""), spam_df, prior=spam_probability)
  })
  
  pr_ham = sapply (candidates, function (p) {
    classify_email (paste(path, p, sep=""), ham_df, prior=(1-spam_probability))
  })
  results = c()
  for (name in names(pr_spam)) {
    results[name] = ifelse(pr_spam[[name]] > pr_ham[[name]], TRUE, FALSE)
  }
  # return (c(pr_spam, pr_ham, results))
  return (results)
}

all_spam = sapply (spam_docs, function (p) { get_message (paste(spam_path, p, sep=""))} )
all_spam = append (all_spam, sapply (spam2_docs, function (p) { get_message (paste (spam2_path, p, sep=""))}))
spam_tdm = get_tdm (all_spam)
spam_matrix = as.matrix (spam_tdm)
spam_counts = rowSums (spam_matrix)
spam_df = data.frame (cbind (names(spam_counts), as.numeric(spam_counts)), stringsAsFactors=FALSE)
names(spam_df) = c('term', 'frequency')
spam_df$frequency = as.numeric(spam_df$frequency)
spam_occurrence = sapply(1:nrow(spam_matrix), function (i) {length(which(spam_matrix[i,]>0))/ncol(spam_matrix)})
spam_density = spam_df$frequency/sum(spam_df$frequency)
spam_df = transform(spam_df, density = spam_density, occurrence = spam_occurrence)

all_ham = sapply (easyham_docs, function (p) { get_message (paste(easyham_path, p, sep=""))} )
all_ham = append (all_ham, sapply(easyham2_docs, function (p) { get_message (paste(easyham2_path, p, sep=""))}))
ham_tdm = get_tdm (all_ham)
ham_matrix = as.matrix (ham_tdm)
ham_counts = rowSums (ham_matrix)
ham_df = data.frame (cbind (names(ham_counts), as.numeric(ham_counts)), stringsAsFactors=FALSE)
names(ham_df) = c('term', 'frequency')
ham_df$frequency = as.numeric(ham_df$frequency)
ham_occurrence = sapply(1:nrow(ham_matrix), function (i) {length(which(ham_matrix[i,]>0))/ncol(ham_matrix)})
ham_density = ham_df$frequency/sum(ham_df$frequency)
ham_df = transform(ham_df, density = ham_density, occurrence = ham_occurrence)

hardham_res = spam_classifier (hardham_path)
summary(hardham_res)