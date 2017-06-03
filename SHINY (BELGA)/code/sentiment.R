
#### sentimentr package (customized by Keven) ####

sentiment_custom <- function(text.var, polarity_dt = lexicon::hash_sentiment_jockers,
                      valence_shifters_dt = lexicon::hash_valence_shifters, hyphen = "",
                      amplifier.weight = .8, n.before = 5, n.after = 2, question.weight = 1,
                      adversative.weight = .85, missing_value = 0, ...){
  
  sentences <- id2 <- pol_loc <- comma_loc <- P <- non_pol <- lens <-
    cluster_tag <- w_neg <- neg <- A <- a <- D <- d <- wc <- id <-
    T_sum <- N <- . <- b <- before <- NULL
  
  ## check to make sure valence_shifters_dt polarity_dt are mutually exclusive
  if(any(valence_shifters_dt[[1]] %in% polarity_dt[[1]])) {
    stop('`polarity_dt` & `valence_shifters_dt` not mutually exclusive')
  }
  
  ## Add "~~" holder for any words `polarity_frame` & `valence_shifters_dt`
  ## that have spaces
  posneg <- polarity_dt[[1]]
  words <- c(posneg, valence_shifters_dt[[1]])
  space_words <-  words[grep("\\s", words)]
  
  # break rows into sentences, count words
  # space fill (~~), break into words
  sents <- get_sents(gsub("(\\s*)([;:,]+)", " \\2", text.var))
  sent_dat <- make_sentence_df2(sents)
  sent_dat[, 'words' := list(make_words(space_fill(sentences, space_words), hyphen = hyphen))]
  
  # make sentence id for each row id
  sent_dat[, id2:=seq_len(.N), by='id']
  
  ## Make the data frame long by stretching out words in sentences
  word_dat <- sent_dat[, .(words = unlist(words)), by = c('id', 'id2')]
  
  ## 1. add polarity word potential locations (seq along) and the
  ##    value for polarized word
  ## 2. add comma locations
  word_dat[, pol_loc:=seq_len(.N), by=c('id', 'id2')]
  word_dat[, comma_loc:=pol_loc]
  word_dat[, "P_W"] <- polarity_dt[word_dat[["words"]]][[1]]
  word_dat[, "P"] <- polarity_dt[word_dat[["words"]]][[2]]
  word_dat[, pol_loc:=ifelse(is.na(P), NA, pol_loc)]
  word_dat[, comma_loc:=ifelse(words %in% c(";", ":", ","), comma_loc, NA)]
  
  ## Get position of polarized word (hits = pol_loc)
  ## Get length of words vect
  word_dat_tmp <- suppressWarnings(word_dat[, .(words, pol_loc = unlist(pol_loc),
                                                comma_loc = unlist(comma_loc), P = unlist(P),
                                                lens = sapply(words, length)), by = c('id', 'id2')])
  word_dat2 = word_dat_tmp[!is.na(word_dat$pol_loc) ,]
  word_sel =  word_dat2[, .(words=words,pol = P), by = c('id', 'id2')]
  
  word_dat <- word_dat[, .(words=list(words),words=list(words), pol_loc=list(rm_na(pol_loc)),
                           comma_loc=list(rm_na(comma_loc)), P= list(rm_na(P))), by = c('id', 'id2')]
  
  ## stretch by prior polarized word hits
  word_dat <- suppressWarnings(word_dat[, .(words, pol_loc = unlist(pol_loc),
                                            comma_loc = unlist(comma_loc), P = unlist(P),
                                            lens = sapply(words, length)), by = c('id', 'id2')])

  ## Grab the cluster of non-polarity words (n.before/n.after taking into account [,;:]
  cols2 <- c('id', 'id2', 'pol_loc', 'P')
  word_dat <- word_dat[, non_pol :=  list(comma_reducer(words, comma_loc, pol_loc, lens, n.before, n.after))][,
                                                                                                              list(words, non_pol, lens = sapply(words, length)), by = cols2]
  
  ## save just polarized data for later merge
  pol_dat <- word_dat[, c("id", "id2", "pol_loc", "P","words"), with=FALSE]
  
  ## grab just desired columns needed for valence shifters and stretch by words
  word_dat <- word_dat[, .(non_pol = unlist(non_pol)), by = c("id", "id2", "pol_loc")]
  
  ## tag nonpol cluster as negator (1) , amplifier (2), or deamplifier (3)
  word_dat[, "cluster_tag"] <- valence_shifters_dt[word_dat[["non_pol"]]][[2]]
  word_dat[, before := 1 - 2*cumsum(non_pol == "*"), .(id, id2, pol_loc)]
  
  but_dat <- word_dat[cluster_tag == "4", list(
    b =  before*sum2(cluster_tag %in% "4")),
    by = c("id", "id2", "pol_loc", "before")][, before := NULL][,
                                                                list(b = 1 + adversative.weight*sum2(b)), by = c("id", "id2", "pol_loc")]
  
  ## Get counts of negators (neg), amplifiers (a), and deamplifiers (d)
  ## neg is changed to a simple 0/1 if it flips the sign or not
  word_dat <- word_dat[, list(
    neg =  sum2(cluster_tag %in% "1"),
    a =  sum2(cluster_tag %in% "2"),
    d =  sum2(cluster_tag %in% "3")), by = c("id", "id2", "pol_loc")]
  
  word_dat <- merge(word_dat, but_dat, by = c("id", "id2", "pol_loc"), all.x = TRUE)
  
  ## calculate the overall +/- shift of the poalrized word by summing the negators
  word_dat[, w_neg := neg %% 2]
  
  ## merge original word counts, polarized word scores, & valence shifter scores
  sent_dat <- merge(merge(pol_dat, sent_dat[,  c("id", "id2", "wc"), with=FALSE],
                          by = c("id", "id2")),	word_dat, by = c("id", "id2", "pol_loc"))
  
  ## add in the adversative weights
  sent_dat[, a := a + ifelse(!is.na(b) & b > 1, b, 0)]
  sent_dat[, d := d + ifelse(!is.na(b) & b < 1, b, 0)]
 
  ## add the amplifier/deamplifier & total raw sentiment scores
  sent_dat[, A :=  ((1  - w_neg) * a)* amplifier.weight][,
                                                         D := ((-w_neg)*a - d) * amplifier.weight][, D := ifelse(D < -1, -1, D)][,
                                                                                                                                 T := (1 + c(A + D))*(P*((-1)^(2 + w_neg)))]
  
  sent_dat = sent_dat[!duplicated(sent_dat[,1:3]),]
  ## Aggregate (sum) at the sentence level
  sent_dat <- sent_dat[, list(T_sum=sum(T), N = unique(wc)), by=list(id, id2)]
  word_sel =  word_sel[, list(words_sel=paste0(words," : ", pol)), by=list(id, id2)]
  word_sel =  word_sel[, .(words_sel = list(words_sel)),by=list(id, id2)]
  ## Finish by dividing sentiment raw score by sqrt of word count
  sent_dat[, sentiment := T_sum][, sentiment := ifelse(is.na(sentiment) & N > 0, 0, sentiment)]
  sent_dat = join(sent_dat, y = word_sel, by=c("id", "id2"))
  ## weight questions if weight not set to 1
  if (question.weight != 1) {
    q_locs <- stringi::stri_detect_regex(unlist(sents), "\\?\\s*$")
    q_locs[is.na(q_locs)] <- FALSE
    sent_dat[q_locs, "sentiment" := sentiment*question.weight]
  }
  
  # By sentence
  out <- stats::setNames(sent_dat[, c("id", "id2", "N", "sentiment","words_sel"), with = FALSE],
                         c("element_id", "sentence_id", "word_count", "sentiment","words"))
  
  if (!is.null(missing_value)){
    out[, 'sentiment' := replace_na(sentiment, y = missing_value)]
  }
  
  class(out) <- unique(c("sentiment", class(out)))
  sentences <- new.env(FALSE)
  sentences[["sentences"]] <- sents
  attributes(out)[["sentences"]] <- sentences
  out[]
}

