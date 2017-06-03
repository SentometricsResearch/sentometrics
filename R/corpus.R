
# overall depends: stringi, dplyr (@import), data.table (@import), xts, tidytext, tidyr

# INPUT:
### A dataframe with: id (unique), date, text, features (remaining columns) -- id to be determined if useful?

corpuS <- function(text_df, ...) { # constructor

  # clean data
  # order data (time)

  c <- quanteda::corpus(text_df,
                        docid_field = "id",
                        text_field = "text",
                        metacorpus = list(info = "This is a Sentometrics corpuS object directly based on the quanteda corpus"))

  c$features <- names(c$documents)[!(grepl(paste0(c("id", "date", "text"), collapse = "|"), names(c$documents)))]

  # simply add to quanteda corpus?

  class(c) <- c("corpuS", class(c))

  return(c)

}

