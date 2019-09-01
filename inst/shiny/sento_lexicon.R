
build_sento_lexicon <- function(input, output, session, params) {

  sentoLexicon <- reactive({
    lexiconList <- params$lexiconList
    selectedLexicons <- params$selectedLexicons
    useValence <- params$useValence
    selectedValence <- params$selectedValence
    valenceMethod <- params$valenceMethod
    valenceList <- params$valenceList

    if (!is.null(selectedLexicons)) {
      lexiconsIn <- c(lexiconList[selectedLexicons])
    } else {
      return(NULL)
    }
    if (useValence && !is.null(selectedValence)) {
      valenceShiftersIn <- as.data.table(valenceList[[selectedValence]])
      if (valenceMethod == "Bigram") {
        valenceShiftersIn <- valenceShiftersIn[, .(x, y)]
      } else {
        valenceShiftersIn <- valenceShiftersIn[, .(x, t)]
      }
    } else {
      valenceShiftersIn <- NULL
    }
    if (!is.null(lexiconsIn)) {
      lex <- sento_lexicons(lexiconsIn = lexiconsIn, valenceIn = valenceShiftersIn)
    } else {
      lex <- NULL
    }
    lex
  })

}

