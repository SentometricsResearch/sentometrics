
library(shiny)
library(ggplot2)
library(ggthemes)
library(data.table)
library(shinythemes)
library(extrafont)
library(dplyr)
library(scales)
library(TTR)
library(grDevices)
library(grid)
library(gridExtra)

# extrafont::loadfonts(device = "win", quiet = TRUE)

### TODO: selection via data.table, not dplyr

load("sentimentShinyAll.rda") # out

function(input, output) {
  computeData <- reactive({
    data <- out # has all columns required for subsetting (lexicon, language and keywords)
    # names(data) <- gsub(" ", "_", names(data)) # get rid of white spaces in keywords

    validate(
      need((length(input$language) > 0 & length(input$lexicon) > 0),
           paste0("Select at least one input language, one input lexicon and one input topic.",
                  "\n \n",
                  "If none of the desks is selected, all desks are considered."))
    )

    data <- dplyr::filter(data, keyword %in% input$keywords) # selected topic

    if (length(input$desk) > 0) {
      if (length(input$language) == 2) {
        desks <- unlist(strsplit(input$desk, " "))
      } else {
        if (input$language == "nl") desks <- unlist(strsplit(input$desk, " "))[seq(1, length(input$desk) * 2, by = 2)]
        else desks <- unlist(strsplit(input$desk, " "))[seq(1, length(input$desk) * 2, by = 2) + 1]
      }
      data <- dplyr::filter(data, desk %in% desks)
    }

    data <- dplyr::filter(data, word_count >= input$minWords)

    if (input$aggregation == "Sum") fun <- sum
    else fun <- mean

    data <- as.data.table(data)[, list(net_sent = fun(net_sent, na.rm = TRUE), # average net sentiment score
                                    words = sum(word_count), # total number of words
                                    documents = sum(doc_counter)), # total number of documents
                             by = list(date, lexicon, language)] # aggregate over date based on selected keywords

    data <- data[grepl(paste0(input$language, collapse = "|"), data$language), ] # selection based on language input
    data <- data[grepl(paste0(input$lexicon, collapse = "|"), data$lexicon), ] # selection based on lexicons input
    data <- data[data$date >= input$dates[1] & data$date <= input$dates[2], ] # selection based on time frame input
    data <- dplyr::filter(data, documents >= input$minDocs)

    data <- as.data.table(data)
    data <- data[order(lexicon, language, date)]
    data[, "name" := paste0(data$lexicon, " (", data$language, ")")]

    if (nrow(data) > 0) {
      nvals <- data[, list(n = length(net_sent)),
                    by = list(lexicon, language)]$n

      # fill in missing dates with 0 for net_sent, words and documents
      dataNew <- c()
      for (nm in unique(data$name)) {
        dt <- subset(data, name == nm)

        st <- as.POSIXct(min(input$dates2[1]), format = "%Y-%m-%d")
        end <- as.POSIXct(max(input$dates2[2]), format = "%Y-%m-%d")
        date <- as.Date(seq.POSIXt(st, end, by = "day"))

        fill <- dt[1, 2:ncol(dt)]
        fill$net_sent <- 0
        fill$words <- fill$documents <- NA

        oldIn <- date %in% dt$date

        dtNew <- merge(as.data.table(date), dt, by = "date", all = TRUE)
        dtNew[!oldIn, 2:ncol(dtNew)] <- fill

        dataNew <- rbind(dataNew, dtNew)
      }
      data <- dataNew

    } else {
      nvals <- 0 # in case data is empty
    }

    if (any(nvals < 10)) { # if at least one of the series has less than 50 data points, no plot is shown
      valid <- FALSE
    } else {
      valid <- TRUE

      if (input$normalised) {
        # normalisation within selection
        norm <- data[, list(vals = (net_sent - mean(net_sent, na.rm = TRUE)) / sd(net_sent, na.rm = TRUE)),
                     by = list(lexicon, language)]
        data[, "net_sent" := norm$vals]
      }

      # data <- data[order(lexicon, language, date)]
      nSMA <- as.integer(input$nSMA)
      sma <- data[, list(vals = SMA(net_sent, n = nSMA)),
                  by = list(lexicon, language)]
      data[, "sma" := sma$vals]
    }

    return(list(data = data, valid = valid))
  })

  computeDataFull <- reactive({
    data <- out

    data <- dplyr::filter(data, keyword %in% input$keywords) # selected topic
    data <- dplyr::filter(data, word_count >= input$minWords2)

    if (input$aggregation2 == "Sum") fun <- sum
    else fun <- mean

    data <- as.data.table(data)[, list(net_sent = fun(net_sent, na.rm = TRUE), # average net sentiment score
                                       words = sum(word_count), # total number of words
                                       documents = sum(doc_counter)), # total number of documents
                                by = list(date, lexicon, language)] # aggregate over date based on selected keywords

    data <- data[data$date >= input$dates2[1] & data$date <= input$dates2[2], ] # selection based on time frame input

    data <- as.data.table(data)
    data <- data[order(lexicon, language)]
    # dates <- data$date
    data[, "name" := paste0(data$lexicon, " (", data$language, ")")]

    data <- as.data.table(dplyr::filter(data, documents >= input$minDocs2))

    if (nrow(data) > 0) {
      docs <- dplyr::summarise(group_by(data, name), n = sum(documents, na.rm = TRUE))
      words <- dplyr::summarise(group_by(data, name), n = round(mean(words / documents, na.rm = TRUE), 0))
    }

    # add columns for language and lexicon based on weight (two user inputs)
    weightsLex <- rep(input$wLex / 100, nrow(data)) # General
    weightsLex[data$lexicon == "Financial"] <- 1 - (input$wLex / 100)

    weightsLang <- rep(input$wLan / 100, nrow(data)) # fr
    weightsLang[data$language == "nl"] <- 1 - (input$wLan / 100)

    data[, "weightsLex" := weightsLex]
    data[, "weightsLang" := weightsLang]

    # take the weighted sum sentiment per date to obtain fully aggregated sentiment time series
    data <- data[, list(net_sent_full = sum(net_sent * weightsLex * weightsLang, na.rm = TRUE),
                        documents = sum(documents, na.rm = TRUE) / 2,
                        words = sum(words, na.rm = TRUE) / 2),
                 by = list(date)][order(date)]

    # data <- dplyr::filter(data, documents >= input$minDocs2)

    nvals <- length(data$date)
    if (nvals >= 10) { # if the series has less than 10 data points, no plot is shown
      valid <- TRUE

      # fill in missing dates with 0 for net_sent, words and documents
      st <- as.POSIXct(min(input$dates2[1]), format = "%Y-%m-%d")
      end <- as.POSIXct(max(input$dates2[2]), format = "%Y-%m-%d")
      date <- as.Date(seq.POSIXt(st, end, by = "day"))

      fill <- data[1, 2:ncol(data)]
      fill$net_sent_full <- 0
      fill$words <- fill$documents <- NA

      oldIn <- date %in% data$date

      dataNew <- merge(as.data.table(date), data, by = "date", all = TRUE)
      dataNew[!oldIn, 2:ncol(dataNew)] <- fill

      data <- dataNew

      # normalize entire series if option for normalisation is TRUE
      if (input$normalised2) {
        norm <- (data$net_sent_full - mean(data$net_sent_full, na.rm = TRUE)) / sd(data$net_sent_full, na.rm = TRUE)
        data[, "net_sent_full" := norm]
      }

      nSMA <- as.integer(input$nSMA2)
      data[, "sma" := SMA(data$net_sent_full, n = nSMA)]

    } else {
      nvals <- docs <- words <- 0 # in case data is empty
      valid <- FALSE
    }

    return(list(data = data, valid = valid, docs = docs, words = words))
  })

  selData <- reactiveValues()
  selDataFull <- reactiveValues()

  output$GenFR <- renderTable({
    data <- out

    data <- dplyr::filter(data, keyword %in% input$keywords & language == "fr") # selected topic
    data <- dplyr::filter(data, word_count >= input$minWords3)
    data <- data[data$date >= input$dates3[1] & data$date <= input$dates3[2], ] # selection based on time frame input

    dataTmp <- as.data.table(data)
    dataTmp[, "net_sent_abs" := abs(dataTmp$net_sent)]
    topLeads <- dataTmp[order(-net_sent_abs)]

    topGen <- topLeads[lexicon == "General", c("net_sent", "date", "lead")][1:input$nLeads, ]
    topGen$date <- as.character(topGen$date)
    colnames(topGen) <- c("Score", "Date", "Article's Lead")

    return(topGen)
  })

  output$FinFR <- renderTable({
    data <- out

    data <- dplyr::filter(data, keyword %in% input$keywords & language == "fr") # selected topic
    data <- dplyr::filter(data, word_count >= input$minWords3)
    data <- data[data$date >= input$dates3[1] & data$date <= input$dates3[2], ] # selection based on time frame input

    dataTmp <- as.data.table(data)
    dataTmp[, "net_sent_abs" := abs(dataTmp$net_sent)]
    topLeads <- dataTmp[order(-net_sent_abs)]

    topFin <- topLeads[lexicon == "Financial", c("net_sent", "date", "lead")][1:input$nLeads, ]
    topFin$date <- as.character(topFin$date)
    colnames(topFin) <- c("Score", "Date", "Article's Lead")

    return(topFin)
  })

  output$GenNL <- renderTable({
    data <- out

    data <- dplyr::filter(data, keyword %in% input$keywords & language == "nl") # selected topic
    data <- dplyr::filter(data, word_count >= input$minWords3)
    data <- data[data$date >= input$dates3[1] & data$date <= input$dates3[2], ] # selection based on time frame input

    dataTmp <- as.data.table(data)
    dataTmp[, "net_sent_abs" := abs(dataTmp$net_sent)]
    topLeads <- dataTmp[order(-net_sent_abs)]

    topGen <- topLeads[lexicon == "General", c("net_sent", "date", "lead")][1:input$nLeads, ]
    topGen$date <- as.character(topGen$date)
    colnames(topGen) <- c("Score", "Date", "Article's Lead")

    return(topGen)
  })

  output$FinNL <- renderTable({
    data <- out

    data <- dplyr::filter(data, keyword %in% input$keywords & language == "nl") # selected topic
    data <- dplyr::filter(data, word_count >= input$minWords3)
    data <- data[data$date >= input$dates3[1] & data$date <= input$dates3[2], ] # selection based on time frame input

    dataTmp <- as.data.table(data)
    dataTmp[, "net_sent_abs" := abs(dataTmp$net_sent)]
    topLeads <- dataTmp[order(-net_sent_abs)]

    topFin <- topLeads[lexicon == "Financial", c("net_sent", "date", "lead")][1:input$nLeads, ]
    topFin$date <- as.character(topFin$date)
    colnames(topFin) <- c("Score", "Date", "Article's Lead")

    return(topFin)
  })

  output$nDocs <- renderUI({
    validate(
      need((length(input$language) > 0 & length(input$lexicon) > 0),
           "")
    )

    # run computeData() once and assign it to selData for later use
    getData <- computeData()
    selData$data <- getData$data
    selData$valid <- getData$valid

    validate(
      need(selData$valid == TRUE,
           paste0(""))
    )

    data <- selData$data

    docs <- dplyr::summarise(group_by(data, name), n = sum(documents, na.rm = TRUE))

    HTML(paste0("<b> Number of documents: </b>",
                "<ul>",
                paste0("<li>",
                       paste0(unique(docs$n), " ",
                                     strsplit(paste0("(", input$language, collapse = " ", ")"), split = " ")[[1]],
                              collapse = "</li><li>"),
                       "</li"),
                "</ul>"))
  })

  output$nWords <- renderUI({
    validate(
      need((length(input$language) > 0 & length(input$lexicon) > 0),
           "")
    )

    # selData <- selData()
    data <- selData$data

    validate(
      need(selData$valid == TRUE,
           paste0(""))
    )

    words <- dplyr::summarise(group_by(data, name),
                              n = round(mean(words / documents, na.rm = TRUE), 0))

    HTML(paste0("<b> Average words per document: </b>",
                "<ul>",
                paste0("<li>",
                       paste0(unique(words$n), " ",
                              strsplit(paste0("(", input$language, collapse = " ", ")"), split = " ")[[1]],
                              collapse = "</li><li>"),
                       "</li"),
                "</ul>"))
  })

  output$nDocsFull <- renderUI({

    # run computeDataFull() once and assign it to selDataFull for later use
    getData <- computeDataFull()
    selDataFull$data <- getData$data
    selDataFull$valid <- getData$valid
    selDataFull$docs <- getData$docs
    selDataFull$words <- getData$words

    validate(
      need(selDataFull$valid == TRUE,
           paste0(""))
    )

    docs <- selDataFull$docs

    HTML(paste0("<b> Number of documents: </b>",
                "<ul>",
                paste0("<li>",
                       paste0(unique(docs$n), " ",
                              strsplit(paste0("(", c("fr", "nl"), collapse = " ", ")"), split = " ")[[1]],
                              collapse = "</li><li>"),
                       "</li"),
                "</ul>"))
  })

  output$nWordsFull <- renderUI({
    # selData <- selDataFull()
    words <- selDataFull$words

    validate(
      need(selDataFull$valid == TRUE,
           paste0(""))
    )

    HTML(paste0("<b> Average words per document: </b>",
                "<ul>",
                paste0("<li>",
                       paste0(unique(words$n), " ",
                              strsplit(paste0("(", c("fr", "nl"), collapse = " ", ")"), split = " ")[[1]],
                              collapse = "</li><li>"),
                       "</li"),
                "</ul>"))
  })

  output$stats <- renderTable({
    validate(
      need((length(input$language) > 0 & length(input$lexicon) > 0),
           "Select language, lexicon and topic.")
    )

    # selData <- selData()
    data <- selData$data

    validate(
      need(selData$valid == TRUE,
           paste0("Not enough data points.", "\n \n"))
    )

    if (!as.logical(input$normalised)) {
      stats <- dplyr::summarise(group_by(data, name),
                              sentiment = mean(net_sent))
      names(stats) <- c("Selection", "Mean Sentiment")

    return(stats)
    }
  })

  output$meanSent <- renderUI({
    # selData <- selDataFull()
    data <- selDataFull$data

    validate(
      need(selDataFull$valid == TRUE,
           paste0("Not enough data points.", "\n \n"))
    )

    if (!as.logical(input$normalised2)) {
      HTML(paste0("<b> Mean Sentiment: </b> ",
                round(mean(data$net_sent_full, na.rm= TRUE), 4)),
           "<br> <br>")
    }

  })

  output$plot <- renderPlot({
    timeGap <- input$dates[2] - input$dates[1]

    validate(
      need(timeGap > 0,
           "Select an appropriate time interval.")
    )

    # selData <- selData()

    validate(
      need(selData$valid == TRUE,
          paste0("There are too few data points (less than 10 available days) for at least one of the intended sentiment indices.",
                 "\n \n",
                 "Try out a different combination of parameters."))
    )

    data <- selData$data

    p1 <- ggplot(data = data,
                aes(x = date, y = net_sent, color = name)) +
      {if (as.numeric(input$dots)) geom_point()} +
      # geom_line() +
      geom_line(aes(y = sma), size = 1.25) +
      geom_hline(yintercept = 0, size = 0.70, linetype = "dotted") +
      # geom_smooth(method = "auto", se = TRUE, level = 0.30) +
      scale_y_continuous(name = "Sentiment Score") +
      scale_x_date(name = "Date",
                   labels = ifelse(timeGap > 90, date_format("%m-%Y"), date_format("%d-%m-%y"))
                   # , date_breaks = ifelse(timeGap > 90, "1 month", "1 week")
                   ) +
      theme_tufte(ticks = TRUE) +
      theme(legend.title = element_blank(),
            legend.position = "top",
            text = element_text(size = 14),
            plot.margin = unit(c(0, 2.5, 0, 7.5), "mm"), # top, right, bottom, left
            axis.text.x = element_text(angle = 60, hjust = 1),
            axis.ticks = element_blank())

    data$documents[is.na(data$documents)] <- 0
    dataCol <- rbind(dplyr::filter(data[data$language == "nl", ], lexicon == unique(lexicon)[1]),
                     dplyr::filter(data[data$language == "fr", ], lexicon == unique(lexicon)[1]))

    p2 <- ggplot(data = dataCol,
                 aes(x = date, y = documents, color = language)) +
      geom_col(aes(fill = language)) +
      scale_y_continuous(name = "Documents") +
      scale_x_date(name = "") +
      theme_tufte(ticks = TRUE) +
      theme(legend.title = element_blank(),
            legend.position = "top",
            text = element_text(size = 14),
            plot.margin = unit(c(0, 2.5, 0, 7.5), "mm"), # top, right, bottom, left
            axis.text.x = element_blank(),
            axis.ticks = element_blank())

    gA <- ggplot_gtable(ggplot_build(p2))
    gB <- ggplot_gtable(ggplot_build(p1))
    maxWidth = grid::unit.pmax(gA$widths, gB$widths)
    gA$widths <- as.list(maxWidth)
    gB$widths <- as.list(maxWidth)
    grid.newpage()

    grid.arrange(
      arrangeGrob(gA, gB, nrow = 2, heights = c(.3, .7))
    )
  })

  output$plotFull <- renderPlot({
    timeGap <- input$dates2[2] - input$dates2[1]

    validate(
      need(timeGap > 0,
           "Select an appropriate time interval.")
    )

    # selData <- selDataFull()

    validate(
      need(selDataFull$valid == TRUE,
           paste0("There are too few data points (less than 10 available days) for the intended sentiment index.",
                  "\n \n",
                  "Try out a different combination of parameters."))
    )

    data <- selDataFull$data

    p1 <- ggplot(data = data,
                 aes(x = date, y = net_sent_full)) +
      {if (as.numeric(input$dots2)) geom_point()} +
      # geom_line() +
      geom_line(aes(y = sma), size = 1.25) +
      geom_hline(yintercept = 0, size = 0.70, linetype = "dotted") +
      # geom_smooth(method = "auto", se = TRUE, level = 0.30) +
      scale_y_continuous(name = "Sentiment Score") +
      scale_x_date(name = "Date",
                   labels = ifelse(timeGap > 90, date_format("%m-%Y"), date_format("%d-%m-%y"))
                   # , date_breaks = ifelse(timeGap > 90, "1 month", "1 week")
                   ) +
      theme_tufte(ticks = TRUE) +
      theme(legend.title = element_blank(),
            legend.position = "top",
            text = element_text(size = 14),
            plot.margin = unit(c(2.5, 2.5, 0, 7.5), "mm"), # top, right, bottom, left
            axis.text.x = element_text(angle = 60, hjust = 1),
            axis.ticks = element_blank())

    data$documents[is.na(data$documents)] <- 0
    data$col = "col"

    p2 <- ggplot(data = data,
                 aes(x = date, y = documents, color = col)) +
      geom_col(aes(fill = col)) +
      scale_y_continuous(name = "Documents") +
      scale_x_date(name = "") +
      theme_tufte(ticks = TRUE) +
      theme(legend.title = element_blank(),
            legend.position = "none",
            text = element_text(size = 14),
            plot.margin = unit(c(5, 2.5, 0, 7.5), "mm"), # top, right, bottom, left
            axis.text.x = element_blank(),
            axis.ticks = element_blank())

    gA <- ggplot_gtable(ggplot_build(p2))
    gB <- ggplot_gtable(ggplot_build(p1))
    maxWidth = grid::unit.pmax(gA$widths, gB$widths)
    gA$widths <- as.list(maxWidth)
    gB$widths <- as.list(maxWidth)
    grid.newpage()

    grid.arrange(
      arrangeGrob(gA, gB, nrow = 2, heights = c(.3, .7))
    )
  })

  output$methodology <- renderUI({

    HTML(paste0("<p> The methodology to obtain the displayed textual sentiment time series
                can be explained in brief by below series of steps:",
                "<ol>",
                "<li> Assemble a selection of texts (a corpus) </li>",
                "<li> Select the corpus texts of interest" ,
                "<ul> <li> Based on keyword tag (e.g. 'VOETBAL' or 'FOOTBALL') </li>
                      <li> Based on keyword occurrence in text (e.g. all texts in which 'CETA' appears) </li>
                      <li> Based on category tag (e.g. 'POL' for politics) </li>
                      <li> Based on language tag </li> </ul> </li>",
                "<li> Match all words in each text to word lists (lexicons) </li>",
                "<li> Assign a sentiment score per text </li>",
                "<li> Aggregate sentiment scores per text for every date </li>",
                "</ol>",
                "The topics have been selected based on expected fluctuation of news and associated sentiment, for example
                during the Brussels terrorist attacks in March 2016, or during Trump's election in November 2016. </p>",
                "<p> A lexicon is a set of words with an associated polarity. There circulate
                several lexicons in text mining research, primarily in English. Most often, these
                lexicons are general in nature, but some are domain-specific. In this setting, we
                have used an English financial lexicon, translated to both French and English, a
                general French lexicon, and a general English lexicon, translated to Dutch. </p>",
                "<p> Sentiment for a given text is calculated based on the bag-of-words model. Every text is decomposed
                into words and matched against a lexicon. The polarity scores of words present in every text are summed,
                weighted by the proportion of the particular polar words with respect to the total number of words per text.
                This results into a sentiment score for each text. Sentiment on a single date is composed by either summing
                up or averaging sentiment from all texts on that day. Extensions exist under the name of semantic parsing,
                textual analysis in which the potential impact of surrounding words
                near the polarized word is accounted for. This is the most accurate approach, yet computationally more demanding.
                As a simple example, 'not bad' would have an initial sentiment value of -1 due to the word 'bad', but this would
                eventually be reversed because of the presence of the word 'not'. </p>",
                "<p> The fully aggregated sentiment index combines all articles in both French and Dutch
                as well as the different sentiment scores per lexicon. The weight options allow to control
                for the importance of each language or lexicon. </p>"))

  })

  output$analysis <- renderUI({

    HTML(paste0("Constructing a time series is only the first step, extracting information from it is what counts. However,
                significant statistical analysis is required to get the most out of the observed evolution and the level of
                textual sentiment. Elements of this analysis are:",
                "<ul>",
                "<li> The disparity in number of texts is apparent. During and shortly after important events,
                the number of documents is clearly higher. How does the number of news articles spread out to the sentiment value?",
                "<li> It is very much of interest to connect a peak in sentiment to the articles which have propelled that peak. This
                is called attribution, and can be done at various levels. Another example is to link peaks to news sources (e.g. magazines),
                to detect whether some news source is driving sentiment more than others. </li>",
                "<li> How is textual sentiment from articles in one language correlated to articles in another language? Is sentiment
                expressed in different languages, or by journalists in different countries, divergent or rather similar? </li>",
                "<li> When and why is sentiment positive or negative? When and why is sentiment abnormally high or low? </li>",
                "</ul>",
                "The overall framework for textual sentiment calculation is still subject to several
                inefficiencies. Here below a list of the main aspects to be addressed to refine
                the quantification of sentiment from texts, and carry out the suggested statistical analyses: ",
                "<ul>",
                "<li> Translation of lexicons to different languages, such as Dutch and French. Additionally,
                the creation and enhancement of domain-specific lexicons. </li>",
                "<li> Robust detection of abnormal sentiment. </li>",
                "<li> Removal of fake news, and more generally, texts not related to a particular topic. </li>",
                "<li> Optimal aggregation weighting schemes for the computation of specific textual sentiment indices.
                Part of this includes improving the combination of different types of articles (in different languages,
                from different sources, in different writing styles, and alike). </li>",
                "</ul>"
                ))

  })

  output$export <- downloadHandler(
   filename = paste('text-sent-multiple-', Sys.Date(), '.csv', sep = ''),
   content = function(file) {
     # selData <- selData()
     data <- selData$data

     data$sma <- data$name <- data$words <- NULL

     write.csv2(data, file)
   },
   contentType = "text/csv"
  )

  output$exportFull <- downloadHandler(
    filename = paste('text-sent-single-', Sys.Date(), '.csv', sep = ''),
    content = function(file) {
      # selDataFull <- selDataFull()
      dataFull <- selDataFull$data

      dataFull$sma <- NULL

      write.csv2(dataFull, file)
    },
    contentType = "text/csv"
  )

  output$boudt <- renderImage({

    return(list(
      src = "images/boudt.jpg",
      filetype = "image/jpeg"
    ))

  }, deleteFile = FALSE)

  output$ardia <- renderImage({

    return(list(
      src = "images/ardia.jpg",
      filetype = "image/jpeg"
    ))

  }, deleteFile = FALSE)

  output$bluteau <- renderImage({

    return(list(
      src = "images/bluteau.jpg",
      filetype = "image/jpeg"
    ))

  }, deleteFile = FALSE)

  output$borms <- renderImage({

    return(list(
      src = "images/borms.jpg",
      filetype = "image/jpeg"
    ))

  }, deleteFile = FALSE)

  output$wuytack <- renderImage({

    return(list(
      src = "images/wuytack.jpg",
      filetype = "image/jpeg"
    ))

  }, deleteFile = FALSE)

  output$hartmann <- renderImage({

    return(list(
      src = "images/hartmann.jpg",
      filetype = "image/jpeg"
    ))

  }, deleteFile = FALSE)

  output$thewissen <- renderImage({

    return(list(
      src = "images/thewissen.jpg",
      filetype = "image/jpeg"
    ))

  }, deleteFile = FALSE)

  output$algaba <- renderImage({

    return(list(
      src = "images/algaba.jpg",
      filetype = "image/jpeg"
    ))

  }, deleteFile = FALSE)

  output$torsin <- renderImage({

    return(list(
      src = "images/torsin.jpg",
      filetype = "image/jpeg"
    ))

  }, deleteFile = FALSE)

  output$linkedinA <- renderImage({

    return(list(
      src = "images/linkedin.jpg",
      filetype = "image/jpeg"
    ))


  }, deleteFile = FALSE)

  output$linkedinB <- renderImage({

    return(list(
      src = "images/linkedin.jpg",
      filetype = "image/jpeg"
    ))


  }, deleteFile = FALSE)
  output$news4 <- renderUI({

    HTML(paste0("Swissuniversities funding for Sentometrics research of Keven Bluteau"),
         renderUI({
           a(" Link", href="https://www.linkedin.com/feed/update/urn:li:activity:6276875594202386432", target = "_blank")
           })
         )
  })
}

