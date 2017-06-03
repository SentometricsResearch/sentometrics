
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

# extrafont::loadfonts(device = "win", quiet = TRUE)

load("sentimentShiny.rda") # out

function(input, output) {
  selData <- reactive({
    data <- out # has all columns required for subsetting (lexicon, language and keywords)
    names(data) <- gsub(" ", "_", names(data)) # get rid of white spaces in keywords
    
    validate(
      need((length(input$language) > 0 & length(input$lexicon) > 0),
           paste0("Select at least one input language, one input lexicon and one input topic.",
                  "\n \n",
                  "If none of the desks is selected, all desks are considered."))
    )
    
    data <- dplyr::filter(data, keyword %in% input$keywords) # selected topic
    
    if(length(input$desk) > 0) {
      if(length(input$language) == 2) {
        desks <- unlist(strsplit(input$desk, " "))
      } else {
        if(input$language == "nl") desks <- unlist(strsplit(input$desk, " "))[seq(1, length(input$desk) * 2, by = 2)]
        else desks <- unlist(strsplit(input$desk, " "))[seq(1, length(input$desk) * 2, by = 2) + 1]
      }
      data <- dplyr::filter(data, desk %in% desks)
    }
    
    data <- dplyr::filter(data, word_count >= input$minWords)
    
    if(input$aggregation == "Sum") fun <- sum
    else fun <- mean
    
    data <- data.table(data)[, list(net_sent = fun(net_sent, na.rm = TRUE), # average net sentiment score
                                    words = sum(word_count), # total number of words
                                    documents = sum(doc_counter)), # total number of documents
                             by = list(date, lexicon, language)] # aggregate over date based on selected keywords
    
    data <- data[grepl(paste0(input$language, collapse = "|"), data$language), ] # selection based on language input
    data <- data[grepl(paste0(input$lexicon, collapse = "|"), data$lexicon), ] # selection based on lexicons input
    data <- data[data$date >= input$dates[1] & data$date <= input$dates[2], ] # selection based on time frame input
    
    data$name <- paste0(data$lexicon, " (", data$language, ")  ")
  
    data <- dplyr::filter(data, documents >= input$minDocs)
    
    if(nrow(data) > 0) {
      nvals <- data.table(data)[, list(n = length(net_sent)),
                                by = list(lexicon, language)]$n
    } else {
      nvals <- 0 # in case data is empty
    }
    
    nSMA <- 7
    if(any(nvals <= nSMA)) { # if at least one of the series has nSMA or less data points, inform users
      valid <- FALSE
    } else {
      valid <- TRUE

      if(input$normalised) {
        norm <- data.table(data)[, list(vals = (net_sent - mean(net_sent)) / sd(net_sent)), # normalisation within selection
                                 by = list(lexicon, language)]
        data$net_sent <- norm$vals
      }

      sma <- data.table(data)[, list(vals = SMA(net_sent, n = nSMA)), # simple moving average (weekly)
                            by = list(lexicon, language)]
      data$sma <- sma$vals
    }

    return(list(data = data, valid = valid))
  })

  output$nDocs <- renderUI({
    validate(
      need((length(input$language) > 0 & length(input$lexicon) > 0),
           "")
    )
    
    selData <- selData()
    data <- selData$data
    
    docs <- dplyr::summarise(group_by(data, name),
                              n = sum(documents))

    validate(
      need(selData$valid == TRUE,
           paste0(""))
    )
    
    HTML(paste0("Number of documents: ",
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
    
    selData <- selData()
    data <- selData$data

    validate(
      need(selData$valid == TRUE,
           paste0(""))
    )
    
    words <- dplyr::summarise(group_by(data, name),
                              n = round(mean(words / documents), 0))
    
    HTML(paste0("Average number of words per document: ",
                "<ul>",
                paste0("<li>",
                       paste0(unique(words$n), " ", 
                              strsplit(paste0("(", input$language, collapse = " ", ")"), split = " ")[[1]],
                              collapse = "</li><li>"),
                       "</li"),
                "</ul>"))
  })
  
  output$stats <- renderTable({
    validate(
      need((length(input$language) > 0 & length(input$lexicon) > 0),
           "Select language, lexicon and topic.")
    )
    
    selData <- selData()
    data <- selData$data
    
    validate(
      need(selData$valid == TRUE,
           paste0("Not enough data points.", "\n \n"))
    )
    
    stats <- dplyr::summarise(group_by(data, name),
                              sentiment = mean(net_sent))
    names(stats) <- c("Selection", "Average Sentiment Score")
    
    return(stats)
  })

  output$plot <- renderPlot({
    timeGap <- input$dates[2] - input$dates[1]

    validate(
      need(timeGap > 0,
           "Select an appropriate time interval.")
    )

    selData <- selData()

    validate(
      need(selData$valid == TRUE,
          paste0("Too few data points to allow for a decent sentiment index. ",
                 "\n \n",
                 "Try out a different combination of parameters."))
    )

    data <- selData$data

    p <- ggplot(data = data,
                aes(x = date, y = net_sent, color = name)) +
      geom_point() +
      # geom_line() +
      geom_line(aes(y = sma), size = 1.25) +
      geom_hline(yintercept = 0, size = 0.70, linetype = "dotted") + 
      # geom_smooth(method = "auto", se = TRUE, level = 0.30) +
      scale_x_date(name = "Date",
                   labels = ifelse(timeGap > 90, date_format("%m-%Y"), date_format("%d-%m-%y")),
                   date_breaks = ifelse(timeGap > 90, "1 month", "1 week")) +
      scale_y_continuous(name="Sentiment Score") +
      theme_tufte(ticks = TRUE) +
      theme(legend.title = element_blank(),
            legend.position = "top",
            text = element_text(family = "Georgia", size = 14),
            plot.margin = unit(c(0, 2.5, 0, 7.5), "mm"), # top, right, bottom, left
            axis.text.x = element_text(angle = 60, hjust = 1),
            axis.ticks = element_blank())

    return(p)
  })

  output$hoverInfo <- renderUI({
    validate(
      need((length(input$language) > 0 & length(input$lexicon) > 0),
           "")
    )

    hover <- input$plot_hover
    selData <- selData()
    data <- selData$data

    validate(
      need(selData$valid == TRUE,
           "")
    )

    point <- nearPoints(data, hover, threshold = 5, maxpoints = 1, addDist = TRUE)
    if (nrow(point) == 0) return(NULL)

    # calculate distance from left and bottom side of the picture, in pixels based on position inside image
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)

    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)

    # create style properties fot tooltip
    style <- paste0("position: absolute; z-index: 100; ", 
                    "background-color: rgba(240, 240, 240, 0.90); ",
                    "left:", left_px + 0.5, "px; top:", top_px + 0.5, "px;")

    # actual tooltip created as a wellPanel
    wellPanel(
      style = style,
      p(HTML(paste0("<b> Date: </b>", point$date, "<br/>",
                    "<b> Sentiment (mean): </b>", round(point$net_sent, 2), "<br/>",
                    "<b> Documents (total): </b>", point$documents, "<br/>",
                    "<b> Words (mean): </b>", floor(point$words/point$documents), "<br/>")))
    )
  })
  
  output$methodology <- renderUI({
    
    HTML(paste0("The methodology to obtain the displayed textual sentiment time series
                can be explained in brief by below series of steps:",
                "<ol>",
                "<li> Assemble a selection of texts (a corpus) </li>",
                "<li> Select the corpus texts of interest" ,
                "<ul> <li> Based on keyword tag (e.g. 'VOETBAL' or 'FOOTBALL') </li> 
                      <li> Based on keyword occurrence in text (e.g. all texts in which 'Trump' appears) </li> 
                      <li> Based on category tag (e.g. 'POL' for politics) </li> 
                      <li> Based on language tag </li> </ul> </li>",
                "<li> Match all words in each text to word lists (lexicons) </li>",
                "<li> Assign a sentiment score per text </li>",
                "<li> Aggregate sentiment scores per text for every date </li>",
                "</ol>",
                "<br>",
                "<p> A lexicon is a set of words with an associated polarity. There circulate
                several lexicons in text mining research, primarily in English. Most often, these
                lexicons are general in nature, but some are domain-specific. In this setting, we 
                have used an English financial lexicon, translated to both French and English, a
                general French lexicon, and a general English lexicon, translated to Dutch. </p>", 
                "<br>",  
                "<p> Sentiment for a given text is calculated as a weighted (by number of words) sum of 
                sentiment per sentence. The latter is calculated by summing up the polarity of the
                words that match a word in the lexicon, with the potential impact of surrounding words
                near the polarized word accounted for. As a simple example, 'not bad' would
                have an initial sentiment value of -1 due to the word 'bad', but this is eventually
                reversed because of the presence of the word 'not'. Each text thus has a sentiment score,
                and sentiment on a single date is composed by either summing up or averaging sentiment
                from all texts on that day. </p>"))
    
  })
  
  output$spotted <- renderUI({
    
    HTML(paste0("..."))
    
  })
  
  output$toDo <- renderUI({
    
    HTML(paste0("..."))
    
  })
  
}

