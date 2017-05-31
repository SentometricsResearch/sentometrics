
library(shiny)
library(ggplot2)
library(ggthemes)
library(data.table)
library(shinythemes)
library(extrafont)
library(dplyr)
library(scales)
library(shinyjs)
library(TTR)
library(grDevices)

# extrafont::loadfonts(device = "win", quiet = TRUE)

load("sentimentPOL.rda") # out

function(input, output) {
  selData <- reactive({
    
    data <- rbindlist(out) # has all columns required for subsetting (lexicon, language and keywords)
    names(data) <- gsub(" ", "_", names(data)) # get rid of white spaces in keywords
      
    n_kw <- length(input$keywords)
    
    validate(
      need((length(input$language) > 0 & length(input$lexicon) > 0),
           "Select at least one input language and one input lexicon.")
    )
    
    if(n_kw >= 1) {
      shinyjs::disable("and_or") # if selection goes back from >= 2 keywords to 1 keywords
      if(n_kw >= 2) {
        shinyjs::enable("and_or")
      }
      choice <- input$and_or
      cond <- paste0("data$", paste0(input$keywords, collapse = paste0(" == TRUE ", choice, " data$")), " == TRUE")
      data <- filter(data, eval(parse(text = cond)))
      } else {
        shinyjs::disable("and_or")
    }
    
    data <- data.table(data)[, list(net_sent = mean(net_sent, na.rm = TRUE), # average net sentiment score
                                    words = sum(word_count), # total number of words
                                    documents = sum(doc_counter)), # total number of documents
                             by = list(date, lexicon, language)] # aggregate over date based on selected keywords
    
    data <- data[grepl(paste0(input$language, collapse = "|"), data$language), ] # selection based on language input
    data <- data[grepl(paste0(input$lexicon, collapse = "|"), data$lexicon), ] # selection based on lexicons input
    data <- data[data$date >= input$dates[1] & data$date <= input$dates[2], ] # selection based on time frame input
    
    data$name <- paste0(data$lexicon, " (", data$language, ")  ")
    
    if(nrow(data) > 0) {
      nvals <- data.table(data)[, list(n = length(net_sent)), 
                                by = list(lexicon, language)]$n
    } else {
      nvals <- 0 # in case data is empty
    }
    
    if(any(nvals <= 7)) { # if at least one of the series has 7 or less data points, inform users
      valid <- FALSE
    } else {
      valid <- TRUE
      
      if(input$normalised) {
        norm <- data.table(data)[, list(vals = (net_sent - mean(net_sent)) / sd(net_sent)), # normalisation within selection
                                 by = list(lexicon, language)]
        data$net_sent <- norm$vals
      }
      
      sma <- data.table(data)[, list(vals = SMA(net_sent, n = 7)), # simple moving average (weekly)
                            by = list(lexicon, language)]
      data$sma <- sma$vals
    }
    
    return(list(data = data, valid = valid))
  }) 
  
  output$stats <- renderTable({
    validate(
      need((length(input$language) > 0 & length(input$lexicon) > 0),
           "Select language and/or lexicon.")
    )
    
    selData <- selData()
    data <- selData$data
    
    validate(
      need(selData$valid == TRUE,
           "Not enough data points.")
    )
    
    stats <- dplyr::summarise(group_by(data, name), 
                              sentiment = mean(net_sent),
                              words = mean(words / documents),
                              documents = mean(documents))
    
    return(stats)
  })
  
  output$sel_keywords <- renderUI({
    if(length(input$keywords) > 0) {
      HTML(paste0("Selected keywords: ", 
                  "<ul>", 
                  paste0("<li>", paste0(gsub("_", " ", input$keywords), collapse = "</li><li>"), "</li>"),
                  "</ul>"))
    } else {
      HTML(paste0("Selected keywords: ", "none"))
    }
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
            axis.text.x = element_text(angle = 60, hjust = 1))
    
    return(p)
  })
  
  output$hover_info <- renderUI({
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
    style <- paste0("position: absolute; z-index: 100; background-color: rgba(245, 245, 245, 0.85); ",
                    "left:", left_px + 2, "px; top:", top_px + 2, "px;")
    
    # actual tooltip created as a wellPanel
    wellPanel(
      style = style,
      p(HTML(paste0("<b> Date: </b>", point$date, "<br/>",
                    "<b> Sentiment (mean): </b>", round(point$net_sent, 2), "<br/>",
                    "<b> Documents (total): </b>", point$documents, "<br/>",
                    "<b> Words (mean): </b>", floor(point$words/point$documents), "<br/>")))
    )
  })
}
