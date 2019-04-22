

library(shiny)
# install libraries in server:
library(pdftools)
library(tidytext)
library(dplyr)
library(plotly)

# Define UI for data upload app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Seevee (Alpha) - An AI to look at emotions in texts"),
  

    
    
    # Main panel for displaying outputs ----
    mainPanel(
      p('Hello! I am Seevee. An Artificial Intelligence model created by Marcelo Tournier to read sentiments in text.'
        ),
      p('Altough I am not perfect, I can give you insights about the emotions from your written words.'
      ),
      p('Why don\'t you give a try and upload a piece of text from your authorship in the box below?'
      ),
      p('I would be more than happy to analyze you... I mean - your text! :)'
      ),
      # Horizontal line ----
      tags$hr(),
      fileInput("file1", "Upload your text in PDF format",
                multiple = FALSE,
                accept = c(".pdf")),
      
      # line break----
      br(),
      
      # Output: Data file ----
      textOutput("totalwords"),
      
      # line break----
      br(),
      
      # Output: Data file ----
      tableOutput("wcounts"),
      
      # line break ----
      
      br(),

      textOutput("sentnrc"),
      br(),
      plotlyOutput("plot"),
      br(),
      textOutput("msgafinn"),
      br(),
      tableOutput("wcountafinn"),
      br(),
      textOutput("msgbing"),
      br(),
      tableOutput("wcountbing"),
      br(),
    
      textOutput("msgraw"),
      br(),
      # Output: Data file ----
      tableOutput("contents"),
      
      br(),
      p('Seevee, 2019.')
    )
    
  )


# Define server logic to read selected file ----
server <- function(input, output) {
  
  text_data <- eventReactive(input$file1, {
    tryCatch(
      {
        string <- pdf_text(input$file1$datapath)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
      
    )
    return(string)
    })
  
  text_df <- eventReactive(text_data, {
    df <- data_frame(line=1, text=text_data())
    return(df)
    })
  
  text_tokens <- eventReactive(text_df, {    
    tokens <- text_df() %>%
      unnest_tokens(word, text) %>%
      count(word, sort=T) %>%
      ungroup()
    return(tokens)
  })
  text_wcounts <- eventReactive(text_tokens, {    
    word_counts <- text_tokens() %>%
      anti_join(stop_words)
    return(word_counts)
    })

  text_nrc <- eventReactive(text_wcounts, {      
    nrc_counts <- text_tokens() %>%
      inner_join(get_sentiments("nrc")) %>%
      count(word, sentiment, sort=T) %>%
      ungroup()
    colnames(nrc_counts) <- c('word','sentiment',"number of words")
    return(nrc_counts)
    })
    
  text_nrct <- eventReactive(text_nrc, {  
    nrc_table <- text_nrc() %>%
      count(sentiment,sort=T) %>%
      ungroup()
    return(nrc_table)
    })

  text_afinn <- eventReactive(text_nrct, {      
    afinn_counts <- text_tokens() %>%
      inner_join(get_sentiments("afinn")) %>%
      count(word, score, sort=T) %>%
      ungroup()
    colnames(afinn_counts) <- c('word','score',"number of words")
    return(afinn_counts)
  })

  text_bing <- eventReactive(text_afinn, {      
    bing_counts <- text_tokens() %>%
      inner_join(get_sentiments("bing")) %>%
      count(word, sentiment, sort=T) %>%
      ungroup()
    colnames(bing_counts) <- c('word','sentiment',"number of words")
    return(bing_counts)
  })

  # Total number of words
  output$totalwords <- renderText({
    req(input$file1)
    return(paste('Your text has',sum(text_wcounts()$n),'words. Here are the top 20 most used words:'))
  })
  
  # top 20 words
  output$wcounts <- renderTable({
    req(input$file1)
    return(head(text_wcounts(),20))
    })
  
  # message for sentiment plot
  output$sentnrc <- renderText({
    req(input$file1)
    return('I also classified your main keywords by different sentiments. Here goes:')
  })
  
  # sentiment plot
  output$plot <- renderPlotly({
    req(input$file1)
    plot_ly(x = text_nrct()$sentiment,
            y = text_nrct()$n,
            type='bar') %>%
      layout(title = "Main sentiments by keywords")
  })
  
  output$msgafinn <- renderText({
    req(input$file1)
    return('This table shows the top most sentiment keywords, and their respective sentiment scores, from -5 to +5:')
  })
  # top 20 words
  output$wcountafinn <- renderTable({
    req(input$file1)
    return(head(text_afinn(),20))
  })

  output$msgbing <- renderText({
    req(input$file1)
    return(paste('I used a different method to count positive or negative words - ',
                 table(text_bing()$sentiment)[1],'negative and' ,table(text_bing()$sentiment)[1],
                 'words. Here are the top positive or negative words:'))
  })
  # top 20 words
  output$wcountbing <- renderTable({
    req(input$file1)
    return(head(text_bing(),20))
  })
  
  
  output$msgraw <- renderText({
    req(input$file1)
    return('Finally, here is the text, as I read it from your file:')
  })
  
  # raw text data, as imported
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    return(text_data())
  }
  
  )
}

# Create Shiny app ----
shinyApp(ui, server)


