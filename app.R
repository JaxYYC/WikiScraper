## WikiScraper ##

library(shiny)
library(rvest)
library(wordcloud)
library(tm)
library(shinydashboardPlus)
library(shinydashboard)
library(syuzhet)

ui <- tagList(
  navbarPage(
    'WikiScraper',
    tabPanel(
             'Home',
              textInput(inputId = 'input1',label = 'Type your searche key here:',value = 'Washington'),
              plotOutput(outputId = 'plot1',height = 500)
             )
    )
)
server <- function(input, output) {
  wikiCleaner <- function(text){
    myStopWords <- c("may", "now", "also", "many", "use", "used", "typically","given", "like", "will", "can",
                     "often", "see", "one", "pdf", "issn", "journal", tolower(month.name))
    
    docs <- text %>% VectorSource %>% Corpus %>% tm_map(content_transformer(tolower)) %>% 
      tm_map(content_transformer(function(x) {
      x  %>%  gsub(pattern = "–", replacement = "")  %>% gsub(pattern = "•", replacement = ""  %>% return)}))
    
    v <- docs %>% tm_map(removeNumbers) %>% tm_map(removeWords, stopwords("english")) %>% tm_map(removeWords, myStopWords) %>% tm_map(removePunctuation) %>% tm_map(stripWhitespace) %>% TermDocumentMatrix %>% as.matrix %>% rowSums %>% sort(decreasing=TRUE)
    
    data.frame(word = names(v),freq=round(sqrt(v)))
  }
  
  wikiWebScraper <- function(key) {
    d <-  paste0("https://en.wikipedia.org/wiki/",key) %>% read_html %>% html_nodes("#bodyContent") %>% 
      html_text %>% wikiCleaner
    set.seed(5)
    wordcloud(words = d$word, freq = d$freq, max.words=30, random.order=FALSE, rot.per=0.35, 
              min.freq = 1, colors=brewer.pal(8, "Set1"),scale = c(3.5,0.5))
  }
  
  output$plot1 <- renderPlot({
    wikiWebScraper(input$input1)
  })
}


# Run the application 
shinyApp(ui = ui, server = server)

