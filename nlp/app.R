library('shiny')
library('tm')
library('SnowballC')
library('shinythemes')
library('wordcloud')
library('dplyr')

ui <- fluidPage(theme = shinytheme('superhero'),
                tags$head(tags$style('h1 {color:black;text-align:center;background-color:aquamarine;padding:10px;border-radius:10px;font-family:forte;}')),
                fluidRow(column(12,tags$h1('OCEANCLICKS   WORD',icon('cloud') ,'2.0'))),
                tags$br(),
                fluidRow(
                  column(3,
                         wellPanel(
                           tags$br(),
                           tags$p('This has been designed to depict the frequency at which words appear in text data'),
                           tags$br(),
                           tags$br(),
                           fluidRow(column(12,fileInput('file1','Choose file',buttonLabel = 'file',accept = '.csv'))),
                           tags$br(),
                           tags$br(),
                           fluidRow(column(12,radioButtons('rad','Choose wordcloud type',choiceNames = c('Whole content','By label'),choiceValues = c('wcont','bty')))),
                           tags$br(),
                           tags$br(),
                           fluidRow(column(12,textInput('txt','Provide label'))),
                           
                           tags$br())),
                  column(9,fluidRow(
                    column(8,
                           fluidRow(column(12,plotOutput('plot',height = 345))),
                           tags$br(),
                           fluidRow(column(4,offset = 4,submitButton('Form Wordcloud',icon = icon('cloud')))),
                           tags$br(),
                           fluidRow(
                             wellPanel(
                               fluidRow(
                                 column(4,radioButtons('tycolor','Color style',choiceNames = c('Single color','Many colors'),choiceValues = c('sc','mc'))),
                                 column(4,selectInput('col','Color',choices = c('blue','red','black','green'))),
                                 column(4,radioButtons('rco','Random color order',choiceNames = c('True','False'),choiceValues = c('true','false')))
                               )
                             )
                           )),
                    column(4,
                           fluidRow(
                             column(12,
                                    wellPanel(
                                    
                                    fluidRow(sliderInput('slid','Frequency',min = 50,max = 95,value = 75)),
                                    tags$br(),
                                    fluidRow(numericInput('num','Maximum Words',value = 40,min = 10,max = 70)),
                                    tags$br(),
                                    fluidRow(radioButtons('radd2','Random words order',choiceNames = c('True','False'),choiceValues = c(T,F)))))
                           ),
                           fluidRow(
                             column(12,
                                    wellPanel(
                                      fluidRow(checkboxGroupInput('check','Transformation',choiceNames = c('Stem Document','Remove Stopwords','Remove Numbers','Remove Punctuation'),choiceValues = c('stemd','removeWords','removeNumbers','removePunctuation')))
                                    ))
                           ))
                  ))
                ))

server <- shinyServer(function(input,output){
  file1 <- reactive({
    
    path <- input$file1
    df <- read.csv(path$datapath,header = T)
    df
    
  })
  
  fileTU <- reactive({
    
    if (input$rad == 'wcont'){
      
      df <- file1()
      
    }else{
      
      df <- filter(file1(),label == input$txt)
      
    }
    df
    
  })
  
  corp_clean <- reactive({
    
    corpus <- VCorpus(VectorSource(fileTU()$message))
    
    if (('stemd' %in% input$check) & ('removeWords' %in% input$check) & ('removeNumbers' %in% input$check) & ('removePunctuation' %in% input$check)){
      
      corp_clean <- tm_map(corpus,removeWords,stopwords())
      corp_clean2 <- tm_map(corp_clean,removeNumbers)
      corp_clean3 <- tm_map(corp_clean2,stemDocument)
      corp_clean4 <- tm_map(corp_clean3,removePunctuation)
      corp <- corp_clean4
      
    }else if (('stemd' %in% input$check) & ('removeWords' %in% input$check) & ('removeNumbers' %in% input$check)){
      
      corp_clean <- tm_map(corpus,removeWords,stopwords())
      corp_clean2 <- tm_map(corp_clean,removeNumbers)
      corp_clean3 <- tm_map(corp_clean2,stemDocument)
      corp <- corp_clean3
      
    }else if (('stemd' %in% input$check) & ('removeWords' %in% input$check) & ('removePunctuation' %in% input$check)){
      
      corp_clean <- tm_map(corpus,removeWords,stopwords())
      corp_clean2 <- tm_map(corp_clean,stemDocument)
      corp_clean3 <- tm_map(corp_clean2,removePunctuation)
      corp <- corp_clean3
      
    }else if(('stemd' %in% input$check) & ('removePunctuation' %in% input$check) & ('removeNumbers' %in% input$check)){
      
      corp_clean <- tm_map(corpus,removeNumbers)
      corp_clean2 <- tm_map(corp_clean,stemDocument)
      corp_clean3 <- tm_map(corp_clean2,removePunctuation)
      corp <- corp_clean3
      
    }else if(('removePunctuation' %in% input$check) & ('removeWords' %in% input$check) & ('removeNumbers' %in% input$check)){
      
      corp_clean <- tm_map(corpus,removeWords,stopwords())
      corp_clean2 <- tm_map(corp_clean,removeNumbers)
      corp_clean3 <- tm_map(corp_clean2,removePunctuation)
      corp <- corp_clean3
      
    }else if (('stemd' %in% input$check) & ('removeWords' %in% input$check)){
      
      corp_clean <- tm_map(corpus,removeWords,stopwords())
      corp_clean2 <- tm_map(corp_clean,stemDocument)
      corp <- corp_clean2
      
    }else if (('stemd' %in% input$check) & ('removeNumbers' %in% input$check)){
      
      corp_clean <- tm_map(corpus,stemDocument)
      corp_clean2 <- tm_map(corp_clean,removeNumbers)
      corp <- corp_clean2
      
    }else if (('stemd' %in% input$check) & ('removePunctuation' %in% input$check)){
      
      corp_clean <- tm_map(corpus,stemDocument)
      corp_clean2 <- tm_map(corp_clean,removePunctuation)
      corp <- corp_clean2
      
     }else if (('removeNumbers' %in% input$check) & ('removeWords' %in% input$check)){
      
      corp_clean <- tm_map(corpus,removeWords,stopwords())
      corp_clean2 <- tm_map(corp_clean,removeNumbers)
      corp <- corp_clean2
      
    }else if (('removeNumbers' %in% input$check) & ('removePunctuation' %in% input$check)){
      
      corp_clean <- tm_map(corpus,removeNumbers)
      corp_clean2 <- tm_map(corp_clean,removePunctuation)
      corp <- corp_clean2
      
    }else if (('removeWords' %in% input$check) & ('removePunctuation' %in% input$check)){
      
      corp_clean <- tm_map(corpus,removeWords,stopwords())
      corp_clean2 <- tm_map(corp_clean,removePunctuation)
      corp <- corp_clean2
      
    }else if ('stemd' %in% input$check){
      
      corp_clean <- tm_map(corpus,stemDocument)
      corp <- corp_clean
      
    }else if ('removeWords' %in% input$check){
      
      corp_clean <- tm_map(corpus,removeWords,stopwords())
      corp <- corp_clean
      
    }else if ('removeNumbers' %in% input$check){
      
      corp_clean <- tm_map(corpus,removeNumbers)
      corp <- corp_clean
      
    }else if ('removePunctuation' %in% input$check){
      
      corp_clean <- tm_map(corpus,removePunctuation)
      corp <- corp_clean
      
    }else{
      
      corp <- corpus
    }
    
    corp
    
  })
  
  output$plot <- renderPlot({
    
    if (is.null(input$file1) == T){
      return()
    }else{
      
      if (input$tycolor == 'sc'){
        wordcloud(corp_clean(),min.freq = input$slid,max.words = input$num,random.order = input$radd2,colors = input$col)
      }else{
        
        if (input$rco == 'true'){
          
          wordcloud(corp_clean(),min.freq = input$slid,max.words = input$num,random.order = input$radd2,colors = c('blue','green','black','red','purple','gold','orange'))
          
        }else{
          
          wordcloud(corp_clean(),min.freq = input$slid,max.words = input$num,random.order = input$radd2,colors = c('blue','green','black','red','purple','gold','orange'),random.color = F)
          
        }
        
      }
      
    }
    

    
    
    
  })
  
})

shinyApp(ui,server)