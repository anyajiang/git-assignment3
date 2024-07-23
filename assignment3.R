library(shiny)
library(openai)
library(rvest)


AI_content <- reactiveVal(list())
loaded_text <- reactiveVal()

Sys.setenv(OPENAI_API_KEY = 'XXXXXX')
Sys.setenv(DEFAULT_PROMPT = 'prompt.txt')


ui <- fluidPage(
  tags$head(
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/clipboard.js/2.0.6/clipboard.min.js"),
    tags$style(HTML("
      #assistant {
        white-space: pre-wrap;       
        white-space: -moz-pre-wrap;  
        white-space: -pre-wrap;      
        white-space: -o-pre-wrap;    
        word-wrap: break-word;    
      }
    "))),
  tags$script(
    HTML("
      Shiny.addCustomMessageHandler('copyToClipboard', function(message) {
        var dummy = document.createElement('textarea');
        document.body.appendChild(dummy);
        dummy.value = message;
        dummy.select();
        document.execCommand('copy');
        document.body.removeChild(dummy);
        alert('Prompt copied to clipboard!');
      });
    ")
  ),
  titlePanel("Gene Set AI"), 
  sidebarLayout(
    sidebarPanel(
      textAreaInput("geneMessage", "Try a gene set query: ", value = "", rows = 5),
      sliderInput("temperature", "Temperature (low: focused, high: creative):", min = 0, max = 2, value = 0.7, step = 0.1),
      div(style = "display: flex; justify-content: space-between;",
          actionButton("example", "Example Query"),
          actionButton("run_query", "Run Query")
      ), 
      div(style = "display: flex;",
          actionButton("clear", "Clear")
      )
    ),
    mainPanel(
      selectizeInput("model", "Select Model", 
                     choices = c("gpt-4o", "gpt-4", "gpt-3.5-turbo"), 
                     selected = "gpt-4o", multiple = FALSE), 
      actionButton("toggle_file_input", "Optional Prompt Input"),
      conditionalPanel(
        condition = "output.showFileInput == true", 
        fileInput("file", "Input Prompt File")),
      actionButton("prompt", "Show Prompt"), 
      actionButton("copy_response", "Copy Response"),
      actionButton("help", "Help"),
      titlePanel("Response"), 
      verbatimTextOutput("assistant")
    )
  ),
)

server <- function(input, output, session) {
  
  showFileInput <- reactiveVal(FALSE)
  
  output$showFileInput <- reactive({
    showFileInput()
  })
  outputOptions(output, "showFileInput", suspendWhenHidden = FALSE)
  
  observeEvent(input$toggle_file_input, {
    showFileInput(!showFileInput())
  })
  

  observe({
    if(is.null(input$file)) {
      text <- readLines(Sys.getenv("DEFAULT_PROMPT"))
      loaded_text(text)
    } else {
      req(input$file)
      text <- readLines(input$file$datapath)
      loaded_text(text)
    }
    
    assistant_response <- AI_content()
    output$assistant <- renderText({
      if (length(assistant_response) == 0) {
        "\n\nType a message and click submit to start"
      } else {
        paste0("\n", assistant_response$choices$message.content, "\n\nYour Input: ", assistant_response$user_message, "\n\nModel: ", assistant_response$model, "\n\nToken use: ", assistant_response$usage$total_tokens)
      }
    })
  })
  
  observeEvent(input$clear, {
    AI_content(list())
    updateTextAreaInput(session, "geneMessage", value = "")
    text <- readLines(Sys.getenv("DEFAULT_PROMPT"))
    loaded_text(text)
    showFileInput(FALSE)
    reset("file")
  })
  
  observeEvent(input$run_query, {
    output$assistant <- renderText({""})
    prompt_text <- paste(loaded_text(), collapse = "\n")
    user_message <- input$geneMessage
    AI_content(list())
    
    withProgress(message = 'Generating response using OpenAI...', value = 0, {
      assistant_response <- create_chat_completion(
        model = input$model,
        messages = list(list("role" = "user", "content" = prompt_text), list("role" = "user", "content" = user_message)),
        temperature = input$temperature,
        openai_api_key = "XXXXXX"
      )
    })
    
    assistant_response$user_message <- user_message
    AI_content(assistant_response)
    
  })
  
  observeEvent(input$example, {
    updateTextAreaInput(session, "geneMessage", value = "GNG5, TBX5, ISL1, RBPJ, CTNNB1, NOTCH1, SMAD4, EYA1, BMP10, SOX9, HES1, ENG, MKS1, SIX1, TBX3, HAND2, PIM1, BMPR2")
  })
  
  observeEvent(input$prompt, {
    showModal(modalDialog(
      title = "Prompt", 
      verbatimTextOutput("file_content"),
      footer = tagList(
        actionButton("copy_prompt", "Copy Prompt"),
        modalButton("Close")
      )
    ))
    output$file_content <- renderText({ paste(paste(loaded_text(), collapse = "\n"), input$geneMessage) })
  })
  
  observeEvent(input$copy_prompt, {
    req(loaded_text())
    session$sendCustomMessage(type = "copyToClipboard", message = paste(loaded_text(), collapse = "\n"))
  })
  
  observeEvent(input$copy_response, {
    req(AI_content())
    assistant_response <- AI_content()
    session$sendCustomMessage(type = "copyToClipboard", message = paste0("\n", assistant_response$choices$message.content, "\n\nYour Input: ", assistant_response$user_message, "\n\nModel: ", assistant_response$model, "\n\nToken use: ", assistant_response$usage$total_tokens))
  })
  
  observeEvent(input$help, {
    help_text <-read_html("help2.html")
    #help_text <- docx_summary(doc)$text
    showModal(modalDialog(
      title = "Help",
      HTML(paste(help_text, collapse = "<br>")),
      footer = modalButton("Close")
    ))
  })
  
}

shinyApp(ui, server)

