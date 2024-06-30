
library(shiny)
library(openai)

Sys.setenv(OPENAI_API_KEY = 'XXXXXX')

chatHist <- reactiveVal(list())
AI_content <- reactiveVal(list())
loaded_text <- reactiveVal()

ui <- fluidPage(
  tags$head(
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/clipboard.js/2.0.6/clipboard.min.js"),
    tags$style(HTML("
      #assistant {
        white-space: pre-wrap;       /* CSS3 */
        white-space: -moz-pre-wrap;  /* Firefox */
        white-space: -pre-wrap;      /* Opera <7 */
        white-space: -o-pre-wrap;    /* Opera 7 */
        word-wrap: break-word;       /* IE */
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
      actionButton("clear", "Clear Input and Response")
    ),
    mainPanel(
      fluidRow(
        column(6, selectizeInput("model", label = "Select Model", 
                                 choices = c("gpt-4o", "gpt-4", "gpt-3.5-turbo"), 
                                 selected = "gpt-4o", multiple = FALSE)),
        column(6, fileInput("file", "Input Prompt File"))
      ), 
      actionButton("prompt", "Show Prompt"), 
      titlePanel("Response"), 
      verbatimTextOutput("assistant")
    )
  ),
)

server <- function(input, output, session) {
  
  observeEvent(input$clear, {
    chatHist(list())
    AI_content(list())
    updateTextAreaInput(session, "geneMessage", value = "")
  })
  
  observeEvent(input$run_query, {
    prompt_text <- paste(loaded_text(), collapse = "\n")
    user_message <- input$geneMessage
    AI_content(list())
    chat_history <- chatHist() 
    chat_history <- append(chat_history, list(list("role" = "user", "content" = prompt_text), list("role" = "user", "content" = user_message)))
    
    withProgress(message = 'Generating response using OpenAI...', value = 0, {
      assistant_response <- create_chat_completion(
        model = input$model,
        messages = chat_history,
        temperature = input$temperature,
        openai_api_key = Sys.getenv("openai_api_key")
      )
    })
    
    assistant_content <- assistant_response$choices
    chat_history <- append(chat_history, list(list("role" = "assistant", "content" = assistant_content$message.content)))
    chatHist(chat_history)
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
    output$file_content <- renderText({ paste(loaded_text(), collapse = "\n") })
  })
  
  observeEvent(input$copy_prompt, {
    req(loaded_text())
    session$sendCustomMessage(type = "copyToClipboard", message = paste(loaded_text(), collapse = "\n"))
  })
  
  observe({
    req(input$file)
    text <- readLines(input$file$datapath)
    loaded_text(text)
    
    chat_history <- chatHist() 
    assistant_response <- AI_content()
    assistant_content <- assistant_response$choices
    output$assistant <- renderText({
      if (length(assistant_response) == 0) {
        "\n\nType a message and click submit to start"
      } else {
        paste0("\n", assistant_content$message.content, "\n\nYour Input: ", assistant_response$user_message, "\n\nModel: ", assistant_response$model, "\n\nToken use: ", assistant_response$usage$total_tokens)
      }
    })
  })
}

shinyApp(ui, server)

