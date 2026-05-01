library(shiny)
library(bslib)
library(querychat)
library(ellmer)
library(shinychat)
library(ggplot2)
library(dplyr)
library(bsicons)
library(vitals) # Added for the bonus point exploration!

# ---- Data setup ----
# Using txhousing which contains 8,602 rows of Texas housing market data
data_for_app <- na.omit(ggplot2::txhousing) 

# ---- Context & Instructions ----
tx_desc <- "
This dataset contains housing market data for various cities in Texas from 2000 to 2015.
- city: Name of the Texas city or region
- year / month: Time of the record
- sales: Number of sales
- volume: Total value of sales in dollars
- median: Median sale price in dollars
- listings: Total active listings
- inventory: 'Months inventory' (amount of time it would take to sell all current listings at current pace)
"

tx_instructions <- "
- You are a Texas Real Estate Data Analyst.
- Your goal is to help the user filter the txhousing dataset.
- Keep your responses brief and strictly focused on filtering the data.
"

# ---- Greeting ----
tx_greeting <- "
Howdy! Welcome to the Texas Housing Market Explorer. 

Try clicking one of these suggestions to get started:
* <span class='suggestion'>Show me data for Dallas and Austin after 2010</span>
* <span class='suggestion'>Filter to months where the median price was over $200,000</span>
* <span class='suggestion'>Sort the data by highest number of sales</span>
"

# ---- Initialize QueryChat ----
qc <- QueryChat$new(
  data_for_app,
  data_description = tx_desc,
  extra_instructions = tx_instructions,
  client = chat_anthropic(model = "claude-haiku-4-5"), 
  greeting = tx_greeting
)



# ---- UI ----
ui <- page_sidebar(
  title = "Texas Housing Market Explorer",
  theme = bs_theme(version = 5, preset = "flatly"), # Master's level aesthetic
  
  # Put the filter chat in the sidebar
  sidebar = sidebar(
    width = 350,
    title = "1. Filter the Data",
    qc$ui()
  ),
  
  # Main content area
  layout_columns(
    col_widths = 12,
    
    # Value Boxes (Row 1)
    layout_columns(
      col_widths = c(4, 4, 4),
      value_box(
        title = "Filtered Records (Months)",
        value = textOutput("vb_count"),
        showcase = bs_icon("table"),
        theme = "primary"
      ),
      value_box(
        title = "Average Median Price",
        value = textOutput("vb_price"),
        showcase = bs_icon("house-door"),
        theme = "success"
      ),
      value_box(
        title = "Total Sales",
        value = textOutput("vb_sales"),
        showcase = bs_icon("graph-up-arrow"),
        theme = "info"
      )
    ),
    
    # Plotting Card (Row 2)
    card(
      card_header("2. Build a Plot"),
      layout_columns(
        col_widths = c(3, 3, 3, 3),
        selectInput("x", "X-Axis:", choices = names(data_for_app), selected = "year"),
        selectInput("y", "Y-Axis:", choices = names(data_for_app), selected = "median"),
        selectInput("color", "Color by:", choices = c("None" = "", names(data_for_app)), selected = "city"),
        selectInput("geom", "Geom:", choices = c("Points" = "point", "Points + smooth" = "smooth", "Boxplot" = "boxplot"), selected = "smooth")
      ),
      plotOutput("plot", height = "400px")
    ),
    
    # Interpretation Chat (Row 3)
    card(
      card_header("3. Ask about the plot"),
      chat_ui("interp", height = "300px")
    )
  )
)



# ---- Server ----
server <- function(input, output, session) {
  
  # Define the null-coalescing operator
  `%||%` <- function(x, y) {
    if (is.null(x)) y else x
  }
  
  # Activate querychat's reactive outputs
  qc_vals <- qc$server()
  
  # Value Box 1: Count of records [cite: 17]
  output$vb_count <- renderText({
    format(nrow(qc_vals$df()), big.mark = ",")
  })
  
  # Value Box 2: Average of the median prices [cite: 17]
  output$vb_price <- renderText({
    df <- qc_vals$df()
    if(nrow(df) > 0) {
      paste0("$", format(round(mean(df$median, na.rm = TRUE), 0), big.mark = ",")) 
    } else {
      "$0"
    }
  })
  
  # Value Box 3: Sum of all sales [cite: 17]
  output$vb_sales <- renderText({
    df <- qc_vals$df()
    if(nrow(df) > 0) {
      format(sum(df$sales, na.rm = TRUE), big.mark = ",")
    } else {
      "0"
    }
  })
  
  # ---- Stage 2: Build the plot from filtered data + user controls ----
  current_plot <- reactive({
    df <- qc_vals$df()
    req(nrow(df) > 0)
    
    aes_args <- list(x = sym(input$x), y = sym(input$y))
    if (nzchar(input$color)) aes_args$color <- sym(input$color)
    
    p <- ggplot(df, do.call(aes, aes_args))
    
    # Using switch to apply the selected geometry based on UI input
    p <- switch(input$geom,
                point   = p + geom_point(alpha = 0.5),
                smooth  = p + geom_point(alpha = 0.3) + geom_smooth(method = "lm", se = TRUE),
                boxplot = p + geom_boxplot()
    )
    
    p + theme_minimal() + labs(
      title = qc_vals$title() %||% "All Texas Housing Data",
      subtitle = paste(format(nrow(df), big.mark = ","), "rows")
    )
  })
  
  output$plot <- renderPlot({
    print(current_plot())
  })
  
  # ---- Stage 3: Interpretation chat — ellmer with vision ----
  # Requirement: Modify the prompt for plot interpretation so that it is token-efficient[cite: 11].
  # Requirement: Use only the Haiku model[cite: 10].
  interp_chat <- chat_anthropic(
    model = "claude-haiku-4-5",
    system_prompt = "Interpret plot in <20 words. State 1 key trend. Suggest 1 follow-up."
  )
  
  observeEvent(input$interp_user_input, {
    # Make sure the current plot is on the graphics device before capture
    print(current_plot())
    
    df <- qc_vals$df()
    
    # Keeping the context string extremely short to save tokens
    filter_context <- sprintf(
      "Filter: %s (N=%d). User: %s",
      qc_vals$title() %||% "All",
      nrow(df),
      input$interp_user_input
    )
    
    # Stream the response back to the shinychat UI using the vision tool 
    chat_append("interp", interp_chat$stream_async(
      content_image_plot(),
      filter_context
    ))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
