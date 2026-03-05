library(shiny)
library(bslib)
library(ellmer)
library(shinychat) # <-- Add this!
library(ggplot2)
library(DT)
library(plotly)

# Make sure the core functions from Part 1 are loaded in your environment!
source("./utils/ACISfunctions.R")

ui <- page_sidebar(
  # 1. A clearer, more professional title
  title = "Southwest U.S. Climate Assistant",
  
  # 2. Add a modern, clean bslib theme (optional, but looks great!)
  theme = bs_theme(preset = "yeti"), 
  
  sidebar = sidebar(
    width = 450,
    
    # 3. Add an expandable instruction panel
    accordion(
      open = FALSE, # Set to TRUE if you want it open by default
      accordion_panel(
        title = "ℹ️ How to use this tool",
        markdown(
          "
**Welcome!** Ask the AI assistant to fetch and visualize historical daily weather data.

**Example Queries:**
* **Basic Trends:** *\"What was the daily high temperature in Winslow, AZ last week?\"*
* **Departures from Normal:** *\"Show me the daily precipitation departures from normal for Tucson, AZ last month.\"*
* **Threshold Counts:** *\"Which stations in AZ had the most days over 100 degrees last July?\"*
* **Dates of Extremes:** *\"What was the lowest temperature in New Mexico last winter, and what exact date did it happen?\"*

**Data Source:**
All data is retrieved in real-time from the [Applied Climate Information System (ACIS)](http://www.rcc-acis.org/), maintained by the NOAA Regional Climate Centers. 

*Note: This assistant specializes in **historical** data and cannot provide future weather forecasts or hourly/sub-daily observations.*

---

**About the AI:**
This tool translates your questions into database queries using Google's **Gemini 2.5 Flash** large language model.

**Contact:**
Mike Crimmins, University of Arizona, https://cales.arizona.edu/climate  
Questions or feedback? Email: [crimmins@arizona.edu](mailto:crimmins@arizona.edu)
          "
        )
      )
    ),
    
    # The chat interface sits right below the instructions
    chat_mod_ui("podcast_chat")
  ),
  
  # Main area remains the same
  card(
    card_header("Live Data Visualization"),
    #plotOutput("dynamic_plot", height = "400px"),
    plotly::plotlyOutput("dynamic_plot"),
    DT::dataTableOutput("dynamic_table")
  )
)

server <- function(input, output, session) {
  
  # Reactive state to hold the fetched data
  app_state <- reactiveValues(current_data = NULL, plot_type = NULL, title = "")
  
  # --- Define Tools (Updated for ellmer 0.3.0 syntax) ---
  
  tool_phonebook <- tool(
    function(state, city_name) {
      df <- find_station_id_core(state, city_name)
      if(nrow(df) == 0) return("No stations found.")
      return(paste(capture.output(print(df)), collapse = "\n"))
    },
    name = "tool_phonebook",
    description = "Search for an ACIS station ID by city name and state. Do this FIRST if you don't know the exact ID.",
    arguments = list(
      state = type_string("Two-letter state abbreviation (e.g., 'AZ')."),
      city_name = type_string("City name (e.g., 'Winslow').")
    )
  )
  
  tool_ts <- tool(
    function(station_id, start_date, end_date, element, normal_departure = FALSE) {
      df <- get_timeseries_core(station_id, start_date, end_date, element, normal_departure)
      
      app_state$current_data <- df
      app_state$plot_type <- "time_series"
      app_state$title <- paste("Time Series:", element, ifelse(normal_departure, "(Departure from Normal)", ""))
      
      #return(paste("Successfully plotted data. Summarize it."))
      return(paste(capture.output(print(df)), collapse = "\n"))
    },
    name = "tool_ts",
    description = "Fetches daily time-series weather data for a single station ID. Use this to find trends, check specific dates, OR to count days above/below a threshold for a single city (fetch the data and count the occurrences yourself).",
    arguments = list(
      station_id = type_string("The exact ACIS station ID."),
      start_date = type_string("Start date YYYY-MM-DD."),
      end_date = type_string("End date YYYY-MM-DD."),
      element = type_string("Variable: 'maxt', 'mint', 'avgt', or 'pcpn'."),
      normal_departure = type_boolean("Set to TRUE only if the user specifically asks for 'departure from normal' or 'anomalies'. Otherwise FALSE.")
    )
  )
  
  tool_reg <- tool(
    function(states, start_date, end_date, element, summary_type, normal_departure = FALSE, include_date = FALSE) {
      df <- get_regional_core(states, start_date, end_date, element, summary_type, normal_departure, include_date)
      
      app_state$current_data <- df
      app_state$plot_type <- "regional_bar"
      app_state$title <- paste("Regional Top 15:", element, "-", summary_type)
      
      return(paste(capture.output(print(df)), collapse = "\n"))
    },
    name = "tool_reg",
    description = "Fetches summarized regional climate data for specific states to find extremes, counts above thresholds, or departures from normal.",
    arguments = list(
      states = type_string("Comma-separated states (e.g., 'AZ,NM')."),
      start_date = type_string("Start date YYYY-MM-DD."),
      end_date = type_string("End date YYYY-MM-DD."),
      element = type_string("Variable: 'maxt', 'mint', 'avgt', or 'pcpn'."),
      summary_type = type_string("Mathematical summary. Use 'sum', 'max', 'min', or 'mean'. FOR COUNTS OF DAYS ABOVE/BELOW A THRESHOLD: Use the format 'cnt_xx_yyy', where xx is 'ge' (>=), 'gt' (>), 'le' (<=), or 'lt' (<), and yyy is the number. Example: for 'days above 100 degrees', use 'cnt_ge_100'."),
      normal_departure = type_boolean("Set to TRUE if asking for departure from normal. FALSE otherwise."),
      include_date = type_boolean("Set to TRUE if the user asks exactly *when* or *what day* an extreme (max/min) occurred. FALSE otherwise.")
    )
  )
  
  # --- Initialize Gemini and Register Tools ---

  sys_prompt <- paste(
    "You are an informative and neutral climate data assistant.",
    "Today's date is", Sys.Date(), ".",
    "You have access to tools that fetch historical weather data from the RCC-ACIS database.",
    "If the user asks about a specific city and you do not know the exact ACIS station ID, you must use the phonebook tool first to find it.",
    "If the user asks for a count of days meeting a certain threshold for a specific city, use tool_ts to fetch the daily data, then count the matching days yourself to provide the answer.", # <--- NEW INSTRUCTION
    "When you retrieve data using these tools, it is automatically plotted on the user's dashboard.",
    "Your role is to clearly and factually summarize the retrieved data, highlighting any notable trends, averages, or extremes without adding conversational filler."
  )
  
  chat <- chat_google_gemini(
    model = "gemini-2.5-flash",
    system_prompt = sys_prompt
  )
  
  # This is the correct way to attach tools in ellmer!
  chat$register_tool(tool_phonebook)
  chat$register_tool(tool_ts)
  chat$register_tool(tool_reg)
  
  # Connect the Gemini object to the shinychat UI module
  chat_mod_server("podcast_chat", chat)
  
  output$dynamic_plot <- plotly::renderPlotly({
    
    my_data <- app_state$current_data 
    req(my_data) 
    req(app_state$plot_type) # Ensure a plot type exists before rendering
    
    # ==========================================
    # 1. TIME SERIES PLOT LOGIC
    # ==========================================
    if (app_state$plot_type == "time_series") {
      
      if ("departure" %in% names(my_data)) {
        my_data$color_flag <- ifelse(my_data$departure >= 0, "Above Normal", "Below Normal")
        
        p <- ggplot2::ggplot(my_data, ggplot2::aes(
          x = date, 
          y = departure, 
          fill = color_flag,
          text = paste0("Date: ", date, "\nObserved: ", value, "\nDeparture: ", departure)
        )) +
          ggplot2::geom_col() +
          ggplot2::scale_fill_manual(values = c("Above Normal" = "#d73027", "Below Normal" = "#4575b4")) +
          ggplot2::labs(title = app_state$title, x = "Date", y = "Departure") +
          ggplot2::theme_minimal() +
          ggplot2::theme(legend.title = ggplot2::element_blank())
        
        plotly::ggplotly(p, tooltip = "text")
        
      } else {
        p <- ggplot2::ggplot(my_data, ggplot2::aes(
          x = date, 
          y = value,
          group = 1, # <--- THIS FIXES THE BROKEN LINES!
          text = paste0("Date: ", date, "\nValue: ", value)
        )) +
          ggplot2::geom_line(color = "#0072B2", linewidth = 1) +
          ggplot2::geom_point(color = "#0072B2", size = 1.5) +
          ggplot2::labs(title = app_state$title, x = "Date", y = "Value") +
          ggplot2::theme_minimal()
        
        plotly::ggplotly(p, tooltip = "text")
      }
      
      # ==========================================
      # 2. REGIONAL BAR PLOT LOGIC
      # ==========================================
    } else if (app_state$plot_type == "regional_bar") {
      
      if ("departure" %in% names(my_data)) {
        my_data$color_flag <- ifelse(my_data$departure >= 0, "Above Normal", "Below Normal")
        
        p <- ggplot2::ggplot(my_data, ggplot2::aes(
          # reorder() automatically sorts the bars from highest to lowest!
          x = stats::reorder(station, departure), 
          y = departure, 
          fill = color_flag,
          text = paste0("Station: ", station, "\nState: ", state, "\nDeparture: ", departure)
        )) +
          ggplot2::geom_col() +
          ggplot2::coord_flip() + # Makes the bar chart horizontal so station names are readable
          ggplot2::scale_fill_manual(values = c("Above Normal" = "#d73027", "Below Normal" = "#4575b4")) +
          ggplot2::labs(title = app_state$title, x = "Station", y = "Departure") +
          ggplot2::theme_minimal() +
          ggplot2::theme(legend.title = ggplot2::element_blank())
        
        plotly::ggplotly(p, tooltip = "text")
        
      } else {
        
        p <- ggplot2::ggplot(my_data, ggplot2::aes(
          x = stats::reorder(station, value), 
          y = value,
          text = paste0("Station: ", station, "\nState: ", state, "\nValue: ", value)
        )) +
          ggplot2::geom_col(fill = "#2c3e50") +
          ggplot2::coord_flip() + 
          ggplot2::labs(title = app_state$title, x = "Station", y = "Value") +
          ggplot2::theme_minimal()
        
        plotly::ggplotly(p, tooltip = "text")
      }
    }
  })
  
  output$dynamic_table <- DT::renderDataTable({
    req(app_state$current_data)
    
    display_df <- app_state$current_data
    
    # Keep our beautiful date formatting!
    if ("date" %in% names(display_df)) {
      display_df$date <- format(display_df$date, "%b %d, %Y")
    }
    
    # Render the interactive table
    DT::datatable(
      display_df,
      rownames = FALSE, # Hides the unnecessary row numbers
      options = list(
        pageLength = 5,       # Show 5 rows per page to save screen space
        lengthChange = FALSE, # Hides the "Show 10/25/50 entries" dropdown
        searching = TRUE      # Enables the search bar
      )
    )
  })
}

shinyApp(ui, server)