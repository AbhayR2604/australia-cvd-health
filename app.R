# app.R

library(shiny)
library(leaflet)
library(tidyverse)
library(sf)
library(plotly)
library(rintrojs)

# Running our wrangling file 

source("data_wrangling.R")

# --- Load Cleaned CVD Data from CSV ---
cvd_data <- read_csv("Data/cvd_prevalence_by_state.csv") %>%
  mutate(cvd_prevalence_percent = as.numeric(cvd_prevalence_percent))

# --- Load Australian States GeoJSON ---
aus_states <- st_read("Data/aus_states.geojson")

# --- Join CVD data to GeoJSON polygons using full state name ---
aus_states <- aus_states %>%
  left_join(cvd_data, by = c("STATE_NAME" = "state"))

trend_data <- read_csv("Data/conditions_trend_australia.csv") %>%
  rename(
    State = state,
    Condition = condition,
    Year = year,
    Prevalence_Percent = prevalence_percent
  ) %>%
  mutate(
    Condition = case_when(
      Condition == "High blood pressure" ~ "High BP",
      Condition == "CVD" ~ "CVD",
      Condition == "Diabetes" ~ "Diabetes", 
      TRUE ~ Condition
    )
  )

# --- Define UI ---
ui <- fluidPage(
  introjsUI(),  # Required for rintrojs
  titlePanel("Misconceptions Unmasked – What the Data Actually Shows"),
  
  # Introductory narrative
  tags$div(style = "padding-left: 15px; padding-right: 15px;",
           tags$h4(strong("A data-driven look at heart, stroke and vascular disease in Australia")),
           tags$p("Cardiovascular disease (CVD) is one of Australia’s leading causes of death, yet public understanding is often clouded by misconceptions like the belief that it only affects older adults or that lifestyle has little impact."),
           tags$p("This interactive dashboard explores the real patterns and risk factors behind CVD using national health data from 2001 to 2022. Dive into state-by-state insights, track long-term trends, and discover how age, gender, lifestyle, and coexisting conditions like diabetes and high blood pressure contribute to CVD risk."),
           tags$p("Let’s separate fact from fiction—one visual at a time.")
  ),
  
  # Take a Tour button
  div(style = "text-align: right; padding-right: 15px; padding-bottom: 10px;",
      actionButton("start_tour", "Take a Tour", icon = icon("map-signs"), class = "btn-primary")
  ),
  
  # Choropleth Map Section
  tags$div(style = "padding-left: 15px; padding-right: 15px;",
           tags$h3("Geographic Overview", id = "map_title"),
           div(id = "step_choropleth_map", leafletOutput("cvd_choropleth", height = "500px"))
  ),
  
  # Description under the map
  tags$div(style = "padding-left: 15px; padding-right: 15px; padding-top: 10px;",
           tags$h4(strong("Where you live may influence your heart health more than you think")),
           tags$p("This map shows how cardiovascular disease (CVD) prevalence varies across Australia’s states and territories and the differences are striking."),
           tags$p("While some regions show relatively low rates, others, like Tasmania and Queensland, reveal significantly higher CVD prevalence. This variation invites important questions: What role do local health behaviours, access to care, or demographics play?"),
           tags$p("By zooming out and comparing regions side by side, we begin to uncover how location intersects with health outcomes, challenging the idea that CVD is evenly distributed nationwide.")
  ),
  
  # Section: Condition Trend Over Years
  h3("State Health Trends Over Time – CVD, High BP and Diabetes"),
  sidebarLayout(
    sidebarPanel(
      div(id = "step_state_dropdown",
          selectInput("selected_state", "Select State:",
                      choices = c("None", unique(trend_data$State[trend_data$State != "National"])),
                      selected = "None")
      ),
      
      div(id = "step_condition_checkbox",
          checkboxGroupInput("selected_conditions", "Select up to 2 Conditions:",
                             choices = c("CVD", "High BP", "Diabetes"),
                             selected = character(0))
      ),
      
      div(id = "step_national_toggle",
          checkboxInput("show_national", "Show National CVD Trend", value = TRUE)
      ),
      
      helpText("Note: You can select up to 2 conditions to compare.")
    ),
    mainPanel(
      plotlyOutput("trend_plot", height = "500px")
    )
  ),
  
  # Brief Explanation for our State trends
  tags$div(style = "padding-left: 15px; padding-right: 15px;",
           tags$h4(strong("Chronic health conditions don’t follow the same path in every region.")),
           tags$p("By examining long-term trends in cardiovascular disease, high blood pressure, and diabetes across Australian states, this visual highlights how regional health patterns have shifted over the past two decades."),
           tags$p("The optional national trend provides a valuable benchmark, revealing which states are ahead of or falling behind national averages, and prompting a closer look at the factors driving these differences.")
  ),
  
  # Composite Risk Heatmap section
  h3("Age Meets Behaviour: How Risk Levels Stack Up"),
  fluidRow(
    column(12,
           plotlyOutput("risk_heatmap", height = "500px"))
  ),
  
  # Explanation for our Heatmap visualisation
  tags$div(style = "padding-left: 15px; padding-right: 15px;",
           tags$h4(strong("Lifestyle choices influence CVD risk but not all age groups are affected the same way.")),
           tags$p("This visual breaks down how smoking, alcohol consumption, and inactivity contribute to overall cardiovascular risk across age groups, using a colour-coded heatmap to show the severity."),
           tags$p("Some patterns stand out: young adults face high risk from smoking, while older adults are more vulnerable to alcohol and inactivity. By mapping these risk levels, we gain a clearer picture of where targeted health efforts can make the greatest impact.")
  ),
  
  h3("Uncovering the Age and Gender Divide in CVD and Blood Pressure"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("selected_sex", "Select Sex:",
                   choices = c("Persons", "Male", "Female"),
                   selected = "Persons"),
      div(id = "linebar_agegroups",
          selectInput("selected_agegroups", "Select Age Groups:",
                      choices = unique(cvd_bp_age_combined$Age),
                      selected = unique(cvd_bp_age_combined$Age), multiple = TRUE)
      ),
      checkboxInput("swap_axes", "Swap Line/Bar", value = FALSE)
    ),
    mainPanel(
      plotlyOutput("line_bar_plot", height = "500px")
    )
  ),
  # Explanation for CVD and High BP by Age and Gender
  tags$div(style = "padding-left: 15px; padding-right: 15px;",
           tags$h4(strong("CVD and high blood pressure don’t rise at the same rate and the differences deepen with age.")),
           tags$p("This chart allows users to explore how the prevalence of these two conditions shifts across age groups and between genders."),
           tags$p("While high blood pressure increases rapidly after age 45, CVD shows a more gradual climb but becomes notably higher in the 65+ group. The swap option lets you view either condition as a line or bar, helping reveal how steeply each trend rises."),
           tags$p("Gender-based filters uncover further differences, giving a fuller picture of how cardiovascular health risks are shaped over a lifetime.")
  ),
  # --- Concluding Summary ---
  tags$hr(),
  tags$div(style = "padding-left: 15px; padding-right: 15px; padding-top: 10px;",
           tags$h4(strong("Cardiovascular disease isn’t random and it isn’t inevitable.")),
           tags$p("Across states, age groups, and behaviours, the data reveals clear patterns: lifestyle choices like smoking, alcohol use, and inactivity strongly influence CVD risk, and these effects become more pronounced with age."),
           tags$p("By understanding how these risks vary across time and demographic groups, we can better direct awareness, prevention, and policy efforts where they matter most. What the data shows is clear: the earlier we act, the better we protect our hearts.")
  ),
 # ----- Link to the Data Source ---------
 # --- Data Source Footer ---
 tags$div(style = "padding: 10px 15px 30px 15px; font-size: 13px; text-align: left; color: #555;",
          tags$p(HTML(
            '<strong>Data Source:</strong> <a href="https://www.abs.gov.au/statistics/health/health-conditions-and-risks/national-health-survey/2022" target="_blank">
           Australian Bureau of Statistics – National Health Survey, 2022</a>'
          ))
 )
)

# --- Define Server ---
server <- function(input, output, session) {
  observeEvent(input$start_tour, {
    introjs(session, options = list(
      steps = list(
        list(element = "#step_choropleth_map", intro = "This choropleth map shows CVD prevalence across Australian states. Click on a state to view its trend."),
        list(element = "#step_state_dropdown", intro = "Use this dropdown to manually select a state."),
        list(element = "#step_condition_checkbox", intro = "Select up to 2 health conditions to compare."),
        list(element = "#step_national_toggle", intro = "Include the national trend for context."),
        list(element = "#risk_heatmap", intro = "This heatmap shows how age and lifestyle behaviors (like smoking, alcohol, inactivity) affect CVD risk."),
        list(element = "#selected_sex", intro = "Filter by sex to explore condition prevalence by gender."),
        list(element = "#linebar_agegroups", intro = "Select age groups to see how trends differ."),
        list(element = "#swap_axes", intro = "Swap between bar and line views for better comparison.")
      )
    ))
  })
  
  # Choropleth map rendering
  output$cvd_choropleth <- renderLeaflet({
    pal <- colorNumeric("Reds", domain = cvd_data$cvd_prevalence_percent, na.color = "#f0f0f0")
    
    leaflet(data = aus_states) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        layerId = ~STATE_NAME,  # 🔧 Add this line
        fillColor = ~pal(cvd_prevalence_percent),
        color = "white",
        weight = 1,
        fillOpacity = 0.8,
        label = ~paste0(
          "<strong>", STATE_NAME, "</strong><br/>",
          "CVD Prevalence: ", cvd_prevalence_percent, "%"
        ) %>% lapply(htmltools::HTML),
        highlightOptions = highlightOptions(color = "black", weight = 2, bringToFront = TRUE)
      ) %>%
      addLegend(pal = pal, values = cvd_data$cvd_prevalence_percent,
                title = "CVD Prevalence (%)", position = "bottomright")
  })
  
  # --- Highlight selected state on the map ---
  observeEvent(input$selected_state, {
    selected_state <- input$selected_state
    
    # If nothing is selected, just clear the highlight and return
    if (is.null(selected_state) || selected_state == "None") {
      leafletProxy("cvd_choropleth") %>% clearGroup("selected")
      return()
    }
    # Otherwise, highlight the selected state
    leafletProxy("cvd_choropleth", data = aus_states) %>%
      clearGroup("selected") %>%
      addPolygons(
        data = aus_states %>% filter(STATE_NAME == selected_state),
        fillColor = "gold",   
        color = "black",
        weight = 3,
        fillOpacity = 0.4,
        group = "selected"
      )
  })
  
  # Update dropdown when a state is clicked and display the CVD trend of that state
  observeEvent(input$cvd_choropleth_shape_click, {
    clicked_state <- input$cvd_choropleth_shape_click$id
    if (!is.null(clicked_state) && clicked_state %in% unique(trend_data$State)) {
      updateSelectInput(session, "selected_state", selected = clicked_state)
      
      isolate({
        if (length(input$selected_conditions) == 0) {
          updateCheckboxGroupInput(session, "selected_conditions", selected = "CVD")
          updateCheckboxInput(session, "show_national", value = TRUE)
        }
      })
    }
  })
  
  # Limit selection to 2 conditions
  observeEvent(input$selected_conditions, {
    if (length(input$selected_conditions) > 2) {
      updateCheckboxGroupInput(session, "selected_conditions",
                               selected = head(input$selected_conditions, 2))
      showNotification("You can select up to 2 conditions only.", type = "warning")
    }
  })
  
  # Reactive trend dataset with combined legend label
  filtered_trend_data <- reactive({
    # Default: show only national CVD if no state or conditions selected
    if (is.null(input$selected_state) || input$selected_state == "None" ||
        is.null(input$selected_conditions) || length(input$selected_conditions) == 0) {
      return(trend_data %>%
               filter(State == "National", Condition == "CVD") %>%
               mutate(
                 LineType = "National",
                 LegendLabel = "National, CVD"
               ))
    }
    
    # Otherwise, show state-level data
    state_data <- trend_data %>%
      filter(State == input$selected_state,
             Condition %in% input$selected_conditions) %>%
      mutate(
        LineType = "State",
        LegendLabel = paste("State", Condition, sep = ", ")
      )
    
    if (input$show_national) {
      national_cvd <- trend_data %>%
        filter(State == "National", Condition == "CVD") %>%
        mutate(
          LineType = "National",
          LegendLabel = "National, CVD"
        )
      bind_rows(state_data, national_cvd)
    } else {
      state_data
    }
  })
  
  # Render plot
  output$trend_plot <- renderPlotly({
    plot_data <- filtered_trend_data()
    
    # Tooltip column directly inside data
    plot_data <- plot_data %>%
      mutate(tooltip_text = paste0(
        "Year: ", Year, "<br>",
        "Prevalence: ", round(Prevalence_Percent, 2), "%<br>",
        "Condition: ", Condition
      ))
    
    # ggplot (text stays inside aes)
    p <- ggplot(plot_data, aes(x = Year, y = Prevalence_Percent)) +
      geom_line(aes(color = LegendLabel, linetype = LegendLabel, group = LegendLabel, text = tooltip_text),
                linewidth = 1.2, show.legend = TRUE) +
      geom_point(aes(color = LegendLabel, group = LegendLabel, text = tooltip_text),
                 size = 2, show.legend = FALSE) +
      labs(
        title = paste("Trend of Selected Conditions in", input$selected_state),
        x = "Year", y = "Prevalence (%)",
        color = NULL,
        linetype = NULL
      ) +
      theme_minimal() +
      scale_linetype_manual(values = c(
        "State, CVD" = "solid",
        "State, High BP" = "solid",
        "State, Diabetes" = "solid",
        "National, CVD" = "dashed"
      )) +
      scale_color_manual(values = c(
        "State, CVD" = "#F8766D",
        "State, High BP" = "#00BA38",
        "State, Diabetes" = "#619CFF",
        "National, CVD" = "#00BFC4"
      ))
    
    # Let plotly use the embedded text aesthetic for tooltips
    ggplotly(p, tooltip = "text") %>%
      layout(legend = list(orientation = "h", x = 0.1, y = -0.2)) %>%
      style(showlegend = FALSE, traces = which(sapply(.$x$data, function(d) d$mode == "markers")))
  })
  # Creating heatmap based on the risk factors
  output$risk_heatmap <- renderPlotly({
    data <- classified_national_riskfactor_data %>%
      mutate(
        Composite_Risk = factor(Composite_Risk, levels = c("Low", "Moderate", "Medium", "High")),
        Risk_Factor = factor(Risk_Factor, levels = c("Smoking", "Alcohol consumption", "Inactivity")),
        tooltip_text = paste0(
          "Age: ", Age, "<br>",
          "Risk Factor: ", Risk_Factor, "<br>",
          "Risk %: ", round(Risk_Percent, 1), "%<br>",
          "CVD Prevalence: ", round(CVD_Prevalence, 1), "%<br>",
          "Composite Risk: ", Composite_Risk
        )
      )
    
    p <- ggplot(data, aes(x = Risk_Factor, y = Age, fill = Composite_Risk, text = tooltip_text)) +
      geom_tile(color = "white", linewidth = 0.5) +
      geom_text(aes(label = Composite_Risk), size = 4, fontface = "bold", color = "black") +  # 🟢 Labels on tiles
      scale_fill_manual(
        values = c("Low" = "#d4f0d4", "Moderate" = "#ffe699", "Medium" = "#f4b183", "High" = "#ea9999"),
        name = "Composite Risk"
      ) +
      labs(
        title = "How Lifestyle Behaviours Shape CVD Risk by Age",
        x = "Risk Factor", y = "Age Group"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # 🔵 Center align
        axis.title.x = element_text(margin = margin(t = 15)),
        axis.title.y = element_text(margin = margin(r = 15)),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        plot.margin = margin(t = 10, r = 20, b = 10, l = 20)
      )
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        margin = list(t = 60, b = 60, l = 80, r = 80),  
        autosize = TRUE
      )
  })
  # Output for Line + Bar plot
  output$line_bar_plot <- renderPlotly({
    if (is.null(input$selected_agegroups) || length(input$selected_agegroups) == 0) {
      showModal(modalDialog(
        title = "Age Group Required",
        "Please select at least one age group to view the visual.",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      return(NULL)
    }
    
    df <- cvd_bp_age_combined %>%
      filter(Sex == input$selected_sex, Age %in% input$selected_agegroups)
    
    bar_condition <- ifelse(input$swap_axes, "High BP", "CVD")
    line_condition <- ifelse(input$swap_axes, "CVD", "High BP")
    
    bar_df <- df %>% filter(Condition == bar_condition)
    line_df <- df %>% filter(Condition == line_condition)
    
   p <- ggplot() +
  geom_col(data = bar_df, aes(x = Age, y = Prevalence, fill = Condition),
           width = 0.6, alpha = 0.8) +
  geom_line(data = line_df, aes(x = Age, y = Prevalence, color = Condition, group = Condition),
            size = 1.2) +
  geom_point(data = line_df, aes(x = Age, y = Prevalence, color = Condition, group = Condition),
             size = 2.5) +
  labs(
    title = "CVD and High BP Prevalence Across Age Groups",
    x = "Age Group", y = "Prevalence (%)",
    fill = NULL, color = NULL
  ) +
  scale_color_manual(values = c("CVD" = "#001f3f", "High BP" = "#7FDBFF")) +
  scale_fill_manual(values = c("CVD" = "#001f3f", "High BP" = "#7FDBFF")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    legend.position = "right"
  )
   ggplotly(p, tooltip = c("x", "y")) %>%
     layout(
       title = list(text = "High BP and CVD Prevalence Across Various Age Groups"),
       legend = list(title = list(text = "Condition"))
     ) %>%
     style(name = "CVD", traces = 1) %>%
     style(name = "High BP", traces = 2)
  })
}


# --- Run the Shiny App ---
shinyApp(ui, server)
