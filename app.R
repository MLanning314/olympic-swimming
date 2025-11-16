
# Olympic Swimming Dashboard - Final Project ------------------------------

# load packages
library(shiny)
library(bslib)
library(tidyverse)
library(lubridate)

# load data and do some tidying / data wrangling
swim_data <- read_csv("data/olympic_swimming.csv") |>
  janitor::clean_names() |>
  filter(team != "Unified Team",
         results != "Disqualified",
         results != "Did not start",
         results != "Did not finish") |>
  mutate(event = paste(distance_in_meters,stroke)) |>
  mutate(
    results_clean = str_trim(results),  # remove whitespace
    results_seconds = case_when(
      # case 1: the time is already in seconds (i.e. 51.97 seconds)
      str_detect(results_clean, "^[0-9]+\\.?[0-9]*$") ~ as.numeric(results_clean),
      
      # case 2: M:SS(.xx) → pad with 00: (i.e. 15:44.47 minutes)
      str_detect(results_clean, "^[0-9]+:[0-9]{2}(\\.[0-9]+)?$") ~ 
        as.numeric(hms(paste0("00:", results_clean))),
      
      # case 3: HH:MM:SS(.xx) or H:MM:SS(.xx) (i.e. 00:01:09.87 minutes)
      str_detect(results_clean, "^[0-9]+:[0-9]{2}:[0-9]{2}(\\.[0-9]+)?$") ~ 
        as.numeric(hms(results_clean)),
      TRUE ~ NA_real_
    )
  ) |>
  select(-distance_in_meters, -stroke, -results, -results_clean)

# get a list of the countries in the dataset
country_list <- swim_data |>
  pull(team) |>
  unique()

# create unique theme for the dashboard
my_theme <- bs_theme(
  bootswatch = "cerulean",  # optional
  primary = "#0072B2",
  secondary = "#FFD700",
  base_font = font_google("Roboto"),
  heading_font = font_google("Montserrat")
) %>%
  bs_add_rules(
    "
    h2, h4 {
      color: black !important;  /* force black headers */
    }
    "
  )

# display the user interface to aesthetically display the data 
ui <- page_fluid(
  
  theme = my_theme,
  h2("United States Olympic Swimming from 1912-2020",
     style = "text-align:center; margin-bottom: 20px;"),
  
  # create a tab that shows the individual event performance trends over time
  navset_tab(
    nav_panel(
      "Individual Event Times",
      page_sidebar(
        title = NULL,
        sidebar = sidebar(
          helpText("Select the swimming event and gender to see the progression of the fastest Olympic time by an American from 1912 to 2020"),
          selectInput(
            inputId = "event",
            label = "Choose a Swim Event",
            choices = c(
              "50m Freestyle","100m Freestyle","200m Freestyle","400m Freestyle",
              "800m Freestyle","1500m Freestyle","100m Backstroke","200m Backstroke",
              "100m Butterfly","200m Butterfly","100m Breaststroke","200m Breaststroke",
              "200m Individual medley","400m Individual medley"   # <- note exact spelling
            ),
            selected = "50m Freestyle"
          ),
          radioButtons(
            inputId = "gender",
            label = "Choose a Gender",
            choices = c("Female" = "Women", "Male" = "Men"),
            selected = "Women"
          ),
          tags$img(
            src = "olympic_logo.png",  
            width = "100%",           
            alt = "Olympic Logo"
        )),
        layout_column_wrap(
          width = 1,
          card(
            h4("Fastest Individual Event Time Progression"),
            plotOutput("plot", height = "500px"),
            style = "border: 2px solid #0072B2; border-radius: 10px;"
          ),
          card(
            h4("Fastest Individual Event Times"),
            tableOutput("results_table"),
            height = "250px",
            style = "border: 2px solid #0072B2; border-radius: 10px;"
          )
        )
      )
    ),
    
    # create a tab panel that shows the American medal trends over time
    nav_panel(
      "Medal Count",
      page_sidebar(
        title = NULL,
        sidebar = sidebar(
          helpText("Use the slider below to see the medal counts of Americans during different eras. The total count is broken down by gender to see which years men outperformed women and vice versa."),
          sliderInput(
            inputId = "year_range",
            label = "Select Year Range:",
            min = 1912,
            max = 2020,
            value = c(1912, 2020),  
            step = 4,               
            sep = ""                
          ),
          tags$img(
            src = "medal.png",
            width = "30%",             
            style = "display: block;   
           margin-left: auto;
           margin-right: auto;
           margin-top: 10px;", 
            alt = "Medal Logo"
        )),
        card(
          h4("Total Medal Trends"),
          plotOutput("medal_plot"),
          style = "border: 2px solid #0072B2; border-radius: 10px;"
        )
      )
    ),
    
    # create a tab that shows the best individual American performer over time
    nav_panel(
      "Best Individual Performer",
      page_sidebar(
        title = NULL,
        sidebar = sidebar(
          helpText("Choose a gender below to see the 20 top-performing US swimmers by total Olympic medals won."),
          radioButtons(
            inputId = "mvp_gender",  
            label = "Choose a Gender",
            choices = c("Female" = "Women", "Male" = "Men"),
            selected = "Women"
          ),
          tags$img(
            src = "katie.png",
            width = "100%",             
            style = "display: block;   
           margin-left: auto;
           margin-right: auto;
           margin-top: 30px;", 
            alt = "Katie Logo"
        ),
        tags$img(
          src = "michael.png",
          width = "100%",             
          style = "display: block;   
           margin-left: auto;
           margin-right: auto;
           margin-top: 30px;", 
          alt = "Michael Logo"
          ),
        tags$img(
          src = "mary.png",
          width = "100%",             
          style = "display: block;   
           margin-left: auto;
           margin-right: auto;
           margin-top: 30px;", 
          alt = "Mary Logo"
        )),
        
        card(
          h4("Best Performers"),
          tableOutput("mvp_table"),
          style = "border: 2px solid #0072B2; border-radius: 10px;"
        )
      )
    ),
    
    # create a tab to display an explanation of the project itself and key insights
    nav_panel(
      "Description of Project",
      card(
        style = "border: 2px solid #0072B2; border-radius: 10px;",
        card_header("Project Overview",
                    style = "font-weight:bold;"),
        card_body(
          "This dashboard communicates three main insights into American Olympic swimming history:
          1) Swimmers' fastest event times, showing performance trends over time
          2) Medal counts by gender, revealing the demographoc of American dominance across the Olympics
          3) Top performers across the Olympics to highlight individual excellence.
          The widgets like dropdowns, radio buttons, and sliders allow users explore the data at their own pace and highlight trends that may have gone unnoticed in a tradtional graph.")
      ),
      card(
        style = "border: 2px solid #0072B2; border-radius: 10px;",
        card_header("Individual Event Times",
                    style = "font-weight:bold;"),
        card_body(
          "This interactive line chart shows how American swimmers' fastest times in Olympic events have improved since 1912, broken down by event and gender.
          While the overall trends show a decrease in time, meaning Americans have gotten faster over the years, the progress is not always linear.
          There are certain Olympic years where times do not improve, especially in shorter events where significant time drops are rare, but overall, the times have become more competitive.
          This could be due to a variety of factors such as more racing technology, different training methods, or a greater interest in athletics.
          The dropdown for events let users compare across all swimming disciplines and distances to determine trends within different subgroups.
          Additionally, the radio buttons for gender makes it easy to see how performance trends differ between men and women.
          The table underneath the plot complements the line chart with exact times, years, and athletes' names, which allows for more detailed insights to the fastest times."
        )
      ),
      card(
        style = "border: 2px solid #0072B2; border-radius: 10px;",
        card_header("Medal Count",
                    style = "font-weight:bold;"),
        card_body(
          "This barchart displays medal trends for US swimmers across the Olympics, with male vs. female comparisons over time.
          It appears the men initially dominated medal counts, but women's contributions grew dramatically after the 1970s, showing the increase of female performance in the US.
          Additionally, the US team gathered the most medals around the 1970s, showing their global dominance in that era.
          Recently, the US has struggled to reach the peak dominance they displayed, as seen in the lower medal counts post-2012.
          This reveals the increase in competition on the global scale, especially from countries like Australia, Russia, and China.
          The year-range slider lets users explore different historical eras and color-coding by gender makes it visually obvious which gender had more success in a given Olympic Games."
        )
      ),
      card(
        style = "border: 2px solid #0072B2; border-radius: 10px;",
        card_header("Best Individual Performer",
                    style = "font-weight:bold;"),
        card_body(
          "This table ranks the top 20 American swimmers by total Olympic medals won, separated by gender.
          This aims to highlight legends like Katie Ledecky, Michael Phelps, and Mary T. Meagher and show the dominance of certain Americans on the global scale.
          Furthermore, this table shows the depth of US swimming that has allowed victory on the medal table for the past nine Olympics.
          The radio buttons for gender provides gender-specific lists, allowing for comparisons between the best male and female swimmers.
          The athlete photos in the sidebar acts to make the dashboard more engaging and add a human connection to the stats. 
          Overall, this table is mostly for my own curiosity, and gives credit to athletes who will go down in United States history."
        )
      ),
      card(
        style = "border: 2px solid #0072B2; border-radius: 10px;",
        card_header("Citation",
                    style = "font-weight:bold;"),
        card_body(
          markdown("The dataset comes directly from Kaggle by user Data Science Donut ([https://www.kaggle.com/datasets/datasciencedonut/olympic-swimming-1912-to-2020](https://www.kaggle.com/datasets/datasciencedonut/olympic-swimming-1912-to-2020)).
                   Additionally, this data was originally sourced from [https://www.olympics.com/en/olympic-games/tokyo-2020/results/swimming/men-s-100m-backstroke](https://www.olympics.com/en/olympic-games/tokyo-2020/results/swimming/men-s-100m-backstroke).
                   Generative AI was used to troubleshoot specific areas of the app, specifically inputting images, creating a custom theme, and changing the font of the app."
        ))
      )
    )
    
  ))

server <- function(input, output) {
  
  output$plot <- renderPlot({
    req(input$event, input$gender)

    plot_data <- swim_data |>
      filter(event == input$event,
             gender == input$gender,
             team == "USA") |>
      group_by(year, gender) |>
      summarize(fastest_time = min(results_seconds, na.rm = TRUE), .groups = "drop") |>
      arrange(year)

    validate(need(nrow(plot_data) > 0, "No data for that selection."))

    ggplot(plot_data, aes(x = year, y = fastest_time, color = gender)) +
      geom_line(linewidth = 1) +
      geom_point(size = 3) +
      scale_color_manual(values = c("Women" = "pink", "Men" = "lightblue")) +
      labs(
        title = paste("Fastest Olympic Times for", input$gender, input$event, sep = " "),
        x = NULL,
        y = "Time (seconds)"
      ) +
      theme_minimal() +
      theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5))
  })

  output$results_table <- renderTable({
    req(input$event, input$gender)
    
    swim_data |>
      filter(event == input$event,
             gender == input$gender,
             team == "USA") |>
      group_by(year) |>
     
      slice_min(results_seconds, n = 1, with_ties = FALSE) |>
      ungroup() |>
      arrange(year) |>
      mutate(year = as.integer(year)) |>
      select(year, athlete, results_seconds) |>
      rename(
        "Year" = year,
        "Athlete" = athlete,
        "Time (s)" = results_seconds
      )
  
  })
  
  
  output$medal_plot <- renderPlot({
    
    medal_data <- swim_data |>
      filter(team == "USA", rank <= 3,
             year >= input$year_range[1],
             year <= input$year_range[2]) |>
      group_by(year, gender) |>
      summarise(medal_count = n(), .groups = "drop") |>
      arrange(year)
      
      ggplot(medal_data, aes(x = factor(year), y = medal_count, fill = gender)) +
        geom_col(position = "dodge") +  # dodge = side-by-side columns
        scale_fill_manual(values = c("Women" = "pink", "Men" = "lightblue")) +
        labs(
          title = "USA Olympic Swimming Medal Count by Gender",
          x = "Year",
          y = "Number of Medals",
          fill = "Gender"
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              plot.title = element_text(hjust = 0.5))
    })
  
  output$mvp_table <- renderTable({
    req(input$mvp_gender)
    
    swim_data |>
      filter(team == "USA",
             gender == input$mvp_gender,
             rank <= 3,
             athlete != "NA") |>   
      mutate(athlete = case_when(
        athlete %in% c("Gary Jr. Hall", "Gary Hall, Jr.") ~ "Gary Hall, Jr.",
        TRUE ~ athlete
      )) |>
      group_by(athlete) |>
      summarise(Medals = n(), .groups = "drop") |>
      arrange(desc(Medals)) |>
      head(20) |>
      rename(
        "Athlete" = athlete,
        "Medals Won" = Medals
      ) 
  })

}

shinyApp(ui = ui, server = server)
