# College Exploration Shiny App ---------------------------------------------

# This application implements the college exploration shiny application
# built for Statistics Practicum (MATH 343) at Reed College in Spring 2019.

# EJ Arce, Simon P. Couch, and Alexander Moore

# Loading packages --------------------------------------------------------
library(shiny)
library(tidyverse)
library(ggrepel)
library(DT)
library(openintro)
library(qgraph)

# Load in datasets --------------------------------------------------------

# colleges_shiny is a version of colleges_mrc with nicer looking 
# column names and character values
load("colleges_shiny.Rda")
# colleges_mrc is our primary dataset
load("colleges_mrc.Rda")
# shiny_codebook is the data codebook for colleges_mrs
load("shiny_codebook.Rda")
# the plotting section works more intuitively with a dataset sorted by name
colleges_plot <- colleges_shiny %>%
  arrange(School_Name)

# Preliminary Functions ------------------------------------------------

# This function puts together the IC_PCA dataframe based on the 
# reference school and subset of schools chosen. IC_PCA is a
# version of colleges_mrc, where variables representing similar themes
# are collapsed using principal component analysis
build_ic_pca <- function(ref_school = "Reed College", 
                         subset = rep(TRUE, nrow(colleges_mrc)), 
                         global) {
    
    # "df", a subsetted version of colleges_mrc, is the dataframe we'll 
    # be working with in this function
    df <- colleges_mrc[subset,]
  
    # Tidying parts of the data
    df$room_board <- as.numeric(df$room_board)
    df$dorm_capacity <- as.numeric(df$dorm_capacity)
    df$in_state_tuition <- as.numeric(df$in_state_tuition)
    df$out_state_tuition <- as.numeric(df$out_state_tuition)
    df$highest_degree <- as.numeric(df$highest_degree)
    
    # Put together a function that imputes missing values with the
    # median of the relevant column
    median_impute <- function(df) {
        
        for (i in 4:dim(df)[[2]]) {
            df[[i]] <- as.numeric(df[[i]])
            df[[i]][is.na(df[[i]])] <- median(df[[i]][!is.na(df[[i]])])
        }
        
        df
        
    }

    df <- suppressWarnings(median_impute(df))
    
    # convert any columns of all NAs to all zeroes
    isna <- apply(df, 2, is.na)
    tots <- apply(isna, 2, sum)
    for (i in 1:ncol(df)) {
      if (tots[i] != 0) {
        df[i] <- rep(0, nrow(df))
      }
    }
    
    # add cryptographically small noise to every entry so that PCA won't error 
    # out on constant columns when the subsets are small or homogenous
    random_mat <- matrix(rnorm(prod(dim(df)), sd =1e-9), nrow = nrow(df))
    
    df[3:ncol(df)] <- df[3:ncol(df)] + random_mat[,3:ncol(df)]
    
    df <- select(df, -c(state, percent_out_state))
    
    if (global) {
      assign("df", df, envir = .GlobalEnv)
    }
    
    # Institutional Size Features
    
    # make dataframe with just numeric size columns
    size_df <- select(df, school_size, dorm_capacity, total_enrolled, 
                      male_enrolled, female_enrolled, native_enrolled, asian_enrolled, black_enrolled, 
                      hispanic_enrolled, pacific_enrolled, white_enrolled, mixed_enrolled, 
                      first_years_enrolled)
    size_df$school_size <- as.numeric(size_df$school_size)
    
    # run pca on that dataframe
    size_pca <- prcomp(size_df, center = TRUE, scale. = TRUE)
    # find principcal component that explains the most variance to approximate size
    size_axis <- size_pca$x[,1]
    
    # Cost Features
    
    # make dataframe with just numeric cost columns
    cost_df <- select(df, room_board, pct_fin_aid, avg_fin_aid, in_state_tuition, out_state_tuition)
    # run pca on that dataframe
    cost_pca <- prcomp(cost_df, center = TRUE, scale. = TRUE)
    # find principcal component that explains the most variance to approximate size
    cost_axis <- cost_pca$x[,1]
    
    # Exclusivity Features
    
    # make dataframe with just numeric cost columns
    exclu_df <- select(df, act_25, act_75, prop_admit)
    # run pca on that dataframe
    exclu_pca <- prcomp(exclu_df, center = TRUE, scale. = TRUE)
    # find principcal component that explains the most variance to approximate size
    exclu_axis <- exclu_pca$x[,1]
    
    # Mobility Features
    
    # make dataframe with just numeric cost columns
    mobil_df <- select(df, kq5_cond_parq1, ktop1pc_cond_parq1, mr_kq5_pq1, 
                       mr_ktop1_pq1, trend_parq1, trend_bottom40)
    # run pca on that dataframe
    mobil_pca <- prcomp(mobil_df, center = TRUE, scale. = TRUE)
    # find principcal component that explains the most variance to approximate size
    mobil_axis <- mobil_pca$x[,1]
    
    # Diversity Features
    
    # make dataframe with just numeric cost columns
    diver_df <- select(df, prop_male, prop_female, prop_native, prop_black, 
                       prop_white, prop_mixed, prop_asian, prop_hispanic, prop_pacific)
    # run pca on that dataframe
    diver_pca <- prcomp(diver_df, center = TRUE, scale. = TRUE)
    # find principcal component that explains the most variance to approximate size
    diver_axis <- diver_pca$x[,1]
    
    # Education Characteristics
    
    # make dataframe with just numeric cost columns
    edu_df <- select(df, highest_degree, student_faculty_ratio, prop_prof, prop_instr, 
                     control, part_time_grad, part_time_undergrad, 
                     retention_rate)
    # note: full_time_undergrad was removed because it contains zero variance
    # run pca on that dataframe
    edu_pca <- prcomp(edu_df, center = TRUE, scale. = TRUE)
    # find principcal component that explains the most variance to approximate size
    edu_axis <- edu_pca$x[,1]
    
    # build IC_PCA from the axes resulting from PCA. Note that the 
    # distance axis isn't calculated yet--it requires reference schools
    IC_PCA <- data.frame(size_axis, cost_axis, exclu_axis, mobil_axis, diver_axis, 
                         edu_axis)
    
    IC_PCA
    
}


# User Interface (Frontend) ------------------------------------------------
ui <- fluidPage(
    
    # Use a serif font, but otherwise leave style minimal
    theme = "style.css",
    
    # Change the color of the sliders
    tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #678c6b}")),
    tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: #678c6b}")),
    tags$style(HTML(".js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar {background: #678c6b}")),
    tags$style(HTML(".js-irs-3 .irs-single, .js-irs-3 .irs-bar-edge, .js-irs-3 .irs-bar {background: #678c6b}")),
    tags$style(HTML(".js-irs-4 .irs-single, .js-irs-4 .irs-bar-edge, .js-irs-4 .irs-bar {background: #678c6b}")),
    tags$style(HTML(".js-irs-5 .irs-single, .js-irs-5 .irs-bar-edge, .js-irs-5 .irs-bar {background: #678c6b}")),
    tags$style(HTML(".js-irs-6 .irs-single, .js-irs-6 .irs-bar-edge, .js-irs-6 .irs-bar {background: #678c6b}")),
    
    # Add a title
    titlePanel("College Exploration App"),
    
    # Add a navigation bar for plot and table tabs. We abuse the conventional
    # 80 character limit here to somewhat preserve argument indentation
    navbarPage("",
               tabPanel("Basic Plotting",
                        # This sidebar is where the user can make plot choices
                        fluidRow(
                            sidebarLayout(
                                sidebarPanel(
                                    # The "title" of the sidebar
                                    p("Choose any college characteristics you're interested in!"),
                                    
                                    # A dropdown menu for the x-axis
                                    selectInput(inputId = "x",
                                                label = "x-axis:",
                                                # We want our options to be all numeric variables, displayed
                                                # with underscores replaced by spaces
                                                choices = structure(colnames(colleges_plot[, unlist(lapply(colleges_plot, is.numeric))]),
                                                                    names = str_replace_all(colnames(colleges_plot[, unlist(lapply(colleges_plot, 
                                                                                                                                    is.numeric))]),
                                                                                            "_",
                                                                                            " ")),
                                                selected = "Longitude"),
                                    
                                    # A dropdown menu for the y-axis
                                    selectInput(inputId = "y",
                                                label = "y-axis:",
                                                # We want our options to be all numeric variables, displayed
                                                # with underscores replaced by spaces
                                                choices = structure(colnames(colleges_plot[, unlist(lapply(colleges_plot, 
                                                                                                            is.numeric))]),
                                                                    names = str_replace_all(colnames(colleges_plot[, unlist(lapply(colleges_plot, 
                                                                                                                                    is.numeric))]),
                                                                                            "_",
                                                                                            " ")),
                                                selected = "Latitude"),
                                    
                                    # A dropdown menu for the size mapping
                                    selectInput(inputId = "size_plot",
                                                label = "Size:",
                                                # We want a third variable to be limited to numeric variables
                                                # displayed by a size mapping
                                                choices = structure(colnames(colleges_plot[, unlist(lapply(colleges_plot, 
                                                                                                            is.numeric))]),
                                                                    names = str_replace_all(colnames(colleges_plot[, unlist(lapply(colleges_plot, 
                                                                                                                                    is.numeric))]),
                                                                                            "_",
                                                                                            " ")),
                                                selected = "No_Selection"),
                                    # A dropdown menu for the color mapping
                                    selectInput(inputId = "color",
                                                label = "Color:",
                                                # This color variable can use almost any variable in colleges_plot
                                                # ---the column types will come into play later
                                                choices = structure(colnames(colleges_plot),
                                                                    names = str_replace_all(colnames(colleges_plot),
                                                                                            "_",
                                                                                            " ")),
                                                selected = "No_Selection"),
                                    
                                    # A dropdown menu for the schools to highlight
                                    selectizeInput(inputId = "school",
                                                   label = "School:",
                                                   choices = sort(colleges_plot$School_Name),
                                                   selected = "Reed College",
                                                   options = list(maxItems = 10,
                                                                  placeholder = "Select up to 10 schools!")),
                                    # Some help text
                                    p("Click, drag, and then double click to zoom in! Double click 
                 without selecting anything to return to the original zoom.")),
                                
                                # The main panel of the page is the ggplot
                                mainPanel(
                                    plotOutput("plot",
                                               # observe a double click
                                               dblclick = "plot1_dblclick",
                                               # observe when the mouse hovers over something
                                               hover = hoverOpts("plot1_hover", 
                                                                 # only check every 300ms
                                                                 delay = 300),
                                               # observe click and drag
                                               brush = brushOpts(id = "plot1_brush",
                                                                 resetOnNew = TRUE)),
                                    # Some text describing the school being hovered over
                                    p(textOutput("hover_text"))))), # the end of fluidRow
                        
                        # add another fluidRow to tabPanel containing the data table
                        fluidRow(
                            DT::dataTableOutput("colleges_table"))
               ),
               
               # The new tab for the similarity scoring section. Again, we abuse
               # the 80 character limit to somewhat preserve indentation
               tabPanel("Similarity Scoring",
                        fluidRow(
                            sidebarLayout(
                                sidebarPanel(
                                    # The "title" of the sidebar
                                    helpText("Choose a school of reference."),
                                    
                                    # A dropdown menu for the x-axis
                                    selectInput(inputId = "school_name",
                                                label = "School",
                                                # We want our options to be all numeric variables, displayed
                                                # with underscores replaced by spaces
                                                choices = sort(colleges_mrc$school_name),
                                                selected = "Reed College"),
                                    
                                    # Add all of the slider inputs, set by default to 
                                    # weight each school characteristic with medium priority
                                    helpText("Adjust the sliders below from 0 (low priority) to 1
                                             (high priority) based on what institutional
                                             characteristics are important to you."),
                                    
                                    sliderInput("size_sim_score", "Size:",
                                                min = 0, max = 1,
                                                value = .5),
                                    helpText("Size of the student population."),
                                    
                                    sliderInput("cost", "Cost:",
                                                min = 0, max = 1,
                                                value = .5),
                                    helpText("Cost of attendance and financial aid."),
                                    
                                    sliderInput("exclusivity", "Exclusivity:",
                                                min = 0, max = 1,
                                                value = .5),
                                    helpText("Selectivity of the school."),
                                    
                                    sliderInput("mobility", "Mobility:",
                                                min = 0, max = 1,
                                                value = .5),
                                    helpText("Social mobility of graduates."),
                                    
                                    sliderInput("diversity", "Diversity:",
                                                min = 0, max = 1,
                                                value = .5),
                                    helpText("Diversity of the student body."),
                                    
                                    sliderInput("education", "Educational Experience:",
                                                min = 0, max = 1,
                                                value = .5),
                                    helpText("Class size, number of programs, etc."),
                                    
                                    sliderInput("location", "Location:",
                                                min = 0, max = 1,
                                                value = .5),
                                    helpText("Geographical distance."),
                                    
                                    # We chose to isolate the above sliders because of
                                    # how computationally intensive this process is.
                                    # Thus, there's a "Go" button for when the
                                    # user has all of the sliders set to where they want.
                                    helpText("Press \"Go!\" when you're ready."), 
                                    actionButton(inputId = "go_button",
                                                 label = "Go!")
                                ),
                                # in the main panel, plot the network visualization
                                # above the similarity scoring table
                                mainPanel(
                                  plotOutput("network_viz"),
                                  DT::dataTableOutput("sim_score_table"))
                                  
                            )
                        )
               ),
               # add a tab panel that just prints the codebook
               tabPanel("Codebook",
                          tableOutput("codebook")
               ),
               # add another tab panel that prints some background on the project
               tabPanel("About",
                        span(textOutput("about"), style = "font-size: 20px")
                        )
               
    ) # the end of navBarPage
) # the end of fluidPage


# Server (Backend) ----------------------------------------------------------
server <- function(input, output){
  
  # Basic Plotting Section --------------------------------------------------
  
  # a list of values that should react as the result of inputs
  reactive_vals <- reactiveValues(data = colleges_plot,
                                  text_labels = geom_text_repel(data = subset(colleges_plot,
                                                                              School_Name %in% "Reed College"),
                                                                label = "Reed College",
                                                                color = "#678c6b",
                                                                size = 5.5,
                                                                family = "Times"),
                                  colors = "#678c6b",
                                  x_lims = NULL,
                                  y_lims = NULL,
                                  hoverinfo = "Hover over a dot to show basic 
                                               information about that school!",
                                  label_color = "#678c6b")
  
  
  # Build the plot from the chosen variables
  output$plot <- renderPlot({
    
    if (input$color == "No_Selection") {
      # If there's no color input, just plot the x and y and
      # highlight the chosen schools in green
      base_axes <- ggplot(data = reactive_vals$data,
                          mapping = aes_string(x = input$x, 
                                               y = input$y))
      
      chosen_colors <- geom_point(data = subset(reactive_vals$data, 
                                                School_Name %in% input$school),
                                  color = "#678c6b",
                                  na.rm = TRUE)
      
    } else {
      # Otherwise, plot the chosen color variable and use the color mapping 
      # to determine dot color
      base_axes <- ggplot(data = reactive_vals$data,
                          mapping = aes_string(x = input$x, 
                                               y = input$y,
                                               color = input$color))

      chosen_colors <- NULL
    }
    
      # we want to map the ranks of the size variable for more
      # informative plotting (raw values are accessible by
      # both hovering and the interactive table)
      rank_size <- reactive_vals$data %>%
                   select(input$size_plot) %>%
                   unlist() %>%
                   unname() %>%
                   rank()
    
      # Plot the chosen x and y variables
      base_axes + 
      # Plot the size variable with a rank transformation
      geom_jitter(data = reactive_vals$data,
                  aes(size = rank_size), 
                  na.rm = TRUE) +
      # Add a label to all colleges if there are < 20 and just the
      # chosen schools otherwise
      reactive_vals$text_labels + 
      # Change the colors of the chosen schools too unless there
      # there is a color aesthetic chosen
      chosen_colors +
      # Some plot aesthetic adjustments
      theme_minimal() +
      theme(text = element_text(family = "serif"),
            axis.text = element_text(size = 14),
            panel.grid.minor = element_blank(),
            axis.title = element_text(size = 20)) +
      # Replace underscores with spaces
      labs(x = str_replace_all(input$x, "_", " "), 
           y = str_replace_all(input$y, "_", " ")) +
      coord_cartesian(xlim = reactive_vals$x_lims, 
                      ylim = reactive_vals$y_lims, 
                      expand = FALSE) +
      # Make the scale of the size of the dots wider
      scale_size(range = c(.1, 2.2)) +
      # Only include the color legend
      guides(size = FALSE,
             color = guide_legend(title = NULL,
                                  label.position = "right"))
      
  }) # the end of the renderPlot call
  
  # Update the school labels when the school input changes
  observeEvent(input$school, {
    # If there is a color mapping chosen, make the label of the chosen
    # schools the same as the rest
    if (input$color == "No_Selection") {
      reactive_vals$label_color <- "#678c6b"
    } else {
      reactive_vals$label_color <- "black"
    }
    
    reactive_vals$text_labels <- geom_text_repel(data = subset(reactive_vals$data,
                                                               School_Name %in% input$school),
                                                 label = sort(input$school),
                                                 color = reactive_vals$label_color,
                                                 size = 5.5,
                                                 family = "Times")
  })
  
  # Basic Plotting: On Double Click ------------------------------------------
  
  observeEvent(input$plot1_dblclick, {
    # On double click, zoom plot to drag range (or back to
    # the original position if no range is selected)
    brush <- input$plot1_brush
    
    if (!is.null(brush)) {
      
      # Subset the data based on the chosen points
      reactive_vals$data <- brushedPoints(colleges_plot, input$plot1_brush)
      
      # Take note of the new axis ranges
      reactive_vals$x_lims <- c(brush$xmin, brush$xmax)
      reactive_vals$y_lims <- c(brush$ymin, brush$ymax)
      
    } else {
      
      # Use all of the data
      reactive_vals$data <- colleges_plot
      
      # Make the axis ranges the whole range of the data
      reactive_vals$x_lims <- NULL
      reactive_vals$y_lims <- NULL
    }
    
    # We would also like to check the number of points in the
    # current pane with the new limits. If there are very few, we'd like
    # to display the college name associated with each dot
    n_dots <- reactive_vals$data %>% nrow()
    
    if (input$color == "No_Selection") {
      reactive_vals$label_color <- "#678c6b"
    } else {
      reactive_vals$label_color <- "black"
    }
    
    if (n_dots < 20) {
      
      # Add a text layer with all college names
      
      # Find where the input school is in the reactive data
      chosen_indices <- which(reactive_vals$data$School_Name %in% input$school)

      # ... as well as the length of the reactive data
      length_data <- nrow(reactive_vals$data)
      
      # now, we want to make a vector of colors, where the color at the 
      # index of the chosen schools is green, unless there is a color aesthetic
      # chosen, and the rest are black.
      if (length_data != 0) {
        # make a vector of length_data of "black", and then replace the
        # relevant indices with green
        colors <- rep("black", length_data)
        for (i in chosen_indices) {
          colors[i] <- reactive_vals$label_color
        }
      } else {
        # In this case, if there is no data, we don't need to supply any labels
        colors <- NULL
      }
      
      reactive_vals$colors <- colors
      
      if (!is.null(colors)) {
        # store the repelled text layer as a reactive object
        reactive_vals$text_labels <- geom_text_repel(data = reactive_vals$data,
                                                     label = reactive_vals$data$School_Name,
                                                     color = colors,
                                                     size = 5.5,
                                                     family = "Times")
      } else {
        # if there aren't any colors to show the labels with, there
        # shouldn't be any labels
        reactive_vals$text_labels <- NULL
      }
      
    } else {
      # The number of dots is greater than 20, so
      # don't display text labels for points except the chosen schools
      reactive_vals$text_labels <- geom_text_repel(data = subset(reactive_vals$data,
                                                                 School_Name %in% input$school),
                                                   label = sort(input$school),
                                                   color = reactive_vals$label_color,
                                                   size = 5.5,
                                                   family = "Times")
    }
    
  })
  
  # Basic Plotting: On Hover ------------------------------------------------
  
  observeEvent(input$plot1_hover, {
    # On hover, check if the cursor is super close to any point. if so,
    # give some extra information on the school
    if (!is.null(input$plot1_hover)) {
      
      # xlims and ylims are null by default...
      # if they haven't been messed with, set them to
      # the range of the lat/lons
      if (!is.null(reactive_vals$xlims)) {
        x_range <- abs(diff(reactive_vals$xlims))
        y_range <- abs(diff(reactive_vals$ylims))
      } else {
        x_range <- abs(diff(range(colleges_plot$Longitude)))
        y_range <- abs(diff(range(colleges_plot$Latitude)))
      }
      
      # Rename the hover object so it's easier to work with
      hover <- input$plot1_hover
      
      # Record the location of the cursor
      x <- hover$x 
      y <- hover$y 
      
      # Extract the chosen columns
      x_chosen <- select(reactive_vals$data, input$x) %>%
        unlist() %>%
        unname()
      
      y_chosen <- select(reactive_vals$data, input$y) %>%
        unlist() %>%
        unname()
      
      # Find the index of the point closest to the cursor
      distance <- (abs(x - x_chosen) / x_range) + (abs(y - y_chosen) / y_range)
      closest_index <- which.min(distance)
      
      # If theres not a point in the pane, then set the
      # distance to the chosen school so far away that the app
      # won't try to do anything with it
      if (nrow(reactive_vals$data) == 0) {
        closest_index <- 1
        distance[1] <- 1e6
      }
      
      # An arbitrary constant denoting how close the cursor has to be to
      # to the point to be worth showing
      sensitivity <- .1
      
      # The default string to display if the mouse isn't near a point
      null_hover_info <- "Hover over a dot to show basic 
                          information about that school!"
      
      # If the mouse is close enough to a point, display information about that
      # point. otherwise, don't display anything.
      if (distance[closest_index] < sensitivity) {
        # Make a vector containing basic information about the school
        # thats being hovered over
        reactive_vals$hoverinfo <- reactive_vals$data[closest_index,] %>% 
          dplyr::select(School_Name,
                        Total_Enrollment,
                        School_Type,
                        Location_Type,
                        State) %>%
          mutate(School_Type = as.character(School_Type),
                 Location_Type = as.character(Location_Type),
                 State = as.character(State))
        as.character()
      } else {
        reactive_vals$hoverinfo <- null_hover_info
      }
    } else {
      reactive_vals$hoverinfo <- null_hover_info
    }
    
    # If the hover data isn't the default, put together a sentence
    # containing some basic info about the school!
    if (reactive_vals$hoverinfo[1] != null_hover_info) {
      output$hover_text <- renderText({paste("Your mouse is hovering near ", 
                                             reactive_vals$hoverinfo[1], ", a ", 
                                             tolower(reactive_vals$hoverinfo[3]), 
                                             " school located in a ",
                                             tolower(reactive_vals$hoverinfo[4]),
                                             " in ", 
                                             abbr2state(reactive_vals$hoverinfo[5]),
                                             " with an 
                                             undergraduate population of ",
                                             reactive_vals$hoverinfo[2], ".",
                                             sep = "")})
    } else {
      output$hover_text <- renderText({null_hover_info})
    }
})
    
    # Put together a table with raw values of the dataset
    output$colleges_table <- DT::renderDataTable({
      
      
      # Select and filter the plotted dataframe based on the
      # selected variables and schools
      colleges_shiny_table <- reactive_vals$data %>%
        select(School_Name, State, School_Type, Carnegie_Classification,
               input$x, input$y, input$size_plot, -c("No_Selection")) %>%
        filter(School_Name %in% input$school)

      # Display not chosen schools below the chosen ones so that they're
      # still in the table. arrange by closeness to the mean x
      # value of the chosen schools.
      colleges_shiny_table_mean_x <- reactive_vals$data %>%
        filter(School_Name %in% input$school) %>%
        select(input$x) %>%
        pull()
      
      colleges_shiny_table_mean_x <- mean(colleges_shiny_table_mean_x)
      
      # Pull out all of the not chosen schools
      colleges_shiny_table_not_chosen <- reactive_vals$data %>%
        select(School_Name, State, School_Type, Carnegie_Classification,
               input$x, input$y, input$size_plot, -c("No_Selection")) %>%
        filter(!(School_Name %in% input$school))
      
      not_chosen_x_vals <- colleges_shiny_table_not_chosen %>% 
        select(input$x) %>%
        pull()
      
      # Take their difference in x value with the mean of the chosen schools
      diff_x <- abs(not_chosen_x_vals - colleges_shiny_table_mean_x)
      
      # Arrange the table of not chosen schools by closeness in x value
      # to the chosen schools
      colleges_shiny_table_not_chosen <- cbind(colleges_shiny_table_not_chosen,
                                               diff_x) %>%
        arrange(diff_x) %>%
        select(-diff_x)
      
      # ...and row bind them!
      colleges_shiny_table <- rbind(colleges_shiny_table,
                                    colleges_shiny_table_not_chosen)
      
       # Output the data as an interactive table
      DT::datatable(colleges_shiny_table,
                    options = list(
                        pageLength = 10
                    ),
                    rownames = FALSE,
                    # replace underscores with spaces
                    colnames = str_replace_all(colnames(colleges_shiny_table),
                                               "_",
                                               " "),
                    filter = "top")
    })
    
    # Similarity Score Tab ---------------------------------------------------
    
    # Only run things when the go button gets pressed--this process is really
    # computationally expensive, so the inputs are set to not trigger
    # reactions by themselves
    observeEvent(input$go_button, {  
        
        # Put together the IC_PCA dataframe on all schools
        IC_PCA <- build_ic_pca(ref_school = isolate(input$school_name),
                               global = TRUE)
        
        # Put together the reactive table
        output$network_viz <-  renderPlot({  
            
            # The distance column is the only one in the
            # IC_PCA dataframe that depends on the input, so we need to
            # put it together inside the reactive setting
            user_spec_PCA_distance <- function(input_name, 
                                               ic_pca = IC_PCA, 
                                               data = colleges_mrc, 
                                               subset = FALSE) {
                
                # If subset is the default value, then use the
                # whole dataframe to make the distance
                if (!(TRUE %in% subset)) {
                  subset <- rep(TRUE, nrow(data))
                }
              
                # These are empty vectors that will eventually contain the 
                # information on the similarity scores
                spec_dist_feature <- rep(NA, nrow(data))
                max_theme <- rep(NA, nrow(data))
                second_max_theme <- rep(NA, nrow(data))
              
                # Geographic distance for location feature
                make_geo_distance <- function(ref_school, df) {
                    geo_dist_feature <- rep(NA, nrow(df))
                    
                    rownum <- match(ref_school, df$school_name)
                    long_col <- which( colnames(df) == "longitude")
                    lat_col <- which( colnames(df) == "latitude" )
                    
                    
                    for (i in 1:nrow(df)) {
                        geo_dist_feature[i] <- sqrt( (df[rownum,long_col] - df[i, long_col])^2 + 
                                                         (df[rownum, lat_col] - df[i, lat_col])^2 )
                    }
                    geo_dist_feature <- geo_dist_feature[subset]
                    
                    scale(geo_dist_feature)
                }
                
                
                # Make the location axis (that depends on the school chosen)
                location_axis <- make_geo_distance(isolate(input$school_name),
                                                   df)
                
                # Attach the location axis onto the premade ic_pca dataframe
                IC_PCA <- cbind(ic_pca, 
                                location_axis)
                
                # Find the row number of the reference school
                row_num <- match(input_name, 
                                 isolate(data$school_name))
                
                # Isolate each variable outside of the for loop so
                # that they are only isolated once (runs sloooow otherwise)
                iso_size_sim_score <- isolate(input$size_sim_score)
                iso_cost <- isolate(input$cost)
                iso_exclusive <- isolate(input$exclusivity)
                iso_mobility <- isolate(input$mobility)
                iso_diversity <- isolate(input$diversity)
                iso_education <- isolate(input$education)
                iso_location <- isolate(input$location)
                
                # Put together a vector of similarity scores by calculating 
                # euclidean distance on the combined PCA axes reference school 
                # and all others, scaled by user preference inputs
                for (i in 1:nrow(data)) {
                  
                  if (data$school_name[i] != input_name) {
                  
                  # Make the similarity scores
                  spec_dist_feature[i] <- sqrt(
                    iso_size_sim_score*(IC_PCA[row_num, 1]- IC_PCA[i, 1])^2 +
                      iso_cost*(IC_PCA[row_num, 2]- IC_PCA[i, 2])^2 + 
                      iso_exclusive*(IC_PCA[row_num, 3]- IC_PCA[i, 3])^2 + 
                      iso_mobility*(IC_PCA[row_num, 4]- IC_PCA[i, 4])^2 + 
                      iso_diversity*(IC_PCA[row_num, 5]- IC_PCA[i, 5])^2 + 
                      iso_education*(IC_PCA[row_num, 6]- IC_PCA[i, 6])^2 + 
                      iso_location*(IC_PCA[row_num, 7]- IC_PCA[i, 7])^2
                      )
                  
                  # Make nice-looking names for the themes
                  themes <- c("Size", "Cost", "Exclusivity", "Mobility", "Diversity", 
                              "Education", "Location")
                  
                  # Rank the contribution of each theme to the similarity score
                  ranked_themes <- rank(c(iso_size_sim_score*(IC_PCA[row_num, 1]- IC_PCA[i, 1])^2,
                                          iso_cost*(IC_PCA[row_num, 2]- IC_PCA[i, 2])^2, 
                                          iso_exclusive*(IC_PCA[row_num, 3]- IC_PCA[i, 3])^2,
                                          iso_mobility*(IC_PCA[row_num, 4]- IC_PCA[i, 4])^2,
                                          iso_diversity*(IC_PCA[row_num, 5]- IC_PCA[i, 5])^2, 
                                          iso_education*(IC_PCA[row_num, 6]- IC_PCA[i, 6])^2, 
                                          iso_location*(IC_PCA[row_num, 7]- IC_PCA[i, 7])^2))
                  
                  # Pull out the second and third most important themes
                  max_theme[i] <- themes[ranked_themes == 1]
                  second_max_theme[i] <- themes[ranked_themes == 2]
                  
                  } else {
                    # The current index is the chosen school, so the similary is 100%
                    spec_dist_feature[i] <- 0
                    max_theme[i] <- NA
                    second_max_theme[i] <- NA
                  }
                  
                  
                }
                
                # Return the similarity scores and most relevant themes
                data.frame(spec_dist_feature = spec_dist_feature, 
                           max_theme = max_theme, 
                           second_max_theme = second_max_theme)
            }
            
            # Build list of unscaled sim scores
            results_frame <- user_spec_PCA_distance(isolate(input$school_name))
            
            # Scale to be (0,100) where 100 is the school (100% similar)
            range01 <- function(x) {
              (x - min(x)) / (max(x) - min(x))
            }
            
            rescore <- function(score_vec) {
              score_vec <- range01(score_vec)
              score_vec <- 1-score_vec
              score_vec <- 100*score_vec
            }
            
            results_frame$spec_dist_feature <- rescore(results_frame$spec_dist_feature)
            
            # Now, make the table that we'll display in the app
            table <- data.frame(Name = colleges_mrc$school_name,
                                Similarity_Score = plyr::round_any(results_frame$spec_dist_feature, .01),
                                State = colleges_shiny$State,
                                Type = colleges_shiny$School_Type,
                                Carnegie_Classification = colleges_shiny$Carnegie_Classification,
                                Most_Similar_Trait = results_frame$max_theme, 
                                Second_Most_Similar_Trait = results_frame$second_max_theme)
            
            # Just display the schools that aren't the chosen one, arranged
            # by how similar they are to the chosen one. This will eventually be
            # displayed, but ignore it for now to be thrown into a
            # renderDataTable call later
            table <- table %>%
              arrange(desc(Similarity_Score)) %>%
              slice(., 1:nrow(.))
            
            assign("table", table, envir = .GlobalEnv)
            
            # Just select the top 10 schools
            top_10_table <- table %>%
              slice(1:10)
            
            # Make a vector indicating which schools in colleges_mrc were in
            # the top 10 most similar to the reference school
            top_10 <- colleges_mrc$school_name %in% top_10_table$Name
            
            # Build another ic_pca with just the 10 chosen schools
            small_ic_pca <- build_ic_pca(ref_school = isolate(input$school_name),
                                         subset = top_10,
                                         global = FALSE)
            
            # We now want to find pairwise comparisons between each school in
            # the top 10 most similar schools to the reference school
            pairwise_similarities <- lapply(top_10_table$Name,
                                            user_spec_PCA_distance,
                                            ic_pca = small_ic_pca,
                                            data = colleges_mrc[top_10,],
                                            subset = top_10)
            
            # We now have a list with 10 elements where each is a 3x10 dataframe 
            # representing the closenesses to the 10 relevant (1 in
            # each will be 0. We would like to convert this to a distance
            # matrix, where zeroes are along the diagonal. However, these
            # entries are out of order. First, extract the spec_dist_feature
            # column from each dataframe.
            similarities <- rep(list(NA), 10)
            for (i in 1:length(pairwise_similarities)) {
              similarities[[i]] <- pairwise_similarities[[i]]$spec_dist_feature
            }
            
            similarity_matrix <- matrix(nrow = 10, ncol = 10)
            for (i in 1:10) {
              # the entry with its zero at index i should be row i in 
              # the resulting similarity matrix
              zero_index <- which(similarities[[i]] == 0)
              similarity_matrix[,zero_index] <- similarities[[i]]
            }
            
            # Pull out the names of the top 10 schools
            top_10_names <- colleges_mrc[top_10,]$school_name
              
            # We'd like to make the shortest possible versions of these names
            # that will still be unique identifiers. Try to find a substring
            # of the school name that is neither "University" or "College." To
            # do so, get rid of those two, as well as some other stop words.
            stop_words <- c(" University", "University ", "of ")
            
            top_10_labels <- str_remove(top_10_names, " University")
            top_10_labels <- str_remove(top_10_labels, "University ")
            top_10_labels <- str_remove(top_10_labels, " College")
            top_10_labels <- str_remove(top_10_labels, "College ")
            top_10_labels <- str_remove(top_10_labels, "of ")
            
            print(similarity_matrix)
            
            # Graph the matrix as a network and send it to the UI!
            qgraph(exp(similarity_matrix*.25), 
                   labels = top_10_labels,
                   palette = "pastel",
                   edge.color = "#497c4f",
                   edge.width = .3,
                   esize = 23)
            }) # the end of render plot
        
            # Sooo... that table made on the way to the network viz?
            output$sim_score_table <-  DT::renderDataTable({ 
              # display it below the network viz
              DT::datatable(table,
                            rownames = FALSE,
                            colnames = str_replace_all(colnames(table),
                                                       "_",
                                                       " "),
                            options = list(paging = TRUE,
                                           searching = TRUE))
            }) # the end of render data table
            
       
    }) # the end of observe event
    
    output$codebook <- renderTable({shiny_codebook})
    
    output$about <- renderText({
      "This shiny app was built by EJ Arce, Simon Couch, and Alexander Moore 
      for Reed College's Statistics Practicum course in Spring 2019. The goal of
      our project is to provide prospective students as well as insitutional
      researchers a tool for comparing and learning about colleges using an
      approach that is both data-centered and deemphasizes hierarchy. There are
      two main deliverables this application provides. The first one is a 
      scatterplot tool that allows the user to visualize 2-4 specific 
      institutional features they may be interested in. The second one 
      implements a model we built that attempts to 
      most objectively quantify the degree of similarity between any
      two schools. The model allows the user to weigh the institutional
      characteristics they deem most important to them, presenting a
      list of options catered to their preferences when evaluating colleges
      The third tab on the shiny app describes the source of the data used for
      this project and explicitly describes what each variable means. Data on
      institutional features about the school are obtained from the Integrated
      Postsecondary Education Data System (IPEDS) 2016 surveys while data on 
      social mobility are obtained from Opportunity Insights' Mobility 
      Report Card. A full report detailing our methods is available at
      https://github.com/Reed-Statistics/College-Exploration"}
      )
    
}

# Run the app
shinyApp(ui = ui, server = server)
