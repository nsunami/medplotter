#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(tidyverse)
library(shape)

# Choices
choice_strings <- c("Establishes temporal precedence", "Does not establish or unclear temporal precedence")
measurement_choices <- c("Measured", "Manipulated")

# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel("medplotter - Plot Mediation"),
  # Sidebar with a slider input for number of bins
  # Show a plot of the generated distribution
  
  sidebarLayout(
    sidebarPanel(
      wellPanel(
        textInput("predictor_string", "Predictor/Independent Variable", "Predictor"),
        radioButtons("predictor_measurement", "Type", measurement_choices),
        radioButtons("path_a_type", "Path connecting predictor to mediator", choice_strings),
        textInput("path_a_coef", "Coefficient", 0.55)
      ),
      wellPanel(
        textInput("mediator_string", "Mediator", "Mediator"),
        radioButtons("mediator_measurement", "Type", measurement_choices),
        radioButtons("path_b_type", "Path connecting mediator to outcome", choice_strings),
        textInput("path_b_coef", "Coefficient", 0.55)
      ),
      wellPanel(
        textInput("outcome_string", "Outcome/Dependent Variable", "Outcome"),
        radioButtons("path_c_type", "Path connecting predictor to outcome", choice_strings),
        textInput("path_c_coef", "Coefficient (direct)", 0.55),
        textInput("path_c_total_coef", "Coefficient (total, c-prime)", 0.55)
      )
    ),
    mainPanel(width = 7,
              plotOutput("plot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  # Render Plot
  output$plot <- renderPlot({    
    # Setup the plotting space ####
    # Size of the screen
    screen_y <- 200
    screen_x <- 572
    
    # Size of the rectangles
    rect_width <- 150
    rect_height <-  72
    
    # X and Y corrdinates for the bottom right hand corners
    predictor_pos <- c(3, screen_y - 135)
    med_pos <- c(212.59, screen_y - 20)
    out_pos <- c(420.85, screen_y - 135)
    poses <- tibble(predictor_pos, med_pos, out_pos)
    
    # Parameters for the arrows 
    # padding for the arrows
    xarrowpad <- 10
    yarrowpad <- 10
    arrowhead_size <- 0.4
    arrowhead_size_small <- 0.2
    line_width <- 3
    line_width_rect <- 3
    measurement_label_size <- 1
    variable_labels_size <- 1.4
    dot_size <- 2
    # offset for the coefficients
    coef_offset <- 10
    
    
    # Making the data frame for posisitions
    # This code can be tidier
    box_positions <- tibble("type" = c("pred", "med", "out"),
                            "xleft" = unlist(poses[1,]),
                            "ytop" = unlist(poses[2,] + rect_height),
                            "xright" = unlist(poses[1,] + rect_width),
                            "ybottom" = unlist(poses[2,])) %>%
      mutate(xlarrow = xleft,
             xmid = (xleft + xright)/2,
             xrarrow = xright,
             ymid = (ybottom + ytop)/2,
             ytoparrow = unlist(poses[2,] + rect_height))
    
    # Draw the plot ####
    # Making the main plot area
    plot(c(0, screen_x), c(0, screen_y + 50), type= "n", xlab = "", ylab = "", axes = FALSE)
    
    # Independent Variable
    pred_positions <- box_positions[1, 2:5]
    
    # This does not run 
    # for(i in nrow(box_positions)){
    #   rect(xleft = box_positions$xleft[i],
    #        ybottom = box_positions$ybottom[i],
    #        xright = box_positions$xright[i],
    #        ytop = box_positions$ytop[i])
    # }
    
    # Box for the independent variable
    rect(xleft = box_positions$xleft[1],
         ybottom = box_positions$ybottom[1],
         xright = box_positions$xright[1],
         ytop = box_positions$ytop[1],
         lwd = line_width_rect)
    
    rect(xleft = box_positions$xleft[2],
         ybottom = box_positions$ybottom[2],
         xright = box_positions$xright[2],
         ytop = box_positions$ytop[2],
         lwd = line_width_rect)
    
    rect(xleft = box_positions$xleft[3],
         ybottom = box_positions$ybottom[3],
         xright = box_positions$xright[3],
         ytop = box_positions$ytop[3],
         lwd = line_width_rect)
    
    
    # Textboxes 
    text((box_positions$xleft[1] + box_positions$xright[1])/2,
         (box_positions$ytop[1] + box_positions$ybottom[1])/2,
         labels = input$predictor_string,
         cex = variable_labels_size)
    
    text((box_positions$xleft[2] + box_positions$xright[2])/2,
         (box_positions$ytop[2] + box_positions$ybottom[2])/2,
         labels = input$mediator_string,
         cex = variable_labels_size)
    
    text((box_positions$xleft[3] + box_positions$xright[3])/2,
         (box_positions$ytop[3] + box_positions$ybottom[3])/2,
         labels = input$outcome_string,
         cex = variable_labels_size)
    
    # Textboxes for measured and manipulated labels #### 
    # Predictor text 
    text((box_positions$xleft[1] + box_positions$xright[1])/2,
         (box_positions$ytop[1] + box_positions$ybottom[1])/2 - 20,
         labels = paste0("(", input$predictor_measurement, ")"),
         cex = measurement_label_size)
    
    # Mediator text
    text((box_positions$xleft[2] + box_positions$xright[2])/2,
         (box_positions$ytop[2] + box_positions$ybottom[2])/2 - 20,
         labels = paste0("(", input$mediator_measurement, ")"),
         cex = measurement_label_size)
    
    # Outcome -- Always Measured
    text((box_positions$xleft[3] + box_positions$xright[3])/2,
         (box_positions$ytop[3] + box_positions$ybottom[3])/2 - 20,
         labels = "(Measured)",
         cex = measurement_label_size)

    # Top and buttom are reversed....
    arrows_df <- combn(box_positions$type, 2) %>% t() %>% as_tibble() %>%
      mutate(type = paste(V1, V2, sep = "2")) %>%
      # with padding
      mutate(x0 = c(box_positions$xmid[1],
                    box_positions$xrarrow[1] + xarrowpad,
                    box_positions$xrarrow[2] + xarrowpad), 
             y0 = c(box_positions$ytoparrow[1] + yarrowpad,
                    box_positions$ymid[1],
                    box_positions$ymid[2]),
             x1 = c(box_positions$xlarrow[2] - xarrowpad,
                    box_positions$xlarrow[3] - xarrowpad,
                    box_positions$xmid[3]),
             y1 = c(box_positions$ymid[2], 
                    box_positions$ymid[3],
                    box_positions$ytoparrow[3] + yarrowpad))
    
    # Texts for the b-weights ####
    # Calculate the degree of the texts 
    # Issue with the degrees
    # path_a_coef_degree <- sin((abs(arrows_df$y0[1] - arrows_df$y1[1]) / 
    #                             sqrt(abs(arrows_df$x0[1] - arrows_df$x1[1])^2 +  abs(arrows_df$y0[1] - arrows_df$y1[1])^2))) * 180 / pi
    path_coef_degree <- sin(abs(arrows_df$y0 - arrows_df$y1) / 
                              sqrt(abs(arrows_df$x0 - arrows_df$x1)^2 +  abs(arrows_df$y0 - arrows_df$y1)^2)) * 180/pi
    
    # Coefficient for Path a
    text((arrows_df$x0[1] + arrows_df$x1[1])/2,
         (arrows_df$y0[1] + arrows_df$y1[1])/2 + coef_offset,
         paste0("b = ", input$path_a_coef), srt = path_coef_degree[1])
    # Coefficient for Path c
    text((arrows_df$x0[2] + arrows_df$x1[2])/2,
         (arrows_df$y0[2] + arrows_df$y1[2])/2 + coef_offset,
         paste0("b = ", input$path_c_coef), srt = path_coef_degree[2])
    # Coefficient for Path c (total)
    text((arrows_df$x0[2] + arrows_df$x1[2])/2,
         (arrows_df$y0[2] + arrows_df$y1[2])/2 - coef_offset * 1.2,
         paste0("(b = ", input$path_c_total_coef, ")"), srt = path_coef_degree[2])
    # Coefficient for Path b (total)
    text((arrows_df$x0[3] + arrows_df$x1[3])/2,
         (arrows_df$y0[3] + arrows_df$y1[3])/2 + coef_offset,
         paste0("b = ", input$path_b_coef), srt = path_coef_degree[3] - 60)
    
    # User-defined fucntion to draw causal arrow from a df
    draw_causal_arrow_df <- function(df, i, lwd = line_width){
      x0 = df$x0[i]
      y0 = df$y0[i]
      x1 = df$x1[i]
      y1 = df$y1[i]
      Arrows(x0 = x0, y0 = y0, x1 = x1, y1 = y1,
             lwd = lwd,
             arr.length = arrowhead_size)
      points(x0, y0, pch = 20, cex = dot_size)
    }
    
    # User-defined function to draw correlational arrows
    draw_correlational_arrow_df <- function(df, i, lwd = line_width,
                                            big_arrowhead = arrowhead_size,
                                            small_arrowhead = arrowhead_size_small){
      x0 = df$x0[i]
      y0 = df$y0[i]
      x1 = df$x1[i]
      y1 = df$y1[i]
      Arrows(x0 = x0, y0 = y0, x1 = x1, y1 = y1,
             arr.length = arrowhead_size,
             lwd = line_width)
      Arrows(x0 = x1, y0 = y1, x1 = x0, y1 = y0,
             arr.length = arrowhead_size_small,
             lwd = line_width)
    }
    
    # Arrow No 1: Arrow connecting the independent variable and the mediator
    if(input$path_a_type == choice_strings[1]){
      draw_causal_arrow_df(arrows_df, 1)
    }
    if(input$path_a_type == choice_strings[2]){
      draw_correlational_arrow_df(arrows_df, 1)
    }
    # Arrow No 2: Arrow connecting the independent variable and the outcome
    if(input$path_c_type == choice_strings[1]){
      draw_causal_arrow_df(arrows_df, 2)
    }
    if(input$path_c_type == choice_strings[2]){
      draw_correlational_arrow_df(arrows_df, 2)
    }
    # Arrow No 3: Arrow connecting the mediator and the outcome
    if(input$path_b_type == choice_strings[1]){
      draw_causal_arrow_df(arrows_df, 3)
    }
    if(input$path_b_type == choice_strings[2]){
      draw_correlational_arrow_df(arrows_df, 3)
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

