# medplotter Shinyapp
# Code written by Nami Sunami (naoyuki.sunami@gmail.com) - http://naoyukisunami.com/
# License: MIT
# MIT License
# 
# Copyright (c) 2019 Naoyuki Sunami
# 
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#   
#   The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

library(shiny)
library(tidyverse)
library(shape)

plot_space_width <- 700
plot_space_height <- 450


# Choices
choice_strings <- c("Establishes temporal precedence", "Does not establish or unclear temporal precedence")
measurement_choices <- c("Measured", "Manipulated")
coef_default_value <- "b = 0.55*"

# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel("medplotter - Plot Mediation"),
  # Sidebar with a slider input for number of bins
  # Show a plot of the generated distribution
  sidebarLayout(sidebarPanel(
                 wellPanel(
                   textInput("predictor_string", "Predictor/Independent Variable", "Predictor"),
                   radioButtons("predictor_measurement", "Type", measurement_choices),
                   radioButtons("path_a_type", "Path connecting predictor to mediator", choice_strings),
                   textInput("path_a_coef", "Coefficient and significance level", coef_default_value)
                 ),
                 wellPanel(
                   textInput("mediator_string", "Mediator", "Mediator"),
                   radioButtons("mediator_measurement", "Type", measurement_choices),
                   radioButtons("path_b_type", "Path connecting mediator to outcome", choice_strings),
                   textInput("path_b_coef", "Coefficient and significance level", coef_default_value)
                 ),
                 wellPanel(
                   textInput("outcome_string", "Outcome/Dependent Variable", "Outcome"),
                   radioButtons("path_c_type", "Path connecting predictor to outcome", choice_strings),
                   textInput("path_c_coef", "Coefficient and significance level after inlcuding the mediator", coef_default_value),
                   textInput("path_c_total_coef", "Coefficient and significance level without the mediator", coef_default_value)
                 ),
                 wellPanel(downloadButton("save", "Download as PNG"))
               ),
      mainPanel(
        plotOutput("plot")
      )
    )
  )


# Define server logic required to draw a histogram
server <- function(input, output) {
  # Render Plot
  output$plot <- renderPlot(width = plot_space_width, height = plot_space_height, {    
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
    xarrowpad <- 1
    yarrowpad <- 1
    arrowhead_size <- 0.4
    arrowhead_size_small <- 0.2
    line_width <- 3
    line_width_rect <- 3
    measurement_label_size <- 1
    variable_labels_size <- 1.4
    dot_size <- 2
    # offset for the coefficients
    coef_offset <- 10
    variable_labels_v_offiset <- 9
    measurement_labels_v_offset <- 20
    
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
    
    current_plot <- function(){
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
         (box_positions$ytop[1] + box_positions$ybottom[1])/2 + variable_labels_v_offiset,
         labels = input$predictor_string,
         cex = variable_labels_size)
    
    text((box_positions$xleft[2] + box_positions$xright[2])/2,
         (box_positions$ytop[2] + box_positions$ybottom[2])/2 + variable_labels_v_offiset,
         labels = input$mediator_string,
         cex = variable_labels_size)
    
    text((box_positions$xleft[3] + box_positions$xright[3])/2,
         (box_positions$ytop[3] + box_positions$ybottom[3])/2 + variable_labels_v_offiset,
         labels = input$outcome_string,
         cex = variable_labels_size)
    
    # Textboxes for measured and manipulated labels #### 
    # Predictor text 
    text((box_positions$xleft[1] + box_positions$xright[1])/2,
         (box_positions$ytop[1] + box_positions$ybottom[1])/2 - measurement_labels_v_offset,
         labels = paste0("(", input$predictor_measurement, ")"),
         cex = measurement_label_size)
    
    # Mediator text
    text((box_positions$xleft[2] + box_positions$xright[2])/2,
         (box_positions$ytop[2] + box_positions$ybottom[2])/2 - measurement_labels_v_offset,
         labels = paste0("(", input$mediator_measurement, ")"),
         cex = measurement_label_size)
    
    # Outcome -- Always Measured
    text((box_positions$xleft[3] + box_positions$xright[3])/2,
         (box_positions$ytop[3] + box_positions$ybottom[3])/2 - measurement_labels_v_offset,
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
    # path_coef_degree <- (asin(abs(arrows_df$y1 - arrows_df$y0) / 
    #                           sqrt(abs(arrows_df$x0 - arrows_df$x1)^2 + 
    #                                  abs(arrows_df$y0 - arrows_df$y1)^2)) * 180)/pi
    # path_coef_degree <- (asin((arrows_df$y1 - arrows_df$y0) /
    #                             # Hypotenuse
    #                           sqrt((arrows_df$x1 + arrows_df$x0)^2 +
    #                                  (arrows_df$y1 - arrows_df$y0)^2)) * 180)/pi
    path_coef_degree <- (asin(abs(arrows_df$y1 - arrows_df$y0) /
                              sqrt(abs(arrows_df$x0 - arrows_df$x1)^2 +
                                     abs(arrows_df$y0 - arrows_df$y1)^2)) * 180)/pi
    
    path_a_str_adjust <- 3
    path_b_str_adjust <- 4
    path_a_str <- path_coef_degree[1] + path_a_str_adjust
    path_b_str <- path_coef_degree[3] + path_b_str_adjust
    
    
    # path_a_str <- path_coef_degree[1]
    # path_b_str <- path_coef_degree[3]
    
    # text(100, 200, path_a_str)
    # text(120, 200, labels = path_a_str)
    
    
    # Coefficient for Path a
    text((arrows_df$x0[1] + arrows_df$x1[1])/2,
         (arrows_df$y0[1] + arrows_df$y1[1])/2 + coef_offset,
         input$path_a_coef, srt = path_a_str)
    
    # Coefficient for Path b
    text((arrows_df$x0[3] + arrows_df$x1[3])/2,
         (arrows_df$y0[3] + arrows_df$y1[3])/2 + coef_offset,
         input$path_b_coef, srt = - path_b_str)
    
    # Coefficient for Path c
    text((arrows_df$x0[2] + arrows_df$x1[2])/2,
         (arrows_df$y0[2] + arrows_df$y1[2])/2 + coef_offset,
         input$path_c_coef, srt = path_coef_degree[2])
    # Coefficient for Path c (total)
    y_offset_total_effect_text <- 1.2
    text((arrows_df$x0[2] + arrows_df$x1[2])/2,
         (arrows_df$y0[2] + arrows_df$y1[2])/2 - coef_offset * y_offset_total_effect_text,
         paste0("(", input$path_c_total_coef, ")"), srt = path_coef_degree[2])

    
    # fucntion to draw causal arrow from a df
    draw_causal_arrow_df <- function(df, i, lwd = line_width,
                                     x0adj = 0, y0adj = 0,
                                     x1adj = 0, y1adj = 0,
                                     xadj = 0, yadj = 0){
      x0 = df$x0[i]
      y0 = df$y0[i]
      x1 = df$x1[i]
      y1 = df$y1[i]
      
      # Shift the small arrow a bit towards the big arrowhead 
      # This is to create a space for the small arrow
      shift_small_arrow <- 1.01
      
      x0 <- x0 + xadj
      y0 <- y0 + yadj
      x1 <- x1
      y1 <- y1
      
      x0_small_arrow <- x0
      y0_small_arrow <- y0
      x1_small_arrow <- x1 - (x1 - x0)/shift_small_arrow
      y1_small_arrow <- y1 - (y1 - y0)/shift_small_arrow
      
      Arrows(x0 = x0,
             y0 = y0,
             x1 = x1,
             y1 = y1,
             lwd = lwd,
             arr.type = "simple",
             arr.length = arrowhead_size,
             arr.adj = 1)
      Arrows(x0 = x0_small_arrow,
             y0 = y0_small_arrow,
             x1 = x1_small_arrow,
             y1 = y1_small_arrow,
             lwd = lwd,
             arr.type = "simple",
             arr.length = arrowhead_size - .1,
             arr.adj = 0.5)
      #points(x0, y0, pch = 20, cex = dot_size)
    }
    
    # function to draw correlational arrows
    draw_correlational_arrow_df <- function(df, i, lwd = line_width,
                                            big_arrowhead = arrowhead_size,
                                            small_arrowhead = arrowhead_size_small,
                                            x0adj = 0, y0adj = 0,
                                            x1adj = 0, y1adj = 0,
                                            xadj = 0){
      x0 = df$x0[i]
      y0 = df$y0[i]
      x1 = df$x1[i]
      y1 = df$y1[i]
      Arrows(x0 = x0, y0 = y0, x1 = x1, y1 = y1,
             arr.length = arrowhead_size,
             lwd = line_width,
             arr.type = "simple",
             arr.adj = 1)
      Arrows(x0 = x1, y0 = y1, x1 = x0, y1 = y0,
             arr.length = arrowhead_size_small,
             lwd = line_width,
             arr.type = "simple",
             arr.adj = 1)
    }
    
    # Adjustment for causal arrows
    yadj_causal1 <- 5
    xadj_causal2 <- 5
    xadj_causal3 <- 6
    
    # Arrow No 1: Arrow connecting the independent variable and the mediator
    if(input$path_a_type == choice_strings[1]){
      draw_causal_arrow_df(arrows_df, 1, yadj = yadj_causal1)
    }
    if(input$path_a_type == choice_strings[2]){
      draw_correlational_arrow_df(arrows_df, 1)
    }
    # Arrow No 2: Arrow connecting the independent variable and the outcome
    if(input$path_c_type == choice_strings[1]){
      draw_causal_arrow_df(arrows_df, 2, xadj = xadj_causal2)
    }
    if(input$path_c_type == choice_strings[2]){
      draw_correlational_arrow_df(arrows_df, 2)
    }
    # Arrow No 3: Arrow connecting the mediator and the outcome
    if(input$path_b_type == choice_strings[1]){
      draw_causal_arrow_df(arrows_df, 3, xadj = xadj_causal3)
    }
    if(input$path_b_type == choice_strings[2]){
      draw_correlational_arrow_df(arrows_df, 3)
    }}
    current_plot()
    output$save <- downloadHandler(
      filename = "save.png" ,
      content = function(file) {
        #ggsave(p(), filename = file)
        png(file = file, width = plot_space_width * 2,
            height = plot_space_height * 2,
            pointsize = 24)
        current_plot()
        dev.off()
      })
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

