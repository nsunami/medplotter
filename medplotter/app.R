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

#
choice_strings <- c("Causal", "Cross-Sectional")
measurement_choices <- c("Measured", "Manipulated")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("medplotter - Plot Mediation"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         textInput("predictor_string", "Predictor/Independent Variable", "Predictor"),
         radioButtons("predictor_measurement", "Measured/Manipulated?", measurement_choices),
         textInput("mediator_string", "Mediator", "Mediator"),
         radioButtons("mediator_measurement", "Measured/Manipulated?", measurement_choices),
         textInput("outcome_string", "Outcome/Dependent Variable", "Outcome"),
         radioButtons("path_a_type", "Path a Type", choice_strings),
         radioButtons("path_b_type", "Path b Type", choice_strings),
         radioButtons("path_c_type", "Path c Type", choice_strings)
         
         

      ),
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("plot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  df <- reactive({
    req(input$predictor_string)
    req(input$mediator_string)
    req(input$mediator_string)
    
    mapped_table <- map_dfc(target_cis(), function(x){
      results <- ci.smd(smd = input$smd,
                        n.1 = input$n1,
                        n.2 = input$n2,
                        conf.level = x)
      return(c("level" = x, 
               "Cohen's d" = results$smd,
               "LL" = results$Lower.Conf.Limit.smd,
               "UL" = results$Upper.Conf.Limit.smd ))})
      })
  
  
  # Variable boxes
  # the 
  
  
   # render plot
   output$plot <- renderPlot({
     # ggplot(data = data.frame(NA)) + 
     #  geom_rect(xmin = 1 , xmax = 2, ymin = 3, ymax = 5,
     #            color = "black") + theme_bw() + 
     #   scale_x_continuous() + 
     #   scale_y_continuous()
     # 
     # n=8
     # ggplot(mtcars[1:n,][order(mtcars$wt[1:n]),], aes(x=wt, y=mpg, xend=lead(wt), yend=lead(mpg))) +
     #   geom_segment(arrow=arrow(type="closed", 
     #                            angle=seq(10,80, length=n), 
     #                            length=unit(seq(8,4,length=n),"mm"))) 
     
     # op <- par(bg = "white")
     # plot(c(100, 250), c(300, 450), type = "n", xlab = "", ylab = "",
     #      main = "2 x 11 rectangles; 'rect(100+i,300+i,  150+i,380+i)'")
     screen_y <- 200
     screen_x <- 572
     
  
     rect_width <- 150
     rect_height <-  72
     
     # X and Y corrdinates for the bottom right hand corners
     predictor_pos <- c(3, screen_y - 135)
     med_pos <- c(212.59, screen_y - 20)
     out_pos <- c(420.85, screen_y - 135)
     poses <- data.frame(predictor_pos, med_pos, out_pos)

     # Making the data frame for posisitions
     # This code can be tidier
     box_positions <- data.frame("type" = c("pred", "med", "out"),
                                 "xleft" = unlist(poses[1,]),
                                 "ybottom" = unlist(poses[2,] + rect_height),
                                 "xright" = unlist(poses[1,] + rect_width),
                                 "ytop" = unlist(poses[2,]))
     
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
            ytop = box_positions$ytop[1])
       
       rect(xleft = box_positions$xleft[2],
            ybottom = box_positions$ybottom[2],
            xright = box_positions$xright[2],
            ytop = box_positions$ytop[2])
       
       rect(xleft = box_positions$xleft[3],
            ybottom = box_positions$ybottom[3],
            xright = box_positions$xright[3],
            ytop = box_positions$ytop[3])
     
       
       # Textboxes 
       text((box_positions$xleft[1] + box_positions$xright[1])/2,
            (box_positions$ytop[1] + box_positions$ybottom[1])/2,
            labels = input$predictor_string)
       
       text((box_positions$xleft[2] + box_positions$xright[2])/2,
            (box_positions$ytop[2] + box_positions$ybottom[2])/2,
            labels = input$mediator_string)
       
       text((box_positions$xleft[3] + box_positions$xright[3])/2,
            (box_positions$ytop[3] + box_positions$ybottom[3])/2,
            labels = input$outcome_string)
       
       # Textboxes for measured and manipulated labels #### 
       # Predictor
       measurement_label_size <- 0.8
       text((box_positions$xleft[1] + box_positions$xright[1])/2,
            (box_positions$ytop[1] + box_positions$ybottom[1])/2 - 20,
            labels = paste0("(", input$predictor_measurement, ")"),
            cex = measurement_label_size)

       # Mediator
       text((box_positions$xleft[2] + box_positions$xright[2])/2,
            (box_positions$ytop[2] + box_positions$ybottom[2])/2 - 20,
            labels = paste0("(", input$mediator_measurement, ")"),
            cex = measurement_label_size)
       
       # Outcome -- Always Measured
       text((box_positions$xleft[3] + box_positions$xright[3])/2,
            (box_positions$ytop[3] + box_positions$ybottom[3])/2 - 20,
            labels = "(Measured)",
            cex = measurement_label_size)
       
       
       # Text boxes for the b-weights
       
       # Arrows
       # Causal Arrows
       # padding for the arrows
       xarrowpad <- 0.01
       arrowhead_size <- 0.25
       arrowhead_size_small <- 0.10
       line_width <- 1.5
       arrow_positions <- box_positions %>% as_tibble() %>%
         mutate(xlarrow = xleft - xarrowpad,
                xmid = (xleft + xright)/2,
                xrarrow = xright + xarrowpad,
                ymid = (ybottom + ytop)/2)
       
       # Arrow connecting the independent variable and the mediator
       # Top and buttom are reversed....
       if(input$path_a_type == choice_strings[1]){
       arrows(x0 = arrow_positions$xmid[1], y0 = arrow_positions$ybottom[1],
             x1 = arrow_positions$xlarrow[2], y1 = arrow_positions$ymid[2],
             length = arrowhead_size,
             lwd = line_width)
         points(arrow_positions$xmid[1], arrow_positions$ybottom[1], pch = 20)
        
       }
       if(input$path_a_type == choice_strings[2]){
         arrows(x0 = arrow_positions$xmid[1], y0 = arrow_positions$ybottom[1],
                x1 = arrow_positions$xlarrow[2], y1 = arrow_positions$ymid[2],
                length = arrowhead_size,
                lwd = line_width)
         arrows(x0 = arrow_positions$xlarrow[2], y0 = arrow_positions$ymid[2],
                x1 = arrow_positions$xmid[1], y1 = arrow_positions$ybottom[1],
                length = arrowhead_size_small,
                lwd = line_width)
       }
       
       # Arrow connecting the mediator and the outcome
       if(input$path_b_type == choice_strings[1]){
         arrows(x0 = arrow_positions$xrarrow[2], y0 = arrow_positions$ymid[2],
                x1 = arrow_positions$xmid[3], y1 = arrow_positions$ybottom[3],
                length = arrowhead_size,
                lwd = line_width)
         points(arrow_positions$xrarrow[2], arrow_positions$ymid[2], pch = 20)
         
       }
       if(input$path_b_type == choice_strings[2]){
         arrows(x0 = arrow_positions$xrarrow[2], y0 = arrow_positions$ymid[2],
                x1 = arrow_positions$xmid[3], y1 = arrow_positions$ybottom[3],
                length = arrowhead_size,
                lwd = line_width)
         arrows(x0 = arrow_positions$xmid[3], y0 = arrow_positions$ybottom[3],
                x1 = arrow_positions$xrarrow[2], y1 = arrow_positions$ymid[2],
                length = arrowhead_size_small,
                lwd = line_width)
       }
       
       # Arrow connecting the independent variable and the outcome
       if(input$path_c_type == choice_strings[1]){
         arrows(x0 = arrow_positions$xrarrow[1], y0 = arrow_positions$ymid[1],
                x1 = arrow_positions$xlarrow[3], y1 = arrow_positions$ymid[3],
                length = arrowhead_size,
                lwd = line_width)
         points(arrow_positions$xrarrow[1], arrow_positions$ymid[1], pch = 20)
         
       }
       if(input$path_c_type == choice_strings[2]){
         arrows(x0 = arrow_positions$xrarrow[1], y0 = arrow_positions$ymid[1],
                x1 = arrow_positions$xlarrow[3], y1 = arrow_positions$ymid[3],
                length = arrowhead_size,
                lwd = line_width)
         arrows(x0 = arrow_positions$xlarrow[3], y0 = arrow_positions$ymid[3],
                x1 = arrow_positions$xrarrow[1], y1 = arrow_positions$ymid[1],
                length = arrowhead_size_small,
                lwd = line_width)
       }
       
     
     
   })

   
}

# Run the application 
shinyApp(ui = ui, server = server)

