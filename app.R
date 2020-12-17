#let's make a version using base Shiny (not shinydashboard)

library(shiny)
library(xgboost)
library(tidymodels)
library(tidyverse)

model <- readRDS("./models/concrete_xgb_model.rds")

# test <- tibble("cement" = 275,
#        "blast_furnace_slag" = 20,
#        "fly_ash" = 0,
#        "water" = 185,
#        "superplasticizer" = 5,
#        "coarse_aggregate" = 975,
#        "fine_aggregate" = 775,
#        "age" = 28)

ui <- fluidPage(
    
    titlePanel("Concrete App"),
    
    sidebarLayout(
        sidebarPanel(
            sliderInput("v_cement", label = "Cement (kg)",
                     min = 100, max = 550, value = 275),
            sliderInput("v_blast_furnace_slag", label = "Blast Furnace Slag (kg)",
                     min = 0, max = 375, value = 20),
            sliderInput("v_fly_ash", label = "Fly Ash (kg)",
                     min = 0, max = 200, value = 0),
            sliderInput("v_water", label = "Water (kg)",
                     min = 100, max = 250, value = 185),
            sliderInput("v_superplasticizer", label = "Super-plasticizer (kg)",
                     min = 0, max = 35, value = 5),
            sliderInput("v_coarse_aggregate", label = "Coarse Aggregate (kg)",
                     min = 800, max = 1150, value = 975),
            sliderInput("v_fine_aggregate", label = "Fine Aggregate (kg)",
                     min = 575, max = 1000, value = 775),
            sliderInput("v_age", label = "Age (days)",
                            min = 1, max = 365, value = 28)  
        ),
        mainPanel(
            h1(textOutput("concrete_prediction"))
        )
    )
)


server <- function(input, output) { 
    
    output$concrete_prediction <- renderText({
        
        prediction <- predict(
            model,
            tibble("cement" = input$v_cement,
                   "blast_furnace_slag" = input$v_blast_furnace_slag,
                   "fly_ash" = input$v_fly_ash,
                   "water" = input$v_water,
                   "superplasticizer" = input$v_superplasticizer,
                   "coarse_aggregate" = input$v_coarse_aggregate,
                   "fine_aggregate" = input$v_fine_aggregate,
                   "age" = input$v_age)
        )
        
        # prediction_prob <- predict(
        #     model,
        #     tibble("island" = input$v_island,
        #            "bill_length_mm" = input$v_bill_length,
        #            "bill_depth_mm" = input$v_bill_depth,
        #            "flipper_length_mm" = input$v_flipper_length,
        #            "body_mass_g" = input$v_body_mass,
        #            "sex" = input$v_sex),
        #     type = "prob"
        # ) %>% 
        #     gather() %>% 
        #     arrange(desc(value)) %>% 
        #     slice(1) %>% 
        #     select(value)
        # 
        # prediction_color <- if_else(prediction$.pred_class == "Adelie", "blue", 
        #                             if_else(prediction$.pred_class == "Gentoo", "red", "yellow"))
        
        # valueBox(
        paste0(round(prediction$.pred, 1), " MPa")
            # subtitle = "Compressive Strength",
            # color = "light-blue",
            # icon = icon("cog")
        # )
        
    })
    
}

shinyApp(ui, server)
