library(shiny)
library(shinydashboard)
library(tidymodels)
library(tidyverse)

model <- readRDS("./models/concrete_xgb_model.rds")

#the calculated specific volumes (m3/kg) for each ingredient
spec_vol <- readRDS("./models/concrete_spec_vol.rds")

# For testing prediction with given values
# test <- tibble("cement" = 275,
#        "blast_furnace_slag" = 20,
#        "fly_ash" = 0,
#        "water" = 185,
#        "superplasticizer" = 5,
#        "coarse_aggregate" = 975,
#        "fine_aggregate" = 775,
#        "age" = 28)

# For testing calculation of specific volume
# test <- tribble(~ingredient, ~amount,
#                "cement", 275,
#                "blast_furnace_slag", 20,
#                "fly_ash", 0,
#                "water", 185,
#                "superplasticizer", 5,
#                "coarse_aggregate", 975,
#                "fine_aggregate", 775) %>%
#     left_join(spec_vol, by = "ingredient") %>%
#     mutate(ingred_vol = amount * specific_volume) %>%
#     pull(ingred_vol) %>%
#     sum


ui <- dashboardPage(
    dashboardHeader(title = "Concrete App"),
    
    dashboardSidebar(
        menuItem(
            "Compressive Strength",
            tabName = "concrete_tab",
            icon = icon("cog")
        )
    ),
    dashboardBody(
        tabItem(
            tabName = "concrete_tab",
            fluidRow(
                column(width = 4,
                       box(sliderInput("v_cement", label = "Cement (kg)",
                                       min = 100, max = 550, value = 275)),
                       box(sliderInput("v_blast_furnace_slag", label = "Blast Furnace Slag (kg)",
                                       min = 0, max = 375, value = 20)),
                       box(sliderInput("v_fly_ash", label = "Fly Ash (kg)",
                                       min = 0, max = 200, value = 0)),
                       box(sliderInput("v_water", label = "Water (kg)",
                                       min = 100, max = 250, value = 185)),
                       box(sliderInput("v_superplasticizer", label = "Super-plasticizer (kg)",
                                       min = 0, max = 35, value = 5)),
                       box(sliderInput("v_coarse_aggregate", label = "Coarse Aggregate (kg)",
                                       min = 800, max = 1150, value = 975)),
                       box(sliderInput("v_fine_aggregate", label = "Fine Aggregate (kg)",
                                       min = 575, max = 1000, value = 775)),
                       box(sliderInput("v_age", label = "Age (days)",
                                       min = 1, max = 365, value = 28)),   
                ),
                
                column(width = 8,
                       valueBoxOutput("concrete_prediction"),
                       valueBoxOutput("concrete_volume")
                ),
            )
        )
    )
)

server <- function(input, output) { 
    
    output$concrete_prediction <- renderValueBox({
        
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
        
        valueBox(
            value = paste0(round(prediction$.pred, 1), "MPa"),
            subtitle = "Compressive Strength",
            color = "light-blue",
            icon = icon("cog")
        )
        
    })
    
    output$concrete_volume <- renderValueBox({
        
        volume <- tribble(~ingredient, ~amount,
               "cement", input$v_cement,
               "blast_furnace_slag", input$v_blast_furnace_slag,
               "fly_ash", input$v_fly_ash,
               "water", input$v_water,
               "superplasticizer", input$v_superplasticizer,
               "coarse_aggregate", input$v_coarse_aggregate,
               "fine_aggregate", input$v_fine_aggregate) %>%
            left_join(spec_vol, by = "ingredient") %>%
            mutate(ingred_vol = amount * specific_volume) %>%
            pull(ingred_vol) %>%
            sum
        
        valueBox(
            value = paste0(round(volume, 1), " m3"),
            subtitle = "Concrete Volume",
            color = "orange",
            icon = icon("cog")
        )
    })
    
}

shinyApp(ui, server)
