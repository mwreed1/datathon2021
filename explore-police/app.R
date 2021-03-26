#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
Sys.setenv(TZ="America/New_York")

library(shiny)
library(tidyverse)
library(sf)
library(lobstr)

police_shape <- st_read("data/Raleigh_Police_Incidents_(NIBRS).shp")
nc <- st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
wake <- nc%>%filter(NAME=="Wake")
sv <- st_read("social_vuln/Overall_2014_Tracts.shp") %>% 
    filter(STATE == "North Carolina",
           COUNTY == "Wake")
census <- st_read("Census_Tracts_2010/Census_Tracts_2010.shp")
calculate_mode <- function(x) {
    uniqx <- unique(na.omit(x))
    uniqx[which.max(tabulate(match(x, uniqx)))]
}
police_shape <- police_shape %>%
    filter(!st_is_empty(geometry))

nc <- st_transform(nc, crs = st_crs(police_shape))

# summarizes by finding the most frequent of the cat vars
cts <- census %>%
    st_join(st_transform(police_shape, st_crs(census))) %>%
    filter(!is.na(TRACTCE10),
           !is.na(OBJECTID.y)) %>%
    group_by(TRACTCE10) %>%
    summarize(n = n(),
              freq_crime_cat = calculate_mode(crime_cate),
              freq_crime_code = calculate_mode(crime_code),
              freq_crime_desc = calculate_mode(crime_desc))

new_cts <- st_transform(cts %>% mutate(st_cast(., "MULTIPOLYGON")), 
                        st_crs(sv))


# Define UI for application that draws a histogram
ui <- fluidPage(

    navbarPage(title = "Wake County Police",
               
               tabPanel(title = "data exploration",
                        
                        sidebarLayout(
                            sidebarPanel(
                               
                                sliderInput(inputId = "date",
                                            label   = "Filter for a range of dates:",
                                            min     = as.Date("01/01/2014", "%m/%d/%Y"), 
                                            max     = as.Date("12/31/2021", "%m/%d/%Y"),
                                            ticks   = FALSE,
                                            value   = c(as.Date("01/01/2014", "%m/%d/%Y"), 
                                                        as.Date("12/31/2021", "%m/%d/%Y"))
                                )
                               
                            ),
                            
                            mainPanel(
                                plotOutput(outputId = "map_plot"),
                                plotOutput(outputId = "ts_plot")
                            )
                        )
               ),
               
               tabPanel(title = "SVI",
                        
                        sidebarLayout(
                            sidebarPanel(
                                
                                sliderInput(inputId = "date",
                                            label   = "Filter for a range of dates:",
                                            min     = as.Date("01/01/2014", "%m/%d/%y"), 
                                            max     = as.Date("12/31/2021", "%m/%d/%y"),
                                            ticks   = FALSE,
                                            value   = c(as.Date("01/01/2014", "%m/%d/%y"), 
                                                        as.Date("12/31/2021", "%m/%d/%y"))
                                ),
                                
                                 selectInput(
                                     inputId     = "term",
                                     label       = "Choose variable to search",
                                     choices = c("Pct Minority" = "EPL_MINRTY", 
                                                 "Pct above 25 w/o a HS diploma" = "E_NOHSDP/E_TOTPOP", 
                                                 "Pct unemployed" = "E_UNEMP/E_TOTPOP", 
                                                 "Pct under the poverty line" = "E_POV/E_TOTPOP", 
                                                 "Pct under the age of 17" = "E_AGE17/E_TOTPOP"),
                                     selectize=FALSE
                                 )
                              
                            ),
                            
                            mainPanel(
                                plotOutput(outputId = "line_plot"),
                                plotOutput(outputId = "sum_plot")
                            )
                        )
               )
    )
)

server <- function(input, output) {

    output$map_plot <- renderPlot({
        
        police_shape_d <- police_shape %>%
            mutate(date = as.Date(reported_d)) %>%
            filter(date >= input$date[1],
                   date <= input$date[2])
            
        police_shape_d %>%
                ggplot() +
                geom_sf(data = wake) +
                geom_sf(aes(alpha = 0.1, color = district)) +
                labs(title = "police inspections") 
        
    })
    
output$ts_plot <- renderPlot({
    
    police_shape_d <- police_shape %>%
        mutate(date = as.Date(reported_d)) %>%
        filter(date >= input$date[1],
               date <= input$date[2])
    
    police_shape_d %>%
        count(date) %>%
        ggplot(aes(x=date, y=n)) +
        geom_line() +
        geom_point() +
        geom_smooth(se=F) +
        geom_vline(xintercept=as.Date("03/13/2020", 
                                      format="%m/%d/%Y"), 
                   color = "red")
})

output$line_plot <- renderPlot({
    

    sv %>%
        st_zm(drop = TRUE, what = "ZM") %>%
        st_join(new_cts, join = st_overlaps) %>%
        ggplot(aes_string(x = "n", y = input$term[[1]])) +
        geom_point() +
        geom_smooth(se = F) +
        labs(x = "number of crimes in a census tract", 
             y = names(input$term))
})

output$sum_plot <- renderPlot({
    
    sv %>%
        st_zm(drop = TRUE, what = "ZM") %>%
        st_join(new_cts, join = st_overlaps) %>%
        ggplot() +
        geom_sf(data = census) +
        geom_sf(aes_string(fill = input$term[[1]])) +
        labs(title = "Wake County")
})

}

# Run the application 
shinyApp(ui = ui, server = server)
