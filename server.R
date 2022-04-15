# Server file for shiny
shinyServer(function(input,output) {
    # select cities
    output$typeSelectOutput <- renderUI({
        selectInput("typeInput","Select Country/(s):",
                    sort(unique(survey$Country)),
                    multiple = TRUE,
                    selected = c("United States","United Kingdom","Canada"))
    })
    
    # formulate data with selected year
    data_reactive <- reactive({
        data_React <- data %>% filter(year >= input$year[1],
                                      year <= input$year[2])
        if(nrow(data_React)==0){
            return(NULL)
        }
        data_React
    })
    
    ####### survey formulate data with selected year
    data_reactive2 <- reactive({
        data_React2 <- survey %>% filter(year >= input$year[1],
                                         year <= input$year[2])
        if(nrow(data_React2)==0){
            return(NULL)
        }
        data_React2
    })
    
    # formulate data with selected cities
    city_reactive <- reactive({
        data_React <- data %>% filter(year >= input$year[1],
                                      year <= input$year[2]) %>% filter(city %in% input$typeInput)
        if(nrow(data_React)==0){
            return(NULL)
        }
        data_React 
    })
    
    ####### survey formulate data with selected cities
    city_reactive2 <- reactive({
        data_React2 <- survey %>% filter(year >= input$year[1],
                                      year <= input$year[2]) %>% filter(Country %in% input$typeInput) %>%
                                        filter(Gender %in% input$genders) %>%
                                        filter(between(Age, input$ages[1],input$ages[2]))
        if(nrow(data_React2)==0){
            return(NULL)
        }
        data_React2 
    })
    
    output$cmpCountry <- renderPlot({
        plot <- ggplot(city_reactive2()) + 
            xlab(paste0(input$questions)) + 
            ylab("Percent")
        
        if(input$questions == "family_history"){
        plot = plot + aes(x= family_history,  group=Country) + 
            geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
            geom_text(aes( label = scales::percent(..prop..),
                           y= ..prop.. ), stat= "count", vjust = -.5) +
            facet_grid(~Country) +
            scale_y_continuous(labels = scales::percent) + 
            ylab("Percent") + xlab(paste0(input$questions)) + ggtitle("Comparison of the Countries")
        }
        else if(input$questions == "treatment"){
            plot = plot + aes(x= treatment,  group=Country) + 
                geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
                geom_text(aes( label = scales::percent(..prop..),
                               y= ..prop.. ), stat= "count", vjust = -.5) +
                facet_grid(~Country) +
                scale_y_continuous(labels = scales::percent) + 
                ylab("Percent") + xlab(paste0(input$questions)) + ggtitle("Comparison of the Countries")
        }
        else if(input$questions == "benefits") {
            plot = plot + aes(x= benefits,  group=Country) + 
                geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
                geom_text(aes( label = scales::percent(..prop..),
                               y= ..prop.. ), stat= "count", vjust = -.5) +
                facet_grid(~Country) +
                scale_y_continuous(labels = scales::percent) + 
                ylab("Percent") + xlab(paste0(input$questions)) + ggtitle("Comparison of the Countries")
        }
        else if(input$questions == "work_interfere") {
            plot = plot + aes(x= work_interfere,  group=Country) + 
                geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
                geom_text(aes( label = scales::percent(..prop..),
                               y= ..prop.. ), stat= "count", vjust = -.5) +
                facet_grid(~Country) +
                scale_y_continuous(labels = scales::percent) + 
                ylab("Percent") + xlab(paste0(input$questions)) + ggtitle("Comparison of the Countries")
        }
        else if(input$questions == "mental_health_consequence") {
            plot = plot + aes(x= mental_health_consequence,  group=Country) + 
                geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
                geom_text(aes( label = scales::percent(..prop..),
                               y= ..prop.. ), stat= "count", vjust = -.5) +
                facet_grid(~Country) +
                scale_y_continuous(labels = scales::percent) + 
                ylab("Percent") + xlab(paste0(input$questions)) + ggtitle("Comparison of the Countries")
        }
        else {
            plot = plot + aes(x= mental_vs_physical,  group=Country) + 
                geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
                geom_text(aes( label = scales::percent(..prop..),
                               y= ..prop.. ), stat= "count", vjust = -.5) +
                facet_grid(~Country) +
                scale_y_continuous(labels = scales::percent) + 
                ylab("Percent") + xlab(paste0(input$questions)) + ggtitle("Comparison of the Countries")
        }
        
        plot
    })
    
    output$cmpSize <- renderPlot({
        
        if(input$questions == "family_history"){ 
        grp_data <- city_reactive2() %>% group_by(no_employees, family_history) %>% summarise(count=n())
        
        plot <- ggplot(grp_data) + 
            xlab(paste0(input$questions)) + 
            ylab("Percent")
        
        plot = plot + aes(fill= family_history , x=count
                                        , y=reorder(no_employees,(count))) +
            geom_bar(position="stack", stat="identity") + ylab("Size of the company") + 
            ggtitle("Comparison on the Size of Organization")
        }
        else if(input$questions == "treatment"){ 
            grp_data <- city_reactive2() %>% group_by(no_employees, treatment) %>% summarise(count=n())
            
            plot <- ggplot(grp_data) + 
                xlab(paste0(input$questions)) + 
                ylab("Percent")
            
            plot = plot + aes(fill= treatment , x=count
                              , y=reorder(no_employees,(count))) +
                geom_bar(position="stack", stat="identity") + ylab("Size of the company") + 
                ggtitle("Comparison on the Size of Organization")
        }
        else if(input$questions == "work_interfere"){ 
            grp_data <- city_reactive2() %>% group_by(no_employees, work_interfere) %>% summarise(count=n())
            
            plot <- ggplot(grp_data) + 
                xlab(paste0(input$questions)) + 
                ylab("Percent")
            
            plot = plot + aes(fill= work_interfere , x=count
                              , y=reorder(no_employees,(count))) +
                geom_bar(position="stack", stat="identity") + ylab("Size of the company") + 
                ggtitle("Comparison on the Size of Organization")
        }
        else if(input$questions == "benefits"){ 
            grp_data <- city_reactive2() %>% group_by(no_employees, benefits) %>% summarise(count=n())
            
            plot <- ggplot(grp_data) + 
                xlab(paste0(input$questions)) + 
                ylab("Percent")
            
            plot = plot + aes(fill= benefits , x=count
                              , y=reorder(no_employees,(count))) +
                geom_bar(position="stack", stat="identity") + ylab("Size of the company") + 
                ggtitle("Comparison on the Size of Organization")
        }
        else if(input$questions == "mental_health_consequence"){ 
            grp_data <- city_reactive2() %>% group_by(no_employees, mental_health_consequence) %>% summarise(count=n())
            
            plot <- ggplot(grp_data) + 
                xlab(paste0(input$questions)) + 
                ylab("Percent")
            
            plot = plot + aes(fill= mental_health_consequence , x=count
                              , y=reorder(no_employees,(count))) +
                geom_bar(position="stack", stat="identity") + ylab("Size of the company") + 
                ggtitle("Comparison on the Size of Organization")
        }
        else { 
            grp_data <- city_reactive2() %>% group_by(no_employees, mental_vs_physical) %>% summarise(count=n())
            
            plot <- ggplot(grp_data) + 
                xlab(paste0(input$questions)) + 
                ylab("Percent")
            
            plot = plot + aes(fill= mental_vs_physical , x=count
                              , y=reorder(no_employees,(count))) +
                geom_bar(position="stack", stat="identity") + ylab("Size of the company") + 
                ggtitle("Comparison on the Size of Organization")
        }
        
        
        
        plot
    })
    
    output$cmpSize2 <- renderPlot({
        
        grp_data <- city_reactive2() %>% filter(tech_company=="Yes") 
        
        if(input$questions == "family_history"){
        grp_data <- grp_data %>% group_by(family_history) %>% summarise(n=n())

        grp_data2 <- grp_data %>% 
            arrange(desc(family_history)) %>%
            mutate(prop = n / sum(grp_data$n) *100) %>%
            mutate(ypos = cumsum(prop)- 0.5*prop )
        
        plot <- ggplot(grp_data2) +xlab(paste0(input$questions)) +ylab("Percent")
        
        plot = plot + aes(x="", y=prop, fill=family_history) + geom_bar(stat="identity", width=1, color="white") +
            coord_polar("y", start=0) + theme_void() + geom_text(aes(y = ypos, label = paste0(round(prop,2),"%") ), color = "white", size=6)  +
            ylab("Size of the company") + ggtitle("Comparison for Tech Company")
        }
        else if(input$questions == "treatment"){
            grp_data <- grp_data %>% group_by(treatment) %>% summarise(n=n())
            
            grp_data2 <- grp_data %>% 
                arrange(desc(treatment)) %>%
                mutate(prop = n / sum(grp_data$n) *100) %>%
                mutate(ypos = cumsum(prop)- 0.5*prop )
            
            plot <- ggplot(grp_data2) +xlab(paste0(input$questions)) +ylab("Percent")
            
            plot = plot + aes(x="", y=prop, fill=treatment) + geom_bar(stat="identity", width=1, color="white") +
                coord_polar("y", start=0) + theme_void() + geom_text(aes(y = ypos, label = paste0(round(prop,2),"%") ), color = "white", size=6)  +
                ylab("Size of the company") + ggtitle("Comparison for Tech Company")
        }
        else if(input$questions == "work_interfere"){
            grp_data <- grp_data %>% group_by(work_interfere) %>% summarise(n=n())
            
            grp_data2 <- grp_data %>% 
                arrange(desc(work_interfere)) %>%
                mutate(prop = n / sum(grp_data$n) *100) %>%
                mutate(ypos = cumsum(prop)- 0.5*prop )
            
            plot <- ggplot(grp_data2) +xlab(paste0(input$questions)) +ylab("Percent")
            
            plot = plot + aes(x="", y=prop, fill=work_interfere) + geom_bar(stat="identity", width=1, color="white") +
                coord_polar("y", start=0) + theme_void() + geom_text(aes(y = ypos, label = paste0(round(prop,2),"%") ), color = "white", size=6)  +
                ylab("Size of the company") + ggtitle("Comparison for Tech Company")
        }
        else if(input$questions == "benefits"){
            grp_data <- grp_data %>% group_by(benefits) %>% summarise(n=n())
            
            grp_data2 <- grp_data %>% 
                arrange(desc(benefits)) %>%
                mutate(prop = n / sum(grp_data$n) *100) %>%
                mutate(ypos = cumsum(prop)- 0.5*prop )
            
            plot <- ggplot(grp_data2) +xlab(paste0(input$questions)) +ylab("Percent")
            
            plot = plot + aes(x="", y=prop, fill=benefits) + geom_bar(stat="identity", width=1, color="white") +
                coord_polar("y", start=0) + theme_void() + geom_text(aes(y = ypos, label = paste0(round(prop,2),"%") ), color = "white", size=6)  +
                ylab("Size of the company") + ggtitle("Comparison for Tech Company")
        }
        else if(input$questions == "mental_health_consequence"){
            grp_data <- grp_data %>% group_by(mental_health_consequence) %>% summarise(n=n())
            
            grp_data2 <- grp_data %>% 
                arrange(desc(mental_health_consequence)) %>%
                mutate(prop = n / sum(grp_data$n) *100) %>%
                mutate(ypos = cumsum(prop)- 0.5*prop )
            
            plot <- ggplot(grp_data2) +xlab(paste0(input$questions)) +ylab("Percent")
            
            plot = plot + aes(x="", y=prop, fill=mental_health_consequence) + geom_bar(stat="identity", width=1, color="white") +
                coord_polar("y", start=0) + theme_void() + geom_text(aes(y = ypos, label = paste0(round(prop,2),"%") ), color = "white", size=6)  +
                ylab("Size of the company") + ggtitle("Comparison for Tech Company")
        }
        else {
            grp_data <- grp_data %>% group_by(mental_vs_physical) %>% summarise(n=n())
            
            grp_data2 <- grp_data %>% 
                arrange(desc(mental_vs_physical)) %>%
                mutate(prop = n / sum(grp_data$n) *100) %>%
                mutate(ypos = cumsum(prop)- 0.5*prop )
            
            plot <- ggplot(grp_data2) +xlab(paste0(input$questions)) +ylab("Percent")
            
            plot = plot + aes(x="", y=prop, fill=mental_vs_physical) + geom_bar(stat="identity", width=1, color="white") +
                coord_polar("y", start=0) + theme_void() + geom_text(aes(y = ypos, label = paste0(round(prop,2),"%") ), color = "white", size=6)  +
                ylab("Size of the company") + ggtitle("Comparison for Tech Company")
        }
        
        
        
        plot

        
    })

    
    # output data table with selected filter city and year 
    output$results <- DT::renderDataTable(
        city_reactive2(),
        options = list(scrollX = TRUE)
    )
    
    # downloading csv
    output$download <- downloadHandler(
        filename = function() {
            "data.csv"
        },
        content = function(con) {
            write.csv(city_reactive2(), con)
        }
    )
    
    # US map plot
    output$plotusa <- renderPlotly({
        
        plot_data = survey %>% filter(year == 2014) %>% filter(Country == "United States") %>% 
            group_by(state, treatment) %>% 
            count()
        
        #plot_data$click  = with(plot_data,paste(state,'<br>',
        #                                        "Population:",population,'<br>',
        #                                        "Rapes:",rape,'<br>',
        #                                        "Assaults",assaults,'<br>',
        ##                                        "Robberies",robberies,'<br>',
         #                                       "Homicides",homicides,'<br>',
         #                                       "Violent Crimes",violent_crimes))
        
        g <- list(
            scope = 'usa',
            projection = list(type = 'albers usa'),
            lakecolor = toRGB('white')
        )
        
        plot_geo() %>% add_trace(z = plot_data$n, span = I(0),locations = plot_data$state, locationmode = 'USA-states') %>%
            layout(geo = g)
    })
    
    output$click <- renderPrint({
        d <- event_data("plotly_click")
        if (is.null(d)) "Click on a state to view event data" else d
    })
    
    
})