#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(AMR)
library(sf)
library(shiny)
library(shinyWidgets)
library(viridis)
library(shinythemes)
library(ggiraph)
library(certegis)
library(kableExtra)
library(stringr)


data <- read.csv("data_1st.csv", sep=',')
uitgifte <- read.csv("Complete_dataset_antibiotica_NoordNL_vs_ddd.csv", sep=';')
uitgifte$YQ <- paste(uitgifte$jaar, uitgifte$kwartaal, sep = '-')
#uitgifte$YQ <- as.numeric(uitgifte$YQ)

ab_alpha <- sort(colnames(data)[25:100])

options(shiny.sanitize.errors = FALSE)

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("lumen"),
                navbarPage(title = div(
                  img(src = 'logo.png',
                      style = "margin-top: -14px; padding-left:10px; padding-right:10px; padding-bottom:10px",
                      height = 60),
                  img(src = 'payoff.png',
                      style = "margin-top: -14px; padding-left:10px; padding-right:10px; padding-bottom:10px",
                      height = 60)
                ),
               tabPanel('Dashboard', fluid=TRUE,uiOutput('lightpag')),
               #tabPanel('Resistentie', fluid=TRUE,uiOutput('resistentiepag')),
               tabPanel('BRMO', fluid=TRUE, uiOutput('brmopag')),
               #tabPanel('Uitgifte', fluid=TRUE, uiOutput('uitgiftepag')),
               ),
    


hr(),
p(align='center',"DISCLAIMER: Dashboard is in ontwikkeling."),
p(align='center',"Resistentiedata: positieve isolaten van Certe, voormalig Izore en UMCG over de periode 2016-2021."),
p(align='center',"Uitgiftedata: data van het SFK over de periode 2016-2021."),
p(align='center',"Geografische data: Certe en het CBS."),
a(actionButton(inputId = "email1", label = " Vragen of feedback", 
               icon = icon("envelope", lib = "font-awesome")),
  href="mailto:g.danoe@umcg.nl")
)


server <- function(input, output, session) {
  
#-----------RESISTENTIE----------------------------    
  
  
    output$resistentiepag <- renderUI(sidebarLayout(
      sidebarPanel(

        pickerInput(
          'ab',
          'Antibioticum',
          choices=ab_alpha,
          selected = 'Amoxicilline',
          multiple = FALSE,
        ),
        
        hr(),
        p('Bacteriesoort:'),
        pickerInput(
          'gram',
          'Gram',
          choices=unique(data$gram),
          selected = unique(data$gram),
          multiple = TRUE,
          pickerOptions(actionsBox = TRUE),
        ),
        
        pickerInput(
          'genus',
          'Genus',
          choices=unique(data$genus),
          selected = unique(data$genus),
          multiple = TRUE,
          pickerOptions(actionsBox = TRUE),
        ),
        
        pickerInput(
          'species',
          'Species',
          choices=unique(data$species),
          selected = unique(data$species),
          multiple = TRUE,
          pickerOptions(actionsBox = TRUE),
        ),
        hr(),
        
        pickerInput(
          'mtrl',
          'Materiaalgroep',
          choices=unique(data$mtrlgroep),
          selected = unique(data$mtrlgroep),
          multiple = TRUE,
          pickerOptions(actionsBox = TRUE),
        ),
        
        checkboxGroupInput(
          'zorglijn',
          'Zorglijn',
          selected = c('1e lijn', '2e lijn', '3e lijn'),
          inline = 'TRUE',
          choices=c('1e lijn', '2e lijn', '3e lijn'),
        ),
        
        #sliderTextInput('schaal', 'Schaal', choices=c('Postcode 4' = 'postcode4','Postcode 2' = 'postcode2','Gemeente' = 'gemeente','NUTS3' = 'nuts3', 'Provincie' = 'provincie'), selected = 'gemeente'),
        
        radioButtons(
          'schaal',
          'Schaal',
          selected = 'gemeente',
          inline = 'FALSE',
          choiceNames = c('Postcode 4','Postcode 2','Gemeente', 'NUTS 3', 'Provincie'),
          choiceValues = c('postcode','postcode2','gemeente', 'nuts3', 'provincie')
        ),
        
        #selectInput(inputId = 'geslacht', "Geslacht",choices = c('BEIDE', unique(data_1st$geslacht))),
        
        checkboxGroupInput(
          'geslacht',
          'Geslacht',
          selected = c('M','V'),
          inline = 'TRUE',
          choiceNames = c('Man','Vrouw'),
          choiceValues = c('M','V')
        ),
        
        # sliderInput(inputId = 'age', label = 'Leeftijd', min = min(data$leeftijd), max = max(data$leeftijd), value = c(min(data$leeftijd),max(data$leeftijd)), step = NULL, round = FALSE,
        #             ticks = TRUE, animate = FALSE,
        #             width = NULL, sep = ",", pre = NULL, post = NULL, timeFormat = NULL,
        #             timezone = NULL, dragRange = TRUE),
        
        pickerInput(
          'age',
          'Leeftijdsgroep',
          choices=c('0-3','4-11','12-17','18-65','>65'),
          selected = c('0-3','4-11','12-17','18-65','>65'),
          multiple = TRUE,
          pickerOptions(actionsBox = TRUE),
        ),
        
        dateRangeInput(inputId = 'datum', label = 'Periode', start = '2016-01-01', end = '2022-01-03', language = 'nl'),
        
        actionButton("resbutton", "Toepassen"),
      ),
      mainPanel(
        h1("Antibioticaresistentie Noord-Nederland"),
        h4("Verhouding resistente en alle isolaten per gebied"),
        h5("Klik op een gebied om de trend te zien"),
        tabPanel('Resistentie',ggiraphOutput("resistentieplot")),
        hr(),
        h4("Trend antibioticaresistentie van 2016 tot 2021"),
        tabPanel('Resistentie',ggiraphOutput("res_trend")),
        tabPanel('Resistentie',htmlOutput("res_sel")),
        
      )
    ))
    
    observeEvent(input$gram, {
      data2 <- data %>% filter(gram %in% input$gram)

      updatePickerInput(session = session, inputId = "genus",
                        choices = unique(data2$genus),
                        selected = unique(data2$genus)
                        )
      
      updatePickerInput(session = session, inputId = "species",
                        choices = unique(data2$species),
                        selected = unique(data2$species)
      )
    })
    
    observeEvent(input$genus, {
      data2 <- data %>% filter(genus %in% input$genus)

      updatePickerInput(session = session, inputId = "species",
                        choices = unique(data2$species),
                        selected = unique(data2$species)
      )
    })

    resfilter <- eventReactive(input$resbutton,
                               {
                                 data <- data %>% filter(gram %in% input$gram)
                                 data <- data %>% filter(genus %in% input$genus)
                                 data <- data %>% filter(species %in% input$species)
                                 data <- data %>% filter(leeftijdgroep %in% input$age)
                                 data <- data %>% filter(geslacht %in% input$geslacht)
                                 data <- data %>% filter(mtrlgroep %in% input$mtrl)
                                 data <- data %>% filter(zorglijn %in% input$zorglijn)
                                 
                                 data2 <- data %>% dplyr::filter(input$datum[1] < ontvangstdatum & ontvangstdatum < input$datum[2])
                                 
                                 res_gebied <<- input$schaal
                       
                                 if(input$schaal == 'postcode') {resdata_table <<- data2 %>% group_by(postcode) %>%
                                   summarise(resistance = round(resistance(get(input$ab)),digits = 3),isolaten=n_rsi(get(input$ab))) %>%
                                   left_join(select(geo_postcodes4,geometry,inwoners,postcode), by='postcode')}
                                 else if(input$schaal == 'postcode2') {resdata_table <<- data2 %>% group_by(postcode2) %>%
                                   summarise(resistance = round(resistance(get(input$ab)),digits = 3),isolaten=n_rsi(get(input$ab))) %>%
                                   left_join(select(geo_postcodes2,geometry,inwoners,postcode), by=c('postcode2' = 'postcode'))}
                                 else if(input$schaal == 'gemeente') {resdata_table <<- data2 %>% group_by(gemeente) %>% 
                                   summarise(resistance = round(resistance(get(input$ab)),digits = 3), isolaten = n_rsi(get(input$ab))) %>%
                                   left_join(select(geo_gemeenten,geometry,inwoners,gemeente), by='gemeente')}
                                 else if(input$schaal == 'nuts3') {resdata_table <<- data2 %>% group_by(nuts3) %>%
                                   summarise(resistance = round(resistance(get(input$ab)),digits = 3), isolaten = n_rsi(get(input$ab))) %>% 
                                   left_join(select(geo_nuts3,geometry,inwoners,nuts3), by='nuts3')}
                                 else if(input$schaal == 'provincie') {resdata_table <<- data2 %>% group_by(provincie) %>%
                                   summarise(resistance = round(resistance(get(input$ab)),digits = 3), isolaten=n_rsi(get(input$ab))) %>% 
                                   left_join(select(geo_provincies,geometry,inwoners,provincie), by='provincie')}
                                 
                                 resdata_table_trend <<- data %>% group_by(get(input$schaal),jaar) %>% summarise(resistance = round(resistance(get(input$ab)),digits = 3))
                                 colnames(resdata_table_trend)[1] <<- 'gebied'                                             
                                 
                                 names(resdata_table)[names(resdata_table) == 'postcode' | names(resdata_table) == 'postcode2' | names(resdata_table) == 'gemeente' | names(resdata_table) == 'nuts3' | names(resdata_table) == 'provincie'] <<- 'gebied'
                                 #resdata_table_now <<- resdata_table %>% filter(jaar == 2021)
                                 
                                 plot <- ggplot() + 
                                   geom_sf_interactive(data=resdata_table, aes(fill=resistance, geometry=geometry, tooltip=sprintf("Gebied: %s<br/>Inwoners: %s<br/>Isolaten: %s<br/>Resistentie: %s",gebied,inwoners,isolaten,resistance),data_id=gebied)) + 
                                   coord_sf(datum = NA) + 
                                   theme_classic() +
                                   labs(fill = 'Resistentie') +
                                   scale_fill_viridis(option='plasma', direction = 1)
                                 
                                 girafe(code = print(plot), width_svg = 10)
                               },
                               ignoreNULL= FALSE
      
    )
    
    output$resistentieplot <- renderggiraph({
      resfilter()
    })
    
    res_sel <- reactive({resdata_table[,c('gebied','inwoners','isolaten','resistance')] %>% filter(gebied %in% input$resistentieplot_selected) %>% kable('html', col.names = c('Gebied', 'Inwoners', 'Isolaten', 'Resistentie')) %>% kable_styling('striped',full_width = F)})
    
    output$res_sel <- renderText({
      res_sel()
    })
    
    res_trend_sel <- reactive({
      
      resdata_table_trend_sel <- resdata_table_trend[,c('gebied','resistance','jaar')] %>% filter(gebied %in% input$resistentieplot_selected)
      res_trend_plot <- ggplot(data=resdata_table_trend_sel, aes(x=jaar, y=resistance, color=gebied)) + 
        geom_line_interactive() + 
        geom_point_interactive(aes(tooltip = resistance)) +
        labs(color = 'Gebied', x = 'Jaar', y = 'Resistentie')
      girafe(code = print(res_trend_plot))
      })
    
    
    # trend plot
    
    output$res_trend <- renderggiraph({
      res_trend_sel()
    })
    
#---------------------BRMO-------------------------  
    
    output$brmopag <- renderUI(sidebarLayout(
        sidebarPanel(

          pickerInput(
            'brmo',
            'BRMO',
            choices=c('ALLE'='bevat_brmo','MRSA'='is_mrsa','ESBL'='is_esbl','VRE'='is_vre'),
            selected = 'bevat_brmo',
          ),
          
          pickerInput(
            'brmosoort',
            'Bacteriesoort',
            choices=unique(data$bacteriesoort),
            selected = unique(data$bacteriesoort),
            multiple = TRUE,
            pickerOptions(actionsBox = TRUE),
          ),

          pickerInput(
            'brmomtrl',
            'Materiaalgroep',
            choices=unique(data$mtrlgroep),
            selected = unique(data$mtrlgroep),
            multiple = TRUE,
            pickerOptions(actionsBox = TRUE),
          ),
          
          checkboxGroupInput(
            'brmozorglijn',
            'Zorglijn',
            selected = c('1e lijn', '2e lijn', '3e lijn'),
            inline = 'TRUE',
            choices=c('1e lijn', '2e lijn', '3e lijn'),
          ),
          
          radioButtons(
            'brmoschaal',
            'Schaal',
            selected = 'gemeente',
            inline = 'FALSE',
            choiceNames = c('Postcode 4','Postcode 2','Gemeente', 'NUTS 3', 'Provincie'),
            choiceValues = c('postcode','postcode2','gemeente', 'nuts3', 'provincie')
          ),
          
          checkboxGroupInput(
            'brmogeslacht',
            'Geslacht',
            selected = c('M','V'),
            inline = 'TRUE',
            choiceNames = c('Man','Vrouw'),
            choiceValues = c('M','V')
          ),
          
          pickerInput(
            'brmoage',
            'Leeftijdsgroepen',
            choices=c('0-3','4-11','12-17','18-65','>65'),
            selected = c('0-3','4-11','12-17','18-65','>65'),
            multiple = TRUE,
            pickerOptions(actionsBox = TRUE),
          ),
          
          dateRangeInput(inputId = 'brmodatum', label = 'Periode', start = '2016-01-01', end = '2022-01-03', language = 'nl'),
          
          actionButton("brmobutton", "Toepassen"),
          
        ),
        mainPanel(
          h1("BRMO's Noord-Nederland"),
          h4("Percentage Bijzonder Resistente Micro-Organismen in isolaten per gebied"),
          h5("Klik op een gebied om de trend te zien"),
          tabPanel('BRMO',ggiraphOutput("brmoplot")),
          hr(),
          h4("Trend BRMO's van 2016 tot 2021"),
          tabPanel('BRMO',ggiraphOutput("brmo_trend")),
          tabPanel('BRMO',htmlOutput("brmo_sel"))
        )
    ))
    
    observeEvent(input$brmo, {
      data2 <- data %>% filter(get(input$brmo) == TRUE)

      updatePickerInput(session = session, inputId = "brmosoort",
                        choices = unique(data2$bacteriesoort),
                        selected = unique(data2$bacteriesoort)
      )
    })
    
    observeEvent(input$brmogram, {
      data2 <- data %>% filter(gram %in% input$brmogram)
      
      updatePickerInput(session = session, inputId = "brmogenus",
                        choices = unique(data2$genus),
                        selected = unique(data2$genus)
      )
      
      updatePickerInput(session = session, inputId = "brmospecies",
                        choices = unique(data2$species),
                        selected = unique(data2$species)
      )
    })
    
    observeEvent(input$brmogenus, {
      data2 <- data %>% filter(genus %in% input$brmogenus)
      
      updatePickerInput(session = session, inputId = "brmospecies",
                        choices = unique(data2$species),
                        selected = unique(data2$species)
      )
    })
    
    brmofilter <- eventReactive(input$brmobutton, {
      data <- data %>% filter(gram %in% input$brmogram)
      data <- data %>% filter(genus %in% input$brmogenus)
      data <- data %>% filter(species %in% input$brmospecies)
      data <- data %>% filter(leeftijdgroep %in% input$brmoage)
      data <- data %>% filter(input$brmodatum[1] < ontvangstdatum & ontvangstdatum < input$brmodatum[2])
      data <- data %>% filter(geslacht %in% input$brmogeslacht)
      data <- data %>% filter(mtrlgroep %in% input$brmomtrl)
      data <- data %>% filter(zorglijn %in% input$brmozorglijn)
      
      
      if(input$brmoschaal == 'postcode') {brmodata_table <<- data %>% group_by(postcode) %>%
        summarise(aantal = sum(get(input$brmo), na.rm = TRUE), isolaten=n()) %>%
        left_join(select(geo_postcodes4,geometry, inwoners,postcode), by='postcode') %>%
        mutate(percentage = round((aantal/isolaten*100),digits=2))}
      else if(input$brmoschaal == 'postcode2') {brmodata_table <<- data %>% group_by(postcode2) %>%
        summarise(aantal = sum(get(input$brmo), na.rm = TRUE), isolaten=n()) %>%
        left_join(select(geo_postcodes2,geometry,inwoners,postcode), by=c('postcode2' = 'postcode')) %>%
        mutate(percentage = round((aantal/isolaten*100),digits=2))}
      else if(input$brmoschaal == 'gemeente') {brmodata_table <<- data %>% group_by(gemeente) %>% 
        summarise(aantal = sum(get(input$brmo), na.rm = TRUE), isolaten=n()) %>%
        left_join(select(geo_gemeenten,geometry,inwoners,gemeente), by='gemeente') %>%
        mutate(percentage = round((aantal/isolaten*100),digits=2))}
      else if(input$brmoschaal == 'nuts3') {brmodata_table <<- data %>% group_by(nuts3) %>%
        summarise(aantal = sum(get(input$brmo), na.rm = TRUE), isolaten=n()) %>% 
        left_join(select(geo_nuts3,geometry,inwoners,nuts3), by='nuts3') %>%
        mutate(percentage = round((aantal/isolaten*100),digits=2))}
      else if(input$brmoschaal == 'provincie') {brmodata_table <<- data %>% group_by(provincie) %>%
        summarise(aantal = sum(get(input$brmo), na.rm = TRUE), isolaten=n()) %>% 
        left_join(select(geo_provincies,geometry,inwoners,provincie), by='provincie') %>%
        mutate(percentage = round((aantal/isolaten*100),digits=2))}
      
      brmodata_table_trend <<- data %>% group_by(get(input$brmoschaal),jaar) %>% summarise(aantal = sum(get(input$brmo), na.rm = TRUE), isolaten=n()) %>% mutate(percentage = round((aantal/isolaten*100),digits=2))
      colnames(brmodata_table_trend)[1] <<- 'gebied'    
      
      names(brmodata_table)[names(brmodata_table) == 'postcode' | names(brmodata_table) == 'postcode2' | names(brmodata_table) == 'gemeente' | names(brmodata_table) == 'nuts3' | names(brmodata_table) == 'provincie'] <<- 'gebied'
      
      plot <- ggplot() + 
        geom_sf_interactive(data=brmodata_table, aes(fill=percentage, geometry=geometry, tooltip=sprintf("Gebied: %s<br/>Inwoners: %s<br/>Isolaten: %s<br/>BRMO's: %s<br/> %% BRMO's in isolaten: %s",gebied,inwoners,isolaten,aantal,percentage),data_id=gebied)) + 
        coord_sf(datum = NA) + 
        theme_classic() +
        labs(fill = "% BRMO's\n in isolaten") +
        scale_fill_viridis(option='plasma', direction = 1)
      girafe(code = print(plot), width_svg = 10)
    }, ignoreNULL = FALSE)
    
    output$brmoplot <- renderggiraph({
      brmofilter()
      })
    
    brmo_sel <- reactive({brmodata_table[,c('gebied','inwoners','isolaten','aantal','percentage')] %>% filter(gebied %in% input$brmoplot_selected) %>% kable('html', col.names = c('Gebied', 'Inwoners', 'Isolaten', "Aantal BRMO's", 'Percentage')) %>% kable_styling('striped',full_width = F)
    })
    
    output$brmo_sel <- renderText({
      brmo_sel()
    })
    
    brmo_trend_sel <- reactive({
      
      brmodata_table_trend_sel <- brmodata_table_trend[,c('gebied','percentage','jaar')] %>% filter(gebied %in% input$brmoplot_selected)
      brmo_trend_plot <- ggplot(data=brmodata_table_trend_sel, aes(x=jaar, y=percentage, color=gebied)) + 
        geom_line_interactive() + 
        geom_point_interactive(aes(tooltip = percentage)) +
        labs(color = 'Gebied', x = 'Jaar', y = 'Percentage')
      girafe(code = print(brmo_trend_plot))
    })
    
    
    # trend plot
    
    output$brmo_trend <- renderggiraph({
      brmo_trend_sel()
    })
    
#----------------UITGIFTE------------------------
    
    output$uitgiftepag <- renderUI(sidebarLayout(
      sidebarPanel(
        
        selectInput(
          'uitgifteab',
          'Antibioticum',
          choices=unique(uitgifte$atc_5_omschrijving)
        ),
        
        
        checkboxGroupInput(
          'uitgeslacht',
          'Geslacht',
          selected = c('M','V'),
          inline = 'TRUE',
          choiceNames = c('Man','Vrouw'),
          choiceValues = c('M','V')
        ),
        
        pickerInput(
          'uitage',
          'Leeftijdsgroepen',
          choices=c('0-3','4-11','12-17','18-65','>65'),
          selected = c('0-3','4-11','12-17','18-65','>65'),
          multiple = TRUE,
          pickerOptions(actionsBox = TRUE),
        ),
        
        pickerInput('uitdatum',
                    'Van (jaar-kwartaal)',
                    choices = c('2016-1','2016-2','2016-3','2016-4','2017-1','2017-2','2017-3','2017-4','2018-1','2018-2','2018-3','2018-4','2019-1','2019-2','2019-3','2019-4','2020-1','2020-2','2020-3','2020-4','2021-1','2021-2','2021-3','2021-4'),
                    selected = '2016-1'),
            
        pickerInput('uitdatum2',
                    'Tot (jaar-kwartaal)',
                    choices = c('2016-1','2016-2','2016-3','2016-4','2017-1','2017-2','2017-3','2017-4','2018-1','2018-2','2018-3','2018-4','2019-1','2019-2','2019-3','2019-4','2020-1','2020-2','2020-3','2020-4','2021-1','2021-2','2021-3','2021-4'),
                    selected = '2021-4'),
        actionButton("uitbutton", "Toepassen"),
                    
      ),
      mainPanel(
        tabPanel('Uitgifte',ggiraphOutput("uitplot")),
        #tabPanel('BRMO',downloadButton('download',"Download data")),
        tabPanel('Uitgifte',htmlOutput("uit_sel"))
      )
    ))
    
    uitfilter <- eventReactive(input$uitbutton, {
      uitgifte <- uitgifte %>% dplyr::filter(leeftijd_a %in% input$uitage)
      #uitgifte <- uitgifte %>% dplyr::filter(input$uitdatum[1] < ontvangstdatum & ontvangstdatum < input$uitdatum[2])
      uitgifte <- uitgifte %>% dplyr::filter(geslacht %in% input$uitgeslacht)
      uitgifte <- uitgifte %>% dplyr::filter(atc_5_omschrijving %in% input$uitgifteab)
      
      uitdatum <- gsub("-", "", input$uitdatum)
      uitdatum <- as.numeric(uitdatum)
      uitdatum2 <- gsub("-", "", input$uitdatum2)
      uitdatum2 <- as.numeric(uitdatum2)      
      uitgifte$YQ <- gsub("-", "", uitgifte$YQ)
      uitgifte$YQ <- as.numeric(uitgifte$YQ)
      
      uitgifte <- uitgifte %>% dplyr::filter(uitdatum < YQ & YQ < uitdatum2)
      
      uitdata_table <<- uitgifte %>% group_by(postcode_strip) %>%
        summarise(dddsum = sum(ddd, na.rm=TRUE)) %>%
        full_join(select(geo_postcodes2,geometry, inwoners,postcode), by=c('postcode_strip' = 'postcode')) %>%
        filter(postcode_strip %in% c(77,78,79) | postcode_strip >= 83)
     
      uitdata_table <<- uitdata_table[-c(17),]
      
      plot <- ggplot() + 
        geom_sf_interactive(data=uitdata_table, aes(fill=dddsum, geometry=geometry, tooltip=sprintf("Postcode 2: %s<br/>Inwoners: %s<br/>Uitgifte: %s",postcode_strip,inwoners,dddsum),data_id=postcode_strip)) + 
        coord_sf(datum = NA) + 
        theme_classic() +
        labs(fill = "DDD") +
        scale_fill_viridis(option='plasma', direction = 1)
      girafe(code = print(plot), width_svg = 10)
      

    }, ignoreNULL = FALSE)
    
    
    output$uitplot <- renderggiraph({
      uitfilter()
    })
    
    uit_sel <- reactive({
      uitdata_table[,c('postcode_strip','inwoners','dddsum')] %>% filter(postcode_strip %in% input$uitplot_selected) %>% kable('html', col.names = c('Postcode 2', 'Inwoners', 'DDD')) %>% kable_styling('striped',full_width = F)
    })
    
    output$uit_sel <- renderText({
      uit_sel()
    })
    
#--------------------------------------------LIGHT------------------------------------------------------
    
    output$lightpag <- renderUI(sidebarLayout(
      sidebarPanel( 
        
        pickerInput(
          'lightmtrl',
          'Materiaalsoort',
          choices = c('Urine'='Urine', 'Bloed'='Bloed', 'Luchtweg'='Respiratoir'),
          selected = 'Urine',
          multiple = FALSE,
          pickerOptions(actionsBox = TRUE),
        ),
        
        pickerInput(
          'lightsoort',
          'Bacteriesoort',
          choices=tail(names(sort(table(data$bacteriesoort))), 10),
          selected = 'Escherichia coli',
          multiple = FALSE,
          pickerOptions(actionsBox = TRUE),
        ),
        
        pickerInput(
          'lightab',
          'Antibioticum',
          choices=ab_alpha,
          selected = 'Nitrofurantoine',
          multiple = FALSE,
        ),
        
        
        checkboxGroupInput(
          'lightzorglijn',
          'Zorglijn',
          selected = c('1e lijn', '2e lijn', '3e lijn'),
          inline = 'TRUE',
          choices=c('1e lijn', '2e lijn', '3e lijn'),
        ),
        
        
        radioButtons(
          'lightschaal',
          'Schaal',
          selected = 'gemeente',
          inline = 'TRUE',
          choiceNames = c('Gemeente', 'Provincie'),
          choiceValues = c('gemeente', 'provincie')
        ),
        
        
        checkboxGroupInput(
          'lightgeslacht',
          'Geslacht',
          selected = c('M','V'),
          inline = 'TRUE',
          choiceNames = c('Man','Vrouw'),
          choiceValues = c('M','V')
        ),
        
        # sliderInput(inputId = 'age', label = 'Leeftijd', min = min(data$leeftijd), max = max(data$leeftijd), value = c(min(data$leeftijd),max(data$leeftijd)), step = NULL, round = FALSE,
        #             ticks = TRUE, animate = FALSE,
        #             width = NULL, sep = ",", pre = NULL, post = NULL, timeFormat = NULL,
        #             timezone = NULL, dragRange = TRUE),
        
        pickerInput(
          'lightage',
          'Leeftijdsgroep',
          choices=c('0-3','4-11','12-17','18-65','>65'),
          selected = c('0-3','4-11','12-17','18-65','>65'),
          multiple = TRUE,
          pickerOptions(actionsBox = TRUE),
        ),
        
        dateRangeInput(inputId = 'lightdatum', label = 'Periode', start = '2016-01-01', end = '2022-01-03', language = 'nl'),
        
        actionButton("lightbutton", "Toepassen"),
      ),
      mainPanel(
        h1("Antibioticaresistentie Noord-Nederland"),
        h4("Verhouding resistente en alle isolaten per gebied"),
        h5("Klik op een gebied om de trend te zien"),
        tabPanel('Dashboard',ggiraphOutput("lightresistentieplot")),
        hr(),
        h4("Trend antibioticaresistentie van 2016 tot 2021"),
        tabPanel('Dashboard',ggiraphOutput("lightres_trend")),
        tabPanel('Dashboard',htmlOutput("lightres_sel")),
        
      )
    ))
    
    
    lightresfilter <- eventReactive(input$lightbutton,
                               {
                                 data <- data %>% filter(bacteriesoort %in% input$lightsoort)
                                 data <- data %>% filter(leeftijdgroep %in% input$lightage)
                                 data <- data %>% filter(geslacht %in% input$lightgeslacht)
                                 data <- data %>% filter(mtrlgroep %in% input$lightmtrl)
                                 data <- data %>% filter(zorglijn %in% input$lightzorglijn)
                                 
                                 data2 <- data %>% dplyr::filter(input$lightdatum[1] < ontvangstdatum & ontvangstdatum < input$lightdatum[2])
                                 
                                 res_gebied <<- input$lightschaal
                                 

                                 if(input$lightschaal == 'gemeente') {lightresdata_table <<- data2 %>% group_by(gemeente) %>% 
                                   summarise(resistance = round(resistance(get(input$lightab)),digits = 3), isolaten = n_rsi(get(input$lightab))) %>%
                                   left_join(select(geo_gemeenten,geometry,inwoners,gemeente), by='gemeente')}
                                 else if(input$lightschaal == 'provincie') {lightresdata_table <<- data2 %>% group_by(provincie) %>%
                                   summarise(resistance = round(resistance(get(input$lightab)),digits = 3), isolaten=n_rsi(get(input$lightab))) %>% 
                                   left_join(select(geo_provincies,geometry,inwoners,provincie), by='provincie')}
                                 
                                 lightresdata_table_trend <<- data %>% group_by(get(input$lightschaal),jaar) %>% summarise(resistance = round(resistance(get(input$lightab)),digits = 3))
                                 colnames(lightresdata_table_trend)[1] <<- 'gebied'                                             
                                 
                                 names(lightresdata_table)[names(lightresdata_table) == 'gemeente' | names(lightresdata_table) == 'provincie'] <<- 'gebied'
                                 
                                 plot <- ggplot() + 
                                   geom_sf_interactive(data=lightresdata_table, aes(fill=resistance, geometry=geometry, tooltip=sprintf("Gebied: %s<br/>Inwoners: %s<br/>Isolaten: %s<br/>Resistentie: %s",gebied,inwoners,isolaten,resistance),data_id=gebied)) + 
                                   coord_sf(datum = NA) + 
                                   theme_classic() +
                                   labs(fill = 'Resistentie') +
                                   scale_fill_viridis(option='plasma', direction = 1)
                                 
                                 girafe(code = print(plot), width_svg = 10)
                               },
                               ignoreNULL= FALSE
                               
    )
    
    observeEvent(input$lightmtrl, {
      data2 <- data %>% filter(mtrlgroep == input$lightmtrl)
      
      updatePickerInput(session = session, inputId = "lightsoort",
                        choices = head(names(sort(table(data2$bacteriesoort),decreasing = TRUE)), 5),
                        selected = head(names(sort(table(data2$bacteriesoort),decreasing = TRUE)), 5)
      )
      
    })
    
    output$lightresistentieplot <- renderggiraph({
      lightresfilter()
    })
    
    lightres_sel <- reactive({lightresdata_table[,c('gebied','inwoners','isolaten','resistance')] %>% filter(gebied %in% input$lightresistentieplot_selected) %>% kable('html', col.names = c('Gebied', 'Inwoners', 'Isolaten', 'Resistentie')) %>% kable_styling('striped',full_width = F)})
    
    output$lightres_sel <- renderText({
      lightres_sel()
    })
    
    lightres_trend_sel <- reactive({
      
      lightresdata_table_trend_sel <- lightresdata_table_trend[,c('gebied','resistance','jaar')] %>% filter(gebied %in% input$lightresistentieplot_selected)
      lightres_trend_plot <- ggplot(data=lightresdata_table_trend_sel, aes(x=jaar, y=resistance, color=gebied)) + 
        geom_line_interactive() + 
        geom_point_interactive(aes(tooltip = resistance)) +
        labs(color = 'Gebied', x = 'Jaar', y = 'Resistentie')
      girafe(code = print(lightres_trend_plot))
    })
    
    
    # trend plot
    
    output$lightres_trend <- renderggiraph({
      lightres_trend_sel()
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
