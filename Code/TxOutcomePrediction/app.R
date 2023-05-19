library(shiny)
library(shinyWidgets)
library(plotly)

# Define UI for slider demo app ----
ui <- fluidPage(
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar to demonstrate various slider options ----
    sidebarPanel(
      h4('Intake Results', align = 'center'),
      
      sliderInput("iniSev", "Initial Severity:",
                  min = -2, max = 6,
                  value = c(2,4), step = 0.5),
      
      selectInput('type', 'Personality Measurement Type:',
                  c('Dimensional' = "dimensional",
                    'Categorical' = 'categorical')),
      
      uiOutput('sliders')
   
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      #Input: Outcome 
      br(),
      h4('Treatment Outcome Trajectory', align = 'center'),
    
      selectInput('outcome', 'Outcome Type:',
                  c('Depression' = "DEPRS",
                    'Quality of Life' = 'LIFEQ',
                    'Mania' = 'MANIC', 
                    'Panic' = 'PANIC', 
                    'Psychosis' = 'PSYCS', 
                    'Substance Abuse' = 'SA', 
                    'Social Conflict' = 'SCONF', 
                    'Sexual Functioning' = 'SEXFN',
                    'Sleep Problems' = 'SLEEP', 
                    'Suicidality' = 'SUICD', 
                    'Violence' = 'VIOLN',
                    'Work Functioning' = 'WORKF'
                  )),
      
      # Output: Table summarizing the values entered ----
      br(),
      br(),
      br(),
      br(),
      plotlyOutput("outcomePlot"),
      br(),
      br(),
      fluidRow(align = 'center', htmlOutput("modelDescription"))
      
    )
  )
)


server <- function(input, output) {
  
 
  output$sliders <- renderUI({
    if(input$type == 'dimensional'){
      tagList(
        
        # Input: Negative Affect
        sliderInput("NEG", "Negative Affectivity:",
                    min = -2, max = 4,
                    value = c(-1,1), step = 0.5),
        
        # Input: Detachment
        sliderInput("DET", "Detachment:",
                    min = -2, max = 4,
                    value = c(-1,1), step = 0.5),
        
        # Input:Antagonism
        sliderInput("ANT", "Antagonism:",
                    min = -2, max = 4,
                    value = c(-1,1), step = 0.5),
        
        # Input: Disinhibition
        sliderInput("DIS", "Disinhibition:",
                    min = -2, max = 4,
                    value = c(-1,1), step = 0.5),
        
        # Input: Psychoticism
        sliderInput("PSY", "Psychoticism:",
                    min = -2, max = 4,
                    value = c(-1,1), step = 0.5))
    }else{
      tagList(
        
        sliderTextInput("Paranoid", "Paranoid:",
                        choices= c('Absent', 'Present')),
        
        sliderTextInput("Schizoid", "Schizoid:",
                        choices= c('Absent', 'Present')),
        
        sliderTextInput("Schizotypal", "Schizotypal:",
                        choices= c('Absent', 'Present')),
        
        sliderTextInput("Antisocial", "Antisocial:",
                        choices= c('Absent', 'Present')),
      
        sliderTextInput("Borderline", "Borderline:",
                        choices= c('Absent', 'Present'),
                        selected = 'Present'),
        
        sliderTextInput("Histrionic", "Histrionic:",
                        choices= c('Absent', 'Present')),
        
        sliderTextInput("Narcissistic", "Narcissistic:",
                        choices= c('Absent', 'Present')),
        
        sliderTextInput("Avoidant", "Avoidant:",
                        choices= c('Absent', 'Present')),
        
        sliderTextInput("Dependent", "Dependent:",
                        choices= c('Absent', 'Present')),
        
        sliderTextInput("Obsessive_compulsive", "Obsessive-Compulsive:",
                        choices= c('Absent', 'Present')))
  
    }
    
    
  })
  
  
  # Reactive expression to create data frame of all input values ----
  
  
  sliderOutput <- reactive({
    
    require(dplyr)
    require(plotly)
    df = read.csv('pid5TOP_valid.csv')
    selectModelBIC = read.csv('selectModelBIC.csv')
    
    outcome = input$outcome
    
    dfModel = 
      df %>%
      mutate_at(vars('Paranoid', 'Schizoid', 'Schizotypal', 'Antisocial', 'Borderline', 'Histrionic',
                     'Narcissistic', 'Avoidant', 'Dependent', 'Obsessive_compulsive'), as.factor) %>%
      mutate(outcome = .[,outcome]) %>%
      group_by(ID) %>%
      mutate(iniSev = outcome[1]) %>%
      mutate(SESSION = SESSION-1) %>%
      ungroup() 
    
    if (input$type == 'dimensional'){
     
       if(length(input$NEG)>0){
      
        filteredDf = 
          dfModel %>%
          filter(
            iniSev > input$iniSev[1] & iniSev <= input$iniSev[2] &
              NEG > input$NEG[1] & NEG <= input$NEG[2] & 
              DET > input$DET[1] & DET <= input$DET[2] &
              ANT > input$ANT[1] & ANT <= input$ANT[2] &
              DIS > input$DIS[1] & DIS <= input$DIS[2] &
              PSY > input$PSY[1] & PSY <= 1)
        
     
        groupMean =
          filteredDf %>%
          group_by(ID) %>%
          filter(row_number()==1) %>%
          select(ID, iniSev, NEG, DET, ANT, DIS, PSY) %>%
          colMeans()
        
        # selectedMean = data.frame(
        #   iniSev = mean(input$iniSev),
        #   NEG = mean(input$NEG),
        #   DET = mean(input$DET),
        #   ANT = mean(input$ANT),
        #   DIS = mean(input$DIS),
        #   PSY = mean(input$PSY)
        # )
        
        predData = 
          dfModel %>%
          filter(SESSION != 0)
        
        if (grepl('ucgL', selectModelBIC[selectModelBIC$Outcome == input$outcome, 'Model'],  fixed = F)){
          model = lm(outcome ~ SESSION*iniSev*(NEG+DET+ANT+DIS+PSY), predData)
        } else if (grepl('ucgQ', selectModelBIC[selectModelBIC$Outcome == input$outcome, 'Model'],  fixed = F)){
          model = lm(outcome ~ (SESSION+I(SESSION^2))*iniSev*(NEG+DET+ANT+DIS+PSY), predData)
        }else if (grepl('ucgC', selectModelBIC[selectModelBIC$Outcome == input$outcome, 'Model'],  fixed = F)){
          model = lm(outcome ~ (SESSION+I(SESSION^2)+I(SESSION^3))*iniSev*(NEG+DET+ANT+DIS+PSY), predData)
          }
        
        filteredDf_pred = 
          filteredDf %>%
          filter(SESSION != 0) %>%
          mutate(indPred = predict(model, .))  %>%
          select(ID, SESSION, indPred)
        
        filteredDf = 
        filteredDf %>%
          left_join(filteredDf_pred, by = c('ID', 'SESSION')) 
        
        if (!is.na(groupMean[2])){
          meanSession = 
            filteredDf %>% 
            group_by(ID) %>%
            filter(row_number() == max(row_number())) %>%
            select(ID, SESSION) %>%
            ungroup() %>%
            summarize_at('SESSION', mean, na.rm = T) %>%
            round()
          
            if(meanSession==1){
              groupMeanPredData = data.frame(t(c(SESSION = 1, groupMean)))
              
            }else{
              groupMeanPredData = data.frame(SESSION = 0:meanSession[[1]],
                                             apply(t(data.frame(groupMean)), 2,
                                                   rep, 1+meanSession[[1]])) %>%
                filter(SESSION!=0)
            }
          
          groupMeanPredVal = predict(model,  groupMeanPredData)
          
          # selectedMeanPredData = data.frame(SESSION = 1:meanSession[[1]],
          #                                apply(as.data.frame(selectedMean), 2, 
          #                                      rep, meanSession[[1]]))
          # 
          # selectedMeanPredVal = predict(model,  selectedMeanPredData)
          
        }else{
         groupMeanPredVal = NA
         #selectedMean = NA
         meanSession = 0
        }
        
        
        filteredDf %>%   
          plot_ly() %>%
          add_trace(x = ~ SESSION, y = ~ outcome,
                    type = 'scatter', mode = 'markers', color = ~as.factor(ID),
                    #name = ~paste0('ID ', ID),
                    alpha = 0.5, showlegend = F,
                    text = ~paste0('<b>ID ', ID,'</b> \n',
                                   'Inital Severity:', round(iniSev, 2), '\n',
                                   'NEG: ', round(NEG, 2), '\n',  
                                   'DET: ', round(DET,2), '\n',
                                   'ANT: ', round(ANT,2), '\n',  
                                   'DIS: ', round(DIS,2), '\n',  
                                   'PSY: ', round(PSY,2), '\n \n',
                                   'SESSION: ', SESSION, '\n',
                                   'Z-Score: ', round(outcome,2)),
                    hoverinfo= 'text') %>%
          add_lines(x = ~ SESSION, y = ~ indPred,
                    mode = mode, color = ~as.factor(ID),
                    name = ~paste0('ID ', ID),
                    alpha = 0.5, line = list(width = 3), 
                    text = ~paste0('<b>ID ', ID,'</b> \n',
                                   'Initial Severity: ', round(iniSev, 2), '\n',
                                   'NEG: ', round(NEG, 2), '\n',  
                                   'DET: ', round(DET,2), '\n',
                                   'ANT: ', round(ANT,2), '\n',  
                                   'DIS: ', round(DIS,2), '\n',  
                                   'PSY: ', round(PSY,2), '\n \n',
                                   'SESSION: ', SESSION, '\n',
                                   'Pred Score: ', round(indPred,2)),
                    hoverinfo= 'text') %>%
          layout(
            legend = list(title = list(text = '<b>Prediction Line</b>')),
            hoverlabel = list(bordercolor = 'transparent', font = list(color = 'black')),
            xaxis = list(title = 'SESSION NUMBER',
                         linecolor = 'black',
                         zerolinecolor = '#ffff',
                         zerolinewidth = 2,
                         gridcolor = 'ffff',
                         range = list(0, 50)),
            
            yaxis = list(title = 'OUTCOME Z-SCORE',
                         linecolor = 'black',
                         zerolinecolor = '#ffff',
                         zerolinewidth = 2,
                         gridcolor = 'ffff',
                         range = list(-3, 7),
                         hoverformat = '.2r')
          ) %>%
          add_trace(x = c(1:meanSession[[1]]), y = groupMeanPredVal,
                    type = 'scatter', mode = 'lines+markers', 
                    line = list(color = 'black', width = 4),
                    marker = list(color = 'black', size = 3),
                    name = 'Group Mean',
                    text = paste0('Initial Severity: ', round(groupMean['iniSev'],2), '\n',
                                  'NEG: ', round(groupMean['NEG'],2), '\n',  
                                  'DET: ', round(groupMean['DET'],2) , '\n',
                                  'ANT: ', round(groupMean['ANT'],2) , '\n',  
                                  'DIS: ', round(groupMean['DIS'],2) , '\n',  
                                  'PSY: ', round(groupMean['PSY'],2) , '\n',
                                  'Mean Treatment Length: ', meanSession[[1]], ' Sessions \n \n'),
                    hoverinfo= 'text+x+y', mode='lines',
                    hovertemplate = '%{text}SESSION: %{x} \n Z-Score: %{y}'
          ) %>%
          layout(
            hoverlabel = list(bordercolor = 'transparent', font = list(color = 'white')),
            yaxis = list( hoverformat = '.2r')
          )
      }
      }else if(length(input$Paranoid) > 0){
        
        filteredDf = 
        dfModel %>%
            mutate_at(vars('Paranoid', 'Schizoid', 'Schizotypal', 'Antisocial', 'Borderline', 'Histrionic',
                           'Narcissistic', 'Avoidant', 'Dependent', 'Obsessive_compulsive'),
                      ~factor(., labels = c('Absent', 'Present'))) %>%
            filter(iniSev > input$iniSev[1] & iniSev <= input$iniSev[2] &
              Paranoid == input$Paranoid & 
              Schizoid == input$Schizoid &
              Schizotypal == input$Schizotypal &
              Antisocial == input$Antisocial &
              Borderline == input$Borderline &
              Histrionic == input$Histrionic &
              Narcissistic== input$Narcissistic &
              Avoidant == input$Avoidant &
              Dependent == input$Dependent &
              Obsessive_compulsive == input$Obsessive_compulsive) %>%
          mutate_at(vars('Paranoid', 'Schizoid', 'Schizotypal', 'Antisocial', 'Borderline', 'Histrionic',
                         'Narcissistic', 'Avoidant', 'Dependent', 'Obsessive_compulsive'),
                    ~factor(., levels = c('Absent', 'Present'), labels = c('0','1'))) 
       
        predData = 
          dfModel %>%
          filter(SESSION!=0)
        
        meanAndMode = function(x){
          if(is.factor(x)){
            freqTable = table(x)
            mode = names(freqTable)[which(freqTable == max(freqTable))]
            return(factor(mode, levels = c('0','1')))
          }else{
            return(mean(x))
          }
        }
        
        groupMean =
          filteredDf %>%
          group_by(ID) %>%
          filter(row_number()==1) %>%
          ungroup() %>%
          select(ID, iniSev, Paranoid, Schizoid, Schizotypal,
                 Antisocial, Borderline, Histrionic, Narcissistic,
                 Avoidant, Dependent, Obsessive_compulsive) %>%
          summarize_all(meanAndMode)


        if (grepl('ucgL', selectModelBIC[selectModelBIC$Outcome == input$outcome, 'Model'],  fixed = F)){
          model = lm(outcome ~ SESSION*iniSev*(Paranoid+Schizoid+Schizotypal+Antisocial+
                                                 Borderline+Histrionic+Narcissistic+Avoidant+
                                                 Dependent+Obsessive_compulsive), predData)
          
        } else if (grepl('ucgQ', selectModelBIC[selectModelBIC$Outcome == input$outcome, 'Model'],  fixed = F)){
          model = lm(outcome ~ (SESSION+I(SESSION^2))*iniSev*(Paranoid+Schizoid+Schizotypal+Antisocial+
                                                                Borderline+Histrionic+Narcissistic+Avoidant+
                                                                Dependent+Obsessive_compulsive), predData)
       
          
        }else if (grepl('ucgC', selectModelBIC[selectModelBIC$Outcome == input$outcome, 'Model'],  fixed = F)){
          model = lm(outcome ~ (SESSION+I(SESSION^2)+I(SESSION^3))*iniSev*(Paranoid+Schizoid+Schizotypal+Antisocial+
                                                                             Borderline+Histrionic+Narcissistic+Avoidant+
                                                                             Dependent+Obsessive_compulsive), predData)
        }
        
      
        filteredDf_pred = filteredDf %>%
          filter(SESSION!=0) %>%
          mutate(indPred = predict(model,.)) %>%
          select(ID, SESSION, indPred)
        
        filteredDf = 
          filteredDf %>%
          left_join(filteredDf_pred, by = c('ID', 'SESSION'))
        
        
        if (is.na(groupMean[2]) == F){
          
          meanSession = 
            filteredDf %>% 
            group_by(ID) %>%
            filter(row_number() == max(row_number())) %>%
            select(ID, SESSION) %>%
            ungroup() %>%
            summarize_at('SESSION', mean, na.rm = T) %>%
            round()
          
          if(meanSession==1){
            groupMeanPredData = c(SESSION = 1, groupMean)
            
          }else{
            groupMeanPredData = data.frame(SESSION = 1:meanSession[[1]],
                                           groupMean)
          }
            
          
          groupMeanPredVal = predict(model,  groupMeanPredData)
          
        }else{
          groupMeanPredVal = NA
          meanSession = 0 
        }
        
        
        filteredDf %>%   
          plot_ly() %>%
          add_trace(x = ~ SESSION, y = ~ outcome,
                    type = 'scatter', mode = 'markers', color = ~as.factor(ID),
                    #name = ~paste0('ID ', ID),
                    alpha = 0.5, showlegend = F,
                    text = ~paste0('<b>ID ', ID,'</b> \n',
                                   'Inital Severity:', round(iniSev, 2), '\n',
                                   'SESSION: ', SESSION, '\n',
                                   'Z-Score: ', round(outcome,2)),
                    hoverinfo= 'text') %>%
          add_lines(x = ~ SESSION, y = ~ indPred,
                    mode = 'lines', color = ~as.factor(ID),
                    name = ~paste0('ID ', ID),
                    alpha = 0.5, line = list(width = 3), 
                    text = ~paste0('<b>ID ', ID,'</b> \n',
                                   'Initial Severity: ', round(iniSev, 2), '\n',
                                   'SESSION: ', SESSION, '\n',
                                   'Pred Score: ', round(indPred,2)),
                    hoverinfo= 'text') %>%
          layout(
            hoverlabel = list(bordercolor = 'transparent', font = list(color = 'black')),
            xaxis = list(title = 'SESSION NUMBER',
                         linecolor = 'black',
                         zerolinecolor = '#ffff',
                         zerolinewidth = 2,
                         gridcolor = 'ffff',
                         range = list(0, 50)),
            
            yaxis = list(title = 'OUTCOME Z-SCORE',
                         linecolor = 'black',
                         zerolinecolor = '#ffff',
                         zerolinewidth = 2,
                         gridcolor = 'ffff',
                         range = list(-3, 7),
                         hoverformat = '.2r')
          ) %>%
          add_trace(x = c(1:meanSession[[1]]), y = groupMeanPredVal,
                    type = 'scatter', mode = 'lines+markers', 
                    line = list(color = 'black', width = 4),
                    marker = list(color = 'black', size = 3),
                    name = 'Group Mean',
                    text = paste0('Initial Severity: ', round(groupMean['iniSev'],2), '\n',
                                  'Mean Treatment Length: ', meanSession[[1]], ' Sessions \n'),
                    hoverinfo= 'text+x+y', mode='lines',
                    hovertemplate = '%{text}SESSION: %{x} \n Z-Score: %{y}'
          ) %>%
          layout(
            hoverlabel = list(bordercolor = 'transparent', font = list(color = 'white')),
            yaxis = list( hoverformat = '.2r')
          )
      }
    
  })
  textOutput <- reactive({
    selectModelBIC = read.csv('selectModelBIC.csv')
    if (grepl('ucgL', selectModelBIC[selectModelBIC$Outcome == input$outcome, 'Model'],  fixed = F)){
      modelDescription =  paste0('Model: Outcome ~ Session Number x Initial Severity x Personality Measures')
    } else if (grepl('ucgQ', selectModelBIC[selectModelBIC$Outcome == input$outcome, 'Model'],  fixed = F)){
      modelDescription =  paste0('Model: Outcome ~ (Session Number + Session Number',
                                 tags$sup("2"),') x Initial Severity x Personality Measures')
    }else if (grepl('ucgC', selectModelBIC[selectModelBIC$Outcome == input$outcome, 'Model'],  fixed = F)){
      modelDescription =  paste0('Model: Outcome ~ (Session Number + Session Number',
                                 tags$sup("2"),'+ Session Number',tags$sup("3"),') x Initial Severity x Personality Measures')
    }
    modelDescription
  })
  
  # Show the values in an HTML table ----
  output$outcomePlot <- renderPlotly({
    sliderOutput()
  })
  
  output$modelDescription =  renderText({ textOutput()})
 
  
}

shinyApp(ui = ui, server = server)
