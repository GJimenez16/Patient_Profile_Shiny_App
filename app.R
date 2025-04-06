library(shiny)
library(bslib)
library(bsicons)
library(shinyWidgets)
library(dplyr)
library(DT)
library(future)
library(promises)
library(random.cdisc.data)
library(plotly)


source("R/mod_valuebox.R")
source("R/plots.R")

ui <- page_navbar(
  
  title = "Patient Profile",
  theme = bs_theme(version = 5, 
                   preset = "shiny"
                   ),
  # Overview of Patient Profile -- demographics, disposition
  nav_panel(
    title = "General Overview",
    h2(paste("Study", unique(data$adsl$STUDYID))),
    
    
    layout_column_wrap(
      mod_valuebox_ui("vb_total_patients"),
      mod_valuebox_ui("vb_completed_tr"),
      mod_valuebox_ui("vb_ongoing_tr")
    ),
    
    card(
      card_header("Age distribution by Sex"),
      card_body(plotlyOutput("agedist"))
    ),
    
    card(
      card_header("Race Distribution"),
      card_body(plotlyOutput("racedist"))
    )
    
    
    
    
  ),
  
  # Tab for deeper dive by selecting one patient
  nav_panel(
    title = "Patient Overview",
    selectInput(inputId = "subjectid",label = "Choose Subject", choices = unique(data$adsl$SUBJID)),
    
    page_navbar(
      title = "",
      
      nav_panel(
        "Overview",
        
        layout_column_wrap(
          mod_valuebox_ui("patient_age", has_icon = FALSE),
          mod_valuebox_ui("patient_sex", has_icon = FALSE),
          mod_valuebox_ui("patient_race", has_icon = FALSE)
        ),
        
        layout_column_wrap(
          mod_valuebox_ui("patient_trtsdtm", has_icon = FALSE),
          mod_valuebox_ui("patient_trtedtm", has_icon = FALSE),
          mod_valuebox_ui("patient_eosstt", has_icon = FALSE),
          mod_valuebox_ui("patient_dcsreas", has_icon = FALSE)
        ),
        
        card(
          card_header("Adverse Events"),
          card_body(DTOutput("ae_table"))
        )
      ),
      
      nav_panel(
        "Adverse Events Timeline",
        card(
          card_header("AE Timeline"),
          card_body(plotlyOutput("ae_gantt"))
        )
      ),
      
      nav_panel(
        "Labs",
        layout_sidebar(
          fill = FALSE,
          
          sidebar = sidebar(
            pickerInput("labfilter", "Choose labs", 
                        choices = unique(data$adlb$LBTEST), 
                        selected = unique(data$adlb$LBTEST),
                        options = pickerOptions(actionsBox = TRUE),
                        multiple = TRUE)
          ),
          
          card(
            card_header("Timeline"),
            card_body(plotlyOutput("lab_plot"))
          ),
          
          card(
            card_header("Complete lab data"),
            card_body(DTOutput("lab_table"))
          ),
        )
      )
    )
  ),
  
  nav_panel(
    "eNotes",
    h2("eNotes"),
    
    card(
      card_body(
        h2("Still in development"),
        DTOutput("eNote_table"),
        layout_column_wrap(
          actionButton("save", "Save Changes"),
          actionButton("download", "Download eNotes")
        )
      )
    )
  )
)

server <- function(input,output, session) {
  
  filtered_adae <- reactive({
    
    req(input$subjectid)
    data$adae %>% 
      filter(input$subjectid == SUBJID)
    
  })
  filtered_adlb <- reactive({
    data$adlb %>% 
      filter(LBTEST %in% input$labfilter,
             SUBJID == filtered_adae()$SUBJID)
  })
  
  # General Overview panel
  
  mod_valuebox_server("vb_total_patients", "Total Patients", n_distinct(data$adsl$USUBJID), "person-standing")
  mod_valuebox_server("vb_completed_tr", title = "Completed treatments", value = nrow(subset(data$adsl, EOSSTT == "COMPLETED")), icon = "check-circle-fill")
  mod_valuebox_server("vb_ongoing_tr", "On going treatments", nrow(subset(data$adsl, EOSSTT == "ONGOING")), "clipboard2-pulse-fill")
  
  output$agedist <- renderPlotly({
    agedist_plot
  })
  
  output$racedist <- renderPlotly({
    racedist_plot
  })
  
  
  # Patient Overview panel
  output$ae_table <- renderDT({
    datatable(
      filtered_adae() %>%
        select(SUBJID,
               TRT01SDTM,
               TRT02SDTM,
               AETERM,
               AETOXGR,
               AESEV,
               AEACN,
               AEOUT,
               ASTDY,
               AENDY,
               AERELNST,
               AEACNOTH
        ),
      filter = "top"
    )
  })
  
  mod_valuebox_server("patient_age",title = "Age", value = paste(last(filtered_adae()$AGE)))
  
  mod_valuebox_server("patient_sex", title = "Sex", value = paste(last(filtered_adae()$SEX)))
  
  mod_valuebox_server("patient_race", title = "Race", value = paste(last(filtered_adae()$RACE)))
  
  mod_valuebox_server("patient_trtsdtm", title = "Date of first exposure to treatment (dd-mm-yyyy)", value = paste(format(last(filtered_adae()$TRTSDTM), "%d-%m-%Y")))
  
  mod_valuebox_server("patient_trtedtm", title = "Date of last exposure to treatment (dd-mm-yyyy)", value = paste(format(last(filtered_adae()$TRTEDTM), "%d-%m-%Y")))
  
  mod_valuebox_server("patient_eosstt", title = "End of Study Status", value = paste(last(filtered_adae()$EOSSTT)))
  
  mod_valuebox_server("patient_dcsreas", "Reason of Discontinuation", value = paste(last(filtered_adae()$DCSREAS)))

  
  
  # Gantt chart -- TODO: 2 events for the same AETERM issue
  output$ae_gantt <- renderPlotly({
    ae_gantt_plot(filtered_adae())
  })
  
  
  # Lab tab
  output$lab_plot <- renderPlotly({
    labs_plot(filtered_adlb()) 
  })
  
  output$lab_table <- renderDT({
    
    datatable(
      filtered_adlb() %>% 
            select(SUBJID,
                   LBTEST, 
                   LBCAT, 
                   AVAL, 
                   AVALU, 
                   BASE, 
                   CHG2,
                   ANRIND, 
                   ADTM, 
                   ADY, 
                   ANRLO, 
                   ANRHI),
      filter = "top"
    ) 
  })
  
}


shinyApp(ui, server)
