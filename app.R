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
library(RMySQL)
library(pool)


# Enable multi-session (parallel execution)
plan(multisession)


# Pull data 
sapply(list.files("data/", pattern = "\\.R$", full.names = TRUE), source)

# Pull modules and plots needed in the app
sapply(list.files("R/", pattern = "\\.R$", full.names = TRUE), source)


################################################################################
# UI
################################################################################

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
        DTOutput("eNote_table"),
        layout_column_wrap(
          actionButton("add_row", "Add New Row", icon = icon("square-plus")),
          actionButton("save_db", "Save Changes", icon = icon("floppy-disk")),
          downloadButton("download", "Download eNotes", icon = icon("download"))
        )
      )
    )
  )
)

################################################################################
# Server
################################################################################

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
  db_table <- reactiveVal(enotes)
  
  ##############################################################################
  # General Overview panel
  ##############################################################################
  
  mod_valuebox_server("vb_total_patients", "Total Patients", n_distinct(data$adsl$USUBJID), "person-standing")
  mod_valuebox_server("vb_completed_tr", title = "Completed treatments", value = nrow(subset(data$adsl, EOSSTT == "COMPLETED")), icon = "check-circle-fill")
  mod_valuebox_server("vb_ongoing_tr", "On going treatments", nrow(subset(data$adsl, EOSSTT == "ONGOING")), "clipboard2-pulse-fill")
  
  output$agedist <- renderPlotly({
    agedist_plot
  })
  output$racedist <- renderPlotly({
    racedist_plot
  })
  
  ##############################################################################
  # Patient Overview panel
  ##############################################################################
  
  ### First tab
  mod_valuebox_server("patient_age",title = "Age", value = paste(last(filtered_adae()$AGE)))
  mod_valuebox_server("patient_sex", title = "Sex", value = paste(last(filtered_adae()$SEX)))
  mod_valuebox_server("patient_race", title = "Race", value = paste(last(filtered_adae()$RACE)))
  mod_valuebox_server("patient_trtsdtm", title = "Date of first exposure to treatment (dd-mm-yyyy)", value = paste(format(last(filtered_adae()$TRTSDTM), "%d-%m-%Y")))
  mod_valuebox_server("patient_trtedtm", title = "Date of last exposure to treatment (dd-mm-yyyy)", value = paste(format(last(filtered_adae()$TRTEDTM), "%d-%m-%Y")))
  mod_valuebox_server("patient_eosstt", title = "End of Study Status", value = paste(last(filtered_adae()$EOSSTT)))
  mod_valuebox_server("patient_dcsreas", "Reason of Discontinuation", value = paste(last(filtered_adae()$DCSREAS)))
  
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
  
  ### Gantt chart -- TODO: 2 events for the same AETERM issue
  output$ae_gantt <- renderPlotly({
    ae_gantt_plot(filtered_adae())
  })
  
  ### Lab tab
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
  
  ##############################################################################
  # eNotes panel
  ##############################################################################
  
  # eNotes DT
  output$eNote_table <- renderDT({
    datatable(
      db_table() %>% select(-row_names),
      filter = "top"
    )
  })
  
  ### Buttons functionality
  # Dispay popup when user hits "add row"
  observe({
    
    showModal(
      modalDialog(
        paste0("User:", Sys.info()["user"],
               "\n",
               "Date:", Sys.Date()),
        selectInput("select_patient", "Select Patient:", choices = unique(data$adsl$SUBJID)),
        selectInput("select_adam", "Select ADaM:", choices = names(data)),
        textInput("note", "Write Note"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_add", "Add Row")
        )
      )
    )
  }) %>% bindEvent(input$add_row)
  
  # Adding the new row to the db table
  observe({
    
    # Storing the new row
    new_row <- data.frame(
      row_names = max(as.numeric(db_table()$row_names)) + 1,
      User = as.character(Sys.info()["user"]),
      Date = Sys.Date(),
      Patient = input$select_patient,
      ADaM = input$select_adam,
      Comment = input$note,
      row.names = NULL
    )
    
    # Binding the new row with the db table
    db_table(rbind(db_table(), new_row))
    removeModal()
  }) %>% bindEvent(input$confirm_add)
  
  # Writing back to the database
  observe({
    
    # Define df outside future_promise because reactives only lives inside shiny
    final_df <- db_table() %>% select(-row_names)
    
    future_promise({
      con2 <-  dbConnect(MySQL(), 
                         user = db_user, 
                         password = db_password,
                         dbname = db_name, 
                         host = db_host, 
                         port = db_port)
      
      on.exit(DBI::dbDisconnect(con2), add = TRUE)  
      
      DBI::dbWriteTable(
        conn = con,
        name = "eNotes", 
        value = final_df,
        overwrite = TRUE         
      )
    }) %...>% {
      showNotification("Data saved to the database!", 
                       type = "message")
    } %...!% {
      showNotification(paste("Error:", .), 
                       type = "error")
       }
      
  }) %>% bindEvent(input$save_db)
  
  # Downloading eNotes as a csv
  output$download <- downloadHandler(
    filename = function() {
      paste0("notes_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(db_table() %>% select(-row_names), file, row.names = FALSE)
    }
  )
}


shinyApp(ui, server)

