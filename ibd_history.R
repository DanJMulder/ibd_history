## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
##   Shiny App for Quickly Summarizing an IBD Patient's History                              ##
##   Written by Daniel Mulder, Feb 2022                                                      ##
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##

## Preamble
# This script does not constitute medical advice and is only to be used for the purposes of learning or preparing personal templates
# This script contains no real medical information, any information contained within is fictional example information

# Load required packages ----
library(shiny) # for interactive web application framework
library(tidyverse) # for basic data organization
library(glue) # for gluing together text
library(lubridate) # for creating/parsing date objects

# Columns from the database spreadsheet
ibd_history_columns <- c("name",
                         "date_of_birth",
                         "sex",
                         "cr",
                         "diagnosis",
                         "presentation",
                         "location",
                         "diagnosis_date",
                         "paris_classification",
                         "induction_therapy",
                         "ASCA_ANCA",
                         "eims",
                         "complications",
                         "last_endoscopy",
                         "last_mre",
                         "last_pucai_pcdai",
                         "last_tdm",
                         "fecal_calprotectin",
                         "therapy_history",
                         "current_therapy",
                         "dose",
                         "frequency",
                         "covered",
                         "insurance_company",
                         "other_notes")

#Source in my previous patient data
ibd_database <- read_csv("IBD.csv",
                                col_types = list(col_character(), col_character(), col_character(), col_character(), col_character(), col_character(), col_character(),
                                              col_character(), col_character(), col_character(), col_character(), col_character(), col_character(), col_character(), col_character(), col_character(),
                                              col_character(), col_character(), col_character(), col_character(), col_character(), col_character(), col_character(), col_character(), col_character()))


ui <- fluidPage(
  br(),
  tabsetPanel(type = "tabs",
              tabPanel("Input",
                       br(),
                       tags$b("IBD History:"),
                       br(),
                       br(),
                       numericInput("cr_number", width = '50%', "Record Number:", value = 1234567, min = 0, max = 999999999),
                       # load demographic data if patient already known to me
                       actionButton("load_demographics", "Load demographics (for known patients)", icon("download")),
                       br(),
                       br(),
                       textInput("name", width = '50%', "Patient name (First Last):", value = ""),
                       textInput("date_of_birth", width = '50%', "Date of Birth (YYYY-MM-DD):", value = "2010-01-01"),
                       textOutput("current_age"),
                       br(),
                       selectInput("sex", width = '50%', "Sex:", choices = c("female", "male", "nonbinary person")),
                       textInput("diagnosis", width = '50%', "Diagnosis - ", value = ""),
                       textAreaInput("presentation", width = '50%', "Presentation - ", value = ""),
                       textAreaInput("location", width = '50%', rows = 6, "Location - ", value = ""),
                       textInput("diagnosis_date", width = '50%', "Date of Diagnosis (YYYY-MM-DD):", value = "2021-01-01"),
                       textInput("paris_classification", width = '50%', "Paris Classification - ", value = ""),
                       textInput("induction_therapy", width = '50%', "Induction Therapy - ", value = ""),
                       textInput("ASCA_ANCA", width = '50%', "IBD Serology - ", value = ""),
                       textInput("eims", width = '50%', "Extraintestinal Manifestations - ", value = ""),
                       textAreaInput("complications", width = '50%', "Complications - ", value = ""),
                       textInput("last_endoscopy", width = '50%', "Last Endoscopy (YYYY-MM-DD) - ", value = ""),
                       textInput("last_mre", width = '50%', "Last MRE - ", value = ""),
                       textInput("last_pucai_pcdai", width = '50%', "Last PUCAI/PCDAI - ", value = ""),
                       textInput("last_tdm", width = '50%', "Last TDM - ", value = ""),
                       textInput("fecal_calprotectin", width = '50%', "Fecal Calprotectin - ", value = ""),
                       textAreaInput("therapy_history", width = '50%', rows = 5, "Therapy History - ", value = ""),
                       textAreaInput("current_therapy", width = '50%', "Current Therapy - ", value = ""),
                       textInput("dose", width = '50%', "Current Therapy Dose - ", value = ""),
                       textInput("frequency", width = '50%', "Current Therapy Frequency - ", value = ""),
                       textInput("covered", width = '50%', "Current Therapy Covered - ", value = ""),
                       textInput("insurance_company", width = '50%', "Insurance Company - ", value = ""),
                       textAreaInput("other_notes", width = '50%', rows = 5, "Other Notes - ", value = ""),
                       br(),
                       actionButton("save", "Update Database", icon("database")),
                       br(),
                       br()
              ),
              
              tabPanel("Text Output (for copy/paste)",
                       
                       # Tab 2: Output (text note) Preview ----
                       br(),
                       tagAppendAttributes(textOutput("full_note"), style = "white-space:pre-wrap;"),
                       br(),
                       actionButton("save", "Update Database", icon("database")),
                       br(),
                       br()
              )
  )
)


server <- function(input, output, session) {
  
  # When the load demographics button is pressed, autofill the previous info...
  # by first running the "loadDemographics function and then updating the info using the "updateText" etc functions
  
  loadDemographics <- reactive({
    if (input$cr_number %in% ibd_database$cr) {
      is_known_patient <<- TRUE
      print("Matching record found, loading prior information...")
      known_patient <<- filter(ibd_database, ibd_database$cr == input$cr_number)
      knonw_patient_cr <<- known_patient[[4]]
      known_patient_name <<- known_patient[[1]][1]
      known_patient_dob <<- known_patient[[2]][1]
      known_patient_sex <<- known_patient[[3]][1]
      known_patient_diagnosis <<- known_patient[[5]][1]
      known_patient_presentation <<- known_patient[[6]][1]
      known_patient_location <<- known_patient[[7]][1]
      known_patient_date <<- known_patient[[8]][1]
      known_patient_paris <<- known_patient[[9]][1]
      known_patient_induction <<- known_patient[[10]][1]
      known_patient_asca_anca <<- known_patient[[11]][1]
      known_patient_eims <<- known_patient[[12]][1]
      known_patient_complications <<- known_patient[[13]][1]
      known_patient_last_endo <<- known_patient[[14]][1]
      known_patient_last_mre <<- known_patient[[15]][1]
      known_patient_last_ai <<- known_patient[[16]][1]
      known_patient_last_tdm <<- known_patient[[17]][1]
      known_patient_fcal <<- known_patient[[18]][1]
      known_patient_therapy_history <<- known_patient[[19]][1]
      known_patient_current_therapy <<- known_patient[[20]][1]
      known_patient_dose <<- known_patient[[21]][1]
      known_patient_frequency <<- known_patient[[22]][1]
      known_patient_covered <<- known_patient[[23]][1]
      known_patient_insurance <<- known_patient[[24]][1]
      known_patient_other_notes <<- known_patient[[25]][1]
      print(paste0(known_patient))
    } else {
      is_known_patient <<- FALSE
      print("No prior matching records found")
    }
  })
  
  observeEvent(input$load_demographics, {
    loadDemographics()
    updateTextInput(session, "name",
                    value =
                      if (is_known_patient == TRUE) {
                        paste(known_patient_name)
                      })
    updateTextInput(session, "date_of_birth",
                    value =
                      if (is_known_patient == TRUE) {
                        paste(known_patient_dob)
                      })
    updateTextInput(session, "sex",
                      value =
                        if (is_known_patient == TRUE) {
                          paste(known_patient_sex)
                        })
    updateTextInput(session, "diagnosis",
                    value =
                      if (is_known_patient == TRUE) {
                        paste(known_patient_diagnosis)
                      })
    updateTextInput(session, "presentation",
                    value =
                      if (is_known_patient == TRUE) {
                        paste(known_patient_presentation)
                      })
    updateTextInput(session, "location",
                    value =
                      if (is_known_patient == TRUE) {
                        paste(known_patient_location)
                      })
    updateTextInput(session, "diagnosis_date",
                    value =
                      if (is_known_patient == TRUE) {
                        paste(known_patient_date)
                      })
    updateTextInput(session, "paris_classification",
                    value =
                      if (is_known_patient == TRUE) {
                        paste(known_patient_paris)
                      })
    updateTextInput(session, "induction_therapy",
                    value =
                      if (is_known_patient == TRUE) {
                        paste(known_patient_induction)
                      })
    updateTextInput(session, "ASCA_ANCA",
                    value =
                      if (is_known_patient == TRUE) {
                        paste(known_patient_asca_anca)
                      })
    updateTextInput(session, "eims",
                    value =
                      if (is_known_patient == TRUE) {
                        paste(known_patient_eims)
                      })
    updateTextInput(session, "complications",
                    value =
                      if (is_known_patient == TRUE) {
                        paste(known_patient_complications)
                      })
    updateTextInput(session, "last_endoscopy",
                    value =
                      if (is_known_patient == TRUE) {
                        paste(known_patient_last_endo)
                      })
    updateTextInput(session, "last_mre",
                    value =
                      if (is_known_patient == TRUE) {
                        paste(known_patient_last_mre)
                      })
    updateTextInput(session, "last_pucai_pcdai",
                    value =
                      if (is_known_patient == TRUE) {
                        paste(known_patient_last_ai)
                      })
    updateTextInput(session, "last_tdm",
                    value =
                      if (is_known_patient == TRUE) {
                        paste(known_patient_last_tdm)
                      })
    updateTextInput(session, "fecal_calprotectin",
                    value =
                      if (is_known_patient == TRUE) {
                        paste(known_patient_fcal)
                      })
    updateTextInput(session, "therapy_history",
                    value =
                      if (is_known_patient == TRUE) {
                        paste(known_patient_therapy_history)
                      })
    updateTextInput(session, "current_therapy",
                    value =
                      if (is_known_patient == TRUE) {
                        paste(known_patient_current_therapy)
                      })
    updateTextInput(session, "dose",
                    value =
                      if (is_known_patient == TRUE) {
                        paste(known_patient_dose)
                      })
    updateTextInput(session, "frequency",
                    value =
                      if (is_known_patient == TRUE) {
                        paste(known_patient_frequency)
                      })
    updateTextInput(session, "covered",
                    value =
                      if (is_known_patient == TRUE) {
                        paste(known_patient_covered)
                      })
    updateTextInput(session, "insurance_company",
                    value =
                      if (is_known_patient == TRUE) {
                        paste(known_patient_insurance)
                      })
    updateTextInput(session, "other_notes",
                    value =
                      if (is_known_patient == TRUE) {
                        paste(known_patient_other_notes)
                      })
  })
  
  output$current_age <- renderText({
    
    age_raw <-
      if (is.na(input$date_of_birth)) {
        paste("Unable to calculate current age")
      } else {
        as.period(interval(start = input$date_of_birth, end = today()))
      }

    age_text <-
      if(age_raw$year < 2){
        if(age_raw$month <1){
          paste(age_raw$day, "day old")
        } else {
            paste(age_raw$month + (12*age_raw$year), "month old")}
      } else {
          paste(age_raw$year, "year", age_raw$month, "month old")}

    return(paste("Current age:", age_text))
  })
  
  # Updating Database ----
  
  # when the save button is pressed, the function below will load the IBD spreadsheet, update it, and save the new info to it
  # if there is no "encounter_data.csv" file in the working directory then the code below will create one
  # if there already is a "encounter_data.csv" file, then the code below will add a line to it
  
  observeEvent(input$save, {
    
    # below are a series of objects created from the textbox information input into the app
    
    responses_database <- read_csv("IBD.csv",
                                   col_types = list(col_character(), col_character(), col_character(), col_character(), col_character(), col_character(), col_character(),
                                                    col_character(), col_character(), col_character(), col_character(), col_character(), col_character(), col_character(), col_character(), col_character(),
                                                    col_character(), col_character(), col_character(), col_character(), col_character(), col_character(), col_character(), col_character(), col_character()))
    
    this_patient <- t(as.data.frame(c(input$name,
                                      input$date_of_birth,
                                      input$sex,
                                      input$cr_number,
                                      input$diagnosis,
                                      input$presentation,
                                      input$location,
                                      input$diagnosis_date,
                                      input$paris_classification,
                                      input$induction_therapy,
                                      input$ASCA_ANCA,
                                      input$eims,
                                      input$complications,
                                      input$last_endoscopy,
                                      input$last_mre,
                                      input$last_pucai_pcdai,
                                      input$last_tdm,
                                      input$fecal_calprotectin,
                                      input$therapy_history,
                                      input$current_therapy,
                                      input$dose,
                                      input$frequency,
                                      input$covered,
                                      input$insurance_company,
                                      input$other_notes)))
    colnames(this_patient) <- c("name",
                                "date_of_birth",
                                "sex",
                                "cr",
                                "diagnosis",
                                "presentation",
                                "location",
                                "diagnosis_date",
                                "paris_classification",
                                "induction_therapy",
                                "ASCA_ANCA",
                                "eims",
                                "complications",
                                "last_endoscopy",
                                "last_mre",
                                "last_pucai_pcdai",
                                "last_tdm",
                                "fecal_calprotectin",
                                "therapy_history",
                                "current_therapy",
                                "dose",
                                "frequency",
                                "covered",
                                "insurance_company",
                                "other_notes")
    filtered_db <- responses_database %>%
      filter(input$cr_number != cr)
    updated_db <- rbind(filtered_db, this_patient)
    write_csv(updated_db, "IBD2.csv")
    
  })
  
  # When the save button is clicked, the function below will save the text as a .docx file
  # (also saves as a txt file in case user does not use office)
  # In order to create the filename it requires the arguments: patient_name, cr and visit_type
  # the rest of the note is created from the formData() function above
  
  formData <- reactive({
    return(glue("IBD History:",
                "\n",
                paste("Diagnosis -", input$diagnosis),
                "\n",
                paste("Presentation -", input$presentation),
                "\n",
                paste("Disease Location -", input$location),
                "\n",
                paste("Diagnosis Date -", ymd(input$diagnosis_date)),
                "\n",
                paste("Paris Classification -", input$paris_classification),
                "\n",
                paste("Induction Therapy -", input$induction_therapy),
                "\n",
                paste("IBD Serology -", input$ASCA_ANCA),
                "\n",
                paste("Extra-intestinal Manifestations -", input$eims),
                "\n",
                paste("Complications -", input$complications),
                "\n",
                paste("Last Endoscopy -", input$last_endoscopy),
                "\n",
                paste("Last MRE -", input$last_mre),
                "\n",
                paste("Last PUCAI or PCDAI -", input$last_pucai_pcdai),
                "\n",
                paste("Last TDM -", input$last_tdm),
                "\n",
                paste("Fecal Calprotectin -", input$fecal_calprotectin),
                "\n",
                paste("Therapy History -", input$therapy_history),
                "\n",
                paste("Current Therapy -", input$current_therapy),
                "\n",
                paste("Dose -", input$dose),
                "\n",
                paste("Frequency -", input$frequency),
                "\n",
                paste("Drug Covered -", input$covered),
                "\n",
                paste("Insurance Company -", input$insurance_company),
                "\n",
                paste("Other Notes -", input$other_notes)
    )
    )
  })
  
  output$full_note <- renderPrint(formData())
  
}

shinyApp(ui, server)
