require(jsonlite)
require(dplyr)
require(reshape)
require(ggplot2)
require(ggthemes)
require(shiny)
require(DT)

pipeline_statuses <- c(
  "Signed",
  "ANS",
  "ANS - Expired",
  "Declined",
  "Decision Pending",
  "Waitlist",
  "Reject",
  "Interview Pending",
  "Warm lead",
  "Dead lead"
)

pipeline_colors <-
  c(
    "#60992D",
    "#B1E285",
    "#C5F29D",
    "#9F4A54",
    "#418AD8",
    "#F2E94E",
    "#E3170A",
    "#A3D9FF",
    "#98E0BC",
    "#F34213"
  )

names(pipeline_colors) <- pipeline_statuses

shinyServer(function(input, output) {
  cleaned_dat <- reactive({
    df <- bind_rows(fromJSON("srtfetch.json"))
    
    df <- select(
      dat_df,
      "Pinyin_Name" = "job-title",
      "English_Name" = name,
      "Chinese_Name" = 'chinese-name-characters',
      "Gender" = gender,
      "Graduation_Year" = 'graduation-year-2',
      "Market" = pool,
      "Region" = "pool-2",
      "SRT_Liaison" = "srt-liaison",
      "SRT_Liaison_(Full)" = "srt-liaison-2",
      "Pipeline_Status" = "pipeline-status",
      "Events_Attended" = "events-attended",
      "Number_of_Events_Attended" = "number-of-events-attended",
      "Source" = source,
      "Student_Wechat" = "wechat-id",
      "Student_Phone_Number" = "phone-number",
      "Student_Email" = "email-address",
      "Mothers_Name_Chinese" = "mothers-name-chinese",
      "Mothers_Phone" = "mothers-phone",
      "Mothers_Wechat" = "mothers-wechat",
      "Mothers_Email" = "mothers-email",
      "Fathers_Name_Chinese" = "fathers-name-chinese",
      "Fathers_Phone" = "fathers-phone",
      "Fathers_Wechat" = "fathers-wechat",
      "Fathers_Email" = "fathers-email",
      "Has_Parent_Contact?" = "parent-contact-captured",
      "Number_of_Outreach" = "of-times-contacted",
      "POT_Call_List" = "call-list-2",
      "Nominator(s)" = "nominators",
      "High_Test?" = "hi-test",
      "Lead_Acquisition_Date" = "lead-acquisition-date",
      "Interviewer" = "interviewer",
      "Interview_Date" = "interview-date"
    )
    
    df$SRT_Liaison <- factor(df$SRT_Liaison)
    
    df$Pipeline_Status <- factor(df$Pipeline_Status,
                                 levels = pipeline_statuses)
    df$Market <- factor(df$Market)
    df$Region <- factor(df$Region)
    df$Source <- factor(df$Source)
    df$Lead_Acquisition_Date <-
      as.Date(df$Lead_Acquisition_Date, "%Y-%m-%d")
    df$Events_Attended <-
      gsub(", (?=,|$)",
           "",
           as.character(prospies_df$Events_Attended),
           perl = TRUE)
    
    df
    
  })
  
  filtered_dat <- reactive({
    df_0 <- cleaned_dat()
    
    df_0
    
  })
  
  pivot <- reactive({
    df <-
      cast(melt(
        filtered_dat(),
        SRT_Liaison ~ Pipeline_Status,
        fun.aggregate = length
      ))
    
    df <- mutate(
      filtered_dat(),
      Interviewed = ANS +
        `ANS - Expired` +
        `Decision Pending` +
        Declined +
        `Interview Pending` +
        Reject +
        Signed +
        Waitlist
    )
    
    df
    
  })
  
  output$events_attended_ui <- renderUI({
    selectizeInput(inputId = "events_attended",
                   label = "Events Attended",
                   multiple = TRUE,
                   choices = unique(unlist(
                     sapply(cleaned_dat()$Events_Attended,
                            function(x) {
                              strsplit(x, split = ", ", fixed = TRUE)
                            })
                   )))
  })
  
  output$region_ui <- renderUI({
    selectizeInput(inputId = "region",
                   label = h4("Region"),
                   multiple = TRUE,
                   choices = unique(cleaned_dat()$Region))
  })
  
output$call_list <- renderDT(cleaned_dat())
  
  
})