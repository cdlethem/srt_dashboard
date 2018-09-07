require(jsonlite)
require(dplyr)
require(reshape)
require(ggplot2)
require(ggthemes)

dat_df <- bind_rows(fromJSON("srtfetch.json"))

prospies_df <- select(
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

prospies_df$SRT_Liaison <- factor(prospies_df$SRT_Liaison)

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

prospies_df$Pipeline_Status <- factor(
  prospies_df$Pipeline_Status,
  levels = pipeline_statuses
)
# 
# pivot <-
#   cast(melt(prospies_df),
#        SRT_Liaison ~ Pipeline_Status,
#        fun.aggregate = length)
# 
# pivot <- pivot %>%
#   mutate(
#     Interviewed = ANS + `ANS - Expired` + `Decision Pending` + Declined + `Interview Pending` + Reject + Signed + Waitlist
#   )

pipeline_colors <- c("#60992D","#B1E285", "#C5F29D", "#9F4A54",  "#418AD8",  "#F2E94E",  "#E3170A", "#A3D9FF", "#98E0BC", "#F34213")
names(pipeline_colors) <- pipeline_statuses

pipeline_breakdown <- ggplot(prospies_df, aes(SRT_Liaison)) +
  geom_bar(aes(fill = Pipeline_Status)) +
  coord_flip() +
  theme_fivethirtyeight() + 
  ggtitle("Pipeline Breakdown by SRT Liaison") +
  scale_fill_manual(values = pipeline_colors)

liaison_pl_summary <- function(liaison) {
  df <- filter(prospies_df, SRT_Liaison == liaison)
  p <- ggplot(df, aes(Pipeline_Status)) +
    geom_bar(aes(fill = Pipeline_Status)) + 
    theme_fivethirtyeight() +
    ggtitle(paste0(liaison," Pipeline Breakdown")) +
    scale_fill_manual(values = pipeline_colors) 
  
  return(p)
}

# saveRDS(prospies_df,file = "prospieDF.RData")
