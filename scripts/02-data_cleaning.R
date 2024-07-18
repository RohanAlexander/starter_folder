#### Preamble ####
# Purpose: Cleans the raw survey data recored by Rohan at the end of STA302
# Author: Luca Carnegie
# Date: 16 July 2024
# Contact: luca.carnegie@mail.utoronto.ca or rohan.alexander@utoronto.ca
# Pre-requisites: 01-download_data.R

#### Workspace setup ####
library(tidyverse)
library(janitor)


#### Clean data ####
raw_data <- read_csv("data/raw_data/raw_STA302_postcourse_survey_w24.csv")

raw_data <- as_tibble(raw_data)

# Clean up names of columns

raw_data <- raw_data |> 
  clean_names()

raw_data <- raw_data |> 
  rename(
    consent = after_carefully_reading_the_informed_consent_document_please_indicate_below_whether_you_consent_to_have_your_anonymized_responses_included_in_the_research_study,
    student_name = what_is_your_full_name_on_quercus,
    student_id = what_is_your_student_id,
    year = what_year_are_you,
    program_of_study = what_is_your_specialization, 
    major = what_is_are_your_major_s,
    minor = what_is_are_your_minor_s,
    gpa = what_is_your_gpa, 
    ai_familiarity = how_familiar_are_you_with_using_generative_ai_tools_such_as_open_a_is_chat_gpt_or_equivalents, 
    used_ai_any_reason = have_you_used_any_generative_ai_tools_such_as_open_a_is_chat_gpt_or_equivalents_for_any_reason_personal_or_educational, 
    ai_use_cases = if_you_have_used_generative_ai_tools_such_as_open_a_is_chat_gpt_or_equivalents_in_what_ways_have_you_used_it_select_all_that_apply, 
    ai_schoolwork_appropriate = to_what_extent_do_you_think_using_generative_ai_tools_such_as_chat_gpt_by_open_ai_or_equivalents_is_ethical_and_appropriate_for_schoolwork, 
    ai_schoolwork_appropriate_comments = please_elaborate_on_your_answer_above,
    used_ai_in_sta302 = did_you_use_any_generative_ai_tools_such_as_open_a_is_chat_gpt_or_equivalents_for_sta302, 
    ai_helpful_weekly_quiz = weekly_quiz,
    ai_helpful_weekly_mini_essay = weekly_mini_essay,
    ai_helpful_papers_generating_ideas = papers_generating_ideas,
    ai_helpful_papers_writing_code = papers_writing_code,
    ai_helpful_papers_writing_content = papers_writing_content,
    ai_helpful_papers_improving_content = papers_improving_content,
    recs_for_ai_sta302 = what_is_your_recommendation_for_how_generative_ai_tools_such_as_chat_gpt_by_open_ai_or_equivalents_should_be_used_in_the_course_in_future, 
    other_comments = optional_any_other_comments
  )

# Drop rows for students who did not consent
raw_data <- raw_data |>
  filter(consent == "Yes, I authorize the use of the data collected about me for the STA302 course survey to be used. I will be compensated 1% of my course grade for completing the survey.")





# Drop unnecessary columns

raw_data <- raw_data |>
  select(-c(consent, 
            student_name, 
            student_id, 
            major, #none submitted
            minor, #none submitted
            
            ))


# Clean specialization column (text data oh god)

#column_clean <-

# Clean GPA column
  
# Assign NA or numeric -> trim down decimal pts to 1 point. 

testo <- raw_data |>
  mutate(GPA = function(grade) {
    ifelse(grade.is.numeric(), grade.as.numeric(), NA)
  }) |>
  mutate(GPA = round(GPA, 1))
  


# Fix text errors in self-perception question

# Replace erroneous characters with NA, etc. 

# Fix "Any other comments?" column


#### Save data ####
write_csv(cleaned_data, "data/analysis_data/clean_STA302_postcourse_survey_w24.csv")
