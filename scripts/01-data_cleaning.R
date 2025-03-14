#### Preamble ####
# Purpose: Prepare the data
# Author: Rohan Alexander
# Date: 12 August 2024
# Contact: rohan.alexander@utoronto.ca


#### Workspace setup ####
library(tidyverse)
library(here)
library(janitor)
library(readxl)
library(arrow)


#### Read in data ####
all_data <-
  read_excel(here("data/merged_data.xlsx"), sheet = "Sheet1") |>
  clean_names()


#### Clean data ####
# Add completion time
all_data <-
  all_data |>
  mutate(mins_complete = completion_time - start_time) |>
  filter(mins_complete < 1000) # Remove one student who took 4,000 minutes to complete

# Fix the "What year are you?"
all_data <-
  all_data |>
  mutate(
    what_year_are_you = case_when(
      what_year_are_you == "PEY" ~ "4th",
      # Change PEY to 4th year
      what_year_are_you == "5th" ~ "5th or over",
      what_year_are_you == "6th" ~ "5th or over",
      TRUE ~ what_year_are_you
    )
  )

# AI familiarity
all_data <-
  all_data |>
  rename(ai_familiarity = how_familiar_are_you_with_using_generative_ai_tools_such_as_open_a_is_chat_gpt_or_equivalents) |>
  mutate(
    ai_familiarity = case_when(
      ai_familiarity == "Limited to some liberal arts, such as history courses" ~ "Somewhat familiar",
      TRUE ~ ai_familiarity
    )
  ) |>
  mutate(ai_familiarity = factor(
    ai_familiarity,
    levels = c("Very familiar", "Somewhat familiar", "Not familiar")
  ))

# AI appropriateness
all_data <-
  all_data |>
  rename(ai_schoolwork_appropriate = to_what_extent_do_you_think_using_generative_ai_tools_such_as_chat_gpt_by_open_ai_or_equivalents_is_ethical_and_appropriate_for_schoolwork) |>
  mutate(
    ai_schoolwork_appropriate = case_when(
      ai_schoolwork_appropriate == "Depends of what the main goal of schoolwork is" ~ "It depends",
      ai_schoolwork_appropriate == "I believe student can only use it if they are allowed to" ~ "It depends",
      ai_schoolwork_appropriate == "a controllable amount" ~ "It depends",
      ai_schoolwork_appropriate == "appropriate depending on the context" ~ "It depends",
      ai_schoolwork_appropriate == "appropriate with rules for usage" ~ "It depends",
      ai_schoolwork_appropriate == "depends" ~ "It depends",
      ai_schoolwork_appropriate == "depends on how it is used" ~ "It depends",
      ai_schoolwork_appropriate == "half and half" ~ "It depends",
      ai_schoolwork_appropriate == "unsure" ~ "It depends",
      TRUE ~ ai_schoolwork_appropriate
    )
  ) |>
  mutate(ai_schoolwork_appropriate = factor(
    ai_schoolwork_appropriate,
    levels = c("Appropriate", "It depends", "Inappropriate")
  ))

# Change LLM usage
all_data <-
  all_data |>
  mutate(llm_usage =
           if_else(llm_usage %in% c("Minimal", "None"), "None or minimal", llm_usage)) |>
  mutate(llm_usage = factor(llm_usage, levels = c("Extensive", "Somewhat", "None or minimal")))

# Remove those who are missing GPA
all_data <-
  all_data |>
  filter(!is.na(what_is_your_gpa))

# Change name
all_data <-
  all_data |>
  rename(how_have_you_used = if_you_have_used_generative_ai_tools_such_as_open_a_is_chat_gpt_or_equivalents_in_what_ways_have_you_used_it_select_all_that_apply)

# Keep only relevant variables
all_data <-
  all_data |>
  select(
    -what_is_your_recommendation_for_how_generative_ai_tools_such_as_chat_gpt_by_open_ai_or_equivalents_should_be_used_in_the_course_in_future,
    -x33,
    -please_elaborate_on_your_answer_above,
    -start_time,
    -completion_time,
    -what_is_your_specialization,
    -what_is_are_your_major_s,
    -what_is_are_your_minor_s
  )


#### Save data ####
write_parquet(x = all_data, sink = "data/analysis_data.parquet")
