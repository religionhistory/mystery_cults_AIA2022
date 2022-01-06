#  Make nexus 

rm(list = ls())

source("../project_support.r")

# Load data
data <- fread("./input/drh_data.csv")
analysis_questions <- fread("./input/questions.csv")

# Set names for joining
setnames(analysis_questions, c("Poll_1", "Question_ID_1", "Question_1"), c("Poll", "Question ID", "Question"))

# Filter only questions of interests
data_filt <- data[`Question ID` %in% analysis_questions$`Question ID`]

# Convert questions from different polls into equivalent v6 questions
data_stand <- analysis_questions[data_filt, on = c("Question ID", "Question", "Poll")][
  , `Question ID` := if_else(Poll != "Religious Group (v6)" & !is.na(Question_ID_2), Question_ID_2, `Question ID`)][
  , Question := if_else(Poll != "Religious Group (v6)" & !is.na(Question_2), Question_2, Question)][
  , c("Poll", "Poll_2", "Question_ID_2", "Question_2", "Question description", "Parent question ID", "Parent question", "Variable types", "Parent answer", "Parent answer value") := NULL]

# Remove Field doesn't know and I don't know answers 
data_stand <- data_stand[Answers != "I don't know" & Answers != "Field doesn't know"]

# Convert to tibble
data_tib <- as_tibble(data_stand) %>%
  select(`Entry ID`, `Entry name`, `Question ID`, `Answer values`) %>%
  arrange(`Question ID`) %>%
  distinct() 

# Handle disparate answers to the same questions
data_multi_ans <- data_tib %>% 
  group_by(`Entry ID`, `Entry name`, `Question ID`) %>% 
  tally() %>%
  filter(n > 1) %>% 
  left_join(data_tib) %>%
  mutate(`Answer values` = ifelse(lead(`Answer values`) == 1 & `Answer values` == 0 | lag(`Answer values`) == 1 & `Answer values` == 0 | lead(`Answer values`) == 0 & `Answer values` == 1 | lag(`Answer values`) == 0 & `Answer values` == 1, "{01}", `Answer values`)) %>%
  select(-n) %>%
  distinct()

# Recombine with single answers
data_single_ans <- data_tib %>% 
  group_by(`Entry ID`, `Entry name`, `Question ID`) %>% 
  tally() %>%
  filter(n == 1) %>% 
  left_join(data_tib) %>%
  select(-n)
data_ans <- bind_rows(data_single_ans, data_multi_ans)

# Convert from long to wide
data_t <- data_ans %>%
  ungroup() %>%
  pivot_wider(names_from = `Question ID`, values_from = `Answer values`)

# Remove constant columns
data_non_constant <- data_t[sapply(data_t, function(x) length(unique(na.omit(x)))) > 1]

# Filter questions with <50% missing answers
question_50_na <- miss_var_summary(data_non_constant) %>%
  filter(variable != "Entry ID" & variable != "Entry name") %>%
  mutate(pct_miss = round(pct_miss, 2)) %>%
  filter(pct_miss < 50)

# Select questions with <50% missing answers
data_na_filt <- data_non_constant %>%
  select(`Entry ID`, `Entry name`, all_of(question_50_na$variable))

# Convert to nexus format
nexus_data <- nexus_formatting(data_na_filt)

# Create question dictionary
question_dict <- as_tibble(data_stand) %>%
  select(`Question ID`, Question) %>%
  filter(`Question ID` %in% colnames(data_na_filt)) %>%
  distinct() %>%
  arrange(`Question ID`)

# Create output directory
make.dir("./output")

# Save data
write_csv(data_non_constant, "./output/mystery_cults.csv")
write_csv(question_dict, "./output/question_dict.csv")
write_morph_nexus(nexus_data, file = "./output/mystery_cults.nex")

rm(list = ls())
