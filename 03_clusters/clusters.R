# Extract clusters and find discriminating questions between clusters

rm(list = ls())

source("../project_support.r")

# Load data
data <- read_csv("./input/mystery_cults.csv")
dictionary <- read_csv("./input/question_dict.csv")
tree <- read.nexus(file = "./input/mystery_cults.tree")

# Extract clusters
tip_lab <- data.frame(`Entry ID` = tree$tip.label, check.names = FALSE)
data_cluster <- tip_lab %>%
  mutate(`Entry ID` = as.numeric(`Entry ID`)) %>%
  left_join(data) %>%
  mutate(Subcluster = ifelse(`Entry name` == "Valentinians" |
                             `Entry name` == 'The New Prophecy or ""Montanism""' |
                             `Entry name` == "Marcionites" |
                             `Entry name` == "The Essenes" | 
                             `Entry name` == "Sethian Gnostic" |
                             `Entry name` == "Eastern Roman Manichaeism" |
                             `Entry name` == "Theurgy", "C1.2",
                    ifelse(`Entry name` == "Roman Divination" |
                             `Entry name` == "Mithraism" | 
                             `Entry name` ==  "Paul the Apostle" |
                             `Entry name` ==  "Ancient Thessalians" |
                             `Entry name` ==  "Roman Imperial Cult" |
                             `Entry name` ==  "Archaic Spartan Cults" |
                             `Entry name` ==  "Religion in Greco-Roman Alexandria" |
                             `Entry name` ==  "Roman private religion" |
                             `Entry name` ==  "Religion in Roman Ostia" |
                             `Entry name` == "Cult of Isis (Mysteries of Isis)" |
                             `Entry name` == "Orphism", "C2", "C1.1"))) %>%
  mutate(Cluster = case_when(Subcluster == "C1.1" | Subcluster == "C1.2" ~ "C1",
                             Subcluster == "C2" ~ "C2")) %>%
  select(`Entry ID`, `Entry name`, Subcluster, Cluster, everything()) 

# Find distinguishing questions between clusters
clusters_quest <- data_cluster %>%
  mutate(Cluster = as.factor(as.character(Cluster))) %>%
  select(-Subcluster) %>%
  select(-`Entry ID`, -`Entry name`) %>%
  mutate_all(as.character) %>%
  pivot_longer(c(-Cluster), names_to = "Question", values_to = "Answers") %>%
  group_by(Cluster, Question, Answers) %>%
  summarise(Frequency = n(), .groups = "keep") %>%
  ungroup() %>%
  group_by(Cluster, Question) %>%
  mutate(group_total = sum(Frequency)) %>%
  group_by(Cluster, Question, Answers) %>%
  mutate(Percentage = case_when(Cluster == "C1" ~ Frequency/group_total * 100,
                                Cluster == "C2" ~ Frequency/group_total * 100))  %>%
  mutate(Percentage = round(Percentage, 2)) %>%
  select(-Frequency, -group_total) %>%
  pivot_wider(names_from = Cluster, values_from = Percentage) %>%
  mutate(across(C1:C2, ~ifelse(is.na(.), 0, .))) %>%
  ungroup() %>%
  mutate(Difference = abs(C1 - C2)) %>%
  rename("Question ID" = "Question") %>%
  mutate(`Question ID` = as.numeric(`Question ID`)) %>%
  inner_join(dictionary) %>%
  select(`Question ID`, Question, everything()) %>%
  arrange(desc(Difference)) %>%
  mutate(Answers = case_when(Answers == "1" ~ "Yes", Answers == "0" ~ "No", Answers == "{01}" ~ "Yes or No", is.na(Answers) ~ "Missing"))

# Reformat data for exploration
discr_quest <- clusters_quest %>%
  group_by(`Question ID`) %>%
  mutate(Difference = ifelse(Answers == "Missing", 0, Difference)) %>%
  mutate(total_diff = sum(Difference)) %>%
  ungroup() %>%
  select(-Difference) %>%
  pivot_wider(names_from = Answers, names_sep = " ", values_from = c(C1, C2)) %>%
  mutate(across(starts_with("C"), ~ifelse(is.na(.), 0, .))) %>%
  arrange(desc(total_diff)) %>%
  select(-total_diff) %>%
  select(`Question ID`, Question, `C1 Yes`, `C1 No`, `C1 Missing`, `C2 Yes`, `C2 No`, `C2 Missing`)

# Find distinguishing questions between subclusters
subclusters <- data_cluster %>%
  filter(Cluster == "C1") %>%
  select(-Cluster) %>%
  mutate(Subcluster = as.factor(as.character(Subcluster))) %>%
  select(-`Entry ID`, -`Entry name`) %>%
  mutate_all(as.character) %>%
  pivot_longer(c(-Subcluster), names_to = "Question", values_to = "Answers") %>%
  group_by(Subcluster, Question, Answers) %>%
  summarise(Frequency = n(), .groups = "keep") %>%
  ungroup() %>%
  group_by(Subcluster, Question) %>%
  mutate(group_total = sum(Frequency)) %>%
  group_by(Subcluster, Question, Answers) %>%
  mutate(Percentage = case_when(Subcluster == "C1.1" ~ Frequency/group_total * 100,
                                Subcluster == "C1.2" ~ Frequency/group_total * 100)) %>%
  mutate(Percentage = round(Percentage, 2)) %>%
  select(-Frequency, -group_total) %>%
  pivot_wider(names_from = Subcluster, values_from = Percentage) %>%
  mutate(across(C1.1:C1.2, ~ifelse(is.na(.), 0, .))) %>%
  ungroup() %>%
  mutate(Difference = abs(C1.1 - C1.2)) %>%
  rename("Question ID" = "Question") %>%
  mutate(`Question ID` = as.numeric(`Question ID`)) %>%
  inner_join(dictionary) %>%
  select(`Question ID`, Question, everything()) %>%
  arrange(desc(Difference)) %>%
  mutate(Answers = case_when(Answers == "1" ~ "Yes", Answers == "0" ~ "No", Answers == "{01}" ~ "Yes or No", is.na(Answers) ~ "Missing"))

# Reformat data for exploration
subcluster_discr_quest <- subclusters %>%
  group_by(`Question ID`) %>%
  mutate(Difference = ifelse(Answers == "Missing", 0, Difference)) %>%
  mutate(total_diff = sum(Difference)) %>%
  ungroup() %>%
  select(-Difference) %>%
  pivot_wider(names_from = Answers, names_sep = " ", values_from = c(C1.1, C1.2)) %>%
  mutate(across(starts_with("C"), ~ifelse(is.na(.), 0, .))) %>%
  arrange(desc(total_diff)) %>%
  select(-total_diff) %>%
  select(`Question ID`, Question, `C1.1 Yes`, `C1.1 No`, `C1.1 Missing`, `C1.1 Yes or No`, `C1.2 Yes`, `C1.2 No`, `C1.2 Missing`, `C1.2 Yes or No`)

# Create output directory
make.dir("./output")

# Save data
write_csv(discr_quest, "./output/C1vC2.csv")
write_csv(subcluster_discr_quest, "./output/C1.1vC1.2.csv")

rm(list = ls())


