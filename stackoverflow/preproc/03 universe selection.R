source("init.R")

dataset = readRDS("data/clean/02_dummy.rds")

dataset = dataset %>% 
  filter(!is.na(ConvertedSalary)) %>% 
  filter(Country == "United States") %>% 
  filter(Employment == "Employed full-time") %>% 
  select(-Salary) %>% 
  select(-Respondent) %>% 
  select(-starts_with("Gender")) %>%
  select(-starts_with("SexualOrientation")) %>%
  # select(-starts_with("HypotheticalTools")) %>%
  # select(-starts_with("JobContactPriorities")) %>%
  # select(-starts_with("AssessJob")) %>%
  # select(-starts_with("AssessBenefits")) %>%
  # select(-starts_with("JobEmailPriorities")) %>%
  # select(-starts_with("AdsPriorities")) %>%
  filter(ConvertedSalary < 200000 & ConvertedSalary > 50000) %>% 
  filter(!is.na(Male) )

saveRDS(dataset,"data/clean/03_ready.rds")