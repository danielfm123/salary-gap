source("init.R")

zz=gzfile('data/input/StackOverflow/developer_survey_2018/survey_results_public.csv.gz','rt')   
dataset_raw = read.csv(zz,header = T)

satisfaction_levels = c("Extremely dissatisfied",
                        "Moderately dissatisfied",
                        "Slightly dissatisfied",
                        "Neither satisfied nor dissatisfied",
                        "Slightly satisfied",
                        "Moderately satisfied",
                        "Extremely satisfied")

agree_levels = c("Strongly disagree",
                 "Disagree",
                 "Neither Agree nor Disagree",
                 "Agree",
                 "Strongly agree")

rangeToFactor = function(x){
  x = str_split(x,"-|\\sto\\s")
  x = map(x,str_remove_all,"[a-z\\sA-Z\\,]")
  x = map_dbl(x,function(y) round(mean(as.numeric(y))) )
  # x = factor(x, ordered = T)
  return(x)
}


toNPS = function(x){
    # val =  (x>5) - 1*(x<5)
    # val = factor(val,c(-1,0,1),order=T)
    return(x)
}

toNPS1_10 = function(x){
  x = str_extract(x,"^[0-9]+")
  x = as.numeric(x)
  # x = factor(-1*(x < 7) + (x > 8)*1,ordered = T)
  return(x)
}

# tipos de datos
dataset = dataset %>% 
  mutate(Hobby = Hobby == "Yes",
         OpenSource = OpenSource == "Yes",
         Male = recode(Gender,
                       "Male" = T,
                       "Female" = F,
                       .default = NA),
         Dependents = Dependents == "Yes",
         MilitaryUS = MilitaryUS == "Yes") %>%
  mutate(JobSearchStatus = factor(JobSearchStatus, c('I am actively looking for a job',
                                                     'I’m not actively looking, but I am open to new opportunities',
                                                     'I am not interested in new job opportunities'),
                                  ordered = T),
         LastNewJob = factor(LastNewJob,c('I\'ve never had a job',
                                          'Less than a year ago',
                                          'Between 1 and 2 years ago',
                                          'Between 2 and 4 years ago',
                                          'More than 4 years ago'),
                             ordered = T),
         TimeFullyProductive = factor(TimeFullyProductive,c('Less than a month',
                                                            'One to three months',
                                                            'Three to six months',
                                                            'Six to nine months',
                                                            'Nine months to a year',
                                                            'More than a year'),
                                      ordered = T),
         TimeAfterBootcamp = factor(TimeAfterBootcamp,
                                    c("I already had a full-time job as a developer when I began the program",
                                      "Immediately after graduating",
                                      "Less than a month",
                                      "One to three months",
                                      "Four to six months",
                                      "Six months to a year",
                                      "Longer than a year",
                                      "I haven’t gotten a developer job"
                                    ),ordered = T),
         CheckInCode = factor(CheckInCode,c("Multiple times per day",
                                            "Once a day",
                                            "A few times per week",
                                            "Weekly or a few times per month",
                                            "Less than once per month",
                                            "Never"),ordered = T),
         EthicsChoice = factor(EthicsChoice,c("No",
                                              "Depends on what it is",
                                              "Yes"),ordered = T),
         StackOverflowConsiderMember = factor(StackOverflowConsiderMember,c("No",
                                                                            "I'm not sure",
                                                                            "Yes"),ordered = T),
         StackOverflowVisit = factor(StackOverflowVisit,c("Multiple times per day",
                                                          "Daily or almost daily",
                                                          "A few times per week",
                                                          "A few times per month or weekly",
                                                          "Less than once per month or monthly",
                                                          "I have never visited Stack Overflow (before today)"),ordered = T),
         StackOverflowParticipate = factor(StackOverflowParticipate,c("Multiple times per day",
                                                                      "Daily or almost daily",
                                                                      "A few times per week",
                                                                      "A few times per month or weekly",
                                                                      "Less than once per month or monthly",
                                                                      "I have never visited Stack Overflow (before today)"),ordered = T),
         SurveyTooLong = factor(SurveyTooLong,c("The survey was too short",
                                                "The survey was an appropriate length",
                                                "The survey was too long"),ordered = T),
         SurveyEasy = factor(SurveyEasy,c("Very difficult",
                                          "Somewhat difficult",
                                          "Neither easy nor difficult",
                                          "Somewhat easy",
                                          "Very easy"),ordered = T),
         WakeTime = factor(WakeTime,c("Before 5:00 AM",
                                      "Between 5:00 - 6:00 AM",
                                      "Between 6:01 - 7:00 AM",
                                      "Between 7:01 - 8:00 AM",
                                      "Between 8:01 - 9:00 AM",
                                      "Between 9:01 - 10:00 AM",
                                      "Between 10:01 - 11:00 AM",
                                      "Between 11:01 AM - 12:00 PM",
                                      "After 12:01 PM",
                                      "I work night shifts"),ordered = T)) %>% 
  mutate(NumberMonitors = recode(NumberMonitors,"More than 4" = "5"),
         HoursOutside = recode(HoursOutside,
                               "Less than 30 minutes" = 0.5,
                               "30 - 59 minutes" = 1,
                               "1 - 2 hours" = 2,
                               "3 - 4 hours" = 3,
                               "Over 4 hours" = 4),
         HoursOutside = factor(HoursOutside, ordered = T),
         SkipMeals = recode(SkipMeals,
                            "Never" = 0,
                            "1 - 2 times per week" = 2,
                            "3 - 4 times per week" = 4,
                            "Daily or almost every day" = 7),
         SkipMeals = factor(SkipMeals, ordered = T),
         Exercise = recode(Exercise,
                            "I don't typically exercise" = 0,
                            "1 - 2 times per week" = 2,
                            "3 - 4 times per week" = 4,
                            "Daily or almost every day" = 7),
         Exercise = factor(Exercise, ordered = T)
         ) %>% 
  mutate_at(c("StackOverflowRecommend","StackOverflowJobsRecommend"),toNPS1_10) %>% 
  mutate_at(vars(matches("Satisfaction$")),factor,satisfaction_levels,ordered = T) %>% 
  mutate_at(vars(matches("^AgreeDisagree|^AdsAgreeDisagree")),factor,agree_levels,ordered = T) %>% 
  mutate_at(vars(matches("^JobEmailPriorities|^AssessBenefits|^AssessJob|^AdsPriorities")),toNPS) %>% 
  mutate_at(vars(starts_with("HypotheticalTools")),factor,c("Not at all interested",
                                                            "Somewhat interested",
                                                            "A little bit interested",
                                                            "Very interested",
                                                            "Extremely interested")) %>% 
  mutate_at(c("YearsCoding","YearsCodingProf","HoursComputer","NumberMonitors","CompanySize","Age"),rangeToFactor) %>% 
  mutate_at(multiple_opt,str_split,";")

saveRDS(dataset,"clean/01_categorisado.rds")
         
