#Load the required packages

library(readxl) #Read Excel files
library(tidyverse) # Load the tidyverse packages
library(afex) # ANOVA functions
library(emmeans) # Needed for pairwise comparisons
library(ggplot2) # For bar charts
library(ggthemes) # Extended theme package
library(stats) #For Median Absolute Deviation
library(visdat) #Check for missing data
library(lsr) #Cohen's d
library(ggpubr) #Q-Q plots
library(rstatix) #Statistical analysis R functions and partial eta squared

#Read in our data

file.list <- list.files(pattern='*.xlsx')
df.list <- lapply(file.list, read_excel)
SSDT_data <- plyr::ldply(df.list, data.frame)

#Calculate mean age and count gender

age <- SSDT_data %>%
  group_by(Subject) %>%
  summarise(age_subject = mean(Age)) %>%
  summarise(mean_age = mean(age_subject), sd_age = sd(age_subject))

gender <- SSDT_data %>%
  group_by(Gender, Subject) %>%
  summarise(gender_rows = n()) %>%
  summarise(count_gender = n())

#Tyding up our data

tidied_SSDT_data <- SSDT_data %>%
  rename(Procedure = Procedure.Block.,
         Light_side = Side,
         Response = ResponsePrompt.RESP,
         Trial_type = TrialType,
         Correct_Response= CorrectResp) %>%
  filter(Procedure == 'TrialsProc') %>%
  select(Subject,
         Light_side,
         Trial_type,
         Response,
         Correct_Response) %>%
  mutate(Response = recode(Response,
                           '{-1}' = 'Yes',
                           '{-2}' = 'No')) %>%
  mutate(Correct_Response = recode(Correct_Response,
                                   '{-1}' = 'Yes',
                                   '{-2}' = 'No')) %>%
  mutate(Trial_type = recode(Trial_type,
                             'Light' = 'Light/No Touch',
                             'Weak' = 'No Light/Touch',
                             'Both' = 'Light/Touch',
                             'catch' = 'No Light/No Touch')) %>%
  mutate(Outcome = ifelse(Trial_type == 'Light/No Touch' & Response == 'No', 'Correct Rejection', 
                   ifelse(Trial_type == 'Light/No Touch' & Response == 'Yes', 'False Alarm',
                   ifelse(Trial_type == 'No Light/Touch' & Response == 'No', 'Miss',
                   ifelse(Trial_type == 'No Light/Touch' & Response == 'Yes', 'Hit',
                   ifelse(Trial_type == 'Light/Touch' & Response == 'No', 'Miss',
                   ifelse(Trial_type == 'Light/Touch' & Response == 'Yes', 'Hit',
                   ifelse(Trial_type == 'No Light/No Touch' & Response == 'No', 'Correct Rejection',
                          'False Alarm'))))))))

#Check for missing data

vis_miss(tidied_SSDT_data)

#Counting Hits, False Alarms, Misses, and Correct Rejections

tidied_SSDT_data_by_subject <- tidied_SSDT_data %>% 
  mutate(left_present_hit = ifelse(Light_side == 'Left' &
                                   Trial_type == 'Light/Touch' &
                                   Outcome == 'Hit', 1, 0), 
         left_present_miss = ifelse(Light_side == 'Left' & 
                                    Trial_type == 'Light/Touch' &
                                    Outcome == 'Miss', 1, 0),
         left_present_FA = ifelse(Light_side == 'Left' & 
                                  Trial_type == 'Light/No Touch' &
                                  Outcome == 'False Alarm', 1, 0),
         left_present_CJ = ifelse(Light_side == 'Left' & 
                                  Trial_type == 'Light/No Touch' &
                                  Outcome == 'Correct Rejection', 1, 0)) %>%
  mutate(left_absent_hit = ifelse(Light_side == 'Left' &
                                  Trial_type == 'No Light/Touch' &
                                  Outcome == 'Hit', 1, 0),
         left_absent_miss = ifelse(Light_side == 'Left' & 
                                   Trial_type == 'No Light/Touch' &
                                   Outcome == 'Miss', 1, 0),
         left_absent_FA = ifelse(Light_side == 'Left' & 
                                 Trial_type == 'No Light/No Touch' &
                                 Outcome == 'False Alarm', 1, 0),
         left_absent_CJ = ifelse(Light_side == 'Left' & 
                                 Trial_type == 'No Light/No Touch' &
                                 Outcome == 'Correct Rejection', 1, 0)) %>%
  mutate(right_present_hit = ifelse(Light_side == 'Right' &
                                    Trial_type == 'Light/Touch' &
                                    Outcome == 'Hit', 1, 0),
         right_present_miss = ifelse(Light_side == 'Right' & 
                                     Trial_type == 'Light/Touch' &
                                     Outcome == 'Miss', 1, 0),
         right_present_FA = ifelse(Light_side == 'Right' & 
                                   Trial_type == 'Light/No Touch' &
                                   Outcome == 'False Alarm', 1, 0),
         right_present_CJ = ifelse(Light_side == 'Right' & 
                                   Trial_type == 'Light/No Touch' &
                                   Outcome == 'Correct Rejection', 1, 0)) %>%
  mutate(right_absent_hit = ifelse(Light_side == 'Right' &
                                   Trial_type == 'No Light/Touch' &
                                   Outcome == 'Hit', 1 ,0),
         right_absent_miss = ifelse(Light_side == 'Right' & 
                                    Trial_type == 'No Light/Touch' &
                                    Outcome == 'Miss', 1 ,0),
         right_absent_FA = ifelse(Light_side == 'Right' & 
                                  Trial_type == 'No Light/No Touch' &
                                  Outcome == 'False Alarm', 1, 0),
         right_absent_CJ = ifelse(Light_side == 'Right' & 
                                  Trial_type == 'No Light/No Touch' &
                                  Outcome == 'Correct Rejection', 1, 0)) %>%
  group_by(Subject) %>% 
  summarise(n_left_present_hit = sum(left_present_hit),
            n_left_present_miss = sum(left_present_miss),
            n_left_present_FA = sum(left_present_FA),
            n_left_present_CJ = sum(left_present_CJ),
            
            n_left_absent_hit = sum(left_absent_hit),
            n_left_absent_miss = sum(left_absent_miss),
            n_left_absent_FA = sum(left_absent_FA),
            n_left_absent_CJ = sum(left_absent_CJ),
            
            n_right_present_hit = sum(right_present_hit),
            n_right_present_miss = sum(right_present_miss),
            n_right_present_FA = sum(right_present_FA),
            n_right_present_CJ = sum(right_present_CJ),
            
            n_right_absent_hit = sum(right_absent_hit),
            n_right_absent_miss = sum(right_absent_miss),
            n_right_absent_FA = sum(right_absent_FA),
            n_right_absent_CJ = sum(right_absent_CJ))

#Calculating the rates of the dependent variables

DV_Rates <- tidied_SSDT_data_by_subject %>%
  mutate(Hit_Rate_Left_Present = (n_left_present_hit + 0.5)/(n_left_present_hit + n_left_present_miss +1),
         FA_Rate_Left_Present = (n_left_present_FA + 0.5)/(n_left_present_FA + n_left_present_CJ +1),
         ZHR_Left_Present = qnorm(Hit_Rate_Left_Present),
         ZFA_Left_Present = qnorm(FA_Rate_Left_Present)) %>%
  mutate(Hit_Rate_Left_Absent = (n_left_absent_hit + 0.5)/(n_left_absent_hit + n_left_absent_miss +1),
         FA_Rate_Left_Absent = (n_left_absent_FA + 0.5)/(n_left_absent_FA + n_left_absent_CJ +1),
         ZHR_Left_Absent = qnorm(Hit_Rate_Left_Absent),
         ZFA_Left_Absent = qnorm(FA_Rate_Left_Absent)) %>%
  mutate(Hit_Rate_Right_Present = (n_right_present_hit + 0.5)/(n_right_present_hit + n_right_present_miss +1),
         FA_Rate_Right_Present = (n_right_present_FA + 0.5)/(n_right_present_FA + n_right_present_CJ +1),
         ZHR_Right_Present = qnorm(Hit_Rate_Right_Present),
         ZFA_Right_Present = qnorm(FA_Rate_Right_Present)) %>%
  mutate(Hit_Rate_Right_Absent = (n_right_absent_hit + 0.5)/(n_right_absent_hit + n_right_absent_miss +1),
         FA_Rate_Right_Absent = (n_right_absent_FA + 0.5)/(n_right_absent_FA + n_right_absent_CJ +1),
         ZHR_Right_Absent = qnorm(Hit_Rate_Right_Absent),
         ZFA_Right_Absent = qnorm(FA_Rate_Right_Absent)) %>%
  select(Subject,
         
         Hit_Rate_Left_Present,
         FA_Rate_Left_Present,
         ZHR_Left_Present,
         ZFA_Left_Present,
         
         Hit_Rate_Left_Absent,
         FA_Rate_Left_Absent,
         ZHR_Left_Absent,
         ZFA_Left_Absent,
         
         Hit_Rate_Right_Present,
         FA_Rate_Right_Present,
         ZHR_Right_Present,
         ZFA_Right_Present,
         
         Hit_Rate_Right_Absent,
         FA_Rate_Right_Absent,
         ZHR_Right_Absent,
         ZFA_Right_Absent)
  
#Calculating d' and c estimates

d_and_c <- DV_Rates %>%
  mutate(d_Prime_Left_Present = ZHR_Left_Present - ZFA_Left_Present,
         c_Left_Present = (ZHR_Left_Present + ZFA_Left_Present)*(-0.5),
         beta_Left_Present = exp(-ZHR_Left_Present*ZHR_Left_Present/2+ZFA_Left_Present*ZFA_Left_Present/2)) %>%
  mutate(d_Prime_Left_Absent = ZHR_Left_Absent - ZFA_Left_Absent,
         c_Left_Absent = (ZHR_Left_Absent + ZFA_Left_Absent)*(-0.5),
         beta_Left_Absent = exp(-ZHR_Left_Absent*ZHR_Left_Absent/2+ZFA_Left_Absent*ZFA_Left_Absent/2)) %>%
  mutate(d_Prime_Right_Present = ZHR_Right_Present - ZFA_Right_Present,
         c_Right_Present = (ZHR_Right_Present+ZFA_Right_Present)*(-0.5),
         beta_Right_Present = exp(-ZHR_Right_Present*ZHR_Right_Present/2+ZFA_Right_Present*ZFA_Right_Present/2)) %>%
  mutate(d_Prime_Right_Absent = ZHR_Right_Absent - ZFA_Right_Absent,
         c_Right_Absent = (ZHR_Right_Absent+ZFA_Right_Absent)*(-0.5),
         beta_Right_Absent = exp(-ZHR_Right_Absent*ZHR_Right_Absent/2+ZFA_Right_Absent*ZFA_Right_Absent/2)) %>%
  select(Subject,
         
         d_Prime_Left_Present,
         c_Left_Present,
         beta_Left_Present,
         
         d_Prime_Left_Absent,
         c_Left_Absent,
         beta_Left_Absent,
         
         d_Prime_Right_Present,
         c_Right_Present,
         beta_Right_Present,
         
         d_Prime_Right_Absent,
         c_Right_Absent,
         beta_Right_Absent)

#Calculating MAD to replace outliers

MAD_Table <- d_and_c %>%
  mutate(MAD_d_Prime_Left_Present = mad(d_Prime_Left_Present),
         MAD_d_Prime_Left_Absent = mad(d_Prime_Left_Absent),
         MAD_d_Prime_Right_Present = mad(d_Prime_Right_Present),
         MAD_d_Prime_Right_Absent = mad(d_Prime_Right_Absent)) %>%
  
  mutate(Median_d_Prime_Left_Present = median(d_Prime_Left_Present),
         Median_d_Prime_Left_Absent = median(d_Prime_Left_Absent),
         Median_d_Prime_Right_Present = median(d_Prime_Right_Present),
         Median_d_Prime_Right_Absent = median(d_Prime_Right_Absent)) %>%
  
  mutate(High_d_Prime_Left_Present = Median_d_Prime_Left_Present + 3*MAD_d_Prime_Left_Present,
         High_d_Prime_Left_Absent = Median_d_Prime_Left_Absent + 3*MAD_d_Prime_Left_Absent,
         High_d_Prime_Right_Present = Median_d_Prime_Right_Present + 3*MAD_d_Prime_Right_Present,
         High_d_Prime_Right_Absent = Median_d_Prime_Right_Absent + 3*MAD_d_Prime_Right_Absent) %>%
  
  mutate(Low_d_Prime_Left_Present = Median_d_Prime_Left_Present - 3*MAD_d_Prime_Left_Present,
         Low_d_Prime_Left_Absent = Median_d_Prime_Left_Absent - 3*MAD_d_Prime_Left_Absent,
         Low_d_Prime_Right_Present = Median_d_Prime_Right_Present - 3*MAD_d_Prime_Right_Present,
         Low_d_Prime_Right_Absent = Median_d_Prime_Right_Absent - 3*MAD_d_Prime_Right_Absent) %>%
  
  mutate(MAD_c_Left_Present = mad(c_Left_Present),
         MAD_c_Left_Absent = mad(c_Left_Absent),
         MAD_c_Right_Present = mad(c_Right_Present),
         MAD_c_Right_Absent = mad(c_Right_Absent)) %>%
  
  mutate(Median_c_Left_Present = median(c_Left_Present),
         Median_c_Left_Absent = median(c_Left_Absent),
         Median_c_Right_Present = median(c_Right_Present),
         Median_c_Right_Absent = median(c_Right_Absent)) %>%
  
  mutate(High_c_Left_Present = Median_c_Left_Present + 3*MAD_c_Left_Present,
         High_c_Left_Absent = Median_c_Left_Absent + 3*MAD_c_Left_Absent,
         High_c_Right_Present = Median_c_Right_Present + 3*MAD_c_Right_Present,
         High_c_Right_Absent = Median_c_Right_Absent + 3*MAD_c_Right_Absent) %>%
  
  mutate(Low_c_Left_Present = Median_c_Left_Present - 3*MAD_c_Left_Present,
         Low_c_Left_Absent = Median_c_Left_Absent - 3*MAD_c_Left_Absent,
         Low_c_Right_Present = Median_c_Right_Present - 3*MAD_c_Right_Present,
         Low_c_Right_Absent = Median_c_Right_Absent - 3*MAD_c_Right_Absent)

updated_d_and_c <- MAD_Table %>% #Median +- 3*MAD +- 0.1 so we preserve their 
                                 #nature of being outliers (beyond 3*MAD), 
                                 #but not up to the point where they bias the data
  mutate(d_Prime_Left_Present = ifelse(d_Prime_Left_Present > High_d_Prime_Left_Present, 
                                       High_d_Prime_Left_Present + 0.1, 
                                ifelse(d_Prime_Left_Present < Low_d_Prime_Left_Present, 
                                       Low_d_Prime_Left_Present - 0.1,
                                       d_Prime_Left_Present))) %>%
  mutate(d_Prime_Left_Absent = ifelse(d_Prime_Left_Absent > High_d_Prime_Left_Absent,
                                      High_d_Prime_Left_Absent + 0.1,
                               ifelse(d_Prime_Left_Absent < Low_d_Prime_Left_Absent,
                                      Low_d_Prime_Left_Absent - 0.1,
                                      d_Prime_Left_Absent))) %>%
  mutate(d_Prime_Right_Present = ifelse(d_Prime_Right_Present > High_d_Prime_Right_Present,
                                        High_d_Prime_Right_Present + 0.1,
                                 ifelse(d_Prime_Right_Present < Low_d_Prime_Right_Present,
                                        Low_d_Prime_Right_Present - 0.1,
                                        d_Prime_Right_Present))) %>%
  mutate(d_Prime_Right_Absent = ifelse(d_Prime_Right_Absent > High_d_Prime_Right_Absent,
                                       High_d_Prime_Right_Absent + 0.1,
                                 ifelse(d_Prime_Right_Absent < Low_d_Prime_Right_Absent,
                                        Low_d_Prime_Right_Absent - 0.1,
                                        d_Prime_Right_Absent))) %>%
  
  mutate(c_Left_Present = ifelse(c_Left_Present > High_c_Left_Present, 
                                 High_c_Left_Present + 0.1, 
                          ifelse(c_Left_Present < Low_c_Left_Present, 
                                 Low_c_Left_Present - 0.1,
                                 c_Left_Present))) %>%
  mutate(c_Left_Absent = ifelse(c_Left_Absent > High_c_Left_Absent,
                                High_c_Left_Absent + 0.1,
                         ifelse(c_Left_Absent < Low_c_Left_Absent,
                                Low_c_Left_Absent - 0.1,
                                c_Left_Absent))) %>%
  mutate(c_Right_Present = ifelse(c_Right_Present > High_c_Right_Present,
                                  High_c_Right_Present + 0.1,
                           ifelse(c_Right_Present < Low_c_Right_Present,
                                  Low_c_Right_Present - 0.1,
                                  c_Right_Present))) %>%
  mutate(c_Right_Absent = ifelse(c_Right_Absent > High_c_Right_Absent,
                                 High_c_Right_Absent + 0.1,
                          ifelse(c_Right_Absent < Low_c_Right_Absent,
                                 Low_c_Right_Absent - 0.1,
                                 c_Right_Absent)))

#Rearranging data to conduct factorial ANOVA for d'

d_Prime <- updated_d_and_c %>%
  pivot_longer(cols = c(d_Prime_Left_Present, 
                        d_Prime_Left_Absent, 
                        d_Prime_Right_Present, 
                        d_Prime_Right_Absent),
               names_to = 'Condition',
               values_to = 'd_Prime') %>%
  mutate(Light = ifelse(Condition == 'd_Prime_Left_Present', 'Present',
                 ifelse(Condition == 'd_Prime_Left_Absent', 'Absent',
                 ifelse(Condition == 'd_Prime_Right_Present', 'Present', 'Absent')))) %>%
  mutate(Light_Location = ifelse(Condition == 'd_Prime_Left_Present', 'Left',
                          ifelse(Condition == 'd_Prime_Left_Absent', 'Left',
                          ifelse(Condition == 'd_Prime_Right_Present', 'Right', 'Right')))) %>%
  mutate(Light = factor(Light), Light_Location = factor(Light_Location)) %>%
  select(Subject,
         Light_Location,
         Light,
         d_Prime)
  #Check normality assumption with Q-Q plots and Shapiro-Wilk test
  ggqqplot(d_Prime$d_Prime)
  shapiro.test(d_Prime$d_Prime) #Normal distribution

F1_ANOVA_d <- aov_4(d_Prime ~ Light_Location * Light + 
                  (1 + Light_Location * Light | Subject), 
                  data = d_Prime, na.rm = TRUE)
anova(F1_ANOVA_d)

#Interpretation of ANOVA for d'

#Adjust with Bonferroni yourself because otherwise it makes 6 comparisons not 4
emmeans(F1_ANOVA_d, pairwise ~ Light_Location * Light, adjust = 'none')

#Rearranging data to conduct factorial ANOVA for c

c_estimate <- updated_d_and_c %>%
  pivot_longer(cols = c(c_Left_Present, 
                        c_Left_Absent, 
                        c_Right_Present, 
                        c_Right_Absent),
               names_to = 'Condition',
               values_to = 'c') %>%
  mutate(Light = ifelse(Condition == 'c_Left_Present', 'Present',
                 ifelse(Condition == 'c_Left_Absent', 'Absent',
                 ifelse(Condition == 'c_Right_Present', 'Present', 'Absent')))) %>%
  mutate(Light_Location = ifelse(Condition == 'c_Left_Present', 'Left',
                          ifelse(Condition == 'c_Left_Absent', 'Left',
                          ifelse(Condition == 'c_Right_Present', 'Right', 'Right')))) %>%
  mutate(Light = factor(Light), Light_Location = factor(Light_Location)) %>%
  select(Subject,
         Light_Location,
         Light,
         c)
  #Check normality assumption with Q-Q plots and Shapiro-Wilk test
  ggqqplot(c_estimate$c)
  shapiro.test(c_estimate$c) #Normal distribution

F1_ANOVA_c <- aov_4(c ~ Light_Location * Light + 
                    (1 + Light_Location * Light | Subject), 
                    data = c_estimate, na.rm = TRUE)
anova(F1_ANOVA_c)

#Interpretation of ANOVA for c

#Adjust with Bonferroni yourself because otherwise it makes 6 comparisons not 4
emmeans(F1_ANOVA_c, pairwise ~ Light_Location * Light, adjust = 'none')

#Secondary analysis for hit rates

Hit_Rate_ANOVA <- DV_Rates %>%
  pivot_longer(cols = c(Hit_Rate_Left_Present, 
                        Hit_Rate_Left_Absent, 
                        Hit_Rate_Right_Present, 
                        Hit_Rate_Right_Absent),
               names_to = 'Condition',
               values_to = 'Hit_Rate') %>%
  mutate(Light = ifelse(Condition == 'Hit_Rate_Left_Present', 'Present',
                 ifelse(Condition == 'Hit_Rate_Left_Absent', 'Absent',
                 ifelse(Condition == 'Hit_Rate_Right_Present', 'Present', 'Absent')))) %>%
  mutate(Light_Location = ifelse(Condition == 'Hit_Rate_Left_Present', 'Left',
                          ifelse(Condition == 'Hit_Rate_Left_Absent', 'Left',
                          ifelse(Condition == 'Hit_Rate_Right_Present', 'Right', 'Right')))) %>%
  mutate(Light = factor(Light), Light_Location = factor(Light_Location)) %>%
  select(Subject,
         Light_Location,
         Light,
         Hit_Rate)

  #Check normality assumption with Q-Q plots and Shapiro-Wilk test
  ggqqplot(Hit_Rate_ANOVA$Hit_Rate) #Negatively skewed
  shapiro.test(Hit_Rate_ANOVA$Hit_Rate) #Not normally distributed - requires correction
  
Log_Hit_Rate_ANOVA <- Hit_Rate_ANOVA %>%
  mutate(Hit_Rate = log10(Hit_Rate))
  shapiro.test(Log_Hit_Rate_ANOVA$Hit_Rate)

Sqrt_Hit_Rate_ANOVA <- Hit_Rate_ANOVA %>%
  mutate(Hit_Rate = sqrt(Hit_Rate))
  shapiro.test(Sqrt_Hit_Rate_ANOVA$Hit_Rate)
  #Normalisation failed - continue with non-parametric tests

  #Filter data for Wilcoxon Signed-Rank Tests

Wilcoxon_HR_Light_Location_Left <- Hit_Rate_ANOVA %>%
  filter(Light_Location == 'Left') %>%
  wilcox_test(Hit_Rate ~ Light, paired = TRUE) %>%
  add_significance()
  Wilcoxon_HR_Light_Location_Left
  
Wilcoxon_HR_Light_Location_Right <- Hit_Rate_ANOVA %>%
  filter(Light_Location == 'Right') %>%
  wilcox_test(Hit_Rate ~ Light, paired = TRUE) %>%
  add_significance()
  Wilcoxon_HR_Light_Location_Right
    
Wilcoxon_HR_Light_Present <- Hit_Rate_ANOVA %>%
  filter(Light == 'Present') %>%
  wilcox_test(Hit_Rate ~ Light_Location, paired = TRUE) %>%
  add_significance()
  Wilcoxon_HR_Light_Present

Wilcoxon_HR_Light_Absent <- Hit_Rate_ANOVA %>%
  filter(Light == 'Absent') %>%
  wilcox_test(Hit_Rate ~ Light_Location, paired = TRUE) %>%
  add_significance()
  Wilcoxon_HR_Light_Absent

  #Adjust p-value to 0.05/4 = 0.0125
  
#Effect size for Wilcoxon HR

Wilcoxon_HR_Light_Location_Left_ES <- Hit_Rate_ANOVA %>%
  filter(Light_Location == 'Left') %>%
  wilcox_effsize(Hit_Rate ~ Light, paired = TRUE)
  Wilcoxon_HR_Light_Location_Left_ES
  
Wilcoxon_HR_Light_Location_Right_ES <- Hit_Rate_ANOVA %>%
  filter(Light_Location == 'Right') %>%
  wilcox_effsize(Hit_Rate ~ Light, paired = TRUE)
  Wilcoxon_HR_Light_Location_Right_ES

Wilcoxon_HR_Light_Present_ES <- Hit_Rate_ANOVA %>%
  filter(Light == 'Present') %>%
  wilcox_effsize(Hit_Rate ~ Light_Location, paired = TRUE)
  Wilcoxon_HR_Light_Present_ES

Wilcoxon_HR_Light_Absent_ES <- Hit_Rate_ANOVA %>%
  filter(Light == 'Absent') %>%
  wilcox_effsize(Hit_Rate ~ Light_Location, paired = TRUE)
  Wilcoxon_HR_Light_Absent_ES

#Secondary analysis for false alarm rates

FA_Rate_ANOVA <- DV_Rates %>%
  pivot_longer(cols = c(FA_Rate_Left_Present, 
                        FA_Rate_Left_Absent, 
                        FA_Rate_Right_Present, 
                        FA_Rate_Right_Absent),
               names_to = 'Condition',
               values_to = 'FA_Rate') %>%
  mutate(Light = ifelse(Condition == 'FA_Rate_Left_Present', 'Present',
                 ifelse(Condition == 'FA_Rate_Left_Absent', 'Absent',
                 ifelse(Condition == 'FA_Rate_Right_Present', 'Present', 'Absent')))) %>%
  mutate(Light_Location = ifelse(Condition == 'FA_Rate_Left_Present', 'Left',
                          ifelse(Condition == 'FA_Rate_Left_Absent', 'Left',
                          ifelse(Condition == 'FA_Rate_Right_Present', 'Right', 'Right')))) %>%
  mutate(Light = factor(Light), Light_Location = factor(Light_Location)) %>%
  select(Subject,
         Light_Location,
         Light,
         FA_Rate)

  #Check normality assumption with Q-Q plots and Shapiro-Wilk test
  ggqqplot(FA_Rate_ANOVA$FA_Rate) #Negatively skewed
  shapiro.test(FA_Rate_ANOVA$FA_Rate) #Not normally distributed - requires correction

Log_FA_Rate_ANOVA <- FA_Rate_ANOVA %>%
  mutate(FA_Rate = log10(FA_Rate))
  shapiro.test(Log_FA_Rate_ANOVA$FA_Rate)

Sqrt_FA_Rate_ANOVA <- FA_Rate_ANOVA %>%
  mutate(FA_Rate = sqrt(FA_Rate))
  shapiro.test(Sqrt_FA_Rate_ANOVA$FA_Rate)
#Normalisation failed - continue with non-parametric tests

  #Filter data for Wilcoxon Signed-Rank Tests
  
Wilcoxon_FA_Rate_Light_Location_Left <- FA_Rate_ANOVA %>%
  filter(Light_Location == 'Left') %>%
  wilcox_test(FA_Rate ~ Light, paired = TRUE) %>%
  add_significance()
  Wilcoxon_FA_Rate_Light_Location_Left
  
Wilcoxon_FA_Rate_Light_Location_Right <- FA_Rate_ANOVA %>%
  filter(Light_Location == 'Right') %>%
  wilcox_test(FA_Rate ~ Light, paired = TRUE) %>%
  add_significance()
  Wilcoxon_FA_Rate_Light_Location_Right
  
Wilcoxon_FA_Rate_Light_Present <- FA_Rate_ANOVA %>%
  filter(Light == 'Present') %>%
  wilcox_test(FA_Rate ~ Light_Location, paired = TRUE) %>%
  add_significance()
  Wilcoxon_FA_Rate_Light_Present
  
Wilcoxon_FA_Rate_Light_Absent <- FA_Rate_ANOVA %>%
  filter(Light == 'Absent') %>%
  wilcox_test(FA_Rate ~ Light_Location, paired = TRUE) %>%
  add_significance()
  Wilcoxon_FA_Rate_Light_Absent
  
  #Adjust p-value to 0.05/4 = 0.0125
  
  #Effect size for Wilcoxon FA
  
Wilcoxon_FA_Rate_Light_Location_Left_ES <- FA_Rate_ANOVA %>%
  filter(Light_Location == 'Left') %>%
  wilcox_effsize(FA_Rate ~ Light, paired = TRUE)
  Wilcoxon_FA_Rate_Light_Location_Left_ES
  
Wilcoxon_FA_Rate_Light_Location_Right_ES <- FA_Rate_ANOVA %>%
  filter(Light_Location == 'Right') %>%
  wilcox_effsize(FA_Rate ~ Light, paired = TRUE)
  Wilcoxon_FA_Rate_Light_Location_Right_ES
  
Wilcoxon_FA_Rate_Light_Present_ES <- FA_Rate_ANOVA %>%
  filter(Light == 'Present') %>%
  wilcox_effsize(FA_Rate ~ Light_Location, paired = TRUE)
  Wilcoxon_FA_Rate_Light_Present_ES
  
Wilcoxon_FA_Rate_Light_Absent_ES <- FA_Rate_ANOVA %>%
  filter(Light == 'Absent') %>%
  wilcox_effsize(FA_Rate ~ Light_Location, paired = TRUE)
  Wilcoxon_FA_Rate_Light_Absent_ES

#Cohen's d for d prime, c, hit and false alarm rates

Cohen_d_Table_d_prime <- updated_d_and_c %>%
  mutate(LP_LA = cohensD(d_Prime_Left_Present, d_Prime_Left_Absent),
         LP_RP = cohensD(d_Prime_Left_Present, d_Prime_Right_Present),
         LA_RA = cohensD(d_Prime_Left_Absent, d_Prime_Right_Absent),
         RP_RA = cohensD(d_Prime_Right_Present, d_Prime_Right_Absent)) %>%
  select(LP_LA,
         LP_RP,
         LA_RA,
         RP_RA)

Cohen_d_Table_c_estimate <- updated_d_and_c %>%
  mutate(LP_LA = cohensD(c_Left_Present, c_Left_Absent),
         LP_RP = cohensD(c_Left_Present, c_Right_Present),
         LA_RA = cohensD(c_Left_Absent, c_Right_Absent),
         RP_RA = cohensD(c_Right_Present, c_Right_Absent)) %>%
  select(LP_LA,
         LP_RP,
         LA_RA,
         RP_RA)

#No cohen D if not normally distributed
Cohen_d_Table_HR <- DV_Rates %>%
  mutate(LP_LA = cohensD(Hit_Rate_Left_Present, Hit_Rate_Left_Absent),
         LP_RP = cohensD(Hit_Rate_Left_Present, Hit_Rate_Right_Present),
         LA_RA = cohensD(Hit_Rate_Left_Absent, Hit_Rate_Right_Absent),
         RP_RA = cohensD(Hit_Rate_Right_Present, Hit_Rate_Right_Absent)) %>%
  select(LP_LA,
         LP_RP,
         LA_RA,
         RP_RA)

#No cohen D if not normally distributed
Cohen_d_Table_FA_Rate <- DV_Rates %>%
  mutate(LP_LA = cohensD(FA_Rate_Left_Present, FA_Rate_Left_Absent),
         LP_RP = cohensD(FA_Rate_Left_Present, FA_Rate_Right_Present),
         LA_RA = cohensD(FA_Rate_Left_Absent, FA_Rate_Right_Absent),
         RP_RA = cohensD(FA_Rate_Right_Present, FA_Rate_Right_Absent)) %>%
  select(LP_LA,
         LP_RP,
         LA_RA,
         RP_RA)

#Partial ETA squared - eta squared functions not working, calculating manually
#This is a type II ANOVA we are computing just to calculate eta_squared, but is
#not the right one as it assumes no interaction between IVs. The previously used
#one (aov_4) is a type III ANOVA, which is the correct one because it assumes that
#Light and Light_Location interact with each other creating a synergistic effect

summary(F1_ANOVA_d) #SSeffect and SSerror
#Light_Location = 0.24/(0.24+16.507) = 0.0143
#Light = 0.03/(0.03+8.293) = 0.0036
#Interaction = 0.01/(0.01+8.720) = 0.0011

summary(F1_ANOVA_c) #SSeffect and SSerror
#Light_Location = 0.184/(0.184+3.807) = 0.0461
#Light = 0.529/(0.529+2.916) = 0.1536
#Interaction = 0.021/(0.021+2.060) = 0.0101

#The assumption of sphericity will be automatically checked during the 
#computation of the ANOVA test using the ANOVA R functions from rstatix and afex.
#The Mauchly's test is internally used to assess the sphericity assumption. 
#By using the function get_anova_table() [rstatix] to extract the ANOVA table, 
#the Greenhouse-Geisser sphericity correction is automatically applied to factors
#violating the sphericity assumption.
#Nothing changed, no correction mentioned, so Mauchly's W statistic is not significant.
#Also only required when there are 3 or more levels in the IV.

threshold_mean <- SSDT_data %>%
  select(Subject,
         StimVal) %>%
  mutate(mean_threshold = mean(as.numeric(StimVal),na.rm=TRUE),
         SD_threshold = sd(as.numeric(StimVal),na.rm=TRUE))

#Check normality for each condition
shapiro.test(DV_Rates$Hit_Rate_Left_Present)
shapiro.test(DV_Rates$Hit_Rate_Left_Absent)
shapiro.test(DV_Rates$Hit_Rate_Right_Present)
shapiro.test(DV_Rates$Hit_Rate_Right_Absent)

shapiro.test(DV_Rates$FA_Rate_Left_Present)
shapiro.test(DV_Rates$FA_Rate_Left_Absent)
shapiro.test(DV_Rates$FA_Rate_Right_Present)
shapiro.test(DV_Rates$FA_Rate_Right_Absent)

#Descriptive statistics

descriptive_stats_HR_FA <- DV_Rates %>%
  mutate(HR_LP_mean = mean(Hit_Rate_Left_Present),
         HR_LP_SD = sd(Hit_Rate_Left_Present),
         HR_LP_median = median(Hit_Rate_Left_Present),
         HR_LP_range = diff(range(Hit_Rate_Left_Present)),
         HR_LA_mean = mean(Hit_Rate_Left_Absent),
         HR_LA_SD = sd(Hit_Rate_Left_Absent),
         HR_LA_median = median(Hit_Rate_Left_Absent),
         HR_LA_range = diff(range(Hit_Rate_Left_Absent)),
         HR_RP_mean = mean(Hit_Rate_Right_Present),
         HR_RP_SD = sd(Hit_Rate_Right_Present),
         HR_RP_median = median(Hit_Rate_Right_Present),
         HR_RP_range = diff(range(Hit_Rate_Right_Present)),
         HR_RA_mean = mean(Hit_Rate_Right_Absent),
         HR_RA_SD = sd(Hit_Rate_Right_Absent),
         HR_RA_median = median(Hit_Rate_Right_Absent),
         HR_RA_range = diff(range(Hit_Rate_Right_Absent))) %>%
  
  mutate(FA_LP_mean = mean(FA_Rate_Left_Present),
         FA_LP_SD = sd(FA_Rate_Left_Present),
         FA_LP_median = median(FA_Rate_Left_Present),
         FA_LP_range = diff(range(FA_Rate_Left_Present)),
         FA_LA_mean = mean(FA_Rate_Left_Absent),
         FA_LA_SD = sd(FA_Rate_Left_Absent),
         FA_LA_median = median(FA_Rate_Left_Absent),
         FA_LA_range = diff(range(FA_Rate_Left_Absent)),
         FA_RP_mean = mean(FA_Rate_Right_Present),
         FA_RP_SD = sd(FA_Rate_Right_Present),
         FA_RP_median = median(FA_Rate_Right_Present),
         FA_RP_range = diff(range(FA_Rate_Right_Present)),
         FA_RA_mean = mean(FA_Rate_Right_Absent),
         FA_RA_SD = sd(FA_Rate_Right_Absent),
         FA_RA_median = median(FA_Rate_Right_Absent),
         FA_RA_range = diff(range(FA_Rate_Right_Absent)))
  #report median and range because non-normal distribution

descriptive_stats_d_c <- updated_d_and_c %>% 
  mutate(d_LP_mean = mean(d_Prime_Left_Present),
         d_LP_SD = sd(d_Prime_Left_Present),
         d_LA_mean = mean(d_Prime_Left_Absent),
         d_LA_SD = sd(d_Prime_Left_Absent),
         d_RP_mean = mean(d_Prime_Right_Present),
         d_RP_SD = sd(d_Prime_Right_Present),
         d_RA_mean = mean(d_Prime_Right_Absent),
         d_RA_SD = sd(d_Prime_Right_Absent)) %>%
  
  mutate(c_LP_mean = mean(c_Left_Present),
         c_LP_SD = sd(c_Left_Present),
         c_LA_mean = mean(c_Left_Absent),
         c_LA_SD = sd(c_Left_Absent),
         c_RP_mean = mean(c_Right_Present),
         c_RP_SD = sd(c_Right_Present),
         c_RA_mean = mean(c_Right_Absent),
         c_RA_SD = sd(c_Right_Absent))

#d prime grouped bar chart

#Summary of d_Prime after updating data based on MAD
d_Prime_summary <- d_Prime %>% 
  group_by(Light_Location, Light) %>%
  summarise(n = n(), mean_d_Prime = mean(d_Prime), SD_d_Prime = sd(d_Prime)) %>%
  mutate(se = SD_d_Prime/sqrt(n))

ggplot(d_Prime_summary, aes(fill = Light, y = mean_d_Prime, x = Light_Location)) + 
  geom_col(colour = "black",width = 0.5,    
           position = position_dodge(0.5)) +
  geom_errorbar(aes(x = Light_Location, ymin = mean_d_Prime - se, ymax = mean_d_Prime + se), 
                width = .25, colour = "black", position = position_dodge(0.5)) +
  scale_y_continuous(breaks = seq(0, 1.8, 0.2), 
                     limits = c(0, 1.8),
                     expand = c(0, 0)) +
  scale_fill_manual(values = c("gray20", "grey70")) +
  theme_hc() +
  theme(axis.line.y = element_line(size = 0.5, color = "grey"),
        axis.ticks = element_line(color="grey"),
        legend.position = "right",
        axis.title.y = element_text(vjust = +3),
        axis.title.x = element_text(vjust = -0.75)) +
  labs(x = "Light Location",
       y = "Sensitivity (d')")

#c grouped bar chart

#Summary of c after updating data based on MAD
c_summary <- c_estimate %>% 
  group_by(Light_Location, Light) %>%
  summarise(n = n(), mean_c = mean(c), SD_c = sd(c)) %>%
  mutate(se = SD_c/sqrt(n))

ggplot(c_summary, aes(fill = Light, y = mean_c, x = Light_Location)) + 
  geom_col(colour = "black",width = 0.5,    
           position = position_dodge(0.5)) +
  geom_errorbar(aes(x = Light_Location, ymin = mean_c - se, ymax = mean_c + se), 
                width = .25, colour = "black", position = position_dodge(0.5)) +
  scale_y_continuous(breaks = seq(0, 1.4, 0.2), 
                     limits = c(0, 1.4),
                     expand = c(0, 0)) +
  scale_fill_manual(values = c("gray20", "grey70")) +
  theme_hc() +
  theme(axis.line.y = element_line(size = 0.5, color = "grey"),
        axis.ticks = element_line(color="grey"),
        legend.position = "right",
        axis.title.y = element_text(vjust = +3),
        axis.title.x = element_text(vjust = -0.75)) +
  labs(x = "Light Location",
       y = "Response Criterion (c)")

#Hit rate group bar chart

#Summary of hit rate after updating data based on MAD
Hit_Rate_ANOVA_summary <- Hit_Rate_ANOVA %>% 
  group_by(Light_Location, Light) %>%
  summarise(n = n(), mean_Hit_Rate = mean(Hit_Rate)*100, SD_Hit_Rate = sd(Hit_Rate)*100) %>%
  mutate(se = SD_Hit_Rate/sqrt(n))

ggplot(Hit_Rate_ANOVA_summary, aes(fill = Light, y = mean_Hit_Rate, x = Light_Location)) + 
  geom_col(colour = "black",width = 0.5,    
           position = position_dodge(0.5)) +
  geom_errorbar(aes(x = Light_Location, ymin = mean_Hit_Rate - se, ymax = mean_Hit_Rate + se), 
                width = .25, colour = "black", position = position_dodge(0.5)) +
  scale_y_continuous(breaks = seq(0, 60, 10), 
                     limits = c(0, 60),
                     expand = c(0, 0)) +
  scale_fill_manual(values = c("gray20", "grey70")) +
  theme_hc() +
  theme(axis.line.y = element_line(size = 0.5, color = "grey"),
        axis.ticks = element_line(color="grey"),
        legend.position = "right",
        axis.title.y = element_text(vjust = +3),
        axis.title.x = element_text(vjust = -0.75)) +
  labs(x = "Light Location",
       y = "Hit Rate (%)")

#FA rate group bar chart

#Summary of FA rate after updating data based on MAD
FA_Rate_ANOVA_summary <- FA_Rate_ANOVA %>% 
  group_by(Light_Location, Light) %>%
  summarise(n = n(), mean_FA_Rate = mean(FA_Rate)*100, SD_FA_Rate = sd(FA_Rate)*100) %>%
  mutate(se = SD_FA_Rate/sqrt(n))

ggplot(FA_Rate_ANOVA_summary, aes(fill = Light, y = mean_FA_Rate, x = Light_Location)) + 
  geom_col(colour = "black",width = 0.5,    
           position = position_dodge(0.5)) +
  geom_errorbar(aes(x = Light_Location, ymin = mean_FA_Rate - se, ymax = mean_FA_Rate + se), 
                width = .25, colour = "black", position = position_dodge(0.5)) +
  scale_y_continuous(breaks = seq(0, 14, 2), 
                     limits = c(0, 14),
                     expand = c(0, 0)) +
  scale_fill_manual(values = c("gray20", "grey70")) +
  theme_hc() +
  theme(axis.line.y = element_line(size = 0.5, color = "grey"),
        axis.ticks = element_line(color="grey"),
        legend.position = "right",
        axis.title.y = element_text(vjust = +3),
        axis.title.x = element_text(vjust = -0.75)) +
  labs(x = "Light Location",
       y = "False Alarm Rate (%)")
