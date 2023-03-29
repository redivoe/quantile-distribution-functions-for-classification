library(tidyverse)
library(here)

# Heart Disease Data Set 
# https://archive.ics.uci.edu/ml/datasets/heart+disease

cleveland<-read.csv(
  url("https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data"),
  header = FALSE,
  na.strings = "?")

# Processing based on
# https://www.r-bloggers.com/2019/09/heart-disease-prediction-from-patient-data-in-r/

names <- c("Age",
           "Sex",
           "Chest_Pain_Type",
           "Resting_Blood_Pressure",
           "Serum_Cholesterol",
           "Fasting_Blood_Sugar",
           "Resting_ECG",
           "Max_Heart_Rate_Achieved",
           "Exercise_Induced_Angina",
           "ST_Depression_Exercise",
           "Peak_Exercise_ST_Segment",
           "Num_Major_Vessels_Flouro",
           "Thalassemia",
           "Diagnosis_Heart_Disease")

# Apply column names to the dataframe
colnames(cleveland) <- names

cleveland <- cleveland %>% 
  as_tibble %>% 
  janitor::clean_names() %>% 
  mutate(across(c(resting_ecg,fasting_blood_sugar,sex,
                  exercise_induced_angina,peak_exercise_st_segment,
                  chest_pain_type,thalassemia,
                  diagnosis_heart_disease),
                as_factor)) %>% 
  mutate(num_major_vessels_flouro = as.numeric(num_major_vessels_flouro)) %>%
  mutate(diagnosis_heart_disease = forcats::fct_collapse(
    diagnosis_heart_disease, "1"=c("1","2","3","4"))) %>% 
  dplyr::select(age,resting_blood_pressure,serum_cholesterol,max_heart_rate_achieved,
                st_depression_exercise,num_major_vessels_flouro,thalassemia,everything()) %>% 
  drop_na()



# Breast Cancer Wisconsin (Diagnostic) Data Set
# https://archive.ics.uci.edu/ml/datasets/Breast+Cancer+Wisconsin+(Diagnostic)

wbcd <-
  read.csv(
    url("https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data"),
    header = FALSE,
    na.strings = "?"
  )

wbcd <- wbcd %>% 
  mutate(outcome = as_factor(V2)) %>% 
  dplyr::select(-c(V1,V2))



# Credit Approval Data Set 
# https://archive.ics.uci.edu/ml/datasets/credit+approval

credit <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/credit-screening/crx.data"),
                   header = FALSE,
                   na.strings = "?")
credit <- mutate(credit, across(c(V1, V4, V5, V6, V7, V9, V10, V12, V13, V16),.fns = as_factor))
credit <- drop_na(credit)




# Thyroid Disease Data Set 
# https://archive.ics.uci.edu/ml/datasets/thyroid+disease

# install.packages("MLDataR")
library(MLDataR)
thyroid <- MLDataR::thyroid_disease

n_unique <- thyroid |> 
  summarise(across(.fns = \(x) length(unique(x)))) |> 
  as_vector()

less_than <- names(n_unique)[which(n_unique <= 5)]
thyroid <- thyroid |> 
  mutate(across(.cols = all_of(less_than), .fns = factor)) |> 
  drop_na() |> 
  janitor::clean_names() |> 
  as_tibble()


library(hhcartr)
vehicle <- bind_cols("y" = hhcartr::vehicle$y, hhcartr::vehicle$X)



# Waveform Database Generator (Version 1) Data Set
# https://archive.ics.uci.edu/ml/datasets/Waveform+Database+Generator+(Version+1)
# waveform <- read_csv(file = url("https://archive.ics.uci.edu/ml/machine-learning-databases/waveform/waveform-+noise.data.Z"),
#                      col_names = url("https://archive.ics.uci.edu/ml/machine-learning-databases/waveform/waveform-+noise.names"))

# https://datahub.io/machine-learning/waveform-5000
waveform <- read_csv(file = url("https://datahub.io/machine-learning/waveform-5000/r/waveform-5000.csv"))
waveform$class <- factor(waveform$class)



# Statlog (Heart) Data Set 
# https://archive.ics.uci.edu/ml/datasets/statlog+(heart)
heart <- read_delim(url("https://archive.ics.uci.edu/ml/machine-learning-databases/statlog/heart/heart.dat"), 
                    col_names = FALSE)

categorical_vars <- c(2, 6, 9, 7, 3, 13, 11)
heart <- heart |> 
  mutate(across(.cols = categorical_vars, .fns = factor)) |> 
  rename(y = X14) |> 
  mutate(y = factor(y))


data("LetterRecognition", package = "mlbench")
data("PimaIndiansDiabetes", package = "mlbench")
data("Ionosphere", package = "mlbench")
data("Glass", package = "mlbench")
data("Sonar", package = "mlbench")

letter <- LetterRecognition
diabetes <- PimaIndiansDiabetes
ionosphere <- Ionosphere
glass <- Glass
sonar <- Sonar

data_names <- c("cleveland", "credit", "diabetes", "glass", "heart", "ionosphere", "letter", "sonar", "thyroid", "vehicle", "waveform", "wbcd")

rm(list = setdiff(ls(), data_names))
save.image(here("data", "uci-datasets.RData"))
