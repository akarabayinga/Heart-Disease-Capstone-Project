## ----setup, include = FALSE---------------------------------------------------------------------------------------------------------------------------------------------

# Load setup
knitr::opts_chunk$set(
  echo = FALSE,
  message = FALSE,
  warning = FALSE,
  out.width = "95%")


## ----Custom theme, include = FALSE--------------------------------------------------------------------------------------------------------------------------------------

###################################################################################
# Create custom plot theme for all plots in the report
###################################################################################

theme_1 <- function() {
  font <- "sans" # assign family font
  theme_classic() %+replace% # modify from base theme
    theme(
      plot.title = element_text(
        family = font,
        size = 10,
        face = "bold",
        hjust = 0,
        vjust = 2
      ),
      axis.title = element_text(
        family = font,
        size = 8
      ),
      axis.text = element_text(
        family = font,
        size = 7
      ),
      legend.position = "top",
      legend.key.size = unit(0.35, "cm"),
      legend.title = element_text(
        size = 8
      ),
      plot.caption = element_text(
        size = 6,
        hjust = 1
      )
    )
}


## ----Load packages & data set, include = FALSE--------------------------------------------------------------------------------------------------------------------------

# Install required packages
if (!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if (!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")
if (!require(skimr)) install.packages("skimr", repos = "http://cran.us.r-project.org")
if (!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if (!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")
if (!require(naniar)) install.packages("naniar", repos = "http://cran.us.r-project.org")
if (!require(ranger)) install.packages("ranger", repos = "http://cran.us.r-project.org")
if (!require(kknn)) install.packages("kknn", repos = "http://cran.us.r-project.org")
if (!require(workflows)) install.packages("workflows", repos = "http://cran.us.r-project.org")
if (!require(table1)) install.packages("table1", repos = "http://cran.us.r-project.org")
if (!require(grid)) install.packages("grid", repos = "http://cran.us.r-project.org")
if (!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if (!require(ggpubr)) install.packages("ggpubr", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(kableExtra)
library(skimr)
library(data.table)
library(scales)
library(naniar)
library(ranger)
library(kknn)
library(workflows)
library(table1)
library(grid)
library(gridExtra)
library(ggpubr)

# Download dataset from: https://www.kaggle.com/datasets/fedesoriano/heart-failure-prediction
# Save in working directory

# Load data
heart <- read_csv("heart.csv")

# Convert all variable names to lower case
colnames(heart) <- tolower(colnames(heart))


## ----overview-----------------------------------------------------------------------------------------------------------------------------------------------------------

# Create table showing classes and the first 10 observations
rbind(
  lapply(heart, class),
  head(heart, 10)
) %>%
  kbl(
    caption = "Overview of heart data set",
    booktabs = TRUE
  ) %>%
  kable_classic(
    latex_options = c("hold_position", "striped", "scale_down"),
    font_size = 8
  ) %>%
  row_spec(0, bold = TRUE) %>%
  pack_rows("Class", 1, 1) %>%
  pack_rows("First 10 observations", 2, 11)


## ----formating----------------------------------------------------------------------------------------------------------------------------------------------------------

# Convert fastingbs and heartdisease to class factor
heart <- heart %>%
  mutate(
    fastingbs = as.factor(fastingbs),
    heartdisease = as.factor(heartdisease)
  ) %>%
  # convert all age, restingbp, cholesterol and maxhr to integer
  mutate(
    age = as.integer(age),
    restingbp = as.integer(restingbp),
    cholesterol = as.integer(cholesterol),
    maxhr = as.integer(maxhr)
  ) %>%
  # convert oldpeak to double
  mutate(oldpeak = as.double(oldpeak)) %>%
  # convert all character variables to factor
  mutate(
    sex = as.factor(sex),
    chestpaintype = as.factor(chestpaintype),
    restingecg = as.factor(restingecg),
    exerciseangina = as.factor(exerciseangina),
    st_slope = as.factor(st_slope)
  )

# Attach value labels to heartdisease column
heart$heartdisease <- factor(heart$heartdisease,
  levels = c(0, 1),
  labels = c("Absent", "Present")
)

# Ensure that the first level of heartdisease is "present" - what is being predicted
heart$heartdisease <- ordered(heart$heartdisease, levels = c("Present", "Absent"))


## ----data-structure-----------------------------------------------------------------------------------------------------------------------------------------------------

# Create table showing variable labels, description, classes and levels if available
tibble(
  variable_label = c(
    "heartdisease",
    "age",
    "restingbp",
    "cholesterol",
    "maxhr",
    "oldpeak",
    "sex",
    "chestpaintype",
    "fastingbs",
    "restingecg",
    "exerciseangina",
    "st_slope"
  ),
  description = c(
    "Heart disease",
    "Age (in years)",
    "Resting blood pressure (in mmHg on admission to the hospital)",
    "Serum cholesterol (in mg/dL)",
    "Maximum heart rate achieved",
    "ST depression induced by exercise relative to rest",
    "Patient's gender",
    "Type of chest pain",
    "Fasting blood sugar",
    "Resting electrocardiographic results",
    "Exercise-induced angina",
    "Slope of the peak exercise ST segment"
  ),
  class = c(
    "Factor",
    "Numeric - integer",
    "Numeric - integer",
    "Numeric - integer",
    "Numeric - integer",
    "Numeric",
    "Factor",
    "Factor",
    "Factor",
    "Factor",
    "Factor",
    "Factor"
  ),
  levels = c(
    "0: Absent; 1: Present",
    "",
    "",
    "",
    "",
    "",
    "M: Male; F: Female",
    "TA: Typical angina; ATA: Atypical angina; NAP: Non-anginal pain; ASY: asymptomatic",
    "0: </= 120 mg/dL; 1: > 120 mg/dL",
    "Normal: normal; ST: having ST-T wave abnormality (T wave inversions and/or ST elevation or depression of > 0.05 mV);  LVH: showing probable or definite left ventricular hypertrophy by Estes' criteria",
    "Y: Yes; N: No",
    "Up: Upsloping; Flat: Flat; Down: Downsloping"
  )
) %>%
  kbl(
    caption = "Structure of heart data set",
    col.names = c(
      "Variable label",
      "Description",
      "Class",
      "Variable levels"
    ),
    booktabs = TRUE
  ) %>%
  kable_classic(
    latex_options = c("hold_position", "striped", "scale_down")
  ) %>%
  row_spec(0, bold = TRUE) %>%
  pack_rows("Outcome variable", 1, 1) %>%
  pack_rows("Predictor variables", 2, 12) %>%
  column_spec(4, width = "30em")


## ----missing-values, fig.show = 'hide'----------------------------------------------------------------------------------------------------------------------------------

# Check whether heart data set has missing values
gg_miss_var(heart) +
  theme_1() +
  labs(
    title = "Missing values in heartdata set",
    x = "variables",
    y = "Number or missing values"
  )


## ----create model and validation sets-----------------------------------------------------------------------------------------------------------------------------------

library(tidymodels)

# Create model (90% heart) and validation (10% heart) sets
# Set seed to allow reproducibility
set.seed(2000, sample.kind = "Rounding")

# Create an object contain the 90-10 split for heart data set
heart_split <- heart %>%
  initial_split(
    prop = 0.9,
    strata = heartdisease
  )

# Extract model and validation sets
model <- training(heart_split)
validation <- testing(heart_split)


## ----data-figure, fig.cap = "Data set partitions", out.width = "80%"----------------------------------------------------------------------------------------------------

knitr::include_graphics("data_allocation.png")


## ----data-sets-overview-------------------------------------------------------------------------------------------------------------------------------------------------

# Create table showing number of observations and proportions with and without heart disease for data sets from data partitioning I.
tibble(
  data_set = c(
    "Heart",
    "Model",
    "Validation"
  ),
  rows = c(
    nrow(heart),
    nrow(model),
    nrow(validation)
  ),
  heart_percent = c(
    paste0(nrow(heart) / nrow(heart) * 100, "%"),
    paste0(round(nrow(model) / nrow(heart) * 100, 1), "%"),
    paste0(round(nrow(validation) / nrow(heart) * 100, 1), "%")
  ),
  with_heartdisease = c(
    paste0(
      round(sum(heart$heartdisease == "Present") / nrow(heart) * 100, 1), "%"
    ),
    paste0(
      round(sum(model$heartdisease == "Present") / nrow(model) * 100, 1), "%"
    ),
    paste0(
      round(sum(validation$heartdisease == "Present") / nrow(validation) * 100, 1), "%"
    )
  ),
  no_heartdisease = c(
    paste0(
      round(sum(heart$heartdisease == "Absent") / nrow(heart) * 100, 1), "%"
    ),
    paste0(
      round(sum(model$heartdisease == "Absent") / nrow(model) * 100, 1), "%"
    ),
    paste0(
      round(sum(validation$heartdisease == "Absent") / nrow(validation) * 100, 1), "%"
    )
  )
) %>%
  kbl(
    caption = "Data partitioning I - Overview of data sets observations and proportions with and without heart disease",
    col.names = c(
      "Data set",
      "Number of observations",
      "Proportion of heart data set",
      "Proportion with HD",
      "Proportion without HD"
    ),
    booktabs = TRUE
  ) %>%
  kable_classic(
    latex_options = c("hold_position", "striped", "scale_down"),
    font_size = 8
  ) %>%
  row_spec(0, bold = TRUE)


## ----heartdisease-summary-----------------------------------------------------------------------------------------------------------------------------------------------

# Number and percentage of patients with ('present') and without ('absent') heart disease
present_n <- model %>%
  filter(heartdisease == "Present") %>%
  count() %>%
  pull(n)

present_perc <- round(
  present_n / nrow(model) * 100,
  1
)

absent_n <- model %>%
  filter(heartdisease == "Absent") %>%
  count() %>%
  pull(n)

absent_perc <- round(
  absent_n / nrow(model) * 100,
  1
)


## ----heartdisease, fig.cap = "Distribution of HD status", fig.height = 2.5, fig.width = 4-------------------------------------------------------------------------------

# Compare number of patients with and without heart disease
model %>%
  ggplot(aes(heartdisease,
    fill = heartdisease
  )) +
  geom_bar(
    stat = "count",
    position = "stack",
    alpha = 0.9,
    show.legend = FALSE
  ) +
  geom_text(aes(label = ..count..),
    stat = "count",
    position = position_stack(vjust = 1.15),
    size = 2.5,
    color = "black"
  ) +
  geom_text(aes(label = paste0(
    "(",
    round(..count.. / nrow(model) * 100,
      digits = 1
    ), "%", ")"
  )),
  stat = "count",
  position = position_stack(vjust = 1.05),
  size = 2.5,
  color = "black",
  show.legend = FALSE
  ) +
  labs(
    caption = "Source: Heart failure prediction data set",
    x = "HD status",
    y = "Number of patients",
    fill = "HD status"
  ) +
  scale_fill_manual(values = c("#651D32", "#205493")) +
  theme_1()


## ----descriptive-statistics---------------------------------------------------------------------------------------------------------------------------------------------

# Create table of descriptive statistics for all variables stratified by HD

# Create labels for categorical variables
# sex
model$sex <- factor(model$sex,
  levels = c(
    "F",
    "M"
  ),
  labels = c(
    "Female",
    "Male"
  )
)

# chestpaintype
model$chestpaintype <- factor(model$chestpaintype,
  levels = c(
    "TA",
    "ATA",
    "NAP",
    "ASY"
  ),
  labels = c(
    "Typical angina",
    "Atypical angina",
    "Non-anginal pain",
    "Asymptomatic"
  )
)

# fastingbs
model$fastingbs <- factor(model$fastingbs,
  levels = c(
    0,
    1
  ),
  labels = c(
    "</= 120 mg/dL",
    "> 120 mg/dL"
  )
)

# restingecg
model$restingecg <- factor(model$restingecg,
  levels = c(
    "ST",
    "LVH",
    "Normal"
  ),
  labels = c(
    "ST-T wave abnormality",
    "Left ventricular hypertrophy",
    "Normal"
  )
)

# exerciseangina
model$exerciseangina <- factor(model$exerciseangina,
  levels = c(
    "Y",
    "N"
  ),
  labels = c(
    "Yes",
    "No"
  )
)

# st_slope
model$st_slope <- factor(model$st_slope,
  levels = c(
    "Down",
    "Flat",
    "Up"
  ),
  labels = c(
    "Downsloping",
    "Flat",
    "Upsloping"
  )
)

# Re-label all variables
labels <- list(
  variables = list(
    sex = "Sex",
    chestpaintype = "Chest pain type",
    fastingbs = "Fasting blood sugar",
    restingecg = "Resting ECG",
    exerciseangina = "Exercise-induced angina",
    st_slope = "Slope of peak exercise ST segment (st_slope)",
    age = "Age (years)",
    restingbp = "Resting BP - Systolic (mmHg)",
    cholesterol = "Serum cholesterol (mg/dL)",
    maxhr = "Maximum HR achieved",
    oldpeak = "ST depression induced by exercise (oldpeak)"
  )
)

# Rearrange columns to have all patients first
strata <- c(
  list(Total = model),
  split(
    model,
    model$heartdisease
  )
)
# Create table
tab <- table1(
  strata,
  labels
)

# Finalize table
tab %>%
  kbl(
    caption = "Descriptive statistics table for the model data set",
    col.names = c(
      "",
      "All patients",
      "HD - Present",
      "HD - Absent"
    ),
    booktabs = TRUE
  ) %>%
  kable_classic(
    latex_options = c("hold_position", "striped"),
    font_size = 8
  ) %>%
  row_spec(0:1, bold = TRUE) %>%
  pack_rows("Categorical variables", 2, 23) %>%
  pack_rows("Continuous variables", 24, 38)


## ----improbable-values--------------------------------------------------------------------------------------------------------------------------------------------------

# Number and percentage of patients in model data set with resting BP  of 0
zero_restingbp_n <- model %>%
  filter(restingbp == 0) %>%
  count()

zero_restingbp_perc <- round(
  zero_restingbp_n / nrow(model) * 100,
  1
)

# Number and percentage of patients in model data set with cholesterol  of 0
zero_chol_n <- model %>%
  filter(cholesterol == 0) %>%
  count()

zero_chol_perc <- round(
  zero_chol_n / nrow(model) * 100,
  1
)

# Number and percentage of patients in model data set with oldpeak values < 0
zero_oldpeak_n <- model %>%
  filter(oldpeak < 0) %>%
  count()

zero_oldpeak_perc <- round(
  zero_oldpeak_n / nrow(model) * 100,
  1
)


## ----knn-imputation-----------------------------------------------------------------------------------------------------------------------------------------------------

# Perform knn-imputation for resting BP and cholesterol levels of 0 and oldpeak values < 0

# Create new model object with all improbable values converted to NA
model1 <- model %>%
  # Convert all 0s in resting BP, cholesterol into NAs
  mutate(across(restingbp:cholesterol, ~ na_if(., 0))) %>%
  # Convert all negative values in oldpeak into NAs
  mutate(oldpeak = if_else(oldpeak < 0, as.numeric(NA), oldpeak))

library(recipes)

recipe_impute <- recipe(
  data = model1,
  heartdisease ~ .
) %>%
  # impute cholesterol, restingbp and oldpeak values using knn-imputation
  step_impute_knn(
    cholesterol,
    restingbp,
    oldpeak
  )

# Create table of imputed data
model_imputed <- recipe_impute %>%
  prep(model1) %>%
  juice()


## ----imputation-plots, fig.cap = "Scatter plots of cholesterol, resting BP and oldpeak values before and after kNN-imputation of the model data set", fig.height = 5, fig.width = 7----

# cholesterol
chol <- cbind(
  model$cholesterol,
  model_imputed$cholesterol
) %>%
  as.data.frame() %>%
  ggplot(aes(
    V1,
    V2
  )) +
  geom_point(
    color = "#205493"
  ) +
  geom_abline(aes(
    intercept = 0,
    slope = 1
  ),
  color = "#651D32"
  ) +
  labs(
    title = "Cholesterol (mg/dL)",
    x = "Before kNN-imputation",
    y = "After kNN-imputation"
  ) +
  scale_y_continuous(limits = c(0, 650)) +
  theme_1()

# restingBP
rest_bp <- cbind(
  model$restingbp,
  model_imputed$restingbp
) %>%
  as.data.frame() %>%
  ggplot(aes(
    V1,
    V2
  )) +
  geom_point(
    color = "#205493"
  ) +
  geom_abline(aes(
    intercept = 0,
    slope = 1
  ),
  color = "#651D32"
  ) +
  labs(
    title = "Resting BP (mm Hg)",
    x = "Before kNN-imputation",
    y = "After kNN-imputation"
  ) +
  scale_y_continuous(limits = c(0, 250)) +
  theme_1()

# oldpeak
oldpeak <- cbind(
  model$oldpeak,
  model_imputed$oldpeak
) %>%
  as.data.frame() %>%
  ggplot(aes(
    V1,
    V2
  )) +
  geom_point(
    color = "#205493"
  ) +
  geom_abline(aes(
    intercept = 0,
    slope = 1
  ),
  color = "#651D32"
  ) +
  labs(
    title = "Oldpeak",
    x = "Before kNN-imputation",
    y = "After kNN-imputation"
  ) +
  scale_y_continuous(limits = c(-2, 7)) +
  theme_1()

# Plot all graphs together
grid.arrange(
  arrangeGrob(chol,
    rest_bp,
    oldpeak,
    nrow = 2
  ),
  bottom = textGrob("Source: Heart failure prediction data set",
    hjust = 1,
    x = 1,
    gp = gpar(fontsize = 8)
  )
)


## ----descriptive-statistics2--------------------------------------------------------------------------------------------------------------------------------------------

# Descriptive statistics for the 3 variables modified using kNN-imputation
# Re-label all variables
labels <- list(
  variables = list(
    restingbp = "Resting BP - Systolic (mmHg)",
    cholesterol = "Serum cholesterol (mg/dL)",
    oldpeak = "ST depression induced by exercise (oldpeak)"
  )
)

# Rearrange columns to have all patients first
strata <- c(
  list(Total = model_imputed),
  split(
    model_imputed,
    model_imputed$heartdisease
  )
)
# Create table
tab1 <- table1(
  strata,
  labels
)

# Finalize table
tab1 %>%
  kbl(
    caption = "Descriptive statistics table for the model data set - kNN-imputed variables",
    col.names = c(
      "",
      "All patients",
      "HD - Present",
      "HD - Absent"
    ),
    booktabs = TRUE
  ) %>%
  kable_classic(
    latex_options = c("hold_position", "striped"),
    font_size = 8
  ) %>%
  row_spec(0:1, bold = TRUE)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Rename knn-imputed model set as model
model <- model_imputed


## ----sex-summary--------------------------------------------------------------------------------------------------------------------------------------------------------

# Sex counts
sex_n <- table(model$sex)

# Compute sex counts per HD status
sex_hd_n <- table(
  model$heartdisease,
  model$sex
)

# Compute proportions of sex by HD status
sex_hd_prop <- sex_hd_n %>%
  # convert to percent
  proportions(2) * 100


## ----sex-HD-------------------------------------------------------------------------------------------------------------------------------------------------------------

# Distribution of heart disease by sex
sex_HD <- sex_hd_prop %>%
  as.data.frame() %>%
  mutate(Var2 = if_else(Var2 == "Female",
    paste0("Female \n Patients = ", sex_n["Female"]),
    paste0("Male \n Patients = ", sex_n["Male"])
  )) %>%
  ggplot(aes(
    x = Var2,
    y = Freq,
    fill = Var1
  )) +
  geom_col(
    position = position_dodge(width = 0.75),
    alpha = 0.9,
    show.legend = FALSE
  ) +
  geom_text(aes(label = paste0(
    round(Freq,
      digits = 1
    ),
    "%"
  )),
  position = position_dodge(width = 0.75),
  vjust = -0.9,
  size = 2
  ) +
  labs(
    title = "A. Sex",
    x = "",
    y = "Proportion of patients (%)",
    fill = "HD status"
  ) +
  scale_fill_manual(values = c("#651D32", "#205493")) +
  scale_y_continuous(limits = c(0, 85)) +
  theme_1()


## ----cpt-summary--------------------------------------------------------------------------------------------------------------------------------------------------------

# chestpaintype counts
cpt_n <- table(model$chestpaintype)

# Compute chestpaintype counts per HD status
cpt_hd_n <- table(
  model$heartdisease,
  model$chestpaintype
)

# Compute proportions of chestpaintype by HD status
cpt_hd_prop <- cpt_hd_n %>%
  # convert to percent
  proportions(2) * 100


## ----sex-cpt-HD, fig.cap= "Sex and chest pain type by HD status", fig.height = 3, fig.width = 8-------------------------------------------------------------------------

# Distribution of heart disease by chestpaintype
cpt_HD <- cpt_hd_prop %>%
  as.data.frame() %>%
  mutate(Var2 = case_when(
    Var2 == "Typical angina" ~
      paste0(
        "Typical angina \n Patients = ",
        cpt_n["Typical angina"]
      ),
    Var2 == "Atypical angina" ~
      paste0(
        "Atypical angina \n Patients = ",
        cpt_n["Atypical angina"]
      ),
    Var2 == "Non-anginal pain" ~
      paste0(
        "Non-anginal pain \n Patients = ",
        cpt_n["Non-anginal pain"]
      ),
    Var2 == "Asymptomatic" ~
      paste0(
        "Asymptomatic \n Patients = ",
        cpt_n["Asymptomatic"]
      )
  )) %>%
  ggplot(aes(
    x = Var2,
    y = Freq,
    fill = Var1
  )) +
  geom_col(
    position = position_dodge(width = 0.75),
    alpha = 0.9
  ) +
  geom_text(aes(label = paste0(
    round(Freq,
      digits = 1
    ),
    "%"
  )),
  position = position_dodge(width = 0.75),
  vjust = -0.9,
  size = 2
  ) +
  labs(
    title = "B. Chest pain type",
    x = "",
    y = "Proportion of patients (%)",
    fill = "HD status"
  ) +
  scale_fill_manual(values = c("#651D32", "#205493")) +
  scale_y_continuous(limits = c(0, 90)) +
  theme_1()

# Plot sex and chestpaintype graphs side by side
# legend
leg <- get_legend(cpt_HD)

# Plot on one graph
grid.arrange(
  arrangeGrob(nullGrob(), leg, nullGrob(), nrow = 1),
  arrangeGrob(sex_HD, cpt_HD + theme(legend.position = "none"),
    nrow = 1
  ),
  # ncol = 1,
  heights = c(0.5, 4),
  bottom = textGrob("Source: Heart failure prediction data set",
    hjust = 1,
    x = 1,
    gp = gpar(fontsize = 6)
  )
)


## ----FBS-classification-------------------------------------------------------------------------------------------------------------------------------------------------

# Create table showing classification of fasting BS
tibble(
  FBS = c(
    "< 100 mg/dL",
    "100-125 mg/dL",
    "> / = 125 mg/dL"
  ),
  category = c(
    "Normal",
    "Prediabetes (impaired fasting glucose)",
    "Type 2 DM"
  ),
  meaning = c(
    "Healthy range",
    "At increased risk of developing diabetes",
    "At increased risk of heart disease or stroke"
  )
) %>%
  kbl(
    caption = "Categories of FBS in adults",
    col.names = c(
      "Fasting BS Category",
      "Diagnosis",
      "Interpretation"
    ),
    booktabs = TRUE
  ) %>%
  kable_classic(
    latex_options = c("hold_position", "striped"),
    font_size = 8
  ) %>%
  row_spec(0, bold = TRUE)


## ----fastingbs-summary--------------------------------------------------------------------------------------------------------------------------------------------------

# fastingbs counts
fbs_n <- table(model$fastingbs)

# Compute fastingbs counts per HD status
fbs_hd_n <- table(
  model$heartdisease,
  model$fastingbs
)

# Compute proportions of fastingbs by HD status
fbs_hd_prop <- fbs_hd_n %>%
  # convert to percent
  proportions(2) * 100


## ----FBS-HD-------------------------------------------------------------------------------------------------------------------------------------------------------------

# Distribution of heart disease by fastingbs
FBS_HD <- fbs_hd_prop %>%
  as.data.frame() %>%
  mutate(Var2 = if_else(Var2 == "</= 120 mg/dL",
    paste0(
      "</= 120 mg/dL \n Patients = ",
      fbs_n["</= 120 mg/dL"]
    ),
    paste0(
      "> 120 mg/dL \n Patients = ",
      fbs_n["> 120 mg/dL"]
    )
  )) %>%
  ggplot(aes(
    x = Var2,
    y = Freq,
    fill = Var1
  )) +
  geom_col(
    position = position_dodge(width = 0.75),
    alpha = 0.9,
    show.legend = FALSE
  ) +
  geom_text(aes(label = paste0(
    round(Freq,
      digits = 1
    ),
    "%"
  )),
  position = position_dodge(width = 0.75),
  vjust = -0.9,
  size = 2
  ) +
  labs(
    title = "A. Fasting blood sugar",
    x = "",
    y = "Proportion of patients (%)",
    fill = "HD status"
  ) +
  scale_fill_manual(values = c("#651D32", "#205493")) +
  scale_y_continuous(limits = c(0, 85)) +
  theme_1()


## ----restingecg-summary-------------------------------------------------------------------------------------------------------------------------------------------------

# restingecg counts
restingecg_n <- table(model$restingecg)

# Compute restingecg counts per HD status
restingecg_hd_n <- table(
  model$heartdisease,
  model$restingecg
)

# Compute proportions of restingecg by HD status
restingecg_hd_prop <- restingecg_hd_n %>%
  # convert to percent
  proportions(2) * 100


## ----FBS-restECG-HD, fig.cap= "Fasting blood sugar and resting ECG by HD status",  fig.height = 3, fig.width = 8--------------------------------------------------------

# Distribution of restingecg heart disease
restECG_HD <- restingecg_hd_prop %>%
  as.data.frame() %>%
  mutate(Var2 = case_when(
    Var2 == "ST-T wave abnormality" ~
      paste0(
        "ST-T wave abnormality \n Patients = ",
        restingecg_n["ST-T wave abnormality"]
      ),
    Var2 == "Left ventricular hypertrophy" ~
      paste0(
        "Left ventricular hypertrophy \n Patients = ",
        restingecg_n["Left ventricular hypertrophy"]
      ),
    Var2 == "Normal" ~
      paste0(
        "Normal \n Patients = ",
        restingecg_n["Normal"]
      )
  )) %>%
  ggplot(aes(
    x = Var2,
    y = Freq,
    fill = Var1
  )) +
  geom_col(
    position = position_dodge(width = 0.75),
    alpha = 0.9
  ) +
  geom_text(aes(label = paste0(
    round(Freq,
      digits = 1
    ),
    "%"
  )),
  position = position_dodge(width = 0.75),
  vjust = -0.9,
  size = 2
  ) +
  labs(
    title = "B. Resting ECG",
    x = "",
    y = "Proportion of patients (%)",
    fill = "HD status"
  ) +
  scale_fill_manual(values = c("#651D32", "#205493")) +
  scale_y_continuous(limits = c(0, 70)) +
  theme_1()

# Plot fasting bs and resting ECG graphs side by side
# legend
leg <- get_legend(restECG_HD)

# Plot on one graph
grid.arrange(
  arrangeGrob(nullGrob(), leg, nullGrob(), nrow = 1),
  arrangeGrob(FBS_HD, restECG_HD + theme(legend.position = "none"),
    nrow = 1
  ),
  # ncol = 1,
  heights = c(0.5, 4),
  bottom = textGrob("Source: Heart failure prediction data set",
    hjust = 1,
    x = 1,
    gp = gpar(fontsize = 6)
  )
)


## ----exerciseangina-summary---------------------------------------------------------------------------------------------------------------------------------------------

# exerciseangina  counts
exang_n <- table(model$exerciseangina)

# Compute exerciseangina counts per HD status
exang_hd_n <- table(
  model$heartdisease,
  model$exerciseangina
)

# Compute proportions of exerciseangina with HD
exang_hd_prop <- exang_hd_n %>%
  # convert to percent
  proportions(2) * 100


## ----exang-HD-----------------------------------------------------------------------------------------------------------------------------------------------------------

# Distribution of heart disease by exerciseangina
exang_HD <- exang_hd_prop %>%
  as.data.frame() %>%
  mutate(Var2 = if_else(Var2 == "Yes",
    paste0("Yes \n Patients = ", exang_n["Yes"]),
    paste0("No \n Patients = ", exang_n["No"])
  )) %>%
  ggplot(aes(
    x = Var2,
    y = Freq,
    fill = Var1
  )) +
  geom_col(
    position = position_dodge(width = 0.75),
    alpha = 0.9,
    show.legend = FALSE
  ) +
  geom_text(aes(label = paste0(
    round(Freq,
      digits = 1
    ),
    "%"
  )),
  position = position_dodge(width = 0.75),
  vjust = -0.9,
  size = 2
  ) +
  labs(
    title = "A. Exercise-induced angina",
    x = "",
    y = "Proportion of patients (%)",
    fill = "HD status"
  ) +
  scale_fill_manual(values = c("#651D32", "#205493")) +
  scale_y_continuous(limits = c(0, 90)) +
  theme_1()


## ----st_slope-summary---------------------------------------------------------------------------------------------------------------------------------------------------

# st_slope  counts
stslope_n <- table(model$st_slope)

# Compute st_slope counts per HD status
stslope_hd_n <- table(
  model$heartdisease,
  model$st_slope
)

# Compute proportions of st_slope by HD status
stslope_hd_prop <- stslope_hd_n %>%
  # convert to percent
  proportions(2) * 100


## ----exang-ST-HD, fig.cap = "Exercise-induced angina and ST-segment slope by HD status", fig.height = 3, fig.width = 8--------------------------------------------------

# Distribution of heart disease by st_slope
ST_HD <- stslope_hd_prop %>%
  as.data.frame() %>%
  mutate(Var2 = case_when(
    Var2 == "Downsloping" ~
      paste0(
        "Downsloping \n Patients = ",
        stslope_n["Downsloping"]
      ),
    Var2 == "Flat" ~
      paste0(
        "Flat \n Patients = ",
        stslope_n["Flat"]
      ),
    Var2 == "Upsloping" ~
      paste0(
        "Upsloping \n Patients = ",
        stslope_n["Upsloping"]
      )
  )) %>%
  ggplot(aes(
    x = Var2,
    y = Freq,
    fill = Var1
  )) +
  geom_col(
    position = position_dodge(width = 0.75),
    alpha = 0.9
  ) +
  geom_text(aes(label = paste0(
    round(Freq,
      digits = 1
    ),
    "%"
  )),
  position = position_dodge(width = 0.75),
  vjust = -0.9,
  size = 2
  ) +
  labs(
    title = "B. ST-segment slope",
    x = "",
    y = "Percentage of patients",
    fill = "HD status"
  ) +
  scale_fill_manual(values = c("#651D32", "#205493")) +
  scale_y_continuous(limits = c(0, 90)) +
  theme_1()

# Plot exerciseangina and st_slope graphs side by side
# legend
leg <- get_legend(ST_HD)

# Plot on one graph
grid.arrange(
  arrangeGrob(nullGrob(), leg, nullGrob(), nrow = 1),
  arrangeGrob(exang_HD, ST_HD + theme(legend.position = "none"),
    nrow = 1
  ),
  # ncol = 1,
  heights = c(0.5, 4),
  bottom = textGrob("Source: Heart failure prediction data set",
    hjust = 1,
    x = 1,
    gp = gpar(fontsize = 6)
  )
)


## ----age-summary--------------------------------------------------------------------------------------------------------------------------------------------------------

# Compute age summary by HD
summ_age_HD <- model %>%
  group_by(heartdisease) %>%
  summarise(
    min = min(age),
    median = median(age),
    mean = mean(age),
    std_dev = sd(age),
    max = max(age)
  )


## ----age-HD-------------------------------------------------------------------------------------------------------------------------------------------------------------

# Boxplots of age by HD
age_HD <- model %>%
  ggplot(aes(heartdisease, age,
    fill = heartdisease
  )) +
  geom_boxplot(
    alpha = 0.9,
    show.legend = FALSE
  ) +
  labs(
    title = "A. Age",
    x = "HD status",
    y = "Age (years)"
  ) +
  scale_fill_manual(values = c("#651D32", "#205493")) +
  theme_1()


## ----BP-classification--------------------------------------------------------------------------------------------------------------------------------------------------

# Create table showing classification of BP
tibble(
  category = c(
    "Normal",
    "Elevated",
    "Hypertension - Stage I",
    "Hypertension - Stage II"
  ),
  SBP = c(
    "< 120 mm Hg",
    "120-129 mm Hg",
    "130-139 mm Hg",
    "> or = 140 mm Hg"
  ),
  x = c(
    "and",
    "and",
    "or",
    "or"
  ),
  DBP = c(
    "< 80 mm Hg",
    "< 80 mm Hg",
    "80-90 mm Hg",
    "> or = 90 mm Hg"
  )
) %>%
  kbl(
    caption = "Categories of BP in adults",
    col.names = c(
      "BP Category",
      "Systolic BP",
      "",
      "Diastolic BP"
    ),
    booktabs = TRUE
  ) %>%
  kable_classic(
    latex_options = c("hold_position", "striped"),
    font_size = 8
  ) %>%
  row_spec(0, bold = TRUE)


## ----restingbp-summary--------------------------------------------------------------------------------------------------------------------------------------------------

# Compute restingBP summary by HD
summ_rbp_HD <- model %>%
  group_by(heartdisease) %>%
  summarise(
    min = min(restingbp),
    median = median(restingbp),
    mean = mean(restingbp),
    std_dev = sd(restingbp),
    max = max(restingbp)
  )


## ----age-restingbp-HD, fig.cap = "Distribution of age and resting BP by HD status", fig.height = 2.5, fig.width = 8-----------------------------------------------------

# Boxplots of restingbp by HD
restBP_HD <- model %>%
  ggplot(aes(heartdisease, restingbp,
    fill = heartdisease
  )) +
  geom_boxplot(
    alpha = 0.9,
    show.legend = FALSE
  ) +
  labs(
    title = "B. Resting blood pressure",
    x = "HD status",
    y = "Resting blood pressure (mmHg)"
  ) +
  scale_fill_manual(values = c("#651D32", "#205493")) +
  theme_1()

# Plot age and restingbp graphs side by side
grid.arrange(
  arrangeGrob(age_HD,
    restBP_HD,
    nrow = 1
  ),
  bottom = textGrob("Source: Heart failure prediction data set",
    hjust = 1,
    x = 1,
    gp = gpar(fontsize = 6)
  )
)


## ----cholesterol-summary------------------------------------------------------------------------------------------------------------------------------------------------

# Compute cholesterol summary by HD
summ_chol_HD <- model %>%
  group_by(heartdisease) %>%
  summarise(
    min = min(cholesterol),
    median = median(cholesterol),
    mean = mean(cholesterol),
    std_dev = sd(cholesterol),
    max = max(cholesterol)
  )


## ----optimal-cholesterol------------------------------------------------------------------------------------------------------------------------------------------------

# Create table showing optimal cholesterol levels
tibble(
  category = c(
    "Total cholesterol",
    "LDL (“bad”) cholesterol",
    "HDL (“good”) cholesterol",
    "Triglycerides"
  ),
  values = c(
    "About 150 mg/dL",
    "About 100 mg/dL",
    "At least 40 mg/dL in men and 50 mg/dL in women",
    "Less than 150 mg/dL"
  )
) %>%
  kbl(
    caption = "Optimal Cholesterol Levels",
    col.names = c(
      "Optimal Cholesterol Levels",
      ""
    ),
    booktabs = TRUE
  ) %>%
  kable_classic(
    latex_options = c("hold_position", "striped"),
    font_size = 8
  ) %>%
  row_spec(0, bold = TRUE)


## ----cholesterol-HD-----------------------------------------------------------------------------------------------------------------------------------------------------

# Boxplots of cholesterol by HD
chol_HD <- model %>%
  ggplot(aes(heartdisease, cholesterol,
    fill = heartdisease
  )) +
  geom_boxplot(
    alpha = 0.9,
    show.legend = FALSE
  ) +
  labs(
    title = "A. Serum cholesterol",
    x = "HD status",
    y = "Serum cholesterol (mg/dL)"
  ) +
  scale_fill_manual(values = c("#651D32", "#205493")) +
  theme_1()


## ----maxhr-summary------------------------------------------------------------------------------------------------------------------------------------------------------

# Create a summary table for maxhr by HD
summ_maxhr_HD <- model %>%
  group_by(heartdisease) %>%
  summarise(
    min = min(maxhr),
    median = median(maxhr),
    mean = mean(maxhr),
    std_dev = sd(maxhr),
    max = max(maxhr)
  )


## ----chol-maxhr-HD, fig.cap = "Distribution of serum cholesterol and maximum HR by HD status", fig.height = 2.5, fig.width = 8------------------------------------------

# Boxplots of age by HD
maxhr_HD <- model %>%
  ggplot(aes(heartdisease, maxhr,
    fill = heartdisease
  )) +
  geom_boxplot(
    alpha = 0.9,
    show.legend = FALSE
  ) +
  labs(
    title = "B. Maximum heart rate",
    x = "HD status",
    y = "Maximum heart rate"
  ) +
  scale_fill_manual(values = c("#651D32", "#205493")) +
  theme_1()

# Plot cholesterol and maxhr graphs side by side
grid.arrange(
  arrangeGrob(chol_HD,
    maxhr_HD,
    nrow = 1
  ),
  bottom = textGrob("Source: Heart failure prediction data set",
    hjust = 1,
    x = 1,
    gp = gpar(fontsize = 6)
  )
)


## ----oldpeak-summary----------------------------------------------------------------------------------------------------------------------------------------------------

# Create a summary table for oldpeak by HD
summ_oldpeak_HD <- model %>%
  group_by(heartdisease) %>%
  summarise(
    min = min(oldpeak),
    median = median(oldpeak),
    mean = mean(oldpeak),
    std_dev = sd(oldpeak),
    max = max(oldpeak)
  )


## ----oldpeak-HD, fig.cap = "Distribution of ST depression induced by exercise (oldpeak) by HD status", fig.height = 2.5, fig.width = 4----------------------------------

# Boxplots of age by HD
model %>%
  ggplot(aes(heartdisease, oldpeak,
    fill = heartdisease
  )) +
  geom_boxplot(
    alpha = 0.9,
    show.legend = FALSE
  ) +
  labs(
    x = "HD status",
    y = "Oldpeak"
  ) +
  scale_fill_manual(values = c("#651D32", "#205493")) +
  theme_1()


## ----Create train and test sets-----------------------------------------------------------------------------------------------------------------------------------------

# Create train (90% of model) and test (10% of model) sets
# Set seed to allow reproducibility
set.seed(2000, sample.kind = "Rounding")

# Create an object contain the 90-10 split for model_new
model_split <- model %>%
  initial_split(
    prop = 0.9,
    strata = heartdisease
  )

# Extract train and test sets
train <- training(model_split)
test <- testing(model_split)


## ----10-fold cross-validation-------------------------------------------------------------------------------------------------------------------------------------------

# Set seed to ensure reproducibility
set.seed(2000, sample.kind = "Rounding")

# Partition train set into 10 random equal splits
train_cv <- vfold_cv(train,
  strata = heartdisease,
  v = 10
)


## ----rf-recipe----------------------------------------------------------------------------------------------------------------------------------------------------------

# Create a recipe for RF pre-processing
rf_recipe <- recipe(
  data = train,
  heartdisease ~ .
)

rf_recipe


## ----rf-model-----------------------------------------------------------------------------------------------------------------------------------------------------------

# Define random forest model
rf_model <- rand_forest(
  # specify number of trees to be used
  trees = tune(),
  # specify number of predictors randomly sampled at each tree split
  mtry = tune(), # parameter to be tuned
  # specify min number of data points in a node
  min_n = tune() # parameter to be tuned
) %>%
  # specify prediction outcome mode
  set_mode("classification") %>%
  # use 'ranger' engine for fitting and allow variable importance
  set_engine("ranger",
    importance = "impurity"
  )

rf_model


## ----rf-workflow--------------------------------------------------------------------------------------------------------------------------------------------------------

# Create rf_workflow to aggregate all info to fit the model & generate predictions

rf_workflow <- workflow() %>%
  # add random forest model
  add_model(rf_model) %>%
  # add the train recipe
  add_recipe(rf_recipe)

rf_workflow


## ----rf-hyper-parameter-tuning------------------------------------------------------------------------------------------------------------------------------------------

# set seed for reproducibility
set.seed(2000)

# Tune rf hyper-parameters trees, mtry and min_n
rf_params <- rf_workflow %>%
  tune_grid(
    resamples = train_cv,
    metrics = metric_set(accuracy)
  )

# Show best accuracy from RF hyper-parameter tuning
best_rf_acc <- rf_params %>%
  show_best(n = 1) %>%
  pull(mean) %>%
  round(digits = 3)


## ----best-rf-model------------------------------------------------------------------------------------------------------------------------------------------------------

# Select RF hyper-parameters with the highest accuracy
best_rf_params <- rf_params %>%
  select_best()

# Update rf_workflow with best rf parameters
rf_workflow <- rf_workflow %>%
  finalize_workflow(best_rf_params)

rf_workflow


## ----rf-last-fit--------------------------------------------------------------------------------------------------------------------------------------------------------

# Fit best rf model on train set and evaluate on test set
rf_last_fit <- rf_workflow %>%
  last_fit(
    split = model_split,
    metrics = metric_set(accuracy)
  )


## ----knn_recipe---------------------------------------------------------------------------------------------------------------------------------------------------------

# Create a recipe for knn pre-processing
knn_recipe <- recipe(
  data = train,
  heartdisease ~ .
) %>%
  # remove skewness from all numeric predictors
  step_YeoJohnson(all_numeric_predictors()) %>%
  # normalise all numeric predictors (to std dev = 1 and mean = 0)
  step_normalize(all_numeric_predictors()) %>%
  # create dummy variables for all factors
  step_dummy(all_nominal_predictors())

knn_recipe


## ----knn-model----------------------------------------------------------------------------------------------------------------------------------------------------------

# Define knn model
knn_model <- nearest_neighbor(
  # specify number of neighbours considered at each prediction
  neighbors = tune() # parameter to be tuned
) %>%
  # specify prediction outcome mode
  set_mode("classification") %>%
  # use 'kknn' engine for fitting
  set_engine("kknn")

knn_model


## ----knn-workflow-------------------------------------------------------------------------------------------------------------------------------------------------------

# Create knn_workflow to aggregate all info to fit the model & make predictions
knn_workflow <- workflow() %>%
  # add the heart recipe
  add_recipe(knn_recipe) %>%
  # add knn model
  add_model(knn_model)

knn_workflow


## ----knn-hyper-parameter-tuning-----------------------------------------------------------------------------------------------------------------------------------------

# set seed for reproducibility
set.seed(2000)

# Tune knn hyper-parameter k
knn_params <- knn_workflow %>%
  tune_grid(
    resamples = train_cv,
    metrics = metric_set(accuracy)
  )

# Show best accuracy from knn hyper-parameter tuning
best_knn_acc <- knn_params %>%
  show_best(n = 1) %>%
  pull(mean) %>%
  round(digits = 3)


## ----best-knn-model-----------------------------------------------------------------------------------------------------------------------------------------------------

# Select knn hyper-parameter with the highest accuracy
best_knn_params <- knn_params %>%
  select_best()

# Update knn_workflow with best knn hyper-parameter
knn_workflow <- knn_workflow %>%
  finalize_workflow(best_knn_params)

knn_workflow


## ----knn-last-fit-------------------------------------------------------------------------------------------------------------------------------------------------------

# Fit best knn model on train set and evaluate on test set
knn_last_fit <- knn_workflow %>%
  last_fit(
    split = model_split,
    metrics = metric_set(accuracy)
  )


## ----best-model-metrics-------------------------------------------------------------------------------------------------------------------------------------------------

# Extract performance metrics for best rf model
rf_last_fit_metrics <- rf_last_fit %>%
  collect_metrics(summarize = TRUE) %>%
  mutate(model = "RF")

# Extract predictions for best rf model
rf_last_fit_pred <- rf_last_fit %>%
  collect_predictions()

# Extract performance metrics for best knn model
knn_last_fit_metrics <- knn_last_fit %>%
  collect_metrics(summarize = TRUE) %>%
  mutate(model = "kNN")

# Extract predictions for best knn model
knn_last_fit_pred <- knn_last_fit %>%
  collect_predictions()


## ----lastfit-confusion-matrix, fig.cap = "Confusion matrix heatmaps for A) RF and b) kNN last fit models", fig.height = 3-----------------------------------------------

# Create and plot confusion matrices for RF and knn last fit

# RF confusion matrix
rf_last_fit_cm <- rf_last_fit_pred %>%
  conf_mat(
    truth = heartdisease,
    estimate = .pred_class
  )

# Plot heatmap of RF confusion matrix
rf_cm <- rf_last_fit_cm %>%
  autoplot(type = "heatmap") +
  labs(
    title = "A. RF last fit model",
    x = "Observed heart disease",
    y = "Predicted heart disease"
  ) +
  theme_1() +
  theme(
    legend.position = "none"
  )

# knn confusion matrix
knn_last_fit_cm <- knn_last_fit_pred %>%
  conf_mat(
    truth = heartdisease,
    estimate = .pred_class
  )

# Plot heatmap of RF confusion matrix
knn_cm <- knn_last_fit_cm %>%
  autoplot(type = "heatmap", show.legend = FALSE) +
  labs(
    title = "B. kNN last fit model",
    x = "Observed heart disease",
    y = "Predicted heart disease"
  ) +
  theme_1() +
  theme(
    legend.position = "none"
  )

# Show RF and knn confusion matrix heatmaps side by side
grid.arrange(
  knn_cm,
  rf_cm,
  nrow = 1
)


## ----best-model-performance, fig.cap = "Performance metrics for RF and kNN last fit models", fig.height = 2.5, fig.width = 4--------------------------------------------

# Plot and compare performance metrics
comb_metrics <- rbind(
  rf_last_fit_metrics,
  knn_last_fit_metrics
)

comb_metrics %>%
  ggplot(aes(model,
    .estimate,
    fill = model
  )) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = round(
    .estimate,
    2
  )),
  size = 2.5,
  color = "black",
  vjust = -1
  ) +
  labs(
    x = "Model",
    y = "Accuracy",
    fill = "Model"
  ) +
  scale_fill_manual(values = c("#205493", "#651D32")) +
  scale_y_continuous(limits = c(0, 0.95)) +
  theme_1()


## ----fin-algorithm-performance, fig.cap = "Final kNN model results - trained on entire model data set and evaluated on validation data set", fig.height = 3-------------

# Train knn model on model set and evaluate on validation set
fin_alg <- knn_workflow %>%
  last_fit(
    split = heart_split,
    metrics = metric_set(accuracy)
  )

# Performance metrics
fin_alg_metrics <- fin_alg %>%
  collect_metrics()

# Predictions
fin_alg_pred <- fin_alg %>%
  collect_predictions()

# Confusion matrix
fin_alg_cm <- fin_alg_pred %>%
  conf_mat(
    truth = heartdisease,
    estimate = .pred_class
  )

# Plot performance metrics, confusion matrix, and ROC curve plots

# Performance metrics plot
fin_alg_metrics_plot <- fin_alg_metrics %>%
  ggplot(aes(.metric,
    .estimate,
    fill = .metric
  )) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = round(
    .estimate,
    2
  )),
  size = 2.5,
  color = "black",
  vjust = -1
  ) +
  labs(
    title = "A. Performance metrics",
    x = "",
    y = "Score"
  ) +
  scale_fill_manual(values = c("#205493", "#651D32")) +
  scale_y_continuous(limits = c(0, 1)) +
  theme_1()

# Confusion matrix plot
fin_alg_cm_plot <- fin_alg_cm %>%
  autoplot(type = "heatmap") +
  labs(
    title = "B. Confusion matrix",
    x = "Observed heart disease",
    y = "Predicted heart disease"
  ) +
  scale_x_discrete(limits = c("Present", "Absent")) +
  theme_1() +
  theme(
    legend.position = "none"
  )

# Plot all graphs in one
grid.arrange(fin_alg_metrics_plot,
  fin_alg_cm_plot,
  nrow = 1
)

