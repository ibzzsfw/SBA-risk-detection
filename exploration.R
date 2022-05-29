library(tidyverse)
library(corrplot)

# __________ Run at first time __________
raw <- read.csv("SBA.csv")
# _______________________________________

# Reset data here.
data <- sample_frac(raw, 1)

data <- data %>%
  select(-X, -UrbanRural) %>%
  drop_na() %>%
  mutate(State = as.factor(State)) %>%
  mutate(NAICS = as.factor(NAICS)) %>%
  mutate(NewExist = as.factor(NewExist)) %>%
  mutate(RevLineCr = as.factor(RevLineCr)) %>%
  mutate(LowDoc = as.factor(LowDoc)) %>%
  mutate(Franchise = as.factor(Franchise)) %>%
  mutate(RealEstate = as.factor(RealEstate)) %>%
  mutate(Recession = as.factor(Recession)) %>%
  mutate(BankInState = as.factor(BankInState)) %>%
  mutate(Default = as.factor(Default))

# Explore on state
data %>%
  select(State, Default) %>%
  ggplot(aes(State, fill = Default)) +
  geom_bar(position = "stack")

# Explore on industry
data %>%
  select(NAICS, Default) %>%
  ggplot(aes(NAICS, fill = Default)) +
  geom_bar(position = "fill")

data %>%
  select(NAICS, Term) %>%
  group_by(NAICS) %>%
  summarise(Term_by_NAICS = mean(Term)) %>%
  ggplot(aes(NAICS, Term_by_NAICS, fill=NAICS)) +
  geom_col()

# Explore on Term
summary(data$Term) # outliers
data %>%
  select(Term) %>%
  ggplot(aes(Term)) +
  geom_density(alpha=.2, fill="#FF6666")

data %>%
  select(Term) %>%
  arrange(desc(Term)) %>% 
  slice_min(Term, prop = .95) %>%
  ggplot(aes(Term)) +
  geom_density(alpha=.2, fill="#FF6666")

data %>%
  select(Term, Default) %>%
  mutate(Term = case_when(
    Term < quantile(Term, probs = .25) ~ "Q1",
    Term < quantile(Term, probs = .5) ~ "Q2",
    Term < quantile(Term, probs = .75) ~ "Q3",
    Term >= quantile(Term, probs = .75) ~ "Q4",
  )) %>%
  ggplot(aes(Term, fill = Default)) +
  geom_bar(position = "fill") +
  coord_flip()

# Explore on NoEmp
quantile(data$NoEmp) # very left skewed
data %>%
  select(NoEmp) %>%
  ggplot(aes(NoEmp)) +
  geom_density(alpha=.2, fill="#FF6666")

data %>%
  select(NoEmp) %>%
  arrange(desc(NoEmp)) %>% 
  slice_min(NoEmp, prop = .99) %>%
  ggplot(aes(NoEmp)) +
  geom_density(alpha=.2, fill="#FF6666")

data %>%
  select(NoEmp, Default) %>%
  mutate(NoEmp = case_when(
    NoEmp < quantile(NoEmp, probs = .25) ~ "Q1",
    NoEmp < quantile(NoEmp, probs = .5) ~ "Q2",
    NoEmp < quantile(NoEmp, probs = .75) ~ "Q3",
    NoEmp >= quantile(NoEmp, probs = .75) ~ "Q4",
  )) %>%
  ggplot(aes(NoEmp, fill = Default)) +
  geom_bar(position = "fill") +
  coord_flip()

# Explore on CreateJob
quantile(data$CreateJob) # very left skewed
data %>%
  select(CreateJob) %>%
  arrange(desc(CreateJob)) %>% 
  slice_min(CreateJob, prop = .9) %>%
  ggplot(aes(CreateJob)) + geom_histogram(binwidth = 1)

data %>%
  select(CreateJob, Default) %>%
  mutate(isCreate = as.factor(ifelse(CreateJob==0, 'no', 'yes'))) %>%
  ggplot(aes(isCreate, fill = Default)) +
  geom_bar(position = "fill") +
  coord_flip()

# Explore on RetainedJob
quantile(data$RetainedJob) # very left skewed
data %>%
  select(RetainedJob, CreateJob, Default) %>%
  arrange(desc(RetainedJob)) %>% 
  slice_min(RetainedJob, prop = .99) %>%
  arrange(desc(CreateJob)) %>% 
  slice_min(CreateJob, prop = .99) %>%
  group_by(Default) %>%
  sample_frac(.01) %>%
  ggplot(aes(CreateJob, RetainedJob, color=Default)) +
  geom_jitter()

data %>%
  select(RetainedJob, Default) %>%
  mutate(isRemain = as.factor(ifelse(RetainedJob==0, 'no', 'yes'))) %>%
  ggplot(aes(isRemain, fill = Default)) +
  geom_bar(position = "fill") +
  coord_flip()

# Explore on RevLineCr
summary(data$RevLineCr) # 42.07% is revolving lines of credit

# Explore on LowDoc
summary(data$LowDoc) # 5.48% is low doc
data %>%
  select(LowDoc, Default) %>%
  ggplot(aes(LowDoc, fill = Default)) +
  geom_bar(position = "fill") +
  coord_flip()

# Explore on portion
summary(data$Portion)
ggplot(data, aes(Portion)) + geom_histogram(binwidth = 0.1)
summary(data %>%
  select(Portion, Default) %>%
  group_by(Default))
  ggplot(aes(Default, Portion)) +
  geom_boxplot() +
  coord_flip()


# Explore on DisbursementGross
quantile(data$DisbursementGross)
data %>%
  select(DisbursementGross, Default) %>%
  group_by(Default) %>%
  summarize(Minimum = quantile(DisbursementGross, probs = 0), 
            "25% quartile" = quantile(DisbursementGross, probs = .25),
            "50% median" = quantile(DisbursementGross, probs = .5),
            "75% quartile" = quantile(DisbursementGross, probs = .75),
            "100% maximum" = quantile(DisbursementGross, probs = 1))

ggplot(data, aes(DisbursementGross)) + geom_density(kernel = "gaussian") # very left skewed

# Explore on Franchise
summary(data$Franchise) # 3.12341% has franchises
data %>%
  select(Default, Franchise) %>%
  group_by(Default) %>%
  ggplot(aes(Franchise, fill = Default)) +
  geom_bar(position = 'fill') +
  coord_flip()

# Explore on NewExist
summary(data$NewExist) # 26.46127% new
data %>%
  select(Default, NewExist) %>%
  group_by(Default) %>%
  ggplot(aes(NewExist, fill = Default)) +
  geom_bar(position = 'fill') +
  coord_flip()

# Explore on BankInState
summary(data$BankInState) # 45.63305% is 1
data %>%
  select(Default, BankInState) %>%
  group_by(Default) %>%
  ggplot(aes(BankInState, fill = Default)) +
  geom_bar(position = 'fill') +
  coord_flip()

# Explore on RealEstate
summary(data$RealEstate) # 11.02118% is 1
data %>%
  select(Default, RealEstate) %>%
  ggplot(aes(RealEstate, fill = Default)) +
  geom_bar(position = "fill") +
  coord_flip()

# Explore on UrbanRural
summary(data$UrbanRural) #
data %>%
  select(Default, UrbanRural) %>%
  ggplot(aes(UrbanRural, fill = Default)) +
  geom_bar(position = "fill") +
  coord_flip()

# Explore on Recession
summary(data$Recession) # 6.758584% is 1
data %>%
  select(Default, Recession) %>%
  ggplot(aes(Recession, fill = Default)) +
  geom_bar(position = "fill") +
  coord_flip()

# Total/Average disbursed loan amount by industry
data %>%
  select(NAICS, DisbursementGross) %>%
  group_by(NAICS) %>%
  summarise(Gross = sum(DisbursementGross)) %>%
  ggplot(aes(reorder(NAICS, Gross), Gross)) +
  geom_col()

data %>%
  select(NAICS, DisbursementGross) %>%
  group_by(NAICS) %>%
  summarise(Gross = mean(DisbursementGross)) %>%
  ggplot(aes(reorder(NAICS, Gross), Gross)) +
  geom_col()

# Average days to disbursement by industry
data %>%
  select(NAICS, DaysToDisbursement) %>%
  group_by(NAICS) %>%
  summarise(DaysToDisbursement = mean(DaysToDisbursement)) %>%
  ggplot(aes(reorder(NAICS, DaysToDisbursement), DaysToDisbursement)) +
  geom_col()

# Explore on BankInState
summary(data$BankInState)
data %>%
  select(Default, BankInState) %>%
  ggplot(aes(BankInState, fill = Default)) +
  geom_bar(position = "fill") +
  coord_flip()

# Explore on UrbanRural
data %>%
  select(Default, UrbanRural) %>%
  group_by(UrbanRural) %>%
  summarise(n = n())

# Explore on portion
data %>%
  select(Portion, Default) %>%
  ggplot(aes(Default, Portion)) +
  geom_boxplot() +
  coord_flip()

# correlation between variable
# Run very bottom colnames&rownames after this!
cor <- cor(data.frame(
  as.numeric(data$State),
  as.numeric(data$NAICS),
  data$Term,
  data$NoEmp,
  as.numeric(data$NewExist),
  data$CreateJob,
  data$RetainedJob,
  as.numeric(data$RevLineCr),
  as.numeric(data$LowDoc),
  data$DisbursementGross,
  as.numeric(data$Franchise),
  as.numeric(data$RealEstate),
  as.numeric(data$DaysToDisbursement),
  as.numeric(data$Recession),
  data$Portion,
  as.numeric(data$BankInState),
  as.numeric(data$Default)
))

# Rename correlation
names <- c("State",
           "NAICS",
           "Term",
           "Emp",
           "New",
           "Create",
           "Retained",
           "Rev",
           "LowDoc",
           "Gross",
           "Franchise",
           "RealEstate",
           "Days",
           "Recession",
           "Portion",
           "BankInState",
           "Default"
)
colnames(cor) <- names
rownames(cor) <- names

corrplot(cor, method = "number", type = "lower", diag = FALSE)