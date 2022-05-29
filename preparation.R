library(tidyverse)

# _______ Run at the first time. ____________

raw <- read.csv("SBAnational.csv")

is_between <- function(date) {
  return(between(
    date,
    as.Date("2007-12-01"),
    as.Date("2009-06-30")
  ))
}

get_value <- function(str) {
  val <- as.character(str) %>%
    str_replace_all("$", "") %>%
    str_replace_all(",", "") %>%
    substring(2)
  return(as.double(val))
}

get_date <- function(str) {
  list <- strsplit(str, split = "-")[[1]]
  date <- list[1]
  month <- match(list[2], substr(month.name, 0, 3))
  year <- ifelse(list[3] > 50,
                 paste("19", list[3], sep = ""),
                 paste("20", list[3], sep = "")
  )
  return(as.Date(paste(year, month, date, sep = "-")))
}

# _____________________________________

# Reset data here.
data <- sample_frac(raw, 1)

data <- data %>%
  select(
    -ChgOffDate,
  -LoanNr_ChkDgt,
  -Name, -City,
  -Zip,
  -Bank,
  -ApprovalFY,
  -BalanceGross,
  -ChgOffPrinGr
  ) %>% # ChgOffDate has so many NA
  drop_na() %>% # drop NA for first time
  filter(LowDoc %in% c("Y", "N")) %>% # remove data entry errors and missing value
  filter(RevLineCr %in% c("Y", "N")) %>% # remove data entry errors and missing value
  mutate(NAICS = substr(NAICS, 0, 2)) %>% # scale to industry level
  filter(NAICS != 0) %>% # remove missing values
  mutate(Franchise = ifelse(FranchiseCode %in% c("0", "1"), 0, 1)) %>%
  mutate(RealEstate = ifelse(Term >= 240, 1, 0)) %>%
  filter(DisbursementDate != "") %>%
  filter(ApprovalDate != "") %>%
  rowwise() %>%
  mutate(ApprovalDate = get_date(ApprovalDate)) %>%
  rowwise() %>%
  mutate(DisbursementDate = get_date(DisbursementDate)) %>%
  mutate(xx = DisbursementDate + (Term * 30)) %>%
  mutate(DisbursementGross = get_value(DisbursementGross)) %>%
  mutate(DaysToDisbursement = difftime(ApprovalDate, DisbursementDate, units = "days")) %>%
  filter(DaysToDisbursement >= 0) %>%
  mutate(Recession = ifelse(is_between(xx), 1, 0)) %>%
  mutate(SBA_Appv = get_value(SBA_Appv)) %>%
  mutate(GrAppv = get_value(GrAppv)) %>%
  mutate(Portion = SBA_Appv / GrAppv) %>%
  mutate(NewExist = ifelse(NewExist <= 1, 1, 0)) %>%
  mutate(Default = case_when(
    MIS_Status == as.character("P I F") ~ "no",
    MIS_Status == as.character("CHGOFF") ~ "yes"
  )) %>% # create Default
  mutate(BankInState = ifelse(State == BankState, 1, 0)) %>%
  select(
    -FranchiseCode,
    -DisbursementDate,
    -ApprovalDate,
    -MIS_Status,
    -xx,
    -BankState,
    -GrAppv,
    -SBA_Appv) %>% # remove unused variables
  drop_na() %>% # drop NA for latent value or side effect
  mutate(State = as.factor(State)) %>%
  mutate(NAICS = as.factor(NAICS)) %>%
  mutate(Recession = as.factor(Recession)) %>%
  mutate(Default = as.factor(Default)) %>%
  mutate(RealEstate = as.factor(RealEstate)) %>%
  mutate(LowDoc = as.factor(LowDoc)) %>%
  mutate(NewExist = as.factor(NewExist)) %>%
  mutate(UrbanRural = as.factor(UrbanRural)) %>%
  mutate(Franchise = as.factor(Franchise)) %>%
  mutate(BankInState = as.factor(BankInState)) %>%
  mutate(RevLineCr = as.factor(RevLineCr))

# Export data
write.csv(data, "SBA.csv")