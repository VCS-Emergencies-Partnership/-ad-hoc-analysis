library(readxl)
library(janitor)
library(skimr)
library(tidyverse)
library(ggplot2)
library(fuzzyjoin)

# Load the inital data file
data <- readxl::read_xlsx("data/Mailchimp-subscribed-april-2023.xlsx")

data_cleaned <- data |>
  clean_names() |>
  rename_with( .fn = function(.x){paste0("mailchimp_", .x)}) |>
  mutate(email = str_to_lower(mailchimp_email_address))

# Check missing data and uniqueness
data_cleaned |> skimr::skim()

# Load civi data
contacts_civi <- read_csv("data/CiviCRM_Contact_Search.csv") |>
  clean_names() |>
  select(-working_together_agreement_status, -added_to_slack)

# Filter for just individuals
individuals_civi <- contacts_civi |>
  filter(contact_type == "Individual")

# Filter for just organisation in civi
org_civi <- contacts_civi |>
  filter(contact_type == "Organization") |>
  select(org_id = contact_id, org_name = display_name, org_region_covered = region_covered, org_subtype = contact_subtype)

# Load inactive civi data
inactive_contacts_civi <- read_csv("data/inactive_CiviCRM_Contact_Search.csv") |>
  clean_names() |>
  select(email, contact_id, created_date, display_name, employee_of_display_name, contact_type, contact_subtype, job_title, region_covered) |>
  rename_with( .fn = function(.x){paste0("inactive_", .x)}) |>
  drop_na(inactive_email) |>
  rename(email = inactive_email)

inactive_contacts_civi <- subset(inactive_contacts_civi, is.na(inactive_contacts_civi$inactive_employee_of_display_name)) |>
  mutate(inactive_employee_of_display_name = "inactive_relationship_in_civi")

# Merging civi and mailchimp data
# Merge based on email
data_merged1 <- data_cleaned |>
  left_join(contacts_civi, by ="email")

# Merging inactive contacts data
data_merged2 <- data_merged1 |>
  left_join(inactive_contacts_civi, by ="email") |>
  # removing two duplicated records after merging
  filter(!contact_id %in% c(981, 1650)) |>
  rename(org_id = employee_of_contact_id) |>
  left_join(org_civi, by="org_id")

# Select any that have not been found based on direct email
not_in_civi <- data_merged2 |>
  filter(is.na(created_date)) |>
  select(1:5)
# 41 organisations cannot be directly linked from the email

write_csv(not_in_civi, "outputs/contacts_not_in_civi.csv")

contacts_in_civi <- data_merged2 |>
  filter(!is.na(created_date)) |>
  select(1:5, display_name, organisation_name = employee_of_display_name, inactive_employee_of_display_name, inactive_display_name, contact_type, contact_region_covered = region_covered, org_region_covered, org_subtype, contact_id, org_id, created_date, job_title)

write_csv(contacts_in_civi, "outputs/contacts_in_civi.csv")

region <- contacts_in_civi |> tabyl(org_region_covered) |>  arrange(desc(n)) |> mutate(percent = percent *100, valid_percent = valid_percent *100)
write_csv(region, "outputs/region.csv")

org_subtype <- contacts_in_civi |> tabyl(org_subtype) |>  arrange(desc(n)) |> mutate(percent = percent *100, valid_percent = valid_percent *100)
write_csv(org_subtype, "outputs/org_subtype.csv")
