library(readxl)
library(janitor)
library(skimr)
library(tidyverse)
library(ggplot2)
library(fuzzyjoin)

# Load the inital data file
data <- readxl::read_xlsx("data/Working_Together Agreement_ VCS_Emergencies_Partnership_24_March_2023.xlsx") 

data_cleaned <- data |> 
  clean_names() |> 
  # remove field that is called email but is empty
  select(-email) |> 
  rename(engagement_opportunities = tick_all_of_the_different_engagement_opportunities_you_would_like_to_be_involved_in, email = confirm_your_email_address_please_double_check_spelling) |> 
  # Edit spelling of some emails
  mutate(email = str_to_lower(email),
         email = case_when(email == "kali@ukcommmunityfoundations.org" ~ "kali@ukcommunityfoundations.org",
                           email == "emma.aldridge@inkinddrect.org" ~ "emma.aldridge@inkinddirect.org",
         T ~ email)) |> 
  mutate(remove_from_list = replace_na(remove_from_list, "No")) |> 
  # Remove any duplicates that Jon has identified
  filter(remove_from_list != "Remove") |> 
  # Remove duplicate that Jon did not identify 
  filter(!id %in% c("VCS293", "Gov13", "VCS223"))
  
# Data set containing only previously 
data_cleaned_no_new_entries <- data_cleaned |> 
  filter(str_detect(id, "^Gov|^VCS"))

# Dataset containing new partners who have not completed satisfaction questions
new_partners <- data_cleaned |> 
  filter(str_detect(id, "^New"))

unique(data_cleaned$remove_from_list) # NA and remove categories
unique(data_cleaned$id) # VCS Gov and New categories

# Check missing data and uniqueness
data_cleaned |> skimr::skim()
# 438 unique emails, 0 missing an email

activity_involvment <- data_cleaned |> 
  select(id, email, remove_from_list, completion_time, engagement_opportunities) |> 
  mutate(bulletins = case_when(str_detect(engagement_opportunities, pattern = "Monthly EP bulletins") ~ "Yes"),
         network_calls = case_when(str_detect(engagement_opportunities, pattern = "Monthly Network Calls") ~ "Yes"),
         listen_learn = case_when(str_detect(engagement_opportunities, pattern = "EP Listen and Learns") ~ "Yes"),
         slack_group = case_when(str_detect(engagement_opportunities, pattern = "EP Slack Group") ~ "Yes"),
         capability_events = case_when(str_detect(engagement_opportunities, pattern = "Quarterly Capability Events") ~ "Yes"),
         south_east_ep = case_when(str_detect(engagement_opportunities, pattern = "South East EP") ~ "Yes"),
         south_west_ep = case_when(str_detect(engagement_opportunities, pattern = "South West EP") ~ "Yes"),
         north_ep = case_when(str_detect(engagement_opportunities, pattern = "North EP") ~ "Yes"),
         midlands_east_ep = case_when(str_detect(engagement_opportunities, pattern = "Midlands & East EP") ~ "Yes"),
         london_communities_ep = case_when(str_detect(engagement_opportunities, pattern = "London Communities EP") ~ "Yes")) 

duplicated_activity_involvment <- activity_involvment %>% get_dupes(email)

write_csv(activity_involvment, "outputs/activity_involvement_request.csv")
# write_csv(duplicated_activity_involvment, "outputs/duplicated_activity_involvement.csv")

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

# Merging civi and WTA data
# Merge based on email
data_merged_method1 <- data_cleaned_no_new_entries |> 
  left_join(contacts_civi, by ="email")

# Select any that have not been found based on direct email
data_merged_method1_NA <- data_merged_method1 |> 
  filter(is.na(created_date)) |> 
  select(-c(23:34))
# 41 organisations cannot be directly linked from the email 

write_csv(data_merged_method1_NA, "outputs/not_in_civi_yet.csv")

# Remove the part of the email before the @ and after the first .
email_striped_data <- data_merged_method1_NA |> mutate(email_reduced = gsub(".*\\@", "", email),
                                                       email_reduced = gsub("\\..*", "", email_reduced),
                                                       email_reduced = case_when(email_reduced == "dc" ~ "dc.police",
                                                          T ~ email_reduced))
email_striped_civi <- individuals_civi |> mutate(email_reduced = gsub(".*\\@", "", email),
                                                 email_reduced = gsub("\\..*", "", email_reduced),
                                                 email_reduced = case_when(email_reduced == "dc" ~ "dc.police",
                                                                   T ~ email_reduced))

data_merged_method2 <- email_striped_data |> 
  left_join(email_striped_civi, by ="email_reduced")  |> 
  mutate(region_covered = NA,
         job_title = NA) |> 
  distinct(email.x, .keep_all = TRUE) |> 
  select(-email.y, -email_reduced) |> 
  rename(email = email.x)

# Select any that have not been found based on method 2
data_merged_method2_NA <- data_merged_method2 |> 
  filter(is.na(created_date)) 
# Just 4 entries do not have a match

data_merged_method1 <- data_merged_method1 |> 
  drop_na(created_date)

final_dataset <- rbind(data_merged_method2, data_merged_method1) |> 
  rename(org_id = employee_of_contact_id) |> 
  left_join(org_civi, by="org_id") |> 
  mutate(org_subtype = replace_na(org_subtype, "Missing"),
         org_subtype_cleaned = case_when(
    org_subtype == "Local_Government" ~ "Government",
    org_subtype == "Central_Government" ~ "Government",
    org_subtype == "Statutory" ~ "Government",
    org_subtype == "Local_Infrastructure_Organisation" ~ "LIO",
    org_subtype == "Company" ~ "Company/Academic",
    org_subtype == "Academic" ~ "Company/Academic",
    org_subtype == "Local_CVS" ~ "Generalist VCS",
    org_subtype == "National_CVS" ~ "Specialist VCS",
    org_subtype == "Local_Resilience_Forum" ~ "LRF",
    T ~ org_subtype
  )) 

write_csv(final_dataset, "outputs/final_dataset_for_visualisation.csv")

############ CHECK THERE ARE NO DUPLICATES ###########
final_dataset %>% get_dupes(email)

service_satisfaction <- final_dataset |> 
  pivot_longer(cols = -c(1:13, 21:38), names_to = "service", values_to = "satisfaction") |> 
  select(id, email, service, satisfaction, primary_employer, contact_subtype, region_covered, org_id, org_name, org_subtype_cleaned, org_region_covered) |> 
  mutate(satisfaction = replace_na(satisfaction, "Missing"))

count_service_satisfaction <- service_satisfaction |> 
  group_by(service, org_subtype_cleaned) |> 
  count(satisfaction) |> 
  group_by(service, org_subtype_cleaned) |> 
  mutate(total = sum(n)) |> 
  ungroup() |> 
  mutate(Percent = (n / total)*100) |> 
  mutate(Service = case_when(
    service == "please_give_your_opinion_on_your_level_of_engagement_with_the_ep" ~ "Level of engagement\nwith the EP",
    service == "the_ep_has_better_prepared_you_for_what_to_do_in_an_emergency" ~ "The EP has better prepared me\nfor what to do in an emergency",
    service == "the_ep_has_enabled_you_to_access_more_and_or_better_information" ~ "The EP has enabled me to\naccess more or better information",
    service == "the_ep_has_enabled_you_to_better_support_those_people_who_are_most_at_risk_during_an_emergency" ~ "The EP has enabled me to better\nsupport those people who are most\nat risk during an emergency",
    service == "the_ep_has_enabled_you_to_use_information_more_effectively_to_inform_action" ~ "The EP has enabled me to use\ninformation more effectively to\ninform action",
    service == "the_ep_has_improved_your_connections_with_different_vcs_partners" ~ "The EP has improved my connections\nwith different VCS partners",
    service == "the_ep_has_improved_your_connections_with_local_resilience_forums" ~ "The EP has improved my\nconnections with LRFs"
  ))

write_csv(count_service_satisfaction, "outputs/service_satisfaction_table.csv")

count_service_satisfaction$satisfaction <- factor(count_service_satisfaction$satisfaction, levels = c("Strongly agree", "Agree", "Neither agree nor disagree", "Disagree", "Strongly disagree", "Missing"))

ggplot(data = count_service_satisfaction |> filter(service != "please_give_your_opinion_on_your_level_of_engagement_with_the_ep"), aes(x = Service, y = Percent, fill = satisfaction)) + 
  geom_bar(stat="identity") + facet_grid(.~org_subtype_cleaned) + 
  geom_text(aes(label=paste0(n, "\n",round(Percent),"%")), position = position_stack(vjust = 0.5)) +
  coord_flip() +
  labs(y = "Percent (%)",
       caption = "Data source: Working Together Agreement, January 2023 and Civi CRM\nLIO: Local Infrastructure Organisation, LRF: Local Resilience Forum\nPlease note that many LRF partners will also be government partners and vice versa, therefore coding within civi of these categories may be incorrect/inconsistent\nA partners subtype is determined through matching the WTA survey answers with Civi using the email or the domain of the email") +
  scale_fill_manual("Satisfaction", values=c("#024E1B", "#006B3E", "#ffc100", "#ff7400", "#ff0000", "#a6a6a6")) +
  theme_minimal() +
  theme(axis.text.x=element_text(face = "bold", size=10),
        axis.text.y=element_text(face = "bold", size=12),
        axis.title.x = element_text(face = "bold", size = 15),
        axis.title.y = element_text(face = "bold", size = 15),
        strip.text.x = element_text(face = "bold", size = 13),
        legend.position = "bottom") 

ggsave(paste0("outputs/service_satisfaction.png"),
       width = 16, height = 10, 
       dpi = 300)

count_service_satisfaction$satisfaction <- factor(count_service_satisfaction$satisfaction, levels = c("Strongly agree", "Agree", "Neither agree nor disagree", "Disagree", "Strongly disagree", "Too much", "About right", "Too little", "Missing"))

ggplot(data = count_service_satisfaction |> filter(service == "please_give_your_opinion_on_your_level_of_engagement_with_the_ep"), aes(x = Service, y = Percent, fill = satisfaction)) + 
  geom_bar(stat="identity") + facet_grid(.~org_subtype_cleaned) + 
  geom_text(aes(label=paste0(n, "\n",round(Percent),"%")), position = position_stack(vjust = 0.5)) +
  coord_flip() +
  labs(y = "Percent (%)", 
       caption = "Data source: Working Together Agreement, January 2023 and Civi CRM\nLIO: Local Infrastructure Organisation, LRF: Local Resilience Forum\nPlease note that many LRF partners will also be government partners and vice versa, therefore coding within civi of these categories may be incorrect/inconsistent\nA partners subtype is determined through matching the WTA survey answers with Civi using the email or the domain of the email") +
  scale_fill_manual("Satisfaction", values=c("#ff7400", "#006B3E", "#ff0000", "#a6a6a6")) +
  theme_minimal() +
  theme(axis.text.x=element_text(face = "bold", size=10),
        axis.text.y=element_text(face = "bold", size=13),
        axis.title.x = element_text(face = "bold", size = 15),
        axis.title.y = element_text(face = "bold", size = 15),
        strip.text.x = element_text(face = "bold", size = 13),
        legend.position = "bottom") 

ggsave(paste0("outputs/engagement_satisfaction.png"),
       width = 15, height = 4, 
       dpi = 300)

product_satisfaction <- final_dataset|> 
  pivot_longer(cols = -c(1:6, 13:38), names_to = "product", values_to = "satisfaction") |> 
  select(id, email, product, satisfaction, primary_employer, contact_subtype, region_covered, org_id, org_name, org_subtype_cleaned, org_region_covered) |> 
  mutate(satisfaction = replace_na(satisfaction, "Missing"))

count_product_satisfaction <- product_satisfaction |> 
  group_by(product, org_subtype_cleaned) |> 
  count(satisfaction) |> 
  group_by(product, org_subtype_cleaned) |> 
  mutate(total = sum(n)) |> 
  ungroup() |> 
  mutate(percent = (n / total)*100) |> 
  mutate(Product = case_when(
    product == "ep_listen_learns" ~ "Listen and learns", product == "ep_slack_group" ~ "Slack", product == "monthly_ep_bulletins" ~ "Monthly bulletins", product == "monthly_network_calls" ~ "Monthly network\ncalls", product == "quarterly_capability_events" ~ "Quarterly capability\nevents", product == "regional_ep_groups" ~ "Regional EP\ngroups"
  ),
  satisfaction = str_trim(satisfaction))

write_csv(count_product_satisfaction, "outputs/product_satisfaction_table.csv")

count_product_satisfaction$satisfaction <- factor(count_product_satisfaction$satisfaction, levels = c("Very Satisfied", "Satisfied", "Neither satisfied or dissatisfied", "Dissatisfied", "Very Dissatisfied", "I have never used/engaged in this", "Missing"))

ggplot(data = count_product_satisfaction, aes(x = Product, y = percent, fill = satisfaction)) + 
  geom_bar(stat="identity") + facet_grid(.~org_subtype_cleaned) + 
  geom_text(aes(label=n), position = position_stack(vjust = 0.5)) +
  coord_flip() +
  labs(y = "Percent (%)", caption = "Data source: Working Together Agreement, January 2023 and Civi CRM\nLIO: Local Infrastructure Organisation, LRF: Local Resilience Forum\nPlease note that many LRF partners will also be government partners and vice versa, therefore coding within civi of these categories may be incorrect/inconsistent\nA partners subtype is determined through matching the WTA survey answers with Civi using the email or the domain of the email") +
  scale_fill_manual("Satisfaction", values=c("#024E1B", "#006B3E", "#ffc100", "#ff7400", "#ff0000", "#808080", "#e0e0e0")) +
  theme_minimal() +
  theme(axis.text.x=element_text(face = "bold", size=10),
        axis.text.y=element_text(face = "bold", size=13),
        axis.title.x = element_text(face = "bold", size = 15),
        axis.title.y = element_text(face = "bold", size = 15),
        strip.text.x = element_text(face = "bold", size = 13),
        legend.position = "bottom") 

ggsave(paste0("outputs/product_satisfaction.png"),
       width = 15, height = 10, 
       dpi = 300)
