# enrollment trend example

# 2020-02-13

# load -------------------

# first, download the urban ed data package
devtools::install_github('UrbanInstitute/education-data-package-r')

# load packages
library(tidyverse)
library(educationdata)
library(scales)
# get data from urban api ---------

# i don't know lea id's for cleveland, denver, or dc...yet
# let's pull the directory info from 2008
dist_info_08 <- get_education_data(level = 'school-districts', 
                         source = 'ccd', 
                         topic = 'directory', 
                         filters = list(year = 2008),
                         add_labels = TRUE)

# looking at the data, we can see
# cleveland = 3904378
# denver = 0803360
# dc = 1100030

# but that only gives us the large lea - what about charters/other leas?


# get state fips codes
# ohio = 39
# colorado = 8
# dc = 11

# now, let's get all lea data in these states
# starting with dc
dc_enroll_raw <- get_education_data(level = 'school-districts',
                                source = 'ccd',
                                topic = 'directory',
                                filters = list(year = 1986:2016, 
                                               fips = 11))
# hmmm... there's probably something weird w/ cbsa pre-02...


# let's try colorado
co_enroll_raw <- get_education_data(level = 'school-districts',
                                source = 'ccd',
                                topic = 'directory',
                                filters = list(year = 1986:2016, 
                                               fips = 8))

# o-h....i-o
oh_enroll_raw <- get_education_data(level = 'school-districts',
                                    source = 'ccd',
                                    topic = 'directory',
                                    filters = list(year = 1986:2016, 
                                                   fips = 39))


# clean denver  -----------------
denver_enroll_raw <- co_enroll_raw %>%
  filter(city_mailing %in% c("Denver", "DENVER", "denver")) %>%
  filter(enrollment>0)

dps_enroll <- denver_enroll_raw %>%
  filter(leaid == "0803360") %>% 
  mutate(org = "Denver Public Schools")

non_dps_enroll <- denver_enroll_raw %>% 
  filter(leaid != "0803360") %>%
  filter(leaid != "0807230") %>%
  group_by(year) %>%
  summarise(enrollment = sum(enrollment)) %>%
  mutate(org = "Non-DPS")

denver_org_enroll <- bind_rows(dps_enroll, non_dps_enroll) %>%
  mutate(city = "Denver")

denver_summary_enroll <- bind_rows(dps_enroll, non_dps_enroll) %>%
  group_by(year) %>%
  summarise(enrollment = sum(enrollment)) %>%
  mutate(city = "Denver")

ggplot(denver_master_enroll, aes(x = year, y = enrollment))+
  geom_line()

ggplot(denver_org_enroll, aes(x = year, y = enrollment, color = org))+
  geom_line()

# clean dc ----------
dcps_enroll <- dc_enroll_raw %>%
  filter(leaid == "1100030") %>%
  filter(enrollment > 0) %>% 
  mutate(org = "DC Public Schools")

non_dcps_enroll <- dc_enroll_raw %>%
  filter(leaid != "1100030") %>%
  filter(enrollment > 0) %>%
  group_by(year) %>%
  summarise(enrollment = sum(enrollment)) %>%
  mutate(org = "Non-DCPS")

dcps_org_enroll <- bind_rows(dcps_enroll, non_dcps_enroll) %>%
  mutate(city = "Washington, D.C.")

dcps_summary_enroll <- bind_rows(dcps_enroll, non_dcps_enroll) %>%
  group_by(year) %>%
  summarise(enrollment = sum(enrollment)) %>%
  mutate(city = "Washington, D.C.")

ggplot(dcps_org_enroll, aes(x = year, y = enrollment, color = org))+
  geom_line()

ggplot(dcps_summary_enroll, aes(x = year, y = enrollment))+
  geom_line()

# clean cleveland ------------

cps_enroll <- oh_enroll_raw %>%
  filter(leaid == "3904378") %>%
  filter(enrollment >0) %>%
  mutate(org = "Cleveland Public Schools")

ggplot(cps_enroll, aes(x = year, y = enrollment))+
  geom_line()

non_cps_enroll <- oh_enroll_raw %>%
  filter(city_mailing %in% c("Cleveland", "CLEVELAND")) %>%
  filter(leaid != "3904378") %>%
  filter(enrollment >0) %>%
  group_by(year) %>%
  summarise(enrollment = sum(enrollment)) %>%
  mutate(org = "Non-CPS")

cps_org_enroll <- bind_rows(cps_enroll, non_cps_enroll)%>%
  mutate(city = "Cleveland")

cps_summary_enroll <- bind_rows(cps_enroll, non_cps_enroll) %>%
  group_by(year) %>%
  summarise(enrollment = sum(enrollment))%>%
  mutate(city = "Cleveland")

ggplot(cps_org_enroll, aes(x = year, y = enrollment, color = org))+
  geom_line()

ggplot(cps_summary_enroll, aes(x = year, y = enrollment))+
  geom_line() 

# mega summary ------------------

mega_summary <- bind_rows(denver_summary_enroll, dcps_summary_enroll,
                          cps_summary_enroll)

ggplot(mega_summary, aes(x = year, y = enrollment, color = city)) +
  geom_line()+
  scale_y_continuous(labels = comma) +
  labs(x = "Year", y = "Enrollment", color = "City",
       title = "Public School Enrollment Trends, 1986-2016",
       caption = "Source: NCES Common Core of Data")+
  theme_bw()+
  theme(legend.position = "bottom")

ggsave("city_summary.png", height = 6, width = 8, units = "in")

mega_org_summary <- bind_rows(denver_org_enroll, 
                              dcps_org_enroll, cps_org_enroll) %>%
  select(year, org, enrollment, city)%>%
  mutate(org = str_replace_all(org, "Non-.+", "Non-Flagship LEA Total"),
         org = str_replace_all(org, ".+ Public Schools", "Flagship LEA Total"))

ggplot(mega_org_summary, aes(x = year, y = enrollment, color = org)) +
  geom_line()+
  facet_wrap(~city, scales = "free_y")+
  scale_y_continuous(labels = comma) +
  labs(x = "Year", y = "Enrollment", color = "LEA",
       title = "Public School Enrollment Trends, 1986-2016",
       caption = "Source: NCES Common Core of Data")+
  theme_bw()+
  theme(legend.position = c(.8,.2))

ggsave("city_lea_summary.png", height = 4, width = 10, units = "in")
