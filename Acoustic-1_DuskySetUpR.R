library(vroom)
library(janitor)

files <- list.files(path = "data", pattern = "detections", full.names = T)

shark_dets <- vroom(files) %>% clean_names()

metadata <- read.csv("data/Sunrise_Orsted_otn_metadata_tagging.csv") %>% clean_names() %>% 
  mutate(tagname = paste(tag_code_space, tag_id_code, sep = "-"),
         utc_release_date_time = ymd_hms(utc_release_date_time))

dusky <- shark_dets %>% filter(commonname == "dusky shark") %>% 
  left_join(metadata) %>% 
  rename(detection_timestamp_utc = datecollected,
         transmitter_id = tagname,
         receiver_sn = receiver,
         transmitter_codespace = codespace) %>% 
  glatos::false_detections(tf = 3600) %>% filter(passed_filter == 1) %>% 
  mutate(date = date(detection_timestamp_utc))



# Adding time of day ------------------------------------------------------
library(suncalc)
library(lubridate)

dets_tz <- dusky %>% 
  mutate(date_a = as.Date(with_tz(detection_timestamp_utc, tzone = "America/New_York")),
         date_times_a = with_tz(detection_timestamp_utc, tzone = "America/New_York")) 

LI_SUN <- getSunlightTimes(distinct(dets_tz, date_a)$date_a,
                             lat = -72.979606,
                             lon = 40.637524,
                             keep = c("sunrise", "sunset"),
                             tz = "America/New_York")

dets_tz_tod <- dets_tz %>% 
  left_join(LI_SUN, by = c("date_a" = "date")) %>%
  mutate(tod = ifelse(date_times_a >= sunrise & date_times_a < sunset, 'Day', 'Night')) %>% 
  dplyr::select(-c(lat, lon, sunrise, sunset))



# metrics -----------------------------------------------------------------

##################################################
#' Caclulate all our individual metrics for the
#' guitarfish and overall tagging metrics
#################################################

# consecutive days

tag_sn <- unique(dusky$tag_serial_number)

consec <- list()

for (i in 1:length(unique(dusky$tag_serial_number))) {
  consec[[i]] <- dusky %>% filter(tag_serial_number == tag_sn[i]) %>%
    distinct(date, .keep_all = T) %>% 
    group_by(grp = cumsum(c(0, diff(date) > 1)), tag_serial_number) %>% 
    mutate(ConsecutiveDays = row_number()) %>% ungroup(grp) %>% group_by(tag_serial_number) %>% 
    summarise(tag_serial_number = first(tag_serial_number),
              consec = max(ConsecutiveDays))
}

consec <- dplyr::bind_rows(consec)


# full summary table -----------------------------------------------------------


res <- dusky %>% group_by(tag_serial_number) %>% 
  summarise(
    transmitter_id = first(tag_serial_number),
    ID = first(animal_id_floy_tag_id_pit_tag_code_etc),
    Sex = first(sex),
    DW = first(length2_m),
    TL = first(length_m),
    Class = first(life_stage),
    Deployment_Date = first(utc_release_date_time),
    Last_Detection = max(date),
    First_Detection = min(date),
    Total_No_Dets = n(),
    Days_Liberty = as.numeric(first(Last_Detection) - first(First_Detection)) + 1,
    Tag_on = first(utc_release_date_time),
    Tag_off = first(utc_release_date_time) + days(first(est_tag_life)),
    Days_Monitored = if_else(first(Last_Detection) > first(Tag_off),
                             first(Last_Detection) - first(Tag_on) + 1,
                             first(Tag_off) - first(Tag_on) +1),
    Days_Present = length(unique(date)),
    residency_max = as.numeric(Days_Present) / as.numeric(Days_Liberty),
    residency_min = as.numeric(Days_Present)/ as.numeric(Days_Monitored),
    res_ratio = as.numeric(Days_Liberty)/ as.numeric(Days_Monitored),
    # res_type = case_when(
    #   residency_min >= 0.65 & res_ratio > 0.70 ~ "Resident",
    #   residency_min < 0.65 & res_ratio > 0.40 ~ "Intermittent Resident",
    #   residency_min < 0.5 & res_ratio <= 0.5 ~ "Transient",
    #   TRUE ~ NA_character_),
    n_stations = n_distinct(station_no)) %>% 
  left_join(consec, by = "tag_serial_number")


# Tagging efforts ---------------------------------------------------------


# Count -------------------------------------------------------------------
HalaviTaggingMetadata %>% group_by(year) %>% 
  summarise(count = n())


# Sex Ratio ---------------------------------------------------------------
HalaviTaggingMetadata %>% 
  group_by(year) %>% 
  count(sex)


# Age Group ---------------------------------------------------------------
HalaviTaggingMetadata %>%
  group_by(year) %>% 
  count(new_class)


# Location ----------------------------------------------------------------
HalaviTaggingMetadata %>% 
  group_by(year) %>% 
  count(capture_island)



# Size Metrics -------------------------------------------------------------------------
HalaviTaggingMetadata %>% 
  group_by(year) %>% 
  summarise(min_l = min(length_cm, na.rm = T),
            max_l = max(length_cm, na.rm = T),
            avg_l = mean(length_cm, na.rm = T),
            min_d = min(length2_cm, na.rm = T),
            max_d = max(length2_cm, na.rm = T),
            avg_d = mean(length2_cm, na.rm = T))


HalaviTaggingMetadata %>% 
  count(year,tag_model) 



# Detected Individuals ----------------------------------------------------

dets %>% distinct(transmitter_id, .keep_all = T) %>%  nrow()

dets %>% distinct(transmitter_id, .keep_all = T) %>% count(sex)

dets %>% distinct(transmitter_id, .keep_all = T) %>% count(new_class)

dets %>% distinct(transmitter_id, .keep_all = T) %>% count(otn_array)

