
require(readxl)
require(dplyr)

raw <- read_excel('PDT2015 Attendee List for Sponsors_Exhibitors_v2.xlsx', sheet = 1)


dat <- raw %>% 
  mutate(
    name = paste0(FIRST_NAME, ' ', LAST_NAME), 
    name = ifelse(is.na(DESIGNATION), name, 
                  paste0(name, ', ', DESIGNATION)),
    # clean up DC
    city = ifelse(
      CITY %in% c('Washington DC','Washington, DC'),
      'Washington', CITY),
    state = ifelse(
      CITY %in% c('Washington DC','Washington, DC'),
      'DC',STATE_PROVINCE),
    # CLean up AP
    city = ifelse(state=='AP',NA,city),
    state = ifelse(state=='AP',NA,state),
    loc = ifelse(is.na(state), NA,
                 paste(city, state, sep = ', '))
  ) %>% 
  select(name, title = TITLE, company = COMPANY, 
         loc, state, city = CITY, zip=ZIP,
         first = FIRST_NAME, last=LAST_NAME, desig = DESIGNATION)



# Fuzzy matching
unique(agrep('McLean, VA', dat$loc, value = T, ignore.case = T))
unique(agrep('Washington, DC', dat$loc, value = T, ignore.case = T))
unique(agrep('La Plata, MD', dat$loc, value = T, ignore.case = T))


fuzz <- lapply(unique(dat$loc), 
               function(x) agrep(x, unique(dat$loc), value = T, ignore.case = T))
fuzz <- fuzz[sapply(fuzz, length)>1]
sort(unique(unlist(fuzz)))

# clean up city names
e <- environment()
loc_clean <- function(loc, repl=''){
  if(repl=='') repl <- loc
  cat(sort(unique(dat$loc[agrep(loc, dat$loc, ignore.case = T)])))
  e$dat$loc[agrep(loc, dat$loc, ignore.case = T)] <- repl
}

loc_clean("Atlanta, GA")
loc_clean("Falls Church, VA")
loc_clean("Ft. George G. Meade, MD", "Fort George G. Meade, MD")
loc_clean("Herndon, VA")
loc_clean("Indianapolis, IN")
loc_clean("McLean, VA")
loc_clean("La Plata, MD")
loc_clean("St. Louis, MO")

dat$loc[dat$loc=='Rockivlle, MD'] <- 'Rockville, MD'
dat$loc[dat$loc %in% 
          c("Washington Navy Yard, DC",
            "washington, DC", "WASHINGTON, DC")] <- "Washington, DC"


# add cities latitude/longitude - kindly provided by google:
if(file.exists('geo_codes.RData')){
  load('geo_codes.RData')    
} else {
  loc <- sort(unique(dat$loc))
  latlon <- ggmap::geocode(loc) 
  latlon <- data.frame(loc, latlon, stringsAsFactors = F)
  save(latlon, file = 'geo_codes.RData')    
}



# Merge final dataset
final <- dat


# Aggregate
agg.city <- final %>%
  filter(!is.na(loc)) %>% 
  group_by(state, loc) %>%
  summarize(Attendees=n()) %>%
  left_join(latlon, by='loc') %>% 
  rename(city = loc)


agg.state <- final %>%
  filter(!is.na(state)) %>% 
  group_by(state) %>%
  summarize(Attendees=n()) %>%
  arrange(-Attendees)



# save datasets
save(final, agg.city, agg.state, 
     file = 'PDT_2015.RData')


