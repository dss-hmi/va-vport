
ds <- tibble::tribble(
  ~ subject,
  "subSECTION NUMBER  8 SECTION FORMAT face-2-face SECTION LOCATION clinic"
  ,"SECTION NUMBER:  8 SECTION FORMAT Telephone session SECTION LOCATION clinic"
  ,"SECTION NUMBER  20+SECTION FORMAT face-2-faceSECTION LOCATION clinic"
  ,"SECTION NUMBER: 18SECTION FORMAT face-2-face DIAGNOSIS clinic"
  ,"SECTION NUMBER  2 SECTION FORMAT Video Telehealth session SEcSION LOCATION PTSD clinical team DIAGNOSIS: Major"
  
)

# https://regex101.com/r/HAoRoJ/3


import::from("magrittr", "%>%")

s <- c(
  "blah SECTION NUMBER  8 SECTION FORMAT face-2-face SECTION LOCATION clinic",
  "blah SECTION NUMBER:  8 SECTION FORMAT Telephone session SECTION LOCATION clinic",
  "blah SECTION NUMBER  20+SECTION FORMAT face-2-faceSECTION LOCATION clinic",
  "blah SECTION NUMBER: 18SECTION FORMAT face-2-face DIAGNOSIS clinic",
  "SECTION NUMBER  2 SECTION FORMAT Video Telehealth session SECTION LOCATION PTSD clinical team DIAGNOSIS: Major"
)

pattern <- "^(?:.*?SECTION NUMBER:?\\s+)(?<section_number>.{1,10}?)(?:\\s*SECTION FORMAT:?)\\s*(?<value_format>.+?)\\s?(?<key_1>SECTION LOCATION|DIAGNOSIS|NEWLOCATION)"

ds <-
  tibble::tibble(
    person_id = 1:5,
    s = s
  )

ds <-
  ds %>%
  rematch2::bind_re_match(from=s, pattern) %>%
  tibble::as_tibble()
ds

