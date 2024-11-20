################################################################################
# 01 Prep
# 
# Merges & formats raw data
################################################################################


# Dependencies ------------------------------------------------------------

if (!"pacman" %in% installed.packages()) install.packages("pacman")
pacman::p_load(here, tidyverse, readxl, ggmap, sf)


# Import ------------------------------------------------------------------

schools_raw <- read_excel(here("data/raw/Abitur-2024_open.data.export.xlsx"),
           sheet = 3)

addresses_raw <- read_csv2(here("data/raw/Schulen.csv"),
                           locale = locale(encoding = "latin1"),
                           col_select = c(BSN, Name, PLZ, Adresse)) 

# as described in the 2nd sheet of "Abitur-2024_open.data.export.xlsx"
type_dict <- tribble(
  ~ schulform_nummer, ~ schulform_name,
  1, "berufliche Schulen",
  2, "Gymnasien",
  3, "ISS/Gemeinschaftsschulen",
  4, "Schulen in privater Trägerschaft",
  5, "Kollegs, Abendgymnasien"
)

district_dict <- tribble(
  ~ bezirk_nummer, ~ bezirk_name,
  1, "Mitte",
  2, "Friedrichshain-Kreuzberg",
  3, "Pankow",
  4, "Charlottenburg-Wilmersdorf",
  5, "Spandau",
  6, "Steglitz-Zehlendorf",
  7, "Tempelhof-Schöneberg",
  8, "Neukölln",
  9, "Treptow-Köpenick",
  10, "Marzahn-Hellersdorf",
  11, "Lichtenberg",
  12, "Reinickendorf"
)


# Wrangle & Geocode -------------------------------------------------------

key = Sys.getenv("GOOGLE_MAPS_API_KEY")
register_google(key = key)

schools <- schools_raw %>% 
  # keep only total score per school
  filter(Fach == "-") %>% 
  left_join(type_dict, by = c("Schulform" = "schulform_nummer")) %>% 
  left_join(district_dict, by = c("Bezirksnummer" = "bezirk_nummer")) %>% 
  # addresses use a simplified BSN
  mutate(BSN_kurz = str_sub(BSN, 1, 5)) %>% 
  left_join(addresses_raw, by = c("BSN_kurz" = "BSN")) %>% 
  unite("target", PLZ, Adresse, sep = " ", remove = FALSE) %>% 
  # uncomment this to test without using lots of API credits
  # slice(1) %>%
  mutate(coords = geocode(target)) %>% 
  unnest(coords) %>% 
  rename_with(tolower) %>% 
  summarise(bsn,
            name,
            schulform_nummer = schulform,
            schulform_name,
            bezirk_nummer = bezirksnummer,
            bezirk_name,
            plz,
            adresse,
            lon,
            lat,
            n_schueler = as.integer(n),
            n_bestanden = n.scls,
            pct_bestanden = round(as.double(n.best) * 100, 2),
            pct_fail = 100 - pct_bestanden,
            abischnitt = mn.scls)


# Export ------------------------------------------------------------------

write_csv(schools, here("data/processed/schools.csv"))
