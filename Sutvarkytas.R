#Duomenų pertvarkymas
library(readxl)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(tibble)
library(lubridate)
library(ggplot2)
library(scales)
library(sf)
library(geodata)
library(terra)
library(leaflet)
library(htmlwidgets)
library(patchwork)
library(forecast)
library(vars)
library(tseries)

Keyword_Stats_1_ <- read_excel("C:/Users/jurgi/Desktop/Bakalauras/Duomenys/Keyword_Stats (1).xlsx")
head(Keyword_Stats_1_)


#Pašaliname dublikatus pagal 'Keyword' stulpelį
Keyword_Stats_tvarkingi <- Keyword_Stats_1_ %>%
  distinct(Keyword, .keep_all = TRUE)

# Patikriname rezultatą
head(Keyword_Stats_tvarkingi)
nrow(Keyword_Stats_1_)     # Kiek eilučių buvo
nrow(Keyword_Stats_tvarkingi)   # Kiek liko po valymo

#įsikeliu adexspot duomenis ir juos pertvarkau atrenku man reikalingas įmones

All_data_ad_fact <- read_csv("C:/Users/jurgi/Desktop/Bakalauras/Duomenys/bq-results-20260208-142248-1770560586403.CSV")
head(All_data_ad_fact)


Unique_Brands_List <- All_data_ad_fact %>%
  distinct(A_BRAND_E) %>%       
  filter(!is.na(A_BRAND_E))      

print(Unique_Brands_List)



All_data_ad_fact_TVARKINGI_BRAND <- All_data_ad_fact %>%
  mutate(
    A_BRAND_E_CLEAN = str_to_upper(A_BRAND_E),
    A_BRAND_E_CLEAN = str_trim(A_BRAND_E_CLEAN),
    
    A_BRAND_E_CLEAN = case_when(
      A_BRAND_E_CLEAN %in% c("ERMITAŽAS", "WWW.ERMITAZAS.LT") ~ "ERMITAŽAS",
      A_BRAND_E_CLEAN %in% c("SENUKAI", "WWW.SENUKAI.LT") ~ "SENUKAI",
      A_BRAND_E_CLEAN %in% c("MOKI VEŽI", "WWW.MOKIVEZI.LT") ~ "MOKI VEŽI",
      A_BRAND_E_CLEAN == "LYTAGRA" ~ "LYTAGRA",
      A_BRAND_E_CLEAN == "CELSIS" ~ "CELSIS",
      A_BRAND_E_CLEAN == "DEPO" ~ "DEPO",
      A_BRAND_E_CLEAN == "LEMORA" ~ "LEMORA",
      TRUE ~ A_BRAND_E_CLEAN
    )
  )

# PATIKRINIMAS
All_data_ad_fact_TVARKINGI_BRAND %>%
  distinct(A_BRAND_E, A_BRAND_E_CLEAN) %>%
  arrange(A_BRAND_E_CLEAN)

summary(All_data_ad_fact_TVARKINGI_BRAND)


#standartizavimas į mėnesinius pagal brand apjungiant

add_fact_menesinis <- All_data_ad_fact_TVARKINGI_BRAND %>%
  mutate(
    YearMonth = floor_date(date, "month")
  ) %>%
  group_by(YearMonth, A_BRAND_E_CLEAN) %>%
  summarise(
    Spend = sum(expenditure, na.rm = TRUE),
    Impressions = sum(impress, na.rm = TRUE),
    GRP = sum(grp, na.rm = TRUE),
    GRP_calc = sum(GRP_calc, na.rm = TRUE),
    Contacts = sum(Contacts_calc, na.rm = TRUE),
    eq_GRP = sum(eq_GRP, na.rm = TRUE),
    eq_Contacts = sum(eq_Contacts, na.rm = TRUE),
    .groups = "drop"
  )
summary(add_fact_menesinis)


#keyword sisteminimas su brand priskyrimu 
Google_with_brand <- Keyword_Stats_tvarkingi %>%
  mutate(
    Keyword_clean = str_to_upper(Keyword),
    Brand = case_when(
      str_detect(Keyword_clean, "SENUKAI") ~ "SENUKAI",
      str_detect(Keyword_clean, "ERMIT") ~ "ERMITAŽAS",
      str_detect(Keyword_clean, "MOKI") ~ "MOKI VEŽI",
      str_detect(Keyword_clean, "LEMORA") ~ "LEMORA",
      str_detect(Keyword_clean, "DEPO") ~ "DEPO",
      str_detect(Keyword_clean, "LYTAGRA") ~ "LYTAGRA",
      str_detect(Keyword_clean, "CELSIS") ~ "CELSIS",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Brand))

summary(Google_with_brand)


#lokacijos:
store_locations_with_coordinates <- read_excel("C:/Users/jurgi/Desktop/Bakalauras/Duomenys/store_locations_with_coordinates.xlsx")
View(store_locations_with_coordinates)

head(store_locations_with_coordinates)

#gyvnetojai
gyventojai_2025_2026 <- read_excel("C:/Users/jurgi/Desktop/Bakalauras/Duomenys/20260101_Gyventoju_skaicius_savivaldybese_764c2028af.xlsx")
View(gyventojai_2025_2026) 
head(gyventojai_2025_2026)


apskriciu_lentele <- tribble(
  ~`Savivaldybės pavadinimas`, ~Apskritis,
  
  # Alytaus apskritis
  "Alytaus m. sav.", "Alytaus apskritis",
  "Alytaus r. sav.", "Alytaus apskritis",
  "Druskininkų sav.", "Alytaus apskritis",
  "Lazdijų r. sav.", "Alytaus apskritis",
  "Varėnos r. sav.", "Alytaus apskritis",
  
  # Kauno apskritis
  "Birštono sav.", "Kauno apskritis",
  "Jonavos r. sav.", "Kauno apskritis",
  "Kaišiadorių r. sav.", "Kauno apskritis",
  "Kauno m. sav.", "Kauno apskritis",
  "Kauno r. sav.", "Kauno apskritis",
  "Kėdainių r. sav.", "Kauno apskritis",
  "Prienų r. sav.", "Kauno apskritis",
  "Raseinių r. sav.", "Kauno apskritis",
  
  # Klaipėdos apskritis
  "Klaipėdos m. sav.", "Klaipėdos apskritis",
  "Klaipėdos r. sav.", "Klaipėdos apskritis",
  "Kretingos r. sav.", "Klaipėdos apskritis",
  "Neringos sav.", "Klaipėdos apskritis",
  "Palangos m. sav.", "Klaipėdos apskritis",
  "Skuodo r. sav.", "Klaipėdos apskritis",
  "Šilutės r. sav.", "Klaipėdos apskritis",
  
  # Marijampolės apskritis
  "Kalvarijos sav.", "Marijampolės apskritis",
  "Kazlų Rūdos sav.", "Marijampolės apskritis",
  "Marijampolės sav.", "Marijampolės apskritis",
  "Šakių r. sav.", "Marijampolės apskritis",
  "Vilkaviškio r. sav.", "Marijampolės apskritis",
  
  # Panevėžio apskritis
  "Biržų r. sav.", "Panevėžio apskritis",
  "Kupiškio r. sav.", "Panevėžio apskritis",
  "Panevėžio m. sav.", "Panevėžio apskritis",
  "Panevėžio r. sav.", "Panevėžio apskritis",
  "Pasvalio r. sav.", "Panevėžio apskritis",
  "Rokiškio r. sav.", "Panevėžio apskritis",
  
  # Šiaulių apskritis
  "Akmenės r. sav.", "Šiaulių apskritis",
  "Joniškio r. sav.", "Šiaulių apskritis",
  "Kelmės r. sav.", "Šiaulių apskritis",
  "Pakruojo r. sav.", "Šiaulių apskritis",
  "Radviliškio r. sav.", "Šiaulių apskritis",
  "Šiaulių m. sav.", "Šiaulių apskritis",
  "Šiaulių r. sav.", "Šiaulių apskritis",
  
  # Tauragės apskritis
  "Jurbarko r. sav.", "Tauragės apskritis",
  "Pagėgių sav.", "Tauragės apskritis",
  "Šilalės r. sav.", "Tauragės apskritis",
  "Tauragės r. sav.", "Tauragės apskritis",
  
  # Telšių apskritis
  "Mažeikių r. sav.", "Telšių apskritis",
  "Plungės r. sav.", "Telšių apskritis",
  "Rietavo sav.", "Telšių apskritis",
  "Telšių r. sav.", "Telšių apskritis",
  
  # Utenos apskritis
  "Anykščių r. sav.", "Utenos apskritis",
  "Ignalinos r. sav.", "Utenos apskritis",
  "Molėtų r. sav.", "Utenos apskritis",
  "Utenos r. sav.", "Utenos apskritis",
  "Visagino sav.", "Utenos apskritis",
  "Zarasų r. sav.", "Utenos apskritis",
  
  # Vilniaus apskritis
  "Elektrėnų sav.", "Vilniaus apskritis",
  "Šalčininkų r. sav.", "Vilniaus apskritis",
  "Širvintų r. sav.", "Vilniaus apskritis",
  "Švenčionių r. sav.", "Vilniaus apskritis",
  "Trakų r. sav.", "Vilniaus apskritis",
  "Ukmergės r. sav.", "Vilniaus apskritis",
  "Vilniaus m. sav.", "Vilniaus apskritis",
  "Vilniaus r. sav.", "Vilniaus apskritis"
)

gyventojai_su_apskritim <- gyventojai_2025_2026 %>%
  mutate(`Savivaldybės pavadinimas` = str_squish(`Savivaldybės pavadinimas`)) %>%
  left_join(apskriciu_lentele, by = "Savivaldybės pavadinimas") %>%
  mutate(
    across(
      -c(`Eil. nr.`, `Savivaldybės pavadinimas`, Apskritis),
      ~ parse_number(as.character(.x), locale = locale(grouping_mark = ","))
    )
  )

skaiciu_stulpeliai <- setdiff(
  names(gyventojai_su_apskritim),
  c("Eil. nr.", "Savivaldybės pavadinimas", "Apskritis")
)

gyventojai_pagal_apskritis <- gyventojai_su_apskritim %>%
  group_by(Apskritis) %>%
  summarise(
    `Savivaldybių sk.` = n(),
    across(all_of(skaiciu_stulpeliai), ~ sum(.x, na.rm = TRUE)),
    .groups = "drop"
  )

gyventojai_pagal_apskritis

viso <- gyventojai_pagal_apskritis %>%
  summarise(
    Apskritis = "Iš viso",
    `Savivaldybių sk.` = sum(`Savivaldybių sk.`),
    across(where(is.numeric), ~ sum(.x, na.rm = TRUE))
  )

gyventojai_pagal_apskritis <- bind_rows(gyventojai_pagal_apskritis, viso)

gyventojai_pagal_apskritis


#praduotuves

shops <- read_excel("C:/Users/jurgi/Desktop/Bakalauras/Duomenys/store_locations_with_coordinates.xlsx") %>%
  mutate(
    X = as.numeric(X),   
    Y = as.numeric(Y)    
  )

#Kiekis pagal miestus parduotuviu
keikis_parduotuviu <- shops %>%
  group_by(Brand) %>%
  summarise(
    parduotuves = n(),
    miestai = n_distinct(Miestas),
    .groups = "drop"
  ) %>%
  arrange(desc(parduotuves), desc(miestai))

print(keikis_parduotuviu)

# parduotuves pagal miestus
city_summary <- shops %>%
  count(Miestas, name = "shops_n") %>%
  arrange(desc(shops_n), Miestas)

print(city_summary)

most_popular_city <- city_summary %>% slice(1)
print(most_popular_city)

#žemėlapis

lt_apskritys <- geodata::gadm(country = "LTU", level = 1, path = tempdir()) |>
  sf::st_as_sf()

# Sutvarkom apskričių pavadinimus, kad sutaptų su tavo lentele
lt_apskritys <- geodata::gadm(country = "LTU", level = 1, path = tempdir()) |>
  sf::st_as_sf() %>%
  mutate(
    Apskritis = case_when(
      NAME_1 == "Alytaus" ~ "Alytaus apskritis",
      NAME_1 == "Kauno" ~ "Kauno apskritis",
      NAME_1 == "Klaipedos" ~ "Klaipėdos apskritis",
      NAME_1 == "Marijampoles" ~ "Marijampolės apskritis",
      NAME_1 == "Panevezio" ~ "Panevėžio apskritis",
      NAME_1 %in% c("Šiauliai", "Siauliai") ~ "Šiaulių apskritis",
      NAME_1 == "Taurages" ~ "Tauragės apskritis",
      NAME_1 %in% c("Telšiai", "Telsiai") ~ "Telšių apskritis",
      NAME_1 == "Utenos" ~ "Utenos apskritis",
      NAME_1 == "Vilniaus" ~ "Vilniaus apskritis",
      TRUE ~ NA_character_
    )
  )

# Prijungiam gyventojų skaičių prie apskričių žemėlapio
lt_apskritys_plot <- lt_apskritys %>%
  left_join(
    gyventojai_pagal_apskritis %>%
      filter(Apskritis != "Iš viso") %>%
      dplyr::select(Apskritis, `Bendras gyventojų skaičius`),
    by = "Apskritis"
  )


brands_7 <- shops %>%
  count(Brand, sort = TRUE) %>%
  slice_head(n = 7) %>%
  pull(Brand)

shops_7 <- shops %>%
  filter(Brand %in% brands_7) %>%
  filter(!is.na(X), !is.na(Y))

#PAVERČIAM PARDUOTUVES Į sf TAŠKUS

shops_sf <- shops_7 %>%
  st_as_sf(coords = c("Y", "X"), crs = 4326, remove = FALSE) %>%
  st_transform(st_crs(lt_apskritys_plot))

p <- ggplot() +
  geom_sf(
    data = lt_apskritys_plot,
    aes(fill = `Bendras gyventojų skaičius`),
    color = "white",
    linewidth = 0.3
  ) +
  geom_sf(
    data = shops_sf,
    color = "red",
    size = 2.2,
    alpha = 0.9,
    show.legend = FALSE
  ) +
  facet_wrap(~ Brand, ncol = 3) +
  scale_fill_gradientn(
    colors = c("#f7fbff", "#c6dbef", "#6baed6", "#2171b5", "#08306b"),
    labels = comma_format(big.mark = " ")
  ) +
  labs(
    title = "Lietuvos apskritys ir parduotuvių lokacijos pagal prekinį ženklą",
    subtitle = "Fonas: bendras gyventojų skaičius apskrityje | Taškai: parduotuvių vietos",
    fill = "Gyventojai"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    strip.text = element_text(face = "bold", size = 11, hjust = 0.5),
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "right"
  )

print(p)


# kiek kiekvienas brandas turi parduotuvių kiekviename mieste
parduotuviu_kiekiai <- shops %>%
  group_by(Brand, Miestas) %>%
  summarise(
    parduotuviu_sk = n(),
    .groups = "drop"
  )

# suvestinė pagal brandą
parduotuviu_kiekiai <- parduotuviu_kiekiai %>%
  group_by(Brand) %>%
  summarise(
    viso_parduotuviu = sum(parduotuviu_sk, na.rm = TRUE),
    miestu_sk = n(),
    vidurkis = round(mean(parduotuviu_sk, na.rm = TRUE), 2),
    mediana = round(median(parduotuviu_sk, na.rm = TRUE), 2),
    minimumas = min(parduotuviu_sk, na.rm = TRUE),
    maksimumas = max(parduotuviu_sk, na.rm = TRUE),
    sd = round(sd(parduotuviu_sk, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  arrange(desc(viso_parduotuviu), desc(vidurkis))

parduotuviu_kiekiai


shops_sf_full <- shops %>%
  filter(!is.na(X), !is.na(Y)) %>%
  st_as_sf(coords = c("Y", "X"), crs = 4326, remove = FALSE) %>%   # jei reikia, sukeisim
  st_transform(st_crs(lt_apskritys))

shops_su_apskritim <- st_join(
  shops_sf_full,
  lt_apskritys %>% dplyr::select(Apskritis),
  left = TRUE
)

# Patikrinam ar visoms parduotuvėms priskyrė apskritį
shops_su_apskritim %>%
  st_drop_geometry() %>%
  filter(is.na(Apskritis)) %>%
  dplyr::select(Brand, Miestas, X, Y)

#  Analizė: kiek parduotuvių brandas turi kiekvienoje apskrityje
analize <- shops_su_apskritim %>%
  st_drop_geometry() %>%
  filter(!is.na(Apskritis)) %>%
  count(Brand, Apskritis, name = "parduotuviu_sk") %>%
  left_join(
    gyventojai_pagal_apskritis %>%
      filter(Apskritis != "Iš viso") %>%
      dplyr::select(Apskritis, gyventojai = `Bendras gyventojų skaičius`),
    by = "Apskritis"
  ) %>%
  mutate(
    gyventojai_vienai_parduotuvei = gyventojai / parduotuviu_sk,
    parduotuves_100k = parduotuviu_sk / gyventojai * 100000
  )

analize

brand_pop_summary <- analize %>%
  group_by(Brand) %>%
  summarise(
    apskriciu_sk = n(),
    viso_parduotuviu = sum(parduotuviu_sk, na.rm = TRUE),
    vidurkis_gyv_1_pard = round(mean(gyventojai_vienai_parduotuvei, na.rm = TRUE), 0),
    mediana_gyv_1_pard = round(median(gyventojai_vienai_parduotuvei, na.rm = TRUE), 0),
    min_gyv_1_pard = round(min(gyventojai_vienai_parduotuvei, na.rm = TRUE), 0),
    max_gyv_1_pard = round(max(gyventojai_vienai_parduotuvei, na.rm = TRUE), 0),
    vid_parduotuves_100k = round(mean(parduotuves_100k, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  arrange(mediana_gyv_1_pard)

brand_pop_summary

ggplot(brand_pop_summary,
       aes(x = reorder(Brand, mediana_gyv_1_pard),
           y = mediana_gyv_1_pard)) +
  geom_col(fill = "skyblue") +
  coord_flip() +
  scale_y_continuous(labels = comma_format(big.mark = " ")) +
  labs(
    title = "Medianinis gyventojų skaičius vienai parduotuvei pagal prekinį ženklą",
    x = "prekinis ženklas",
    y = "Gyventojai 1 parduotuvei"
  ) +
  theme_minimal(base_size = 12)

ggplot(analize,
       aes(x = reorder(Brand, gyventojai_vienai_parduotuvei, median),
           y = gyventojai_vienai_parduotuvei)) +
  geom_boxplot(fill = "skyblue", alpha = 0.8) +
  scale_y_continuous(labels = comma_format(big.mark = " ")) +
  labs(
    title = "Gyventojų skaičius vienai parduotuvei pagal prekinį ženklą",
    x = "Prekinis ženklas",
    y = "Gyventojai 1 parduotuvei"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5)
  )


#########################################
########  Pradinė analizė
########################################
# Pilnas mėnesių intervalas
all_months <- seq(
  from = min(add_fact_menesinis$YearMonth),
  to   = max(add_fact_menesinis$YearMonth),
  by   = "month"
)
print(all_months)

#Visi brand'ai
all_brands <- unique(add_fact_menesinis$A_BRAND_E_CLEAN)
print(all_brands)
# apjungimas
full_panel <- expand_grid(
  YearMonth = all_months,
  A_BRAND_E_CLEAN = all_brands
)

print(full_panel)

#Sujungiam su tavo agreguotais duomenimis
panel_with_gaps <- full_panel %>%
  left_join(add_fact_menesinis,
            by = c("YearMonth", "A_BRAND_E_CLEAN"))

praleistu_analize <- panel_with_gaps %>%
  group_by(A_BRAND_E_CLEAN) %>%
  summarise(
    missing_months = sum(is.na(Spend)),
    total_months = n(),
    missing_pct = missing_months / total_months
  ) %>%
  arrange(desc(missing_pct))
print(praleistu_analize)

panel_with_gaps <- panel_with_gaps %>%
  mutate(
    reklama_status = ifelse(is.na(Spend),
                            "Nesireklamuoja",
                            "Reklamuojasi")
  )

ggplot(panel_with_gaps,
       aes(x = YearMonth,
           y = A_BRAND_E_CLEAN,
           fill = reklama_status)) +
  geom_tile(color = "grey90") +
  scale_fill_manual(values = c("Reklamuojasi" = "lightgreen",
                               "Nesireklamuoja" = "lightblue"),
                    name = "Reklamos būsena") +
  labs(title = "Prekinių ženklų aktyvumas pagal mėnesius Adexspot",
       x = "Mėnuo",
       y = "Prekinis ženklas") +
  theme_minimal()

panel_with_gaps %>%
  group_by(YearMonth) %>%
  summarise(
    brands_without_activity = sum(is.na(Spend))
  ) %>%
  ggplot(aes(YearMonth, brands_without_activity)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Brandai be aktyvumo")


Google_long <- Google_with_brand %>%
  pivot_longer(
    cols = starts_with("Searches:"),
    names_to = "Month",
    values_to = "Searches"
  ) %>%
  mutate(
    Month = str_remove(Month, "Searches: "),
    YearMonth = parse_date_time(Month, "b Y"),
    Searches = as.numeric(str_replace_all(Searches, ",", ""))  # pašalinam kablelius
  )

Google_menesinis <- Google_long %>%
  group_by(YearMonth, Brand) %>%
  summarise(
    Searches = sum(Searches, na.rm = TRUE),
    .groups = "drop"
  )
google_panel <- expand_grid(
  YearMonth = all_months,
  Brand = all_brands
)

google_with_gaps <- google_panel %>%
  left_join(Google_menesinis,
            by = c("YearMonth", "Brand"))
google_with_gaps <- google_with_gaps %>%
  mutate(
    google_status = ifelse(is.na(Searches) | Searches == 0,
                           "Nėra paieškų",
                           "Yra paieškų")
  )
ggplot(google_with_gaps,
       aes(x = YearMonth,
           y = Brand,
           fill = google_status)) +
  geom_tile(color = "grey90") +
  scale_fill_manual(values = c("Yra paieškų" = "lightgreen",
                               "Nėra paieškų" = "lightblue"),
                    name = "Google aktyvumas") +
  labs(title = "Prekinių ženklų aktyvumas pagal mėnesius Google",
       x = "Mėnuo",
       y = "Prekinis ženklas") +
  theme_minimal()
google_praleistu_analize <- google_with_gaps %>%
  group_by(Brand) %>%
  summarise(
    missing_months = sum(is.na(Searches) | Searches == 0),
    total_months = n(),
    missing_pct = missing_months / total_months
  ) %>%
  arrange(desc(missing_pct))

print(google_praleistu_analize)


options(scipen = 999)

adexpo_data <- All_data_ad_fact_TVARKINGI_BRAND %>%
  filter(!is.na(date)) %>%
  mutate(
    date = as.Date(date),
    Year = year(date),
    Month = month(date)
  ) %>%
  filter(date >= as.Date("2022-01-01"),
         date <= as.Date("2025-06-30"))

options(scipen = 999)

# Duomenu paruosimas: 2022-01-01 iki 2025-06-30


adexpo_data <- All_data_ad_fact_TVARKINGI_BRAND %>%
  filter(!is.na(date)) %>%
  mutate(
    date = as.Date(date),
    Metai = year(date),
    Menuo = month(date)
  ) %>%
  filter(
    date >= as.Date("2022-01-01"),
    date <= as.Date("2025-06-30")
  )

adexpo_data <- adexpo_data %>%
  mutate(
    A_MEDTYP_E = str_to_upper(str_trim(A_MEDTYP_E)),
    Medijos_grupe = case_when(
      A_MEDTYP_E == "TV" ~ "Televizija",
      A_MEDTYP_E == "INTERNET" ~ "Internetas",
      A_MEDTYP_E == "RADIO" ~ "Radijas",
      A_MEDTYP_E == "OUTDOOR" ~ "Lauko reklama",
      A_MEDTYP_E == "STATIC" ~ "Statinė reklama",
      TRUE ~ "Kita"
    ),
    Medijos_grupe = factor(
      Medijos_grupe,
      levels = c("Televizija", "Internetas", "Radijas", "Lauko reklama", "Statinė reklama", "Kita")
    )
  )


bendra_tema <- theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.title = element_text(face = "bold")
  )



#Irasu skaicius pagal medijos tipa
irasu_skaicius_pagal_tipa <- adexpo_data %>%
  filter(!is.na(Medijos_grupe)) %>%
  count(Medijos_grupe, sort = TRUE)

print(irasu_skaicius_pagal_tipa)

p1 <- ggplot(irasu_skaicius_pagal_tipa,
             aes(x = reorder(Medijos_grupe, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  scale_y_continuous(labels = label_number(big.mark = " ")) +
  labs(
    title = "Irašų skaičius pagal medijos tipa",
    x = "Medijos tipas",
    y = "Irašų skaičius"
  ) +
  bendra_tema

#Islaidos pagal medijos tipa
islaidos_pagal_tipa <- adexpo_data %>%
  filter(!is.na(Medijos_grupe)) %>%
  group_by(Medijos_grupe) %>%
  summarise(
    Islaidos = sum(expenditure, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Islaidos_mln = Islaidos / 1e6) %>%
  arrange(desc(Islaidos_mln))

print(islaidos_pagal_tipa)

p2 <- ggplot(islaidos_pagal_tipa,
             aes(x = reorder(Medijos_grupe, Islaidos_mln), y = Islaidos_mln)) +
  geom_col(fill = "darkred") +
  coord_flip() +
  scale_y_continuous(labels = label_number(accuracy = 0.1, big.mark = " ")) +
  labs(
    title = "Reklamos išlaidos pagal medijos tipa",
    x = "Medijos tipas",
    y = "Išlaidos, mln."
  ) +
  bendra_tema

#  Kontaktai pagal medijos tipa
kontaktai_pagal_tipa <- adexpo_data %>%
  filter(!is.na(Medijos_grupe)) %>%
  group_by(Medijos_grupe) %>%
  summarise(
    Kontaktai = sum(Contacts_calc, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Kontaktai_mln = Kontaktai / 1e6) %>%
  arrange(desc(Kontaktai_mln))

print(kontaktai_pagal_tipa)

p3 <- ggplot(kontaktai_pagal_tipa,
             aes(x = reorder(Medijos_grupe, Kontaktai_mln), y = Kontaktai_mln)) +
  geom_col(fill = "darkgreen") +
  coord_flip() +
  scale_y_continuous(labels = label_number(accuracy = 0.1, big.mark = " ")) +
  labs(
    title = "Kontaktai pagal medijos tipa",
    x = "Medijos tipas",
    y = "Kontaktai, mln."
  ) +
  bendra_tema

# Sugrupuotas vaizdas

(p1 | p2 | p3) +
  plot_annotation(
    title = "Pagrindiniai rodikliai pagal medijos tipa",
    theme = theme(
      plot.title = element_text(hjust = 0.5, face = "bold")
    )
  )


# Irasu skaicius pagal metus ir medijos tipa
irasai_pagal_metus_tipa <- adexpo_data %>%
  filter(!is.na(Medijos_grupe)) %>%
  count(Metai, Medijos_grupe)

print(irasai_pagal_metus_tipa)

p4 <- ggplot(irasai_pagal_metus_tipa,
             aes(x = factor(Metai), y = n, fill = Medijos_grupe)) +
  geom_col(position = "dodge") +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Irašu skaičius pagal metus ir medijos tipa",
    x = "Metai",
    y = "Irašų skaičius",
    fill = "Medijos tipas"
  ) +
  bendra_tema

# Islaidos pagal metus ir medijos tipa
islaidos_pagal_metus_tipa <- adexpo_data %>%
  filter(!is.na(Medijos_grupe)) %>%
  group_by(Metai, Medijos_grupe) %>%
  summarise(
    Islaidos = sum(expenditure, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Islaidos_mln = Islaidos / 1e6)

print(islaidos_pagal_metus_tipa)

p5 <- ggplot(islaidos_pagal_metus_tipa,
             aes(x = factor(Metai), y = Islaidos_mln, fill = Medijos_grupe)) +
  geom_col(position = "dodge") +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(labels = label_number(accuracy = 0.1, big.mark = " ")) +
  labs(
    title = "Reklamos išlaidos pagal metus ir medijos tipa",
    x = "Metai",
    y = "Išlaidos, mln.",
    fill = "Medijos tipas"
  ) +
  bendra_tema

#  Kontaktai pagal metus ir medijos tipa
kontaktai_pagal_metus_tipa <- adexpo_data %>%
  filter(!is.na(Medijos_grupe), !is.na(Contacts_calc)) %>%
  group_by(Metai, Medijos_grupe) %>%
  summarise(
    Kontaktai = sum(Contacts_calc, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Kontaktai_mln = Kontaktai / 1e6)

print(kontaktai_pagal_metus_tipa)

p6 <- ggplot(kontaktai_pagal_metus_tipa,
             aes(x = factor(Metai), y = Kontaktai_mln, fill = Medijos_grupe)) +
  geom_col(position = "dodge") +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(labels = label_number(accuracy = 0.1, big.mark = " ")) +
  labs(
    title = "Kontaktai pagal metus ir medijos tipa",
    x = "Metai",
    y = "Kontaktai, mln.",
    fill = "Medijos tipas"
  ) +
  bendra_tema

#  Islaidu daliu pasiskirstymas pagal metus
islaidu_dalys_pagal_metus <- adexpo_data %>%
  filter(!is.na(Medijos_grupe)) %>%
  group_by(Metai, Medijos_grupe) %>%
  summarise(
    Islaidos = sum(expenditure, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(Metai) %>%
  mutate(Dalis = Islaidos / sum(Islaidos, na.rm = TRUE)) %>%
  ungroup()

p7 <- ggplot(islaidu_dalys_pagal_metus,
             aes(x = factor(Metai), y = Dalis, fill = Medijos_grupe)) +
  geom_col(position = "fill") +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(labels = percent) +
  labs(
    title = "Medijos tipu išlaidų dalys pagal metus",
    x = "Metai",
    y = "Dalis",
    fill = "Medijos tipas"
  ) +
  bendra_tema

#  Kontaktu daliu pasiskirstymas pagal metus
kontaktu_dalys_pagal_metus <- adexpo_data %>%
  filter(!is.na(Medijos_grupe), !is.na(Contacts_calc)) %>%
  group_by(Metai, Medijos_grupe) %>%
  summarise(
    Kontaktai = sum(Contacts_calc, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(Metai) %>%
  mutate(Dalis = Kontaktai / sum(Kontaktai, na.rm = TRUE)) %>%
  ungroup()

p8 <- ggplot(kontaktu_dalys_pagal_metus,
             aes(x = factor(Metai), y = Dalis, fill = Medijos_grupe)) +
  geom_col(position = "fill") +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(labels = percent) +
  labs(
    title = "Medijos tipu kontaktu dalys pagal metus",
    x = "Metai",
    y = "Dalis",
    fill = "Medijos tipas"
  ) +
  bendra_tema

(p4 | p5 | p6) / (p7 | p8 | plot_spacer()) +
  plot_annotation(
    title = "Rodikliai pagal metus ir medijos tipa",
    theme = theme(
      plot.title = element_text(hjust = 0.5, face = "bold")
    )
  )
menesiai_lt <- c("Sau", "Vas", "Kov", "Bal", "Geg", "Bir",
                 "Lie", "Rgp", "Rgs", "Spa", "Lap", "Grd")
#  Menesines islaidos pagal metus
menesines_islaidos <- adexpo_data %>%
  filter(!is.na(expenditure)) %>%
  group_by(Metai, Menuo) %>%
  summarise(
    Islaidos = sum(expenditure, na.rm = TRUE),
    .groups = "drop"
  )

print(menesines_islaidos)

p9 <- ggplot(menesines_islaidos,
             aes(x = Menuo, y = Islaidos, color = factor(Metai), group = Metai)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = 1:12, labels = menesiai_lt) +
  scale_y_continuous(labels = label_number(big.mark = " ")) +
  labs(
    title = "Mėnesinės reklamos išlaidos pagal metus",
    x = "Mėnuo",
    y = "Išlaidos",
    color = "Metai"
  ) +
  bendra_tema

#Menesiniai kontaktai pagal metus
menesiniai_kontaktai <- adexpo_data %>%
  filter(!is.na(Contacts_calc)) %>%
  group_by(Metai, Menuo) %>%
  summarise(
    Kontaktai = sum(Contacts_calc, na.rm = TRUE),
    .groups = "drop"
  )

print(menesiniai_kontaktai)

p10 <- ggplot(menesiniai_kontaktai,
              aes(x = Menuo, y = Kontaktai, color = factor(Metai), group = Metai)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = 1:12, labels = menesiai_lt) +
  scale_y_continuous(labels = label_number(big.mark = " ")) +
  labs(
    title = "Mėnesiniai kontaktai pagal metus",
    x = "Mėnuo",
    y = "Kontaktų skaičius",
    color = "Metai"
  ) +
  bendra_tema

(p9 | p10) +
  plot_annotation(
    title = "Mėnesiniai išlaidų rodikliai pagal metus",
    theme = theme(
      plot.title = element_text(hjust = 0.5, face = "bold")
    )
  )


menesiai_lt <- c("Sau", "Vas", "Kov", "Bal", "Geg", "Bir",
                 "Lie", "Rgp", "Rgs", "Spa", "Lap", "Grd")
#Mėnesinės išlaidos pagal metus ir medijos kanalus
menesines_islaidos_kanalai <- adexpo_data %>%
  filter(!is.na(expenditure), !is.na(Medijos_grupe)) %>%
  group_by(Metai, Menuo, Medijos_grupe) %>%
  summarise(
    Islaidos = sum(expenditure, na.rm = TRUE),
    .groups = "drop"
  )

menesines_islaidos_kanalai <- menesines_islaidos_kanalai %>%
  mutate(
    Medijos_grupe = ifelse(as.character(Medijos_grupe) == "Kita",
                           "Lauko reklama",
                           as.character(Medijos_grupe)),
    Medijos_grupe = factor(Medijos_grupe)
  )
print(menesines_islaidos_kanalai)

p11 <- ggplot(menesines_islaidos_kanalai,
              aes(x = Menuo, y = Islaidos, color = factor(Metai), group = Metai)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.8) +
  facet_wrap(~Medijos_grupe, scales = "free_y") +
  scale_x_continuous(breaks = 1:12, labels = menesiai_lt) +
  scale_y_continuous(labels = label_number(big.mark = " ")) +
  labs(
    title = "Mėnesinės reklamos išlaidos pagal metus ir medijos kanalus",
    x = "Mėnuo",
    y = "Išlaidos",
    color = "Metai"
  ) +
  bendra_tema

# Mėnesiniai kontaktai pagal metus ir medijos kanalus
menesiniai_kontaktai_kanalai <- adexpo_data %>%
  filter(!is.na(Contacts_calc), !is.na(Medijos_grupe)) %>%
  group_by(Metai, Menuo, Medijos_grupe) %>%
  summarise(
    Kontaktai = sum(Contacts_calc, na.rm = TRUE),
    .groups = "drop"
  )

menesiniai_kontaktai_kanalai <- menesiniai_kontaktai_kanalai %>%
  mutate(
    Medijos_grupe = ifelse(Medijos_grupe == "Kita", "Lauko reklama", as.character(Medijos_grupe))
  )


print(menesiniai_kontaktai_kanalai)

p12 <- ggplot(menesiniai_kontaktai_kanalai,
              aes(x = Menuo, y = Kontaktai, color = factor(Metai), group = Metai)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.8) +
  facet_wrap(~Medijos_grupe, scales = "free_y") +
  scale_x_continuous(breaks = 1:12, labels = menesiai_lt) +
  scale_y_continuous(labels = label_number(big.mark = " ")) +
  labs(
    title = "Mėnesiniai kontaktai pagal metus ir medijos kanalus",
    x = "Mėnuo",
    y = "Kontaktų skaičius",
    color = "Metai"
  ) +
  bendra_tema

p11 / p12 +
  plot_annotation(
    title = "Mėnesiniai rodikliai pagal metus ir medijos kanalus",
    theme = theme(
      plot.title = element_text(hjust = 0.5, face = "bold")
    )
  )



metine_islaidu_suvestine <- menesines_islaidos %>%
  filter(!(Metai == 2025 & Menuo > 6)) %>%
  group_by(Metai) %>%
  summarise(
    Stebetu_menesiu_sk = n(),
    Vidutines_menesio_islaidos = mean(Islaidos, na.rm = TRUE),
    Mediana_menesio_islaidu = median(Islaidos, na.rm = TRUE),
    Bendros_islaidos = sum(Islaidos, na.rm = TRUE),
    Min_menesio_islaidos = min(Islaidos, na.rm = TRUE),
    Max_menesio_islaidos = max(Islaidos, na.rm = TRUE),
    SD_menesio_islaidu = sd(Islaidos, na.rm = TRUE),
    Patikra = Bendros_islaidos / Stebetu_menesiu_sk,
    .groups = "drop"
  )

print(metine_islaidu_suvestine)

metine_islaidu_long <- metine_islaidu_suvestine %>%
  dplyr::select(Metai, Vidutines_menesio_islaidos, Mediana_menesio_islaidu) %>%
  pivot_longer(
    cols = c(Vidutines_menesio_islaidos, Mediana_menesio_islaidu),
    names_to = "Rodiklis",
    values_to = "Reiksme"
  ) %>%
  mutate(
    Rodiklis = recode(
      Rodiklis,
      Vidutines_menesio_islaidos = "Vidurkis",
      Mediana_menesio_islaidu = "Mediana"
    )
  )

p13 <- ggplot(metine_islaidu_long,
              aes(x = factor(Metai), y = Reiksme, color = Rodiklis, group = Rodiklis)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_y_continuous(labels = label_number(big.mark = " ")) +
  labs(
    title = "Vidutinės ir medianinės mėnesio reklamos išlaidos pagal metus",
    x = "Metai",
    y = "Išlaidos",
    color = "Rodiklis"
  ) +
  bendra_tema

# Pirmi 6 menesiai
menesines_islaidos_pirmi7 <- menesines_islaidos %>%
  filter(Menuo <= 6)

pirmu7_islaidu_suvestine <- menesines_islaidos_pirmi7 %>%
  group_by(Metai) %>%
  summarise(
    Stebetu_menesiu_sk = n(),
    Vidutines_menesio_islaidos = mean(Islaidos, na.rm = TRUE),
    Bendros_islaidos = sum(Islaidos, na.rm = TRUE),
    Patikra = Bendros_islaidos / Stebetu_menesiu_sk,
    .groups = "drop"
  )

print(pirmu7_islaidu_suvestine)

# Friedman testas islaidoms
islaidu_wide <- menesines_islaidos_pirmi7 %>%
  dplyr::select(Metai, Menuo, Islaidos) %>%
  pivot_wider(names_from = Metai, values_from = Islaidos) %>%
  arrange(Menuo)

print(islaidu_wide)

friedman_islaidos <- friedman.test(as.matrix(islaidu_wide[, -1]))
print(friedman_islaidos)




metine_kontaktu_suvestine <- menesiniai_kontaktai %>%
  filter(!(Metai == 2025 & Menuo > 6)) %>%
  group_by(Metai) %>%
  summarise(
    Stebetu_menesiu_sk = n(),
    Vidutinis_menesio_kontaktu_skaicius = mean(Kontaktai, na.rm = TRUE),
    Mediana_menesio_kontaktu = median(Kontaktai, na.rm = TRUE),
    Bendras_kontaktu_skaicius = sum(Kontaktai, na.rm = TRUE),
    Min_menesio_kontaktu = min(Kontaktai, na.rm = TRUE),
    Max_menesio_kontaktu = max(Kontaktai, na.rm = TRUE),
    SD_menesio_kontaktu = sd(Kontaktai, na.rm = TRUE),
    Patikra = Bendras_kontaktu_skaicius / Stebetu_menesiu_sk,
    .groups = "drop"
  )

print(metine_kontaktu_suvestine)

metine_kontaktu_long <- metine_kontaktu_suvestine %>%
  dplyr::select(Metai, Vidutinis_menesio_kontaktu_skaicius, Mediana_menesio_kontaktu) %>%
  pivot_longer(
    cols = c(Vidutinis_menesio_kontaktu_skaicius, Mediana_menesio_kontaktu),
    names_to = "Rodiklis",
    values_to = "Reiksme"
  ) %>%
  mutate(
    Rodiklis = recode(
      Rodiklis,
      Vidutinis_menesio_kontaktu_skaicius = "Vidurkis",
      Mediana_menesio_kontaktu = "Mediana"
    )
  )

p14 <- ggplot(metine_kontaktu_long,
              aes(x = factor(Metai), y = Reiksme, color = Rodiklis, group = Rodiklis)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_y_continuous(labels = label_number(big.mark = " ")) +
  labs(
    title = "Vidutinis ir medianinis mėnesio kontaktų skaičius pagal metus",
    x = "Metai",
    y = "Kontaktų skaičius",
    color = "Rodiklis"
  ) +
  bendra_tema

# Pirmi 7 menesiai
menesiniai_kontaktai_pirmi7 <- menesiniai_kontaktai %>%
  filter(Menuo <= 6)

pirmu7_kontaktu_suvestine <- menesiniai_kontaktai_pirmi7 %>%
  group_by(Metai) %>%
  summarise(
    Stebetu_menesiu_sk = n(),
    Vidutinis_menesio_kontaktu_skaicius = mean(Kontaktai, na.rm = TRUE),
    Bendras_kontaktu_skaicius = sum(Kontaktai, na.rm = TRUE),
    Patikra = Bendras_kontaktu_skaicius / Stebetu_menesiu_sk,
    .groups = "drop"
  )

print(pirmu7_kontaktu_suvestine)

# Friedman testas kontaktams
kontaktu_wide <- menesiniai_kontaktai_pirmi7 %>%
  dplyr::select(Metai, Menuo, Kontaktai) %>%
  pivot_wider(names_from = Metai, values_from = Kontaktai) %>%
  arrange(Menuo)

print(kontaktu_wide)

friedman_kontaktai <- friedman.test(as.matrix(kontaktu_wide[, -1]))
print(friedman_kontaktai)


# VIDUTINIAI METINIAI GRAFIKAI SUGRUPUOTI
(p13 | p14) +
  plot_annotation(title = "Vidutiniai menesiniai rodikliai pagal metus")

# ADEX (nuo 2022)

adex_monthly <- add_fact_menesinis %>%
  filter(YearMonth >= as.Date("2022-01-01"))

#  GOOGLE (nuo 2022)
google_monthly <- Google_menesinis %>%
  filter(YearMonth >= as.Date("2022-01-01"))
#  Bendras mėnesių intervalas


all_months_2022 <- seq(
  from = as.Date("2022-01-01"),
  to   = max(adex_monthly$YearMonth),
  by   = "month"
)

all_brands <- unique(adex_monthly$A_BRAND_E_CLEAN)

# Balanced ADEX panel


adex_panel_2022 <- expand_grid(
  YearMonth = all_months_2022,
  A_BRAND_E_CLEAN = all_brands
) %>%
  left_join(adex_monthly,
            by = c("YearMonth", "A_BRAND_E_CLEAN"))

# Balanced GOOGLE panel


google_panel_2022 <- expand_grid(
  YearMonth = all_months_2022,
  Brand = all_brands
) %>%
  left_join(google_monthly,
            by = c("YearMonth", "Brand"))
panel_full_2022 <- adex_panel_2022 %>%
  left_join(google_panel_2022,
            by = c("YearMonth",
                   "A_BRAND_E_CLEAN" = "Brand"))


panel_full_2022 <- panel_full_2022 %>%
  arrange(A_BRAND_E_CLEAN, YearMonth) %>%
  group_by(A_BRAND_E_CLEAN) %>%
  mutate(
    Spend_lag1 = lag(Spend, 1),
    Spend_lag2 = lag(Spend, 2),
    Spend_lag3 = lag(Spend, 3)
  ) %>%
  ungroup()
colnames(panel_full_2022)
panel_full_2022 <- panel_full_2022 %>%
  rename(GT = Searches)
panel_full_2022 <- panel_full_2022 %>%
  mutate(
    z1 = if_else(!is.na(Spend_lag1) & Spend_lag1 > 0,
                 GT / Spend_lag1,
                 NA_real_),
    z2 = if_else(!is.na(Spend_lag2) & Spend_lag2 > 0,
                 GT / Spend_lag2,
                 NA_real_),
    z3 = if_else(!is.na(Spend_lag3) & Spend_lag3 > 0,
                 GT / Spend_lag3,
                 NA_real_)
  )

summary(panel_full_2022$z1)
summary(panel_full_2022$z2)
summary(panel_full_2022$z3)

panel_full_2022 <- panel_full_2022 %>%
  mutate(
    z1 = ifelse(Spend_lag1 > 0, GT / Spend_lag1, NA_real_),
    z2 = ifelse(Spend_lag2 > 0, GT / Spend_lag2, NA_real_),
    z3 = ifelse(Spend_lag3 > 0, GT / Spend_lag3, NA_real_)
  )

summary(panel_full_2022$z1)
summary(panel_full_2022$z2)
summary(panel_full_2022$z3)

panel_full_2022 <- panel_full_2022 %>%
  mutate(
    google_status = ifelse(GT > 0,
                           "Yra paieškų",
                           "Nėra paieškų")
  )

ggplot(panel_full_2022,
       aes(x = YearMonth,
           y = A_BRAND_E_CLEAN,
           fill = google_status)) +
  geom_tile(color = "grey90") +
  scale_fill_manual(values = c("Yra paieškų" = "lightgreen",
                               "Nėra paieškų" = "lightblue"),
                    name = "Google aktyvumas") +
  labs(title = "Prekinių ženklų aktyvumas pagal mėnesius Google (nuo 2022)",
       x = "Mėnuo",
       y = "Prekinis ženklas") +
  theme_minimal()
google_praleistu_analize <- panel_full_2022 %>%
  group_by(A_BRAND_E_CLEAN) %>%
  summarise(
    missing_months = sum(GT == 0),
    total_months = n(),
    missing_pct = missing_months / total_months
  ) %>%
  arrange(desc(missing_pct))

print(google_praleistu_analize)

panel_full_2022 <- panel_full_2022 %>%
  mutate(
    reklama_status = ifelse(Spend > 0,
                            "Reklamuojasi",
                            "Nesireklamuoja")
  )

panel_full_2022 <- panel_full_2022 %>%
  mutate(
    reklama_status = ifelse(is.na(reklama_status), "Nesireklamuoja", reklama_status)
  )

ggplot(panel_full_2022,
       aes(x = YearMonth,
           y = A_BRAND_E_CLEAN,
           fill = reklama_status)) +
  geom_tile(color = "grey90") +
  scale_fill_manual(
    values = c(
      "Reklamuojasi" = "lightgreen",
      "Nesireklamuoja" = "lightblue"
    ),
    name = "Reklamos būsena"
  ) +
  labs(
    title = "Prekinių ženklų aktyvumas pagal mėnesius Adex (nuo 2022)",
    x = "Mėnuo",
    y = "Prekinis ženklas"
  ) +
  theme_minimal()+
  theme(
    plot.title = element_text(hjust = 0.5)
  )

panel_full_2022 <- panel_full_2022 %>%
  filter(!A_BRAND_E_CLEAN %in% c("LEMORA", "CELSIS"))
unique(panel_full_2022$A_BRAND_E_CLEAN)

#Descriptive dalis

# Bendras vaizdas: mėnesinės išlaidos
ggplot(panel_full_2022,
       aes(x = YearMonth, y = Spend)) +
  geom_line() +
  facet_wrap(~A_BRAND_E_CLEAN, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Mėnesinių išlaidų grafikų panelė pagal prekinius ženklus",
    x = "Metai",
    y = "Išlaidos"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5)
  )


# Mėnesiniai kontaktai
ggplot(panel_full_2022,
       aes(x = YearMonth, y = Contacts)) +
  geom_line(color = "darkred") +
  facet_wrap(~A_BRAND_E_CLEAN, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Mėnesiniai kontaktai",
    x = "Metai",
    y = "Kontaktų kiekis"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5)
  )


# Mėnesinės Google paieškos
ggplot(panel_full_2022,
       aes(x = YearMonth, y = GT)) +
  geom_line(color = "darkred") +
  facet_wrap(~A_BRAND_E_CLEAN, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Mėnesinės Google paieškos",
    x = "Metai",
    y = "GT paieškų kiekis"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5)
  )
panel_clean <- panel_full_2022 %>%
  filter(YearMonth <= as.Date("2025-06-01"))

# BENDROS MĖNESINĖS EILUTĖS


bendras_season <- panel_clean %>%
  group_by(YearMonth) %>%
  summarise(
    Spend = sum(Spend, na.rm = TRUE),
    GT = sum(GT, na.rm = TRUE),
    Contacts = sum(Contacts, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(YearMonth) %>%
  mutate(
    log_Spend = log1p(Spend),
    log_GT = log1p(GT),
    log_Contacts = log1p(Contacts)
  )

start_year  <- year(min(bendras_season$YearMonth))
start_month <- month(min(bendras_season$YearMonth))

ts_spend <- ts(bendras_season$Spend, start = c(start_year, start_month), frequency = 12)
ts_gt <- ts(bendras_season$GT, start = c(start_year, start_month), frequency = 12)
ts_contacts <- ts(bendras_season$Contacts, start = c(start_year, start_month), frequency = 12)

ts_log_spend <- ts(bendras_season$log_Spend, start = c(start_year, start_month), frequency = 12)
ts_log_gt <- ts(bendras_season$log_GT, start = c(start_year, start_month), frequency = 12)
ts_log_contacts <- ts(bendras_season$log_Contacts, start = c(start_year, start_month), frequency = 12)

# Bendros mėnesinės sumos per visus brandus


menesiai_lt <- c("Sau", "Vas", "Kov", "Bal", "Geg", "Bir",
                 "Lie", "Rgp", "Rgs", "Spa", "Lap", "Grd")

# Bendros mėnesinės sumos per visus brandus, be 2025 m. liepos
bendras_season <- panel_clean %>%
  mutate(
    Metai = year(YearMonth),
    Menuo = month(YearMonth)
  ) %>%
  filter(!(Metai == 2025 & Menuo == 7)) %>%
  group_by(YearMonth, Metai, Menuo) %>%
  summarise(
    Spend = sum(Spend, na.rm = TRUE),
    Contacts = sum(Contacts, na.rm = TRUE),
    GT = sum(GT, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(YearMonth)

print(bendras_season)

# Spend sezoniškumas
p_spend <- ggplot(bendras_season,
                  aes(x = Menuo, y = Spend, color = factor(Metai), group = Metai)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = 1:12, labels = menesiai_lt) +
  scale_y_continuous(labels = label_number(big.mark = " ")) +
  labs(
    title = "Išlaidos",
    x = "Mėnuo",
    y = "Išlaidos",
    color = "Metai"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

# Contacts sezoniškumas
p_contacts <- ggplot(bendras_season,
                     aes(x = Menuo, y = Contacts, color = factor(Metai), group = Metai)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = 1:12, labels = menesiai_lt) +
  scale_y_continuous(labels = label_number(big.mark = " ")) +
  labs(
    title = "Kontaktai",
    x = "Mėnuo",
    y = "Kontaktai",
    color = "Metai"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

#  GT sezoniškumas
p_gt <- ggplot(bendras_season,
               aes(x = Menuo, y = GT, color = factor(Metai), group = Metai)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = 1:12, labels = menesiai_lt) +
  scale_y_continuous(labels = label_number(big.mark = " ")) +
  labs(
    title = "Google Trends",
    x = "Mėnuo",
    y = "GT",
    color = "Metai"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

# Visi grafikai kartu
(p_spend | p_contacts | p_gt) +
  plot_annotation(
    title = "Bendras sezoniškumas pagal mėnesius prieš log transformaciją",
    theme = theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
    )
  ) &
  theme(legend.position = "bottom")

# Pasiskirstymo analizė

length(panel_clean$Spend)

# Histograma: Spend (tūkst. €)
hist(panel_clean$Spend / 1000,
     col = "lightblue",
     border = "white",
     main = "",
     xlab = "Mėnesinės reklamos išlaidos, tūkst. €",
     ylab = "Dažnis",
     breaks = 25,
     ylim = c(0, 80))
title(main = "Mėnesinių reklamos išlaidų pasiskirstymas", adj = 0.5)

# Histograma: Contacts (tūkst.)
hist(panel_clean$Contacts / 1000,
     col = "lightblue",
     border = "white",
     main = "",
     xlab = "Mėnesiniai kontaktai, tūkst.",
     ylab = "Dažnis",
     breaks = 25,
     ylim = c(0, 80))

title(main = "Mėnesinių kontaktų pasiskirstymas", adj = 0.5)


# Histograma: GT (tūkst.)
hist(panel_clean$GT / 1000,
     col = "lightblue",
     border = "white",
     main = "",
     xlab = "Mėnesinės Google paieškos, tūkst.",
     ylab = "Dažnis",
     breaks = 25,
     ylim = c(0, 80))
title(main = "Mėnesinių Google paieškų pasiskirstymas", adj = 0.5)

par(mfrow = c(1, 3), mar = c(4, 4, 3, 1), oma = c(0, 0, 2, 0))

# Histograma: Spend
hist(panel_clean$Spend / 1000,
     col = "lightblue",
     border = "white",
     main = "Reklamos išlaidos",
     xlab = "Mėnesinės reklamos išlaidos, tūkst. €",
     ylab = "Dažnis",
     breaks = 25,
     ylim = c(0, 80))

# Histograma: Contacts
hist(panel_clean$Contacts / 1000,
     col = "lightblue",
     border = "white",
     main = "Kontaktai",
     xlab = "Mėnesiniai kontaktai, tūkst.",
     ylab = "Dažnis",
     breaks = 25,
     ylim = c(0, 80))

# Histograma: GT
hist(panel_clean$GT,
     col = "lightblue",
     border = "white",
     main = "Google paieškos",
     xlab = "Mėnesinės Google paieškos",
     ylab = "Dažnis",
     breaks = 25,
     ylim = c(0, 80),
     xaxt = "n")

axis(
  side = 1,
  at = pretty(panel_clean$GT, n = 6),
  labels = pretty(panel_clean$GT, n = 6)
)

mtext("Kintamųjų pasiskirstymo histogramos", outer = TRUE, cex = 1.3)

# Grąžinam į standartinį vaizdą
par(mfrow = c(1, 1))

# QQ grafikas
qqnorm(panel_clean$Spend,
       main = "QQ grafikas: mėnesinės reklamos išlaidos",
       xlab = "Teoriniai kvantiliai",
       ylab = "Stebėti kvantiliai",
       pch = 19,
       col = "darkblue")

qqline(panel_clean$Spend,
       col = "red",
       lwd = 2)


qqnorm(panel_clean$Contacts,
       main = "QQ grafikas: mėnesinės reklamos išlaidos",
       xlab = "Teoriniai kvantiliai",
       ylab = "Stebėti kvantiliai",
       pch = 19,
       col = "darkblue")

qqline(panel_clean$Contacts,
       col = "red",
       lwd = 2)

qqnorm(panel_clean$GT,
       main = "QQ grafikas: mėnesinės reklamos išlaidos",
       xlab = "Teoriniai kvantiliai",
       ylab = "Stebėti kvantiliai",
       pch = 19,
       col = "darkblue")

qqline(panel_clean$GT,
       col = "red",
       lwd = 2)


# Shapiro–Wilk normalumo testas
shapiro_spend <- shapiro.test(panel_clean$Spend)
shapiro_spend

boxplot(Spend ~ A_BRAND_E_CLEAN,
        data = panel_clean,
        col = "lightblue",
        main = "Mėnesinių reklamos išlaidų pasiskirstymas pagal prekinius ženklus",
        xlab = "Prekinis ženklas",
        ylab = "Mėnesinės reklamos išlaidos")


# Shapiro–Wilk normalumo testas spendam

shapiro_spend <- shapiro.test(panel_clean$Spend)
shapiro_spend

boxplot(Spend / 1000 ~ A_BRAND_E_CLEAN,
        data = panel_clean,
        col = "lightblue",
        main = "Mėnesinių reklamos išlaidų pasiskirstymas pagal prekinius ženklus",
        xlab = "Prekinis ženklas",
        ylab = "Mėnesinės reklamos išlaidos, tūkst. €")

# H0: duomenys yra normaliai pasiskirstę
# H1: duomenys nėra normaliai pasiskirstę

# Shapiro–Wilk normalumo testas kontaktam

shapiro_contacts <- shapiro.test(panel_clean$Contacts)
shapiro_contacts

boxplot(Contacts / 1000 ~ A_BRAND_E_CLEAN,
        data = panel_clean,
        col = "lightblue",
        main = "Mėnesinių kontaktų pasiskirstymas pagal prekinius ženklus",
        xlab = "Prekinis ženklas",
        ylab = "Mėnesiniai kontaktai, tūkst.")

# H0: duomenys yra normaliai pasiskirstę
# H1: duomenys nėra normaliai pasiskirstę

shapiro_gt <- shapiro.test(panel_clean$GT)
shapiro_gt

boxplot(GT / 1000 ~ A_BRAND_E_CLEAN,
        data = panel_clean,
        col = "lightblue",
        main = "Mėnesinių Google paieškų pasiskirstymas pagal prekinius ženklus",
        xlab = "Prekinis ženklas",
        ylab = "Mėnesinės Google paieškos, tūkst.")

# H0: duomenys yra normaliai pasiskirstę
# H1: duomenys nėra normaliai pasiskirstę



par(mfrow = c(2, 3), mar = c(4, 4, 3, 1), oma = c(0, 0, 3, 0))

qqnorm(panel_clean$Spend,
       main = "QQ grafikas: išlaidos",
       xlab = "Teoriniai kvantiliai",
       ylab = "Stebėti kvantiliai",
       pch = 19,
       col = "darkblue")
qqline(panel_clean$Spend, col = "red", lwd = 2)

qqnorm(panel_clean$Contacts,
       main = "QQ grafikas: kontaktai",
       xlab = "Teoriniai kvantiliai",
       ylab = "Stebėti kvantiliai",
       pch = 19,
       col = "darkblue")
qqline(panel_clean$Contacts, col = "red", lwd = 2)

qqnorm(panel_clean$GT,
       main = "QQ grafikas: Google Trends",
       xlab = "Teoriniai kvantiliai",
       ylab = "Stebėti kvantiliai",
       pch = 19,
       col = "darkblue")
qqline(panel_clean$GT, col = "red", lwd = 2)

boxplot(Spend / 1000 ~ A_BRAND_E_CLEAN,
        data = panel_clean,
        col = "lightblue",
        main = "Išlaidos pagal prekės ženklą",
        xlab = "Prekės ženklas",
        ylab = "Išlaidos, tūkst. €")

boxplot(Contacts / 1000 ~ A_BRAND_E_CLEAN,
        data = panel_clean,
        col = "lightgreen",
        main = "Kontaktai pagal prekės ženklą",
        xlab = "Prekės ženklas",
        ylab = "Kontaktai, tūkst.")

boxplot(GT ~ A_BRAND_E_CLEAN,
        data = panel_clean,
        col = "lightpink",
        main = "Google Trends pagal prekės ženklą",
        xlab = "Prekės ženklas",
        ylab = "GT reikšmė")

mtext("Normalumo analizė: QQ grafikai ir stačiakampės diagramos", outer = TRUE, cex = 1.4)

par(mfrow = c(1, 1))


normalumas_spend_brand <- panel_clean %>%
  group_by(A_BRAND_E_CLEAN) %>%
  summarise(
    n = sum(!is.na(Spend)),
    shapiro_W = ifelse(n >= 3, shapiro.test(Spend)$statistic, NA),
    p_value = ifelse(n >= 3, shapiro.test(Spend)$p.value, NA),
    .groups = "drop"
  )

normalumas_spend_brand


normalumas_gt_brand <- panel_clean %>%
  group_by(A_BRAND_E_CLEAN) %>%
  summarise(
    n = sum(!is.na(GT)),
    shapiro_W = ifelse(n >= 3, shapiro.test(GT)$statistic, NA),
    p_value = ifelse(n >= 3, shapiro.test(GT)$p.value, NA),
    .groups = "drop"
  )

normalumas_gt_brand


normalumas_Con_brand <- panel_clean %>%
  group_by(A_BRAND_E_CLEAN) %>%
  summarise(
    n = sum(!is.na(Contacts)),
    shapiro_W = ifelse(n >= 3, shapiro.test(Contacts)$statistic, NA),
    p_value = ifelse(n >= 3, shapiro.test(Contacts)$p.value, NA),
    .groups = "drop"
  )

normalumas_Con_brand

normalumo_bendra_be_log <- panel_clean %>%
  dplyr::select(A_BRAND_E_CLEAN, Spend, GT, Contacts) %>%
  pivot_longer(
    cols = -A_BRAND_E_CLEAN,
    names_to = "Kintamasis",
    values_to = "Reiksme"
  ) %>%
  group_by(A_BRAND_E_CLEAN, Kintamasis) %>%
  summarise(
    n = sum(!is.na(Reiksme)),
    shapiro_W = ifelse(n >= 3, shapiro.test(Reiksme[!is.na(Reiksme)])$statistic, NA_real_),
    p_value   = ifelse(n >= 3, shapiro.test(Reiksme[!is.na(Reiksme)])$p.value, NA_real_),
    .groups = "drop"
  ) %>%
  mutate(
    isvada = ifelse(
      p_value < 0.05,
      "Atmetame H0: nenormalus",
      "Neatmetame H0: normalumas galimas"
    )
  ) %>%
  arrange(A_BRAND_E_CLEAN, Kintamasis)

normalumo_bendra_be_log

# Log-transformacija

panel_clean <- panel_clean %>%
  mutate(
    log_Spend = log(Spend + 1),
    log_GT = log(GT + 1),
    log_Contacts = log(Contacts + 1)
  )

# Histogram
hist(panel_clean$log_Spend,
     col = "lightgreen",
     border = "white",
     main = "",
     xlab = "log(Spend + 1)",
     ylab = "Dažnis",
     breaks = 25)
title(main = "Log-transformuotų reklamos išlaidų pasiskirstymas", adj = 0.5)

hist(panel_clean$log_Contacts,
     col = "lightgreen",
     border = "white",
     main = "",
     xlab = "log(Contacts + 1)",
     ylab = "Dažnis",
     breaks = 25)
title(main = "Log-transformuotų kontaktų pasiskirstymas", adj = 0.5)

hist(panel_clean$log_GT,
     col = "lightgreen",
     border = "white",
     main = "",
     xlab = "log(GT + 1)",
     ylab = "Dažnis",
     breaks = 25)
title(main = "Log-transformuotų Google paieškų pasiskirstymas", adj = 0.5)


par(mfrow = c(1, 3), mar = c(5, 4, 3, 1), oma = c(0, 0, 2, 0))

# Histograma: log_Spend
hist(panel_clean$log_Spend,
     col = "lightgreen",
     border = "white",
     main = "Log-transformuotos išlaidos",
     xlab = "log(Išlaidos + 1)",
     ylab = "Dažnis",
     breaks = 25,
     xaxt = "n")

spend_ticks <- pretty(panel_clean$log_Spend, n = 6)
axis(1, at = spend_ticks, labels = round(spend_ticks, 2), cex.axis = 0.9)

# Histograma: log_Contacts
hist(panel_clean$log_Contacts,
     col = "lightgreen",
     border = "white",
     main = "Log-transformuoti kontaktai",
     xlab = "log(Kontaktai + 1)",
     ylab = "Dažnis",
     breaks = 25,
     xaxt = "n")

contacts_ticks <- pretty(panel_clean$log_Contacts, n = 6)
axis(1, at = contacts_ticks, labels = round(contacts_ticks, 2), cex.axis = 0.9)

# Histograma: log_GT
hist(panel_clean$log_GT,
     col = "lightgreen",
     border = "white",
     main = "Log-transformuotos Google paieškos",
     xlab = "log(Google paieškos + 1)",
     ylab = "Dažnis",
     breaks = 25,
     xaxt = "n")

gt_ticks <- pretty(panel_clean$log_GT, n = 6)
axis(1, at = gt_ticks, labels = round(gt_ticks, 2), cex.axis = 0.9)

mtext("Log-transformuotų kintamųjų pasiskirstymo histogramos", 
      outer = TRUE, cex = 1.3)

par(mfrow = c(1, 1))

# QQ plot
qqnorm(panel_clean$log_Spend,
       main = "",
       xlab = "Teoriniai kvantiliai",
       ylab = "Stebėti kvantiliai",
       pch = 19,
       col = "darkblue")
qqline(panel_clean$log_Spend, col = "red", lwd = 2)
title(main = "QQ grafikas: log-transformuotos reklamos išlaidos", adj = 0.5)

qqnorm(panel_clean$log_Contacts,
       main = "",
       xlab = "Teoriniai kvantiliai",
       ylab = "Stebėti kvantiliai",
       pch = 19,
       col = "darkblue")
qqline(panel_clean$log_Contacts, col = "red", lwd = 2)
title(main = "QQ grafikas: log-transformuoti kontaktai", adj = 0.5)

qqnorm(panel_clean$log_GT,
       main = "",
       xlab = "Teoriniai kvantiliai",
       ylab = "Stebėti kvantiliai",
       pch = 19,
       col = "darkblue")
qqline(panel_clean$log_GT, col = "red", lwd = 2)
title(main = "QQ grafikas: log-transformuotos Google paieškos", adj = 0.5)

par(mfrow = c(1, 3), mar = c(4, 4, 3, 1), oma = c(0, 0, 2, 0))

qqnorm(panel_clean$log_Spend,
       main = "Log-transformuotos išlaidos",
       xlab = "Teoriniai kvantiliai",
       ylab = "Stebėti kvantiliai",
       pch = 19,
       col = "darkblue")
qqline(panel_clean$log_Spend, col = "red", lwd = 2)

qqnorm(panel_clean$log_Contacts,
       main = "Log-transformuoti kontaktai",
       xlab = "Teoriniai kvantiliai",
       ylab = "Stebėti kvantiliai",
       pch = 19,
       col = "darkblue")
qqline(panel_clean$log_Contacts, col = "red", lwd = 2)

qqnorm(panel_clean$log_GT,
       main = "Log-transformuotos Google paieškos",
       xlab = "Teoriniai kvantiliai",
       ylab = "Stebėti kvantiliai",
       pch = 19,
       col = "darkblue")
qqline(panel_clean$log_GT, col = "red", lwd = 2)

mtext("Log-transformuotų kintamųjų QQ grafikai", outer = TRUE, cex = 1.3)

par(mfrow = c(1, 1))

#h0 duomenys yra normaliai pasiskirstę
#h1 duomenys nėra normaliai pasiskirstę

shapiro_log_spend <- shapiro.test(panel_clean$log_Spend)
shapiro_log_spend

shapiro_log_contacts <- shapiro.test(panel_clean$log_Contacts)
shapiro_log_contacts

shapiro_log_gt <- shapiro.test(panel_clean$log_GT)
shapiro_log_gt

# Boxplotai pagal brandą

boxplot(log_Spend ~ A_BRAND_E_CLEAN,
        data = panel_clean,
        col = "lightgreen",
        main = "Log-transformuotų reklamos išlaidų pasiskirstymas pagal prekinius ženklus",
        xlab = "Prekinis ženklas",
        ylab = "log(Spend + 1)")

boxplot(log_Contacts ~ A_BRAND_E_CLEAN,
        data = panel_clean,
        col = "lightgreen",
        main = "Log-transformuotų kontaktų pasiskirstymas pagal prekinius ženklus",
        xlab = "Prekinis ženklas",
        ylab = "log(Contacts + 1)")

boxplot(log_GT ~ A_BRAND_E_CLEAN,
        data = panel_clean,
        col = "lightgreen",
        main = "Log-transformuotų Google paieškų pasiskirstymas pagal prekinius ženklus",
        xlab = "Prekinis ženklas",
        ylab = "log(GT + 1)")



par(mfrow = c(1, 3), mar = c(6, 4, 3, 1), oma = c(0, 0, 2, 0))

boxplot(log_Spend ~ A_BRAND_E_CLEAN,
        data = panel_clean,
        col = "lightgreen",
        main = "Log-transformuotos išlaidos",
        xlab = "Prekės ženklas",
        ylab = "log(Išlaidos + 1)",
        las = 2)

boxplot(log_Contacts ~ A_BRAND_E_CLEAN,
        data = panel_clean,
        col = "lightgreen",
        main = "Log-transformuoti kontaktai",
        xlab = "Prekės ženklas",
        ylab = "log(Kontaktai + 1)",
        las = 2)

boxplot(log_GT ~ A_BRAND_E_CLEAN,
        data = panel_clean,
        col = "lightgreen",
        main = "Log-transformuotos Google paieškos",
        xlab = "Prekės ženklas",
        ylab = "log(GT + 1)",
        las = 2)

mtext("Log-transformuotų kintamųjų sklaidos dėžutės pagal prekės ženklą",
      outer = TRUE, cex = 1.3)

par(mfrow = c(1, 1))

#boxplot
y_min <- min(c(panel_clean$log_Spend,
               panel_clean$log_Contacts,
               panel_clean$log_GT), na.rm = TRUE)

y_max <- max(c(panel_clean$log_Spend,
               panel_clean$log_Contacts,
               panel_clean$log_GT), na.rm = TRUE)

y_min <- floor(y_min)
y_max <- ceiling(y_max)

par(mfrow = c(2, 3), mar = c(6, 4, 3, 1), oma = c(0, 0, 3, 0))

# -------------------------
# QQ grafikas: log_Spend
# -------------------------
qqnorm(panel_clean$log_Spend,
       main = "QQ grafikas: log išlaidos",
       xlab = "Teoriniai kvantiliai",
       ylab = "Stebėti kvantiliai",
       pch = 19,
       col = "darkblue")
qqline(panel_clean$log_Spend, col = "red", lwd = 2)

# -------------------------
# QQ grafikas: log_Contacts
# -------------------------
qqnorm(panel_clean$log_Contacts,
       main = "QQ grafikas: log kontaktai",
       xlab = "Teoriniai kvantiliai",
       ylab = "Stebėti kvantiliai",
       pch = 19,
       col = "darkblue")
qqline(panel_clean$log_Contacts, col = "red", lwd = 2)

# -------------------------
# QQ grafikas: log_GT
# -------------------------
qqnorm(panel_clean$log_GT,
       main = "QQ grafikas: log Google Trends",
       xlab = "Teoriniai kvantiliai",
       ylab = "Stebėti kvantiliai",
       pch = 19,
       col = "darkblue")
qqline(panel_clean$log_GT, col = "red", lwd = 2)

# -------------------------
# Boxplot: log_Spend
# -------------------------
boxplot(log_Spend ~ A_BRAND_E_CLEAN,
        data = panel_clean,
        col = "lightgreen",
        main = "Log išlaidos pagal prekės ženklą",
        xlab = "Prekės ženklas",
        ylab = "log(Išlaidos + 1)",
        las = 2,
        ylim = c(y_min, y_max))

# -------------------------
# Boxplot: log_Contacts
# -------------------------
boxplot(log_Contacts ~ A_BRAND_E_CLEAN,
        data = panel_clean,
        col = "lightgreen",
        main = "Log kontaktai pagal prekės ženklą",
        xlab = "Prekės ženklas",
        ylab = "log(Kontaktai + 1)",
        las = 2,
        ylim = c(y_min, y_max))

# -------------------------
# Boxplot: log_GT
# -------------------------
boxplot(log_GT ~ A_BRAND_E_CLEAN,
        data = panel_clean,
        col = "lightgreen",
        main = "Log Google Trends pagal prekės ženklą",
        xlab = "Prekės ženklas",
        ylab = "log(GT + 1)",
        las = 2,
        ylim = c(y_min, y_max))

mtext("Log-transformuotų kintamųjų normalumo analizė: QQ grafikai ir stačiakampės diagramos",
      outer = TRUE, cex = 1.3)

par(mfrow = c(1, 1))

par(mfrow = c(2, 3), mar = c(4, 4, 3, 1), oma = c(0, 0, 3, 0))

qqnorm(panel_clean$log_Spend,
       main = "QQ grafikas: log išlaidos",
       xlab = "Teoriniai kvantiliai",
       ylab = "Stebėti kvantiliai",
       pch = 19,
       col = "darkblue")
qqline(panel_clean$log_Spend, col = "red", lwd = 2)

qqnorm(panel_clean$log_Contacts,
       main = "QQ grafikas: log kontaktai",
       xlab = "Teoriniai kvantiliai",
       ylab = "Stebėti kvantiliai",
       pch = 19,
       col = "darkblue")
qqline(panel_clean$log_Contacts, col = "red", lwd = 2)

qqnorm(panel_clean$log_GT,
       main = "QQ grafikas: log Google Trends",
       xlab = "Teoriniai kvantiliai",
       ylab = "Stebėti kvantiliai",
       pch = 19,
       col = "darkblue")
qqline(panel_clean$log_GT, col = "red", lwd = 2)

boxplot(log_Spend ~ A_BRAND_E_CLEAN,
        data = panel_clean,
        col = "lightblue",
        main = "Log išlaidos pagal prekės ženklą",
        xlab = "Prekės ženklas",
        ylab = "log(Išlaidos + 1)")

boxplot(log_Contacts ~ A_BRAND_E_CLEAN,
        data = panel_clean,
        col = "lightgreen",
        main = "Log kontaktai pagal prekės ženklą",
        xlab = "Prekės ženklas",
        ylab = "log(Kontaktai + 1)")

boxplot(log_GT ~ A_BRAND_E_CLEAN,
        data = panel_clean,
        col = "lightpink",
        main = "Log Google Trends pagal prekės ženklą",
        xlab = "Prekės ženklas",
        ylab = "log(GT + 1)")

mtext("Log-transformuotų kintamųjų normalumo analizė: QQ grafikai ir stačiakampės diagramos",
      outer = TRUE, cex = 1.4)

par(mfrow = c(1, 1))


#shapiro wilk
normalumas_log_spend_brand <- panel_clean %>%
  group_by(A_BRAND_E_CLEAN) %>%
  summarise(
    n = sum(!is.na(log_Spend)),
    shapiro_W = ifelse(n >= 3, shapiro.test(log_Spend)$statistic, NA),
    p_value = ifelse(n >= 3, shapiro.test(log_Spend)$p.value, NA),
    .groups = "drop"
  )

normalumas_log_spend_brand


# SHAPIRO PAGAL BRANDĄ: log_Contacts

normalumas_log_contacts_brand <- panel_clean %>%
  group_by(A_BRAND_E_CLEAN) %>%
  summarise(
    n = sum(!is.na(log_Contacts)),
    shapiro_W = ifelse(n >= 3, shapiro.test(log_Contacts)$statistic, NA),
    p_value = ifelse(n >= 3, shapiro.test(log_Contacts)$p.value, NA),
    .groups = "drop"
  )

normalumas_log_contacts_brand


# SHAPIRO PAGAL BRANDĄ: log_GT

normalumas_log_gt_brand <- panel_clean %>%
  group_by(A_BRAND_E_CLEAN) %>%
  summarise(
    n = sum(!is.na(log_GT)),
    shapiro_W = ifelse(n >= 3, shapiro.test(log_GT)$statistic, NA),
    p_value = ifelse(n >= 3, shapiro.test(log_GT)$p.value, NA),
    .groups = "drop"
  )

normalumas_log_gt_brand


normalumo_rezultatai_log <- panel_clean %>%
  dplyr::select(A_BRAND_E_CLEAN, log_Spend, log_Contacts, log_GT) %>%
  pivot_longer(
    cols = -A_BRAND_E_CLEAN,
    names_to = "Kintamasis",
    values_to = "Reiksme"
  ) %>%
  group_by(A_BRAND_E_CLEAN, Kintamasis) %>%
  summarise(
    n = sum(!is.na(Reiksme)),
    shapiro_W = ifelse(n >= 3, shapiro.test(Reiksme)$statistic, NA),
    p_value = ifelse(n >= 3, shapiro.test(Reiksme)$p.value, NA),
    .groups = "drop"
  ) %>%
  mutate(
    isvada = ifelse(p_value < 0.05,
                    "Atmetame H0: nenormalus",
                    "Neatmetame H0: normalumas galimas")
  )

normalumo_rezultatai_log

# H0: duomenys yra normaliai pasiskirstę
# H1: duomenys nėra normaliai pasiskirstę

#ox cox

lambda_spend <- BoxCox.lambda(na.omit(panel_clean$Spend + 1))
lambda_contacts <- BoxCox.lambda(na.omit(panel_clean$Contacts + 1))
lambda_gt <- BoxCox.lambda(na.omit(panel_clean$GT + 1))

lambda_spend
lambda_contacts
lambda_gt

#logotituomo
ggplot(panel_clean,
       aes(x = YearMonth, y = log_Spend)) +
  geom_line(color = "darkblue") +
  facet_wrap(~A_BRAND_E_CLEAN, scales = "free_y") +
  theme_minimal() +
  labs(title = "Log-transformuotos mėnesinės reklamos išlaidos",
       x = "Metai",
       y = "log(Spend + 1)")
ggplot(panel_clean,
       aes(x = YearMonth, y = log_GT)) +
  geom_line(color = "darkred") +
  facet_wrap(~A_BRAND_E_CLEAN, scales = "free_y") +
  theme_minimal() +
  labs(title = "Log-transformuotos mėnesinės Google paieškos",
       x = "Metai",
       y = "log(GT + 1)")

panel_clean <- panel_full_2022 %>%
  mutate(
    log_Spend = log(Spend + 1),
    log_GT = log(GT + 1)
  )

panel_long_log <- panel_clean %>%
  dplyr::select(YearMonth, A_BRAND_E_CLEAN, log_Spend, log_GT) %>%
  pivot_longer(
    cols = c(log_Spend, log_GT),
    names_to = "Variable",
    values_to = "Value"
  ) %>%
  mutate(
    Variable = recode(
      Variable,
      log_Spend = "Adexspot reklamos išlaidos (log)",
      log_GT = "Google paieškos (log)"
    )
  )

ggplot(panel_long_log,
       aes(x = YearMonth, y = Value, color = Variable, group = Variable)) +
  geom_line(linewidth = 0.9) +
  facet_wrap(~A_BRAND_E_CLEAN, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Log-transformuotų reklamos išlaidų ir Google paieškų dinamika",
    x = "Metai",
    y = "Log reikšmė",
    color = "Kintamasis"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5)
  )
panel_long_log <- panel_clean %>%
  dplyr::select(YearMonth, A_BRAND_E_CLEAN, log_Spend, log_GT) %>%
  pivot_longer(
    cols = c(log_Spend, log_GT),
    names_to = "Variable",
    values_to = "Value"
  ) %>%
  mutate(
    Variable = recode(Variable,
                      log_Spend = "Adexspot reklamos išlaidos (log)",
                      log_GT = "Google paieškos (log)")
  )

ggplot(panel_long_log,
       aes(x = YearMonth, y = Value, color = Variable)) +
  geom_line(linewidth = 0.9) +
  facet_wrap(~A_BRAND_E_CLEAN, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Log-transformuotų reklamos išlaidų (Adexspot) ir Google paieškų dinamika",
    x = "Metai",
    y = "Log reikšmė",
    color = "Kintamasis"
  ) +
  theme(plot.title = element_text(hjust = 0.5))

panel_long_log <- panel_clean %>%
  filter(month(YearMonth) <= 6) %>%
  dplyr::select(YearMonth, A_BRAND_E_CLEAN, log_Spend, log_GT) %>%
  pivot_longer(
    cols = c(log_Spend, log_GT),
    names_to = "Variable",
    values_to = "Value"
  ) %>%
  mutate(
    Variable = recode(
      Variable,
      log_Spend = "Adexspot reklamos išlaidos (log)",
      log_GT = "Google paieškos (log)"
    )
  )

ggplot(panel_long_log,
       aes(x = YearMonth, y = Value, color = Variable)) +
  geom_line(linewidth = 0.9) +
  facet_wrap(~A_BRAND_E_CLEAN, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Log-transformuotų reklamos išlaidų ir Google paieškų dinamika iki birželio",
    x = "Laikotarpis",
    y = "Log reikšmė",
    color = "Kintamasis"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

panel_long_raw <- panel_clean %>%
  filter(month(YearMonth) <= 6) %>%
  dplyr::select(YearMonth, A_BRAND_E_CLEAN, Spend, GT) %>%
  pivot_longer(
    cols = c(Spend, GT),
    names_to = "Variable",
    values_to = "Value"
  ) %>%
  mutate(
    Variable = recode(
      Variable,
      Spend = "Adexspot reklamos išlaidos",
      GT = "Google paieškos"
    )
  )

ggplot(panel_long_raw,
       aes(x = YearMonth, y = Value, color = Variable)) +
  geom_line(linewidth = 0.9) +
  facet_wrap(~A_BRAND_E_CLEAN, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Reklamos išlaidų ir Google paieškų dinamika iki birželio",
    x = "Laikotarpis",
    y = "Reikšmė",
    color = "Kintamasis"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )
ggplot(panel_clean, aes(x = YearMonth, y = z1)) +
  geom_line(color = "darkorange") +
  facet_wrap(~A_BRAND_E_CLEAN, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Normuotas rodiklis z1 = GT / Spend_lag1",
    x = "Metai",
    y = "z1"
  )

ggplot(panel_clean, aes(x = YearMonth, y = z2)) +
  geom_line(color = "darkgreen") +
  facet_wrap(~A_BRAND_E_CLEAN, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Normuotas rodiklis z2 = GT / Spend_lag2",
    x = "Metai",
    y = "z2"
  )

ggplot(panel_clean, aes(x = YearMonth, y = z3)) +
  geom_line(color = "darkred") +
  facet_wrap(~A_BRAND_E_CLEAN, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Normuotas rodiklis z3 = GT / Spend_lag3",
    x = "Metai",
    y = "z3"
  )

head(panel_clean)

ggplot(panel_clean, aes(x = log_Spend, y = log_GT)) +
  geom_point(alpha = 0.7, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  theme_minimal() +
  labs(
    title = "Ryšys tarp log-transformuotų reklamos išlaidų ir Google paieškų",
    x = "log(Išlaidos + 1)",
    y = "log(Google paieškos + 1)"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5)
  )
ggplot(panel_clean, aes(x = log_Spend, y = log_GT)) +
  geom_point(alpha = 0.7, color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  facet_wrap(~A_BRAND_E_CLEAN, scales = "free") +
  theme_minimal() +
  labs(
    title = "Ryšys tarp log-transformuotų reklamos išlaidų ir Google paieškų pagal prekės ženklus",
    x = "log(Išlaidos + 1)",
    y = "log(Google paieškos + 1)"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5)
  )
ggplot(panel_clean, aes(x = log_Spend, y = log_Contacts)) +
  geom_point(alpha = 0.7, color = "darkgreen") +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  theme_minimal() +
  labs(
    title = "Ryšys tarp log-transformuotų reklamos išlaidų ir kontaktų",
    x = "log(Išlaidos + 1)",
    y = "log(Kontaktai + 1)"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5)
  )
names(panel_clean)
# 1. Prieš log
p_pries <- ggplot(panel_clean, aes(x = Spend, y = GT)) +
  geom_point(alpha = 0.7, color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  theme_minimal() +
  labs(
    title = "Prieš log transformaciją",
    x = "Reklamos išlaidos",
    y = "Google paieškos"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

# 2. Po log
p_po <- ggplot(panel_clean, aes(x = log_Spend, y = log_GT)) +
  geom_point(alpha = 0.7, color = "darkgreen") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  theme_minimal() +
  labs(
    title = "Po log transformacijos",
    x = "log(Išlaidos + 1)",
    y = "log(Google paieškos + 1)"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )


p_pries <- ggplot(panel_clean, aes(x = Spend, y = GT)) +
  geom_point(alpha = 0.6, color = "#4A90E2") +
  geom_smooth(method = "lm", se = FALSE, color = "#D0021B", linewidth = 1) +
  labs(
    title = "Prieš log transformaciją",
    subtitle = "Originalūs duomenys",
    x = "Reklamos išlaidos",
    y = "Google paieškos"
  )

p_po <- ggplot(panel_clean, aes(x = log_Spend, y = log_GT)) +
  geom_point(alpha = 0.6, color = "#27AE60") +
  geom_smooth(method = "lm", se = FALSE, color = "#D0021B", linewidth = 1) +
  labs(
    title = "Po log transformacijos",
    subtitle = "Log-transformuoti duomenys",
    x = "log(Išlaidos + 1)",
    y = "log(Google paieškos + 1)"
  )

(p_pries | p_po) +
  plot_annotation(
    title = "Reklamos išlaidų ir Google paieškų sklaidos diagrama",
    theme = theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5))
  )



panel_clean <- panel_clean %>%
  mutate(log_Contacts = log(Contacts + 1))

p_pries <- ggplot(panel_clean, aes(x = Spend, y = Contacts)) +
  geom_point(alpha = 0.6, color = "#4A90E2") +
  geom_smooth(method = "lm", se = FALSE, color = "#D0021B", linewidth = 1) +
  labs(
    title = "Prieš log transformaciją",
    subtitle = "Originalūs duomenys",
    x = "Reklamos išlaidos",
    y = "Kontaktai"
  )

p_po <- ggplot(panel_clean, aes(x = log_Spend, y = log_Contacts)) +
  geom_point(alpha = 0.6, color = "#27AE60") +
  geom_smooth(method = "lm", se = FALSE, color = "#D0021B", linewidth = 1) +
  labs(
    title = "Po log transformacijos",
    subtitle = "Log-transformuoti duomenys",
    x = "log(Išlaidos + 1)",
    y = "log(Kontaktai + 1)"
  )

(p_pries | p_po) +
  plot_annotation(
    title = "Reklamos išlaidų ir kontaktų sklaidos diagrama",
    theme = theme(
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5)
    )
  )

#Abu kartu
p_pries | p_po

p_lag1 <- ggplot(panel_clean, aes(x = log(Spend_lag1 + 1), y = log_GT)) +
  geom_point(alpha = 0.6, color = "#F39C12") +
  geom_smooth(method = "lm", se = FALSE, color = "#D0021B", linewidth = 1) +
  labs(
    title = "Lag 1 (t-1)",
    subtitle = "Praėjusio mėnesio laikotarpis",
    x = "log(Spend t-1 + 1)",
    y = "log(GT + 1)"
  )

p_lag2 <- ggplot(panel_clean, aes(x = log(Spend_lag2 + 1), y = log_GT)) +
  geom_point(alpha = 0.6, color = "#2ECC71") +
  geom_smooth(method = "lm", se = FALSE, color = "#D0021B", linewidth = 1) +
  labs(
    title = "Lag 2 (t-2)",
    subtitle = "Prieš 2 mėnesius",
    x = "log(Spend t-2 + 1)",
    y = "log(GT + 1)"
  )

p_lag3 <- ggplot(panel_clean, aes(x = log(Spend_lag3 + 1), y = log_GT)) +
  geom_point(alpha = 0.6, color = "#3498DB") +
  geom_smooth(method = "lm", se = FALSE, color = "#D0021B", linewidth = 1) +
  labs(
    title = "Lag 3 (t-3)",
    subtitle = "Prieš 3 mėnesius",
    x = "log(Spend t-3 + 1)",
    y = "log(GT + 1)"
  )

(p_lag1 | p_lag2 | p_lag3) +
  plot_annotation(
    title = "Atidėto reklamos poveikio analizė (Lag efektas)",
    theme = theme(
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )
  )

p_t <- ggplot(panel_clean, aes(x = log(Spend + 1), y = log_GT)) +
  geom_point(alpha = 0.6, color = "#8E44AD") +
  geom_smooth(method = "lm", se = FALSE, color = "#D0021B", linewidth = 1) +
  labs(
    title = "Dabartinis laikotarpis (t)",
    subtitle = "To paties mėnesio reklamos išlaidos",
    x = "log(Spend t + 1)",
    y = "log(GT + 1)"
  )

p_lag1 <- ggplot(panel_clean, aes(x = log(Spend_lag1 + 1), y = log_GT)) +
  geom_point(alpha = 0.6, color = "#F39C12") +
  geom_smooth(method = "lm", se = FALSE, color = "#D0021B", linewidth = 1) +
  labs(
    title = "Lag 1 (t-1)",
    subtitle = "Praėjusio mėnesio laikotarpis",
    x = "log(Spend t-1 + 1)",
    y = "log(GT + 1)"
  )

p_lag2 <- ggplot(panel_clean, aes(x = log(Spend_lag2 + 1), y = log_GT)) +
  geom_point(alpha = 0.6, color = "#2ECC71") +
  geom_smooth(method = "lm", se = FALSE, color = "#D0021B", linewidth = 1) +
  labs(
    title = "Lag 2 (t-2)",
    subtitle = "Prieš 2 mėnesius",
    x = "log(Spend t-2 + 1)",
    y = "log(GT + 1)"
  )

p_lag3 <- ggplot(panel_clean, aes(x = log(Spend_lag3 + 1), y = log_GT)) +
  geom_point(alpha = 0.6, color = "#3498DB") +
  geom_smooth(method = "lm", se = FALSE, color = "#D0021B", linewidth = 1) +
  labs(
    title = "Lag 3 (t-3)",
    subtitle = "Prieš 3 mėnesius",
    x = "log(Spend t-3 + 1)",
    y = "log(GT + 1)"
  )

(p_t | p_lag1 | p_lag2 | p_lag3) +
  plot_annotation(
    title = "Atidėto reklamos poveikio analizė (Lag efektas)",
    subtitle = "Lyginamas einamasis mėnesis ir atidėtas reklamos poveikis",
    theme = theme(
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )
  )

p_t <- ggplot(panel_clean, aes(x = Spend, y = GT)) +
  geom_point(alpha = 0.6, color = "#8E44AD") +
  geom_smooth(method = "lm", se = FALSE, color = "#D0021B", linewidth = 1) +
  labs(
    title = "Dabartinis laikotarpis (t)",
    subtitle = "To paties mėnesio reklamos išlaidos",
    x = "Spend t",
    y = "GT"
  )

p_lag1 <- ggplot(panel_clean, aes(x = Spend_lag1, y = GT)) +
  geom_point(alpha = 0.6, color = "#F39C12") +
  geom_smooth(method = "lm", se = FALSE, color = "#D0021B", linewidth = 1) +
  labs(
    title = "Lag 1 (t-1)",
    subtitle = "Praėjusio mėnesio reklamos išlaidos",
    x = "Spend t-1",
    y = "GT"
  )

p_lag2 <- ggplot(panel_clean, aes(x = Spend_lag2, y = GT)) +
  geom_point(alpha = 0.6, color = "#2ECC71") +
  geom_smooth(method = "lm", se = FALSE, color = "#D0021B", linewidth = 1) +
  labs(
    title = "Lag 2 (t-2)",
    subtitle = "Prieš 2 mėnesius",
    x = "Spend t-2",
    y = "GT"
  )

p_lag3 <- ggplot(panel_clean, aes(x = Spend_lag3, y = GT)) +
  geom_point(alpha = 0.6, color = "#3498DB") +
  geom_smooth(method = "lm", se = FALSE, color = "#D0021B", linewidth = 1) +
  labs(
    title = "Lag 3 (t-3)",
    subtitle = "Prieš 3 mėnesius",
    x = "Spend t-3",
    y = "GT"
  )

(p_t | p_lag1 | p_lag2 | p_lag3) +
  plot_annotation(
    title = "Reklamos išlaidų ir Google paieškų ryšys pagal laikotarpį",
    subtitle = "Lyginamas einamasis ir atidėtas reklamos poveikis be log transformacijos",
    theme = theme(
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )
  )

theme_set(
  theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      plot.subtitle = element_text(hjust = 0.5, size = 11),
      axis.title = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )
)

#  SU LOG TRANSFORMACIJA


p_t_log <- ggplot(panel_clean, aes(x = log(Spend + 1), y = log_GT)) +
  geom_point(alpha = 0.6, color = "#8E44AD") +
  geom_smooth(method = "lm", se = FALSE, color = "#D0021B", linewidth = 1) +
  labs(
    title = "Dabartinis laikotarpis (t)",
    subtitle = "Su log transformacija",
    x = "log(Išlaidos t + 1)",
    y = "log(GT + 1)"
  )

p_lag1_log <- ggplot(panel_clean, aes(x = log(Spend_lag1 + 1), y = log_GT)) +
  geom_point(alpha = 0.6, color = "#F39C12") +
  geom_smooth(method = "lm", se = FALSE, color = "#D0021B", linewidth = 1) +
  labs(
    title = "Lag 1 (t-1)",
    subtitle = "Su log transformacija",
    x = "log(Išlaidos t-1 + 1)",
    y = "log(GT + 1)"
  )

p_lag2_log <- ggplot(panel_clean, aes(x = log(Spend_lag2 + 1), y = log_GT)) +
  geom_point(alpha = 0.6, color = "#2ECC71") +
  geom_smooth(method = "lm", se = FALSE, color = "#D0021B", linewidth = 1) +
  labs(
    title = "Lag 2 (t-2)",
    subtitle = "Su log transformacija",
    x = "log(Išlaidos t-2 + 1)",
    y = "log(GT + 1)"
  )

p_lag3_log <- ggplot(panel_clean, aes(x = log(Spend_lag3 + 1), y = log_GT)) +
  geom_point(alpha = 0.6, color = "#3498DB") +
  geom_smooth(method = "lm", se = FALSE, color = "#D0021B", linewidth = 1) +
  labs(
    title = "Lag 3 (t-3)",
    subtitle = "Su log transformacija",
    x = "log(Išlaidos t-3 + 1)",
    y = "log(GT + 1)"
  )

# BE LOG TRANSFORMACIJOS


p_t_raw <- ggplot(panel_clean, aes(x = Spend, y = GT)) +
  geom_point(alpha = 0.6, color = "#8E44AD") +
  geom_smooth(method = "lm", se = FALSE, color = "#D0021B", linewidth = 1) +
  labs(
    title = "Dabartinis laikotarpis (t)",
    subtitle = "Be log transformacijos",
    x = "Išlaidos (t)",
    y = "GT"
  )

p_lag1_raw <- ggplot(panel_clean, aes(x = Spend_lag1, y = GT)) +
  geom_point(alpha = 0.6, color = "#F39C12") +
  geom_smooth(method = "lm", se = FALSE, color = "#D0021B", linewidth = 1) +
  labs(
    title = "Lag 1 (t-1)",
    subtitle = "Be log transformacijos",
    x = "Išlaidos (t-1)",
    y = "GT"
  )

p_lag2_raw <- ggplot(panel_clean, aes(x = Spend_lag2, y = GT)) +
  geom_point(alpha = 0.6, color = "#2ECC71") +
  geom_smooth(method = "lm", se = FALSE, color = "#D0021B", linewidth = 1) +
  labs(
    title = "Lag 2 (t-2)",
    subtitle = "Be log transformacijos",
    x = "Išlaidos (t-2)",
    y = "GT"
  )

p_lag3_raw <- ggplot(panel_clean, aes(x = Spend_lag3, y = GT)) +
  geom_point(alpha = 0.6, color = "#3498DB") +
  geom_smooth(method = "lm", se = FALSE, color = "#D0021B", linewidth = 1) +
  labs(
    title = "Lag 3 (t-3)",
    subtitle = "Be log transformacijos",
    x = "Išlaidos (t-3)",
    y = "GT"
  )

#  VISKAS VIENAME VAIZDE


((p_t_raw | p_lag1_raw | p_lag2_raw | p_lag3_raw) /
   (p_t_log | p_lag1_log | p_lag2_log | p_lag3_log)) +
  plot_annotation(
    title = "Reklamos išlaidų ir Google paieškų ryšys pagal laikotarpį",
    subtitle = "Viršuje – be log transformacijos, apačioje – su log transformacija",
    theme = theme(
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5)
    )
  )

cor_results <- tibble(
  Modelis = c(
    "Spend vs GT (Spearman)",
    "log(Spend) vs log(GT) (Pearson)",
    "lag1 Spend vs GT (Pearson)",
    "lag2 Spend vs GT (Pearson)",
    "lag3 Spend vs GT (Pearson)"
  ),
  Koreliacija = c(
    cor(panel_clean$Spend, panel_clean$GT, method = "spearman", use = "complete.obs"),
    cor(panel_clean$log_Spend, panel_clean$log_GT, use = "complete.obs"),
    cor(log(panel_clean$Spend_lag1 + 1), panel_clean$log_GT, use = "complete.obs"),
    cor(log(panel_clean$Spend_lag2 + 1), panel_clean$log_GT, use = "complete.obs"),
    cor(log(panel_clean$Spend_lag3 + 1), panel_clean$log_GT, use = "complete.obs")
  )
)

cor_results

cor_data <- panel_clean %>%
  dplyr::select(
    Spend, GT, Contacts,
    log_Spend, log_GT, log_Contacts,
    Spend_lag1, Spend_lag2, Spend_lag3,
    z1, z2, z3
  )

# Koreliacijų matrica
cor_matrix <- cor(cor_data, use = "complete.obs")

print(cor_matrix)

corrplot(cor_matrix,
         method = "square",
         type = "upper",
         addCoef.col = "black",
         tl.col = "black",
         tl.srt = 45,
         col = colorRampPalette(c("#B2182B", "#FFFFFF", "#2166AC"))(200),
         diag = FALSE)


# duomen7 paruo6imas pagal brand


panel_clean2 <- panel_clean %>%
  filter(YearMonth != as.Date("2025-07-01"))

panel_by_brand <- panel_clean2 %>%
  group_by(A_BRAND_E_CLEAN  , YearMonth) %>%
  summarise(
    Spend = sum(Spend, na.rm = TRUE),
    GT = sum(GT, na.rm = TRUE),
    Contacts = sum(Contacts, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(A_BRAND_E_CLEAN  ) %>%
  complete(YearMonth = seq(min(YearMonth), max(YearMonth), by = "month")) %>%
  arrange(A_BRAND_E_CLEAN  , YearMonth) %>%
  mutate(
    Spend = replace_na(Spend, 0),
    GT = replace_na(GT, 0),
    Contacts = replace_na(Contacts, 0),
    log_Spend = log1p(Spend),
    log_GT = log1p(GT),
    log_Contacts = log1p(Contacts)
  ) %>%
  ungroup()


# STL FUNKCIJA


run_stl_brand <- function(data, value_col, s_window = "periodic") {
  
  x <- data[[value_col]]
  
  if (length(x) < 24 || sd(x, na.rm = TRUE) == 0) {
    return(NULL)
  }
  
  start_date <- min(data$YearMonth)
  ts_x <- ts(x, start = c(year(start_date), month(start_date)), frequency = 12)
  
  fit <- stl(ts_x, s.window = s_window, robust = TRUE)
  rem <- as.numeric(remainder(fit))
  adf_res <- adf.test(na.omit(rem))
  
  out_data <- tibble(
    BRAND = unique(data$BRAND),
    YearMonth = data$YearMonth,
    variable = value_col,
    original = as.numeric(ts_x),
    trend = as.numeric(trendcycle(fit)),
    seasonal = as.numeric(seasonal(fit)),
    remainder = rem,
    seasadj = as.numeric(seasadj(fit)),
    adf_p_value = adf_res$p.value
  )
  
  list(data = out_data, fit = fit)
}

# STL PALEIDIMAS KIEKVIENAM BRANDUI IR KINTAMAJAM


vars_to_decompose <- c("log_Spend", "log_GT", "log_Contacts")
brands <- unique(panel_by_brand$A_BRAND_E_CLEAN  )

results_nested <- list()

for (b in brands) {
  
  brand_data <- panel_by_brand %>%
    filter(A_BRAND_E_CLEAN   == b) %>%
    arrange(YearMonth)
  
  results_nested[[b]] <- list()
  
  for (v in vars_to_decompose) {
    results_nested[[b]][[v]] <- run_stl_brand(brand_data, v)
  }
}

# rafikai kiekvienam brand ir kintamajam

output_dir <- "stl_grafikai"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

safe_filename <- function(x) {
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
  x <- gsub("[^A-Za-z0-9_\\-]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  x
}


# grafikų eksportavimas i png
pavadinimai <- c(
  log_Spend = "log(Išlaidos)",
  log_GT = "log(Google Trends)",
  log_Contacts = "log(Kontaktai)"
)

for (b in names(results_nested)) {
  for (v in names(results_nested[[b]])) {
    
    res <- results_nested[[b]][[v]]
    if (is.null(res)) next
    
    fit <- res$fit
    ts_data <- fit$time.series
    vardas <- pavadinimai[v]
    
    file_name <- paste0(
      safe_filename(b), "_",
      safe_filename(v), "_STL.png"
    )
    
    file_path <- file.path(output_dir, file_name)
    
    png(
      filename = file_path,
      width = 1400,
      height = 1800,
      res = 180
    )
    
    par(
      mfrow = c(4, 1),
      mar = c(4, 6, 4, 2),
      cex.lab = 1.2,
      cex.main = 1.2,
      cex.axis = 1.0
    )
    
    plot(
      ts_data[, "seasonal"] + ts_data[, "trend"] + ts_data[, "remainder"],
      type = "l",
      main = paste("Prekinis ženklas:", b, "| Laiko eilutė:", vardas),
      xlab = "Laikas",
      ylab = "Reikšmė"
    )
    
    plot(
      ts_data[, "seasonal"],
      type = "l",
      main = paste("Prekinis ženklas:", b, "| Sezoniškumas:", vardas),
      xlab = "Laikas",
      ylab = "Sezoniškumas"
    )
    
    plot(
      ts_data[, "trend"],
      type = "l",
      main = paste("Prekinis ženklas:", b, "| Trendas:", vardas),
      xlab = "Laikas",
      ylab = "Trendas"
    )
    
    plot(
      ts_data[, "remainder"],
      type = "l",
      main = paste("Prekinis ženklas:", b, "| Liekana:", vardas),
      xlab = "Laikas",
      ylab = "Liekana"
    )
    
    dev.off()
  }
}

# ADF TESTO SANTRAUKA KIEKVIENAM BRANDUI

adf_summary_brand <- purrr::map_dfr(names(results_nested), function(b) {
  purrr::map_dfr(names(results_nested[[b]]), function(v) {
    
    res <- results_nested[[b]][[v]]
    
    if (is.null(res)) {
      return(tibble(
        BRAND = b,
        variable = v,
        adf_p_value = NA_real_,
        is_stationary = "Nepavyko apskaičiuoti"
      ))
    }
    
    pval <- res$data$adf_p_value[1]
    
    tibble(
      BRAND = b,
      variable = v,
      adf_p_value = pval,
      is_stationary = ifelse(pval < 0.05, "Taip (stacionari)", "Ne (nestacionari)")
    )
  })
})

print("ADF testas (remainder) kiekvienam brandui:")
print(adf_summary_brand)



rem <- na.omit(results_total[["log_Contacts"]]$data$remainder)

acf(rem)
pacf(rem)

for (b in names(results_nested)) {
  for (v in names(results_nested[[b]])) {
    
    res <- results_nested[[b]][[v]]
    if (is.null(res)) next
    
    rem <- na.omit(res$data$remainder)
    
    old_par <- par(no.readonly = TRUE)
    par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))
    
    acf(rem, main = paste("ACF remainder |", b, "|", v))
    pacf(rem, main = paste("PACF remainder |", b, "|", v))
    
    par(old_par)
  }
}

make_ts_brand <- function(data, value_col) {
  ts(
    data[[value_col]],
    start = c(year(min(data$YearMonth)), month(min(data$YearMonth))),
    frequency = 12
  )
}


adf_diff_summary <- map_dfr(names(results_nested), function(b) {
  
  brand_data <- panel_by_brand %>%
    filter(A_BRAND_E_CLEAN == b) %>%
    arrange(YearMonth)
  
  map_dfr(c("log_Spend", "log_GT", "log_Contacts"), function(v) {
    
    x <- brand_data[[v]]
    
    if (length(na.omit(x)) < 24 || sd(x, na.rm = TRUE) == 0) {
      return(tibble(
        A_BRAND_E_CLEAN = b,
        variable = v,
        testas = c("diff_1", "diff_12", "diff_12_1"),
        adf_p_value = NA_real_
      ))
    }
    
    ts_x <- make_ts_brand(brand_data, v)
    
    p1 <- tryCatch(adf.test(na.omit(diff(ts_x, lag = 1)))$p.value, error = function(e) NA_real_)
    p12 <- tryCatch(adf.test(na.omit(diff(ts_x, lag = 12)))$p.value, error = function(e) NA_real_)
    p12_1 <- tryCatch(adf.test(na.omit(diff(diff(ts_x, lag = 12), lag = 1)))$p.value, error = function(e) NA_real_)
    
    tibble(
      A_BRAND_E_CLEAN = b,
      variable = v,
      testas = c("diff_1", "diff_12", "diff_12_1"),
      adf_p_value = c(p1, p12, p12_1),
      stacionari = ifelse(adf_p_value < 0.05, "Taip", "Ne")
    )
  })
})

print(adf_diff_summary)

# tik moki ir ermitazo kontaktma


check_extra_diff <- function(brand_name) {
  
  brand_data <- panel_by_brand %>%
    filter(A_BRAND_E_CLEAN == brand_name) %>%
    arrange(YearMonth)
  
  ts_contacts <- ts(
    brand_data$log_Contacts,
    start = c(year(min(brand_data$YearMonth)), month(min(brand_data$YearMonth))),
    frequency = 12
  )
  
  p_diff_1_1 <- tryCatch(
    adf.test(na.omit(diff(diff(ts_contacts, lag = 1), lag = 1)))$p.value,
    error = function(e) NA_real_
  )
  
  p_diff_12_1_1 <- tryCatch(
    adf.test(na.omit(diff(diff(diff(ts_contacts, lag = 12), lag = 1), lag = 1)))$p.value,
    error = function(e) NA_real_
  )
  
  tibble(
    BRAND = brand_name,
    testas = c("diff_1_1", "diff_12_1_1"),
    adf_p_value = c(p_diff_1_1, p_diff_12_1_1),
    stacionari = ifelse(adf_p_value < 0.05, "Taip", "Ne")
  )
}

bind_rows(
  check_extra_diff("MOKI VEŽI"),
  check_extra_diff("ERMITAŽAS")
)



# Funkcija parinkti stacioanrumui

get_best_stationary_series <- function(ts_x, brand_name = NA, variable_name = NA) {
  
  candidates <- list(
    original = ts_x,
    diff_1 = diff(ts_x, lag = 1),
    diff_12 = diff(ts_x, lag = 12),
    diff_12_1 = diff(diff(ts_x, lag = 12), lag = 1),
    diff_1_1 = diff(diff(ts_x, lag = 1), lag = 1),
    diff_12_1_1 = diff(diff(diff(ts_x, lag = 12), lag = 1), lag = 1)
  )
  
  test_results <- purrr::map_dfr(names(candidates), function(test_name) {
    
    x <- na.omit(as.numeric(candidates[[test_name]]))
    
    if (length(x) < 12 || sd(x, na.rm = TRUE) == 0) {
      return(tibble(
        BRAND = brand_name,
        variable = variable_name,
        version = test_name,
        adf_p_value = NA_real_,
        stationary = FALSE
      ))
    }
    
    pval <- tryCatch(
      adf.test(x)$p.value,
      error = function(e) NA_real_
    )
    
    tibble(
      BRAND = brand_name,
      variable = variable_name,
      version = test_name,
      adf_p_value = pval,
      stationary = ifelse(!is.na(pval) & pval < 0.05, TRUE, FALSE)
    )
  })
  
  selected <- test_results %>%
    filter(stationary) %>%
    slice(1)
  
  if (nrow(selected) == 0) {
    return(list(
      selected_name = NA_character_,
      selected_series = NULL,
      summary = test_results
    ))
  }
  
  selected_name <- selected$version[1]
  
  list(
    selected_name = selected_name,
    selected_series = na.omit(candidates[[selected_name]]),
    summary = test_results
  )
}
# STACIONARUMAS 


stationary_choice_summary <- purrr::map_dfr(brands, function(b) {
  
  brand_data <- panel_by_brand %>%
    filter(A_BRAND_E_CLEAN == b) %>%
    arrange(YearMonth)
  
  purrr::map_dfr(vars_to_decompose, function(v) {
    
    ts_x <- ts(
      brand_data[[v]],
      start = c(year(min(brand_data$YearMonth)), month(min(brand_data$YearMonth))),
      frequency = 12
    )
    
    stat_res <- get_best_stationary_series(
      ts_x = ts_x,
      brand_name = b,
      variable_name = v
    )
    
    if (is.null(stat_res$selected_series)) {
      return(tibble(
        BRAND = b,
        variable = v,
        selected_version = NA_character_,
        adf_p_value = NA_real_,
        stationary = "Neparinkta"
      ))
    }
    
    chosen_row <- stat_res$summary %>%
      filter(version == stat_res$selected_name) %>%
      slice(1)
    
    tibble(
      BRAND = b,
      variable = v,
      selected_version = stat_res$selected_name,
      adf_p_value = chosen_row$adf_p_value,
      stationary = "Taip"
    )
  })
})

print(stationary_choice_summary, n = Inf)

acf_pacf_dir <- "acf_pacf_grafikai"
if (!dir.exists(acf_pacf_dir)) {
  dir.create(acf_pacf_dir, recursive = TRUE)
}

# Pavadinimų mapping (su LT raidėm)
var_map <- c(
  log_Spend = "Išlaidos",
  log_GT = "GT",
  log_Contacts = "Kontaktai"
)

version_map <- c(
  original = "org",
  diff_1 = "d1",
  diff_12 = "d12",
  diff_12_1 = "d12_1",
  diff_1_1 = "d2",
  diff_12_1_1 = "d12_2"
)

for (b in brands) {
  
  brand_data <- panel_by_brand %>%
    filter(A_BRAND_E_CLEAN == b) %>%
    arrange(YearMonth)
  
  for (v in vars_to_decompose) {
    
    ts_x <- ts(
      brand_data[[v]],
      start = c(year(min(brand_data$YearMonth)), month(min(brand_data$YearMonth))),
      frequency = 12
    )
    
    stat_res <- get_best_stationary_series(
      ts_x = ts_x,
      brand_name = b,
      variable_name = v
    )
    
    if (is.null(stat_res$selected_series)) {
      next
    }
    
    x_stat <- stat_res$selected_series
    versija <- stat_res$selected_name
    
    v_name <- var_map[v]
    ver_name <- version_map[versija]
    
    # FAILO pavadinimas (be LT raidžių dėl saugumo)
    file_name <- paste0(
      safe_filename(b), "_",
      safe_filename(v_name), "_",
      ver_name, "_ACF_PACF.png"
    )
    
    file_path <- file.path(acf_pacf_dir, file_name)
    
    png(
      filename = file_path,
      width = 1400,
      height = 700,
      res = 180
    )
    
    par(mfrow = c(1, 2), mar = c(4, 4, 4, 1))
    
    acf(
      x_stat,
      main = paste(b, "|", v_name, "|", ver_name)
    )
    
    pacf(
      x_stat,
      main = paste(b, "|", v_name, "|", ver_name)
    )
    
    dev.off()
  }
}


stationary_choice_summary

#   Modeliavimas

BRANDS <- c("SENUKAI", "MOKI VEŽI", "DEPO", "ERMITAŽAS", "LYTAGRA")
VARIABLES <- c("log_Spend", "log_GT", "log_Contacts")
TRAIN_RATIOS <- c(0.80) 

#Pagalbin4 funkcija 
calculate_metrics <- function(actual, forecast) {
  valid_idx <- !is.na(actual) & !is.na(forecast) & 
    is.finite(actual) & is.finite(forecast)
  
  if (sum(valid_idx) == 0) {
    return(list(MAE = NA, RMSE = NA, MAPE = NA))
  }
  
  actual_clean <- actual[valid_idx]
  forecast_clean <- forecast[valid_idx]
  
  errors <- actual_clean - forecast_clean
  mae <- mean(abs(errors), na.rm = TRUE)
  rmse <- sqrt(mean(errors^2, na.rm = TRUE))
  
  denominators <- abs(actual_clean) + 1e-10
  mape <- mean(abs(errors / denominators), na.rm = TRUE) * 100
  
  list(MAE = mae, RMSE = rmse, MAPE = mape)
}

get_d_value <- function(brand_name, variable_name, stationary_choice_summary) {
  d_row <- stationary_choice_summary %>%
    filter(BRAND == brand_name, variable == variable_name)
  
  if (nrow(d_row) == 0) return(1)
  
  selected_version <- d_row$selected_version[1]
  
  case_when(
    selected_version == "original" ~ 0,
    selected_version == "diff_1" ~ 1,
    selected_version == "diff_12" ~ 12,
    selected_version == "diff_1_1" ~ 1,
    selected_version == "diff_12_1" ~ 12,
    TRUE ~ 1
  )
}


#  train test 

data_splits <- list()

for (train_ratio in TRAIN_RATIOS) {
  
  cat("Paruošiami duomenys (train ratio: ", train_ratio, ")...\n", sep="")
  
  split_name <- paste0("split_", train_ratio * 100)
  data_splits[[split_name]] <- list()
  
  for (brand_i in BRANDS) {
    for (variable_i in VARIABLES) {
      
      # Gauti duomenis
      brand_data <- panel_clean %>%
        filter(A_BRAND_E_CLEAN == brand_i) %>%
        arrange(YearMonth) %>%
        mutate(
          log_Spend = log(Spend + 1),
          log_GT = log(GT + 1),
          log_Contacts = log(Contacts + 1)
        )
      
      if (nrow(brand_data) < 12) next
      
      # Padalinti
      n_total <- nrow(brand_data)
      n_train <- floor(n_total * train_ratio)
      
      if (n_train < 12) next
      
      train_data <- brand_data %>% slice(1:n_train)
      test_data <- brand_data %>% slice((n_train + 1):n_total)
      
      ts_train <- ts(train_data[[variable_i]], frequency = 12)
      ts_test <- ts(test_data[[variable_i]], frequency = 12)
      
      if (any(is.na(ts_train)) | any(!is.finite(ts_train))) next
      
      key <- paste0(brand_i, "__", variable_i)
      
      data_splits[[split_name]][[key]] <- list(
        brand = brand_i,
        variable = variable_i,
        train_data = train_data,
        test_data = test_data,
        ts_train = ts_train,
        ts_test = ts_test,
        n_train = n_train,
        n_test = length(ts_test),
        train_ratio = train_ratio
      )
    }
  }
  
  n_keys <- length(data_splits[[split_name]])
  cat("Train ratio ", train_ratio*100, ":", n_keys, " derynių\n", sep="")
}


# modeliavimas

all_results <- tibble()


# ARIMA

for (split_name in names(data_splits)) {
  for (key in names(data_splits[[split_name]])) {
    
    data <- data_splits[[split_name]][[key]]
    d_value <- get_d_value(data$brand, data$variable, stationary_choice_summary)
    
    # Fit
    fit <- tryCatch(
      auto.arima(data$ts_train, d = d_value, seasonal = FALSE, stepwise = FALSE),
      error = function(e) NULL
    )
    
    if (is.null(fit)) next
    
    # Forecast
    h <- data$n_test
    forecast_vals <- tryCatch(
      as.numeric(forecast(fit, h = h)$mean),
      error = function(e) NULL
    )
    
    if (is.null(forecast_vals) | any(is.na(forecast_vals))) next
    
    actual_vals <- as.numeric(data$ts_test)
    if (any(is.na(actual_vals))) next
    
    metrics <- calculate_metrics(actual_vals, forecast_vals)
    if (any(is.na(unlist(metrics)))) next
    
    p <- fit$arma[1]; q <- fit$arma[2]
    
    all_results <- bind_rows(all_results, tibble(
      split = gsub("split_", "", split_name),
      train_ratio = data$train_ratio,
      BRAND = data$brand,
      variable = data$variable,
      modelis = "ARIMA",
      
      model_str = paste0("ARIMA(", p, ",", d_value, ",", q, ")"),
      n_train = data$n_train,
      n_test = data$n_test,
      RMSE = round(metrics$RMSE, 4),
      MAE = round(metrics$MAE, 4),
      MAPE = round(metrics$MAPE, 2),
      AIC = round(AIC(fit), 2)
    ))
  }
}


# SARIMA

for (split_name in names(data_splits)) {
  for (key in names(data_splits[[split_name]])) {
    
    data <- data_splits[[split_name]][[key]]
    d_value <- get_d_value(data$brand, data$variable, stationary_choice_summary)
    
    fit <- tryCatch(
      auto.arima(data$ts_train, d = d_value, seasonal = TRUE, stepwise = FALSE),
      error = function(e) NULL
    )
    
    if (is.null(fit)) next
    
    h <- data$n_test
    forecast_vals <- tryCatch(
      as.numeric(forecast(fit, h = h)$mean),
      error = function(e) NULL
    )
    
    if (is.null(forecast_vals) | any(is.na(forecast_vals))) next
    
    actual_vals <- as.numeric(data$ts_test)
    if (any(is.na(actual_vals))) next
    
    metrics <- calculate_metrics(actual_vals, forecast_vals)
    if (any(is.na(unlist(metrics)))) next
    
    p <- fit$arma[1]; q <- fit$arma[2]
    P <- fit$arma[3]; Q <- fit$arma[4]; D <- fit$arma[6]
    
    all_results <- bind_rows(all_results, tibble(
      split = gsub("split_", "", split_name),
      train_ratio = data$train_ratio,
      BRAND = data$brand,
      variable = data$variable,
      modelis = "SARIMA",
      model_str = paste0("SARIMA(", p, ",", d_value, ",", q, ")(", P, ",", D, ",", Q, ")12"),
      n_train = data$n_train,
      n_test = data$n_test,
      RMSE = round(metrics$RMSE, 4),
      MAE = round(metrics$MAE, 4),
      MAPE = round(metrics$MAPE, 2),
      AIC = round(AIC(fit), 2)
    ))
  }
}



# ARIMAX (su Spend xreg)


for (split_name in names(data_splits)) {
  for (key in names(data_splits[[split_name]])) {
    
    data <- data_splits[[split_name]][[key]]
    d_value <- get_d_value(data$brand, data$variable, stationary_choice_summary)
    
    # xreg paruošimas
    xreg_train <- cbind(
      Spend_lag = data$train_data$log_Spend[1:(data$n_train-1)]
    )
    xreg_test <- cbind(
      Spend_lag = data$test_data$log_Spend[1:(data$n_test-1)]
    )
    
    ts_train_use <- data$ts_train[2:length(data$ts_train)]
    ts_test_use <- data$ts_test[2:length(data$ts_test)]
    
    if (nrow(xreg_train) != length(ts_train_use) || 
        nrow(xreg_test) != length(ts_test_use)) next
    
    fit <- tryCatch(
      auto.arima(ts_train_use, xreg = xreg_train, d = d_value, 
                 seasonal = FALSE, stepwise = FALSE),
      error = function(e) NULL
    )
    
    if (is.null(fit)) next
    
    h <- length(ts_test_use)
    forecast_obj <- tryCatch(
      forecast(fit, xreg = xreg_test, h = h),
      error = function(e) NULL
    )
    
    if (is.null(forecast_obj)) next
    
    forecast_vals <- as.numeric(forecast_obj$mean)
    actual_vals <- as.numeric(ts_test_use)
    
    if (any(is.na(forecast_vals)) | any(is.na(actual_vals))) next
    
    metrics <- calculate_metrics(actual_vals, forecast_vals)
    if (any(is.na(unlist(metrics)))) next
    
    p <- fit$arma[1]; q <- fit$arma[2]
    
    all_results <- bind_rows(all_results, tibble(
      split = gsub("split_", "", split_name),
      train_ratio = data$train_ratio,
      BRAND = data$brand,
      variable = data$variable,
      modelis = "ARIMAX",
      model_str = paste0("ARIMAX(", p, ",", d_value, ",", q, ")+xreg"),
      n_train = nrow(xreg_train),
      n_test = h,
      RMSE = round(metrics$RMSE, 4),
      MAE = round(metrics$MAE, 4),
      MAPE = round(metrics$MAPE, 2),
      AIC = round(AIC(fit), 2)
    ))
  }
}

# SARIMAX (seasonal + xreg)



for (split_name in names(data_splits)) {
  for (key in names(data_splits[[split_name]])) {
    
    data <- data_splits[[split_name]][[key]]
    d_value <- get_d_value(data$brand, data$variable, stationary_choice_summary)
    
    if (data$n_train < 18) next
    
    xreg_train <- cbind(
      Spend_lag = data$train_data$log_Spend[1:(data$n_train-1)]
    )
    xreg_test <- cbind(
      Spend_lag = data$test_data$log_Spend[1:(data$n_test-1)]
    )
    
    ts_train_use <- data$ts_train[2:length(data$ts_train)]
    ts_test_use <- data$ts_test[2:length(data$ts_test)]
    
    if (nrow(xreg_train) != length(ts_train_use) || 
        nrow(xreg_test) != length(ts_test_use)) next
    
    fit <- tryCatch(
      auto.arima(ts_train_use, xreg = xreg_train, d = d_value, 
                 seasonal = TRUE, stepwise = FALSE, max.p = 2, max.q = 2),
      error = function(e) NULL
    )
    
    if (is.null(fit)) next
    
    h <- length(ts_test_use)
    forecast_obj <- tryCatch(
      forecast(fit, xreg = xreg_test, h = h),
      error = function(e) NULL
    )
    
    if (is.null(forecast_obj)) next
    
    forecast_vals <- as.numeric(forecast_obj$mean)
    actual_vals <- as.numeric(ts_test_use)
    
    if (length(forecast_vals) != length(actual_vals)) next
    if (any(is.na(forecast_vals)) | any(is.na(actual_vals))) next
    
    metrics <- calculate_metrics(actual_vals, forecast_vals)
    if (any(is.na(unlist(metrics)))) next
    
    p <- fit$arma[1]; q <- fit$arma[2]
    P <- fit$arma[3]; Q <- fit$arma[4]; D <- fit$arma[6]
    
    all_results <- bind_rows(all_results, tibble(
      split = gsub("split_", "", split_name),
      train_ratio = data$train_ratio,
      BRAND = data$brand,
      variable = data$variable,
      modelis = "SARIMAX",
      model_str = paste0("SARIMAX(", p, ",", d_value, ",", q, ")(", P, ",", D, ",", Q, ")12+xreg"),
      n_train = nrow(xreg_train),
      n_test = h,
      RMSE = round(metrics$RMSE, 4),
      MAE = round(metrics$MAE, 4),
      MAPE = round(metrics$MAPE, 2),
      AIC = round(AIC(fit), 2)
    ))
  }
}

# VAR modeliis


for (split_name in names(data_splits)) {
  for (brand_i in BRANDS) {
    
    data_keys <- names(data_splits[[split_name]])
    brand_keys <- data_keys[grepl(paste0("^", brand_i), data_keys)]
    
    if (length(brand_keys) < 3) next
    
    # Gauti train/test duomenis
    data <- data_splits[[split_name]][[brand_keys[1]]]
    
    data_train <- cbind(
      log_Spend = data$train_data$log_Spend,
      log_GT = data$train_data$log_GT,
      log_Contacts = data$train_data$log_Contacts
    )
    
    data_test <- cbind(
      log_Spend = data$test_data$log_Spend,
      log_GT = data$test_data$log_GT,
      log_Contacts = data$test_data$log_Contacts
    )
    
    # Lag selection
    lag_select <- tryCatch(
      VARselect(data_train, lag.max = 3, type = 'const'),
      error = function(e) NULL
    )
    
    if (is.null(lag_select)) next
    
    p_optimal <- as.numeric(lag_select$selection[1])
    
    fit <- tryCatch(
      VAR(data_train, p = p_optimal, type = 'const'),
      error = function(e) NULL
    )
    
    if (is.null(fit)) next
    
    forecast_obj <- tryCatch(
      predict(fit, n.ahead = nrow(data_test)),
      error = function(e) NULL
    )
    
    if (is.null(forecast_obj)) next
    
    # Skaičiuoti RMSE kiekvienam kintamajam
    for (var_idx in 1:3) {
      var_name <- colnames(data_test)[var_idx]
      forecast_vals <- forecast_obj$fcst[[var_idx]][, 1]
      actual_vals <- data_test[, var_idx]
      
      if (length(forecast_vals) != length(actual_vals)) next
      
      metrics <- calculate_metrics(actual_vals, forecast_vals)
      
      all_results <- bind_rows(all_results, tibble(
        split = gsub("split_", "", split_name),
        train_ratio = data$train_ratio,
        BRAND = brand_i,
        variable = var_name,
        modelis = "VAR",
        model_str = paste0("VAR(", p_optimal, ")"),
        n_train = nrow(data_train),
        n_test = nrow(data_test),
        RMSE = round(metrics$RMSE, 4),
        MAE = round(metrics$MAE, 4),
        MAPE = round(metrics$MAPE, 2),
        AIC = NA
      ))
    }
  }
}


# palyginimas rezulatatu

cat("Visi modeliai (sorted by RMSE):\n")
print(all_results %>% 
        arrange(RMSE) %>%
        dplyr::select(split, BRAND, variable, modelis, RMSE, MAE, MAPE),
      n = 30)
cat("top 30 pagal RMSE:\n\n")

top30 <- all_results %>%
  arrange(RMSE) %>%
  slice(1:150)

print(top30 %>% 
        dplyr::select(split, BRAND, variable, RMSE, MAE, MAPE),
      n = 30)


#train trest 
summary_by_split <- all_results %>%
  group_by(split, modelis) %>%
  summarise(
    Sėkmingų = n(),
    Vid_RMSE = round(mean(RMSE, na.rm = TRUE), 4),
    Min_RMSE = round(min(RMSE, na.rm = TRUE), 4),
    Max_RMSE = round(max(RMSE, na.rm = TRUE), 4),
    Vid_MAPE = round(mean(MAPE, na.rm = TRUE), 2),
    .groups = 'drop'
  ) %>%
  arrange(split, Vid_RMSE)

print(summary_by_split)




#  RMSE vs Split
p1 <- ggplot(all_results, aes(x = factor(split), y = RMSE, fill = modelis)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "RMSE pagal Train/Test Split",
       x = "Train/Test Split (%)", y = "RMSE", fill = "Modelis") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

#  Modelio tipai pagal RMSE
p2 <- ggplot(all_results, aes(x = reorder(modelis, RMSE, FUN = median), y = RMSE, fill = modelis)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_brewer(palette = "Dark2") +
  labs(title = "RMSE Palyginimas - Modeliai",
       x = "Modelis", y = "RMSE") +
  theme_minimal() +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, face = "bold"))

# RMSE vs MAPE scatter
p3 <- ggplot(all_results, aes(x = RMSE, y = MAPE, color = split, shape = modelis)) +
  geom_point(size = 2.5, alpha = 0.6) +
  scale_color_manual(values = c("80" = "#185FA5", "70" = "#BA7517", "60" = "#C64545")) +
  labs(title = "RMSE vs MAPE pagal Split",
       x = "RMSE", y = "MAPE (%)", color = "Split %", shape = "Modelis") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

#  Modelio skaičius pagal split
p4 <- all_results %>%
  group_by(split, modelis) %>%
  summarise(count = n(), .groups = 'drop') %>%
  ggplot(aes(x = split, y = count, fill = modelis)) +
  geom_col(alpha = 0.7, position = "dodge") +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Sėkmingų Modelių Skaičius",
       x = "Train/Test Split (%)", y = "Skaičius", fill = "Modelis") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

combined <- (p1 + p2) / (p3 + p4)
print(combined)




# ARIMAX(Contacts ~ Spend)
#  ARIMAX(GT ~ Spend)
#  ARIMA(z) -

# ARIMAX: Contacts ~ Spend
BRANDS <- c("SENUKAI", "MOKI VEŽI", "DEPO", "ERMITAŽAS")

cat("ARIMAX: log_Contacts ~ log_Spend\n")

results_a <- tibble()

for (brand_i in BRANDS) {
  
  brand_data <- panel_clean %>%
    filter(A_BRAND_E_CLEAN == brand_i) %>%
    arrange(YearMonth) %>%
    filter(!is.na(log_Contacts) & !is.na(log_Spend))
  
  if (nrow(brand_data) < 24) next
  
  n_total <- nrow(brand_data)
  n_train <- floor(n_total * 0.80)
  
  train_data <- brand_data %>% slice(1:n_train)
  test_data <- brand_data %>% slice((n_train+1):n_total)
  
  xreg_train <- cbind(
    Spend_lag = train_data$log_Spend[1:(nrow(train_data)-1)]
  )
  xreg_test <- cbind(
    Spend_lag = test_data$log_Spend[1:(nrow(test_data)-1)]
  )
  
  # Contacts iškartota (atmetus 1-ąjį)
  ts_contacts_train <- ts(train_data$log_Contacts[2:nrow(train_data)], frequency = 12)
  ts_contacts_test <- ts(test_data$log_Contacts[2:nrow(test_data)], frequency = 12)
  
  if (nrow(xreg_train) != length(ts_contacts_train) | 
      nrow(xreg_test) != length(ts_contacts_test)) next
  
  d_value <- get_d_value(brand_i, "log_Contacts", stationary_choice_summary)
  
  fit <- tryCatch(
    auto.arima(ts_contacts_train, xreg = xreg_train, d = d_value, 
               seasonal = FALSE, stepwise = FALSE),
    error = function(e) NULL
  )
  
  if (is.null(fit)) next
  
  h <- length(ts_contacts_test)
  forecast_vals <- tryCatch(
    as.numeric(forecast(fit, xreg = xreg_test, h = h)$mean),
    error = function(e) NULL
  )
  
  if (is.null(forecast_vals) | any(is.na(forecast_vals))) next
  
  actual_vals <- as.numeric(ts_contacts_test)
  metrics <- calculate_metrics(actual_vals, forecast_vals)
  if (any(is.na(unlist(metrics)))) next
  
  spend_coef <- NA
  tryCatch({
    spend_coef <- coef(fit)["Spend_lag"]
  }, error = function(e) NULL)
  
  p <- fit$arma[1]; q <- fit$arma[2]
  
  results_a <- bind_rows(results_a, tibble(
    BRAND = brand_i,
    modelis = "ARIMAX(Contacts~Spend)",
    variable = "log_Contacts",
    RMSE = round(metrics$RMSE, 4),
    MAE = round(metrics$MAE, 4),
    MAPE = round(metrics$MAPE, 2),
    Spend_coef = round(spend_coef, 5),
    xreg = "Taip"
  ))
  
  cat( brand_i, " | RMSE=", metrics$RMSE %>% round(4), "\n", sep="")
}

print(results_a)

# ARIMAX: GT ~ Spend

results_b <- tibble()

for (brand_i in BRANDS) {
  
  brand_data <- panel_clean %>%
    filter(A_BRAND_E_CLEAN == brand_i) %>%
    arrange(YearMonth) %>%
    filter(!is.na(log_GT) & !is.na(log_Spend))
  
  if (nrow(brand_data) < 24) next
  
  n_total <- nrow(brand_data)
  n_train <- floor(n_total * 0.80)
  
  train_data <- brand_data %>% slice(1:n_train)
  test_data <- brand_data %>% slice((n_train+1):n_total)
  
  xreg_train <- cbind(
    Spend_lag = train_data$log_Spend[1:(nrow(train_data)-1)]
  )
  xreg_test <- cbind(
    Spend_lag = test_data$log_Spend[1:(nrow(test_data)-1)]
  )
  
  ts_gt_train <- ts(train_data$log_GT[2:nrow(train_data)], frequency = 12)
  ts_gt_test <- ts(test_data$log_GT[2:nrow(test_data)], frequency = 12)
  
  if (nrow(xreg_train) != length(ts_gt_train) | 
      nrow(xreg_test) != length(ts_gt_test)) next
  
  d_value <- get_d_value(brand_i, "log_GT", stationary_choice_summary)  
  fit <- tryCatch(
    auto.arima(ts_gt_train, xreg = xreg_train, d = d_value, 
               seasonal = FALSE, stepwise = FALSE),
    error = function(e) NULL
  )
  
  if (is.null(fit)) next
  
  h <- length(ts_gt_test)
  forecast_vals <- tryCatch(
    as.numeric(forecast(fit, xreg = xreg_test, h = h)$mean),
    error = function(e) NULL
  )
  
  if (is.null(forecast_vals) | any(is.na(forecast_vals))) next
  
  actual_vals <- as.numeric(ts_gt_test)
  metrics <- calculate_metrics(actual_vals, forecast_vals)
  if (any(is.na(unlist(metrics)))) next
  
  spend_coef <- NA
  tryCatch({
    spend_coef <- coef(fit)["Spend_lag"]
  }, error = function(e) NULL)
  
  p <- fit$arma[1]; q <- fit$arma[2]
  
  results_b <- bind_rows(results_b, tibble(
    BRAND = brand_i,
    modelis = "ARIMAX(GT~Spend)",
    variable = "log_GT",
    RMSE = round(metrics$RMSE, 4),
    MAE = round(metrics$MAE, 4),
    MAPE = round(metrics$MAPE, 2),
    Spend_coef = round(spend_coef, 5),
    xreg = "Taip"
  ))
  
  cat( brand_i, " | RMSE=", metrics$RMSE %>% round(4), "\n", sep="")
}

print(results_b)

# ARIMA: z_k ATSKIRAI PAGAL FORMULĘ z_{k,t} = y_t / x_{t-k}
# z1 = GT_t / Spend_{t-1}
# z2 = GT_t / Spend_{t-2}
# z3 = GT_t / Spend_{t-3}


results_c <- tibble()

Z_VARIABLES <- c("z1", "z2", "z3")

for (brand_i in BRANDS) {
  
  cat("  ", brand_i, ":\n", sep = "")
  
  brand_data <- panel_clean %>%
    filter(A_BRAND_E_CLEAN == brand_i) %>%
    arrange(YearMonth) %>%
    filter(!is.na(z1) & !is.na(z2) & !is.na(z3))
  
  if (nrow(brand_data) < 24) next
  
  n_total <- nrow(brand_data)
  n_train <- floor(n_total * 0.80)
  
  train_data <- brand_data %>% slice(1:n_train)
  test_data <- brand_data %>% slice((n_train + 1):n_total)
  
  for (z_var in Z_VARIABLES) {
    
    # Naudojamas stationary_choice_summary
    d_value <- get_d_value(brand_i, z_var, stationary_choice_summary)
    
    ts_z_train <- ts(train_data[[z_var]], frequency = 12)
    ts_z_test <- ts(test_data[[z_var]], frequency = 12)
    
    if (any(is.na(ts_z_train)) | any(!is.finite(ts_z_train))) next
    if (any(is.na(ts_z_test)) | any(!is.finite(ts_z_test))) next
    
    fit_z <- tryCatch(
      auto.arima(
        ts_z_train,
        d = d_value,
        seasonal = FALSE,
        stepwise = FALSE
      ),
      error = function(e) NULL
    )
    
    if (is.null(fit_z)) next
    
    h <- length(ts_z_test)
    
    forecast_vals <- tryCatch(
      as.numeric(forecast(fit_z, h = h)$mean),
      error = function(e) NULL
    )
    
    if (is.null(forecast_vals) | any(is.na(forecast_vals))) next
    
    actual_vals <- as.numeric(ts_z_test)
    metrics <- calculate_metrics(actual_vals, forecast_vals)
    
    if (any(is.na(unlist(metrics)))) next
    
    p <- fit_z$arma[1]
    q <- fit_z$arma[2]
    
    results_c <- bind_rows(results_c, tibble(
      BRAND = brand_i,
      modelis = paste0("ARIMA(", z_var, ")"),
      variable = z_var,
      model_str = paste0("ARIMA(", p, ",", d_value, ",", q, ")"),
      RMSE = round(metrics$RMSE, 4),
      MAE = round(metrics$MAE, 4),
      MAPE = round(metrics$MAPE, 2),
      Spend_coef = NA_real_,
      xreg = "Ne"
    ))
    
    cat( z_var, " | d=", d_value,
        " | RMSE=", round(metrics$RMSE, 4), "\n", sep = "")
  }
  
  cat("\n")
}

cat("\n")
print(results_c)


abc_results <- bind_rows(results_a, results_b, results_c) %>%
  mutate(
    split = "80",
    train_ratio = 0.80,
    AIC = NA_real_,
    model_str = ifelse("model_str" %in% names(.), model_str, modelis)
  ) %>%
  dplyr::select(split, train_ratio, BRAND, variable, modelis, model_str,
                RMSE, MAE, MAPE, AIC, Spend_coef, xreg)

all_results_comparison <- bind_rows(
  all_results %>%
    mutate(
      Spend_coef = NA_real_,
      xreg = ifelse(modelis %in% c("ARIMAX", "SARIMAX"), "Taip", "Ne")
    ) %>%
    dplyr::select(split, train_ratio, BRAND, variable, modelis, model_str,
                  RMSE, MAE, MAPE, AIC, Spend_coef, xreg),
  abc_results
)



# palyginimas

cat("Visi modeliai (sorted by RMSE):\n")
print(all_results_comparison %>% 
        arrange(RMSE) %>%
        dplyr::select(split, BRAND, variable, modelis, RMSE, MAE, MAPE),
      n = 30)
cat("top 30(pagal RMSE):\n\n")

top30 <- all_results_comparison %>%
  arrange(RMSE) %>%
  slice(1:150)

print(top30 %>% 
        dplyr::select(split, BRAND, variable, RMSE, MAE, MAPE),
      n = 30)


#train trest 
summary_by_split <- all_results_comparison %>%
  group_by(split, modelis) %>%
  summarise(
    Sėkmingų = n(),
    Vid_RMSE = round(mean(RMSE, na.rm = TRUE), 4),
    Min_RMSE = round(min(RMSE, na.rm = TRUE), 4),
    Max_RMSE = round(max(RMSE, na.rm = TRUE), 4),
    Vid_MAPE = round(mean(MAPE, na.rm = TRUE), 2),
    .groups = 'drop'
  ) %>%
  arrange(split, Vid_RMSE)

print(summary_by_split)




# RMSE vs Split
p1 <- ggplot(all_results_comparison, aes(x = factor(split), y = RMSE, fill = modelis)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "RMSE pagal Train/Test Split",
       x = "Train/Test Split (%)", y = "RMSE", fill = "Modelis") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

#  Modelio tipai pagal RMSE
p2 <- ggplot(all_results_comparison, aes(x = reorder(modelis, RMSE, FUN = median), y = RMSE, fill = modelis)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_brewer(palette = "Dark2") +
  labs(title = "RMSE Palyginimas - Modeliai",
       x = "Modelis", y = "RMSE") +
  theme_minimal() +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, face = "bold"))

#  RMSE vs MAPE scatter
p3 <- ggplot(all_results_comparison, aes(x = RMSE, y = MAPE, color = split, shape = modelis)) +
  geom_point(size = 2.5, alpha = 0.6) +
  scale_color_manual(values = c("80" = "#185FA5", "70" = "#BA7517", "60" = "#C64545")) +
  labs(title = "RMSE vs MAPE pagal Split",
       x = "RMSE", y = "MAPE (%)", color = "Split %", shape = "Modelis") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


# Modelio skaičius pagal split
p4 <- all_results_comparison %>%
  group_by(split, modelis) %>%
  summarise(count = n(), .groups = 'drop') %>%
  ggplot(aes(x = split, y = count, fill = modelis)) +
  geom_col(alpha = 0.7, position = "dodge") +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Sėkmingų Modelių Skaičius",
       x = "Train/Test Split (%)", y = "Skaičius", fill = "Modelis") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

combined1 <- (p1 + p2) / (p3 + p4)
print(combined1)

# PAPILDOMI GRAFIKAI: FAKTINĖS REIKŠMĖS vs PROGNOZUOTOS

create_forecast_data <- function(actual, forecast, model_name, brand, variable, split_val, dates) {
  tibble(
    date = dates,
    time = 1:length(actual),
    Actual = as.numeric(actual),
    Forecast = as.numeric(forecast),
    Model = model_name,
    Brand = brand,
    Variable = variable,
    Split = split_val,
    Error = as.numeric(actual) - as.numeric(forecast),
    Abs_Error = abs(as.numeric(actual) - as.numeric(forecast))
  )
}


# ARIMA - FAKTINĖS vs PROGNOZUOTOS (visą SPLIT)


arima_forecast_data <- tibble()

for (split_name in names(data_splits)) {
  for (key in names(data_splits[[split_name]])) {
    
    data <- data_splits[[split_name]][[key]]
    d_value <- get_d_value(data$brand, data$variable, stationary_choice_summary)
    
    fit <- tryCatch(
      auto.arima(data$ts_train, d = d_value, seasonal = FALSE, stepwise = FALSE),
      error = function(e) NULL
    )
    
    if (is.null(fit)) next
    
    h <- data$n_test
    forecast_vals <- tryCatch(
      as.numeric(forecast(fit, h = h)$mean),
      error = function(e) NULL
    )
    
    if (is.null(forecast_vals) | any(is.na(forecast_vals))) next
    
    actual_vals <- as.numeric(data$ts_test)
    if (any(is.na(actual_vals))) next
    
    metrics <- calculate_metrics(actual_vals, forecast_vals)
    if (any(is.na(unlist(metrics)))) next
    
    split_val <- gsub("split_", "", split_name)
    
    arima_forecast_data <- bind_rows(
      arima_forecast_data,
      create_forecast_data(
        actual_vals, forecast_vals, "ARIMA",
        data$brand, data$variable, split_val
      )
    )
  }
}

cat("ARIMA prognozės duomenys paruošti (n=", nrow(arima_forecast_data), " eilučių)\n", sep="")

# Keletas pavyzdžių - grafikas
if (nrow(arima_forecast_data) > 0) {
  
  # Pasirinkti du skirtingus brand/variable derinius
  sample_data <- arima_forecast_data %>%
    distinct(Brand, Variable, Split) %>%
    slice(1:2)
  
  p_arima_list <- list()
  
  for (i in 1:nrow(sample_data)) {
    brand_sel <- sample_data$Brand[i]
    var_sel <- sample_data$Variable[i]
    split_sel <- sample_data$Split[i]
    
    plot_data <- arima_forecast_data %>%
      filter(Brand == brand_sel, Variable == var_sel, Split == split_sel)
    
    if (nrow(plot_data) > 0) {
      p <- ggplot(plot_data, aes(x = time)) +
        geom_line(aes(y = Actual, color = "Faktinė"), linewidth = 1) +
        geom_line(aes(y = Forecast, color = "Prognozuota"), linetype = "dashed", linewidth = 1) +
        geom_point(aes(y = Actual, color = "Faktinė"), size = 2, alpha = 0.6) +
        scale_color_manual(values = c("Faktinė" = "#1f77b4", "Prognozuota" = "#ff7f0e")) +
        labs(
          title = paste0("ARIMA: ", brand_sel, " - ", var_sel, " (Split: ", split_sel, "%)"),
          x = "Laiko žingsniai (mėnesiai)",
          y = "Reikšmė",
          color = "Tipo"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 11),
          legend.position = "bottom"
        )
      
      p_arima_list[[i]] <- p
    }
  }
  
  if (length(p_arima_list) > 0) {
    combined_arima <- wrap_plots(p_arima_list, ncol = 1)
    print(combined_arima)
  }
}


# VAR - FAKTINĖS vs PROGNOZUOTOS (multivariatiniu)


cat(" VAR Modeliai - Faktinės vs Prognozuotos (multivariate)\n")


var_forecast_data <- tibble()

for (split_name in names(data_splits)) {
  for (brand_i in BRANDS) {
    
    data_keys <- names(data_splits[[split_name]])
    brand_keys <- data_keys[grepl(paste0("^", brand_i), data_keys)]
    
    if (length(brand_keys) < 3) next
    
    data <- data_splits[[split_name]][[brand_keys[1]]]
    
    data_train <- cbind(
      log_Spend = data$train_data$log_Spend,
      log_GT = data$train_data$log_GT,
      log_Contacts = data$train_data$log_Contacts
    )
    
    data_test <- cbind(
      log_Spend = data$test_data$log_Spend,
      log_GT = data$test_data$log_GT,
      log_Contacts = data$test_data$log_Contacts
    )
    
    lag_select <- tryCatch(
      VARselect(data_train, lag.max = 3, type = 'const'),
      error = function(e) NULL
    )
    
    if (is.null(lag_select)) next
    
    p_optimal <- as.numeric(lag_select$selection[1])
    
    fit <- tryCatch(
      VAR(data_train, p = p_optimal, type = 'const'),
      error = function(e) NULL
    )
    
    if (is.null(fit)) next
    
    forecast_obj <- tryCatch(
      predict(fit, n.ahead = nrow(data_test)),
      error = function(e) NULL
    )
    
    if (is.null(forecast_obj)) next
    
    split_val <- gsub("split_", "", split_name)
    
    for (var_idx in 1:3) {
      var_name <- colnames(data_test)[var_idx]
      forecast_vals <- forecast_obj$fcst[[var_idx]][, 1]
      actual_vals <- data_test[, var_idx]
      
      if (length(forecast_vals) != length(actual_vals)) next
      
      var_forecast_data <- bind_rows(
        var_forecast_data,
        create_forecast_data(
          actual_vals, forecast_vals, "VAR",
          brand_i, var_name, split_val
        )
      )
    }
  }
}

cat("VAR prognozės duomenys paruošti (n=", nrow(var_forecast_data), " eilučių)\n", sep="")


# VAR grafikai
if (nrow(var_forecast_data) > 0) {
  
  sample_var <- var_forecast_data %>%
    distinct(Brand, Variable, Split) %>%
    slice(1:2)
  
  p_var_list <- list()
  
  for (i in 1:nrow(sample_var)) {
    brand_sel <- sample_var$Brand[i]
    var_sel <- sample_var$Variable[i]
    split_sel <- sample_var$Split[i]
    
    plot_data <- var_forecast_data %>%
      filter(Brand == brand_sel, Variable == var_sel, Split == split_sel)
    
    if (nrow(plot_data) > 0) {
      p <- ggplot(plot_data, aes(x = time)) +
        geom_line(aes(y = Actual, color = "Faktinė"), linewidth = 1) +
        geom_line(aes(y = Forecast, color = "Prognozuota"), linetype = "dashed", linewidth = 1) +
        geom_point(aes(y = Actual, color = "Faktinė"), size = 2, alpha = 0.6) +
        scale_color_manual(values = c("Faktinė" = "#2ca02c", "Prognozuota" = "#d62728")) +
        labs(
          title = paste0("VAR: ", brand_sel, " - ", var_sel, " (Split: ", split_sel, "%)"),
          x = "Laiko žingsniai (mėnesiai)",
          y = "Reikšmė",
          color = "Tipo"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 11),
          legend.position = "bottom"
        )
      
      p_var_list[[i]] <- p
    }
  }
  
  if (length(p_var_list) > 0) {
    combined_var <- wrap_plots(p_var_list, ncol = 1)
    print(combined_var)
  }
}

cat(" Prognozės paklaida analiza")

all_forecast_errors <- bind_rows(arima_forecast_data, var_forecast_data)

if (nrow(all_forecast_errors) > 0) {
  
  # Paklaidos grafikai pagal split
  p_error1 <- all_forecast_errors %>%
    group_by(Split, Model) %>%
    summarise(Avg_Abs_Error = mean(Abs_Error, na.rm = TRUE), .groups = 'drop') %>%
    ggplot(aes(x = Split, y = Avg_Abs_Error, fill = Model)) +
    geom_col(alpha = 0.7, position = "dodge") +
    scale_fill_brewer(palette = "Set2") +
    labs(
      title = "Vidutinė Absoliuti Paklaida pagal Split",
      x = "Train/Test Split (%)",
      y = "MAE",
      fill = "Modelis"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  
  print(p_error1)
  
  # Paklaidos pasiskirstymas
  p_error2 <- all_forecast_errors %>%
    ggplot(aes(x = Model, y = Abs_Error, fill = Model)) +
    geom_boxplot(alpha = 0.7) +
    scale_fill_brewer(palette = "Dark2") +
    labs(
      title = "Paklaidos Pasiskirstymas pagal Modelį",
      x = "Modelis",
      y = "Absoliuti Paklaida",
      fill = "Modelis"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      legend.position = "none"
    )
  
  print(p_error2)
}


  
# TRAIN vs TEST PALYGINIMAS - PILNA ANALIZA

all_metrics_detailed <- tibble()


#  ARIMA - TRAIN + TEST

for (split_name in names(data_splits)) {
  for (key in names(data_splits[[split_name]])) {
    
    data <- data_splits[[split_name]][[key]]
    d_value <- get_d_value(data$brand, data$variable, stationary_choice_summary)
    
    fit <- tryCatch(
      auto.arima(data$ts_train, d = d_value, seasonal = FALSE, stepwise = FALSE),
      error = function(e) NULL
    )
    
    if (is.null(fit)) next
    
    # TRAIN FORECAST
    train_forecast_vals <- tryCatch(
      as.numeric(fitted(fit)),
      error = function(e) NULL
    )
    
    train_actual_vals <- as.numeric(data$ts_train)
    
    # TEST FORECAST
    h <- data$n_test
    test_forecast_vals <- tryCatch(
      as.numeric(forecast(fit, h = h)$mean),
      error = function(e) NULL
    )
    
    test_actual_vals <- as.numeric(data$ts_test)
    
    if (is.null(train_forecast_vals) | is.null(test_forecast_vals)) next
    if (any(is.na(train_forecast_vals)) | any(is.na(test_forecast_vals))) next
    if (any(is.na(train_actual_vals)) | any(is.na(test_actual_vals))) next
    
    train_metrics <- calculate_metrics(train_actual_vals, train_forecast_vals)
    test_metrics <- calculate_metrics(test_actual_vals, test_forecast_vals)
    
    if (any(is.na(unlist(train_metrics))) | any(is.na(unlist(test_metrics)))) next
    
    p <- fit$arma[1]; q <- fit$arma[2]
    

    all_metrics_detailed <- bind_rows(all_metrics_detailed, tibble(
      split = gsub("split_", "", split_name),
      BRAND = data$brand,
      variable = data$variable,
      modelis = "ARIMA",
      model_str = paste0("ARIMA(", p, ",", d_value, ",", q, ")"),
      
      # TRAIN
      train_n = data$n_train,
      train_RMSE = round(train_metrics$RMSE, 4),
      train_MAE = round(train_metrics$MAE, 4),
      train_MAPE = round(train_metrics$MAPE, 2),
      
      # TEST
      test_n = data$n_test,
      test_RMSE = round(test_metrics$RMSE, 4),
      test_MAE = round(test_metrics$MAE, 4),
      test_MAPE = round(test_metrics$MAPE, 2),
      
      # DELTA (overfitting indicator)
      delta_RMSE = round(test_metrics$RMSE - train_metrics$RMSE, 4),
      delta_MAE = round(test_metrics$MAE - train_metrics$MAE, 4),
      delta_MAPE = round(test_metrics$MAPE - train_metrics$MAPE, 2),
      
      AIC = round(AIC(fit), 2)
    ))
  }
}


#  SARIMA - TRAIN + TEST



for (split_name in names(data_splits)) {
  for (key in names(data_splits[[split_name]])) {
    
    data <- data_splits[[split_name]][[key]]
    d_value <- get_d_value(data$brand, data$variable, stationary_choice_summary)
    
    fit <- tryCatch(
      auto.arima(data$ts_train, d = d_value, seasonal = TRUE, stepwise = FALSE),
      error = function(e) NULL
    )
    
    if (is.null(fit)) next
    
    # TRAIN
    train_forecast_vals <- tryCatch(
      as.numeric(fitted(fit)),
      error = function(e) NULL
    )
    
    train_actual_vals <- as.numeric(data$ts_train)
    
    # TEST
    h <- data$n_test
    test_forecast_vals <- tryCatch(
      as.numeric(forecast(fit, h = h)$mean),
      error = function(e) NULL
    )
    
    test_actual_vals <- as.numeric(data$ts_test)
    
    if (is.null(train_forecast_vals) | is.null(test_forecast_vals)) next
    if (any(is.na(train_forecast_vals)) | any(is.na(test_forecast_vals))) next
    
    train_metrics <- calculate_metrics(train_actual_vals, train_forecast_vals)
    test_metrics <- calculate_metrics(test_actual_vals, test_forecast_vals)
    
    if (any(is.na(unlist(train_metrics))) | any(is.na(unlist(test_metrics)))) next
    
    p <- fit$arma[1]; q <- fit$arma[2]
    P <- fit$arma[3]; Q <- fit$arma[4]; D <- fit$arma[6]
    
    all_metrics_detailed <- bind_rows(all_metrics_detailed, tibble(
      split = gsub("split_", "", split_name),
      BRAND = data$brand,
      variable = data$variable,
      modelis = "SARIMA",
      model_str = paste0("SARIMA(", p, ",", d_value, ",", q, ")(", P, ",", D, ",", Q, ")12"),
      
      train_n = data$n_train,
      train_RMSE = round(train_metrics$RMSE, 4),
      train_MAE = round(train_metrics$MAE, 4),
      train_MAPE = round(train_metrics$MAPE, 2),
      
      test_n = data$n_test,
      test_RMSE = round(test_metrics$RMSE, 4),
      test_MAE = round(test_metrics$MAE, 4),
      test_MAPE = round(test_metrics$MAPE, 2),
      
      delta_RMSE = round(test_metrics$RMSE - train_metrics$RMSE, 4),
      delta_MAE = round(test_metrics$MAE - train_metrics$MAE, 4),
      delta_MAPE = round(test_metrics$MAPE - train_metrics$MAPE, 2),
      
      AIC = round(AIC(fit), 2)
    ))
  }
}



# ARIMAX - TRAIN + TEST


for (split_name in names(data_splits)) {
  for (key in names(data_splits[[split_name]])) {
    
    data <- data_splits[[split_name]][[key]]
    d_value <- get_d_value(data$brand, data$variable, stationary_choice_summary)
    
    xreg_train <- cbind(
      Spend_lag = data$train_data$log_Spend[1:(data$n_train-1)]
    )
    xreg_test <- cbind(
      Spend_lag = data$test_data$log_Spend[1:(data$n_test-1)]
    )
    
    ts_train_use <- data$ts_train[2:length(data$ts_train)]
    ts_test_use <- data$ts_test[2:length(data$ts_test)]
    
    if (nrow(xreg_train) != length(ts_train_use) || 
        nrow(xreg_test) != length(ts_test_use)) next
    
    fit <- tryCatch(
      auto.arima(ts_train_use, xreg = xreg_train, d = d_value, 
                 seasonal = FALSE, stepwise = FALSE),
      error = function(e) NULL
    )
    
    if (is.null(fit)) next
    
    # TRAIN
    train_forecast_vals <- tryCatch(
      as.numeric(fitted(fit)),
      error = function(e) NULL
    )
    
    train_actual_vals <- as.numeric(ts_train_use)
    
    # TEST
    h <- length(ts_test_use)
    forecast_obj <- tryCatch(
      forecast(fit, xreg = xreg_test, h = h),
      error = function(e) NULL
    )
    
    if (is.null(forecast_obj)) next
    
    test_forecast_vals <- as.numeric(forecast_obj$mean)
    test_actual_vals <- as.numeric(ts_test_use)
    
    if (any(is.na(train_forecast_vals)) | any(is.na(test_forecast_vals))) next
    
    train_metrics <- calculate_metrics(train_actual_vals, train_forecast_vals)
    test_metrics <- calculate_metrics(test_actual_vals, test_forecast_vals)
    
    if (any(is.na(unlist(train_metrics))) | any(is.na(unlist(test_metrics)))) next
    
    p <- fit$arma[1]; q <- fit$arma[2]
    
    all_metrics_detailed <- bind_rows(all_metrics_detailed, tibble(
      split = gsub("split_", "", split_name),
      BRAND = data$brand,
      variable = data$variable,
      modelis = "ARIMAX",
      model_str = paste0("ARIMAX(", p, ",", d_value, ",", q, ")+xreg"),
      
      train_n = nrow(xreg_train),
      train_RMSE = round(train_metrics$RMSE, 4),
      train_MAE = round(train_metrics$MAE, 4),
      train_MAPE = round(train_metrics$MAPE, 2),
      
      test_n = h,
      test_RMSE = round(test_metrics$RMSE, 4),
      test_MAE = round(test_metrics$MAE, 4),
      test_MAPE = round(test_metrics$MAPE, 2),
      
      delta_RMSE = round(test_metrics$RMSE - train_metrics$RMSE, 4),
      delta_MAE = round(test_metrics$MAE - train_metrics$MAE, 4),
      delta_MAPE = round(test_metrics$MAPE - train_metrics$MAPE, 2),
      
      AIC = round(AIC(fit), 2)
    ))
  }
}


#  SARIMAX - TRAIN + TEST


for (split_name in names(data_splits)) {
  for (key in names(data_splits[[split_name]])) {
    
    data <- data_splits[[split_name]][[key]]
    d_value <- get_d_value(data$brand, data$variable, stationary_choice_summary)
    
    if (data$n_train < 18) next
    
    xreg_train <- cbind(
      Spend_lag = data$train_data$log_Spend[1:(data$n_train-1)]
    )
    xreg_test <- cbind(
      Spend_lag = data$test_data$log_Spend[1:(data$n_test-1)]
    )
    
    ts_train_use <- data$ts_train[2:length(data$ts_train)]
    ts_test_use <- data$ts_test[2:length(data$ts_test)]
    
    if (nrow(xreg_train) != length(ts_train_use) || 
        nrow(xreg_test) != length(ts_test_use)) next
    
    fit <- tryCatch(
      auto.arima(ts_train_use, xreg = xreg_train, d = d_value, 
                 seasonal = TRUE, stepwise = FALSE, max.p = 2, max.q = 2),
      error = function(e) NULL
    )
    
    if (is.null(fit)) next
    
    # TRAIN
    train_forecast_vals <- tryCatch(
      as.numeric(fitted(fit)),
      error = function(e) NULL
    )
    
    train_actual_vals <- as.numeric(ts_train_use)
    
    # TEST
    h <- length(ts_test_use)
    forecast_obj <- tryCatch(
      forecast(fit, xreg = xreg_test, h = h),
      error = function(e) NULL
    )
    
    if (is.null(forecast_obj)) next
    
    test_forecast_vals <- as.numeric(forecast_obj$mean)
    test_actual_vals <- as.numeric(ts_test_use)
    
    if (length(test_forecast_vals) != length(test_actual_vals)) next
    if (any(is.na(train_forecast_vals)) | any(is.na(test_forecast_vals))) next
    
    train_metrics <- calculate_metrics(train_actual_vals, train_forecast_vals)
    test_metrics <- calculate_metrics(test_actual_vals, test_forecast_vals)
    
    if (any(is.na(unlist(train_metrics))) | any(is.na(unlist(test_metrics)))) next
    
    p <- fit$arma[1]; q <- fit$arma[2]
    P <- fit$arma[3]; Q <- fit$arma[4]; D <- fit$arma[6]
    
    all_metrics_detailed <- bind_rows(all_metrics_detailed, tibble(
      split = gsub("split_", "", split_name),
      BRAND = data$brand,
      variable = data$variable,
      modelis = "SARIMAX",
      model_str = paste0("SARIMAX(", p, ",", d_value, ",", q, ")(", P, ",", D, ",", Q, ")12+xreg"),
      
      train_n = nrow(xreg_train),
      train_RMSE = round(train_metrics$RMSE, 4),
      train_MAE = round(train_metrics$MAE, 4),
      train_MAPE = round(train_metrics$MAPE, 2),
      
      test_n = h,
      test_RMSE = round(test_metrics$RMSE, 4),
      test_MAE = round(test_metrics$MAE, 4),
      test_MAPE = round(test_metrics$MAPE, 2),
      
      delta_RMSE = round(test_metrics$RMSE - train_metrics$RMSE, 4),
      delta_MAE = round(test_metrics$MAE - train_metrics$MAE, 4),
      delta_MAPE = round(test_metrics$MAPE - train_metrics$MAPE, 2),
      
      AIC = round(AIC(fit), 2)
    ))
  }
}


# VAR - TRAIN + TEST


for (split_name in names(data_splits)) {
  for (brand_i in BRANDS) {
    
    data_keys <- names(data_splits[[split_name]])
    brand_keys <- data_keys[grepl(paste0("^", brand_i), data_keys)]
    
    if (length(brand_keys) < 3) next
    
    data <- data_splits[[split_name]][[brand_keys[1]]]
    
    data_train <- cbind(
      log_Spend = data$train_data$log_Spend,
      log_GT = data$train_data$log_GT,
      log_Contacts = data$train_data$log_Contacts
    )
    
    data_test <- cbind(
      log_Spend = data$test_data$log_Spend,
      log_GT = data$test_data$log_GT,
      log_Contacts = data$test_data$log_Contacts
    )
    
    lag_select <- tryCatch(
      VARselect(data_train, lag.max = 3, type = 'const'),
      error = function(e) NULL
    )
    
    if (is.null(lag_select)) next
    
    p_optimal <- as.numeric(lag_select$selection[1])
    
    fit <- tryCatch(
      VAR(data_train, p = p_optimal, type = 'const'),
      error = function(e) NULL
    )
    
    if (is.null(fit)) next
    
    # Skaičiuoti RMSE kiekvienam kintamajam
    for (var_idx in 1:3) {
      var_name <- colnames(data_test)[var_idx]
      
      # TRAIN predictions (fitted values)
      train_predictions <- tryCatch(
        fitted(fit)[, var_idx],
        error = function(e) NULL
      )
      
      train_actual <- data_train[, var_idx]
      
      # TEST predictions
      forecast_obj <- tryCatch(
        predict(fit, n.ahead = nrow(data_test)),
        error = function(e) NULL
      )
      
      if (is.null(forecast_obj)) next
      
      test_forecast_vals <- forecast_obj$fcst[[var_idx]][, 1]
      test_actual <- data_test[, var_idx]
      
      if (length(test_forecast_vals) != length(test_actual)) next
      if (is.null(train_predictions)) next
      
      train_metrics <- calculate_metrics(train_actual, train_predictions)
      test_metrics <- calculate_metrics(test_actual, test_forecast_vals)
      
      if (any(is.na(unlist(train_metrics))) | any(is.na(unlist(test_metrics)))) next
      
      all_metrics_detailed <- bind_rows(all_metrics_detailed, tibble(
        split = gsub("split_", "", split_name),
        BRAND = brand_i,
        variable = var_name,
        modelis = "VAR",
        model_str = paste0("VAR(", p_optimal, ")"),
        
        train_n = nrow(data_train),
        train_RMSE = round(train_metrics$RMSE, 4),
        train_MAE = round(train_metrics$MAE, 4),
        train_MAPE = round(train_metrics$MAPE, 2),
        
        test_n = nrow(data_test),
        test_RMSE = round(test_metrics$RMSE, 4),
        test_MAE = round(test_metrics$MAE, 4),
        test_MAPE = round(test_metrics$MAPE, 2),
        
        delta_RMSE = round(test_metrics$RMSE - train_metrics$RMSE, 4),
        delta_MAE = round(test_metrics$MAE - train_metrics$MAE, 4),
        delta_MAPE = round(test_metrics$MAPE - train_metrics$MAPE, 2),
        
        AIC = NA
      ))
    }
  }
}




#  ARIMAX(Contacts ~ Spend)
#  ARIMAX(GT ~ Spend)
# ARIMA(z1), ARIMA(z2), ARIMA(z3)

#  ARIMAX: log_Contacts ~ log_Spend
for (brand_i in BRANDS) {
  
  brand_data <- panel_clean %>%
    filter(A_BRAND_E_CLEAN == brand_i) %>%
    arrange(YearMonth) %>%
    filter(!is.na(log_Contacts) & !is.na(log_Spend))
  
  if (nrow(brand_data) < 24) next
  
  n_total <- nrow(brand_data)
  n_train <- floor(n_total * 0.80)
  
  train_data <- brand_data %>% slice(1:n_train)
  test_data <- brand_data %>% slice((n_train + 1):n_total)
  
  xreg_train <- cbind(Spend_lag = train_data$log_Spend[1:(nrow(train_data)-1)])
  xreg_test <- cbind(Spend_lag = test_data$log_Spend[1:(nrow(test_data)-1)])
  
  ts_train_use <- ts(train_data$log_Contacts[2:nrow(train_data)], frequency = 12)
  ts_test_use <- ts(test_data$log_Contacts[2:nrow(test_data)], frequency = 12)
  
  if (nrow(xreg_train) != length(ts_train_use) || nrow(xreg_test) != length(ts_test_use)) next
  
  d_value <- get_d_value(brand_i, "log_Contacts", stationary_choice_summary)
  
  fit <- tryCatch(
    auto.arima(ts_train_use, xreg = xreg_train, d = d_value,
               seasonal = FALSE, stepwise = FALSE),
    error = function(e) NULL
  )
  
  if (is.null(fit)) next
  
  train_forecast_vals <- as.numeric(fitted(fit))
  test_forecast_vals <- tryCatch(
    as.numeric(forecast(fit, xreg = xreg_test, h = length(ts_test_use))$mean),
    error = function(e) NULL
  )
  
  if (is.null(test_forecast_vals)) next
  
  train_metrics <- calculate_metrics(as.numeric(ts_train_use), train_forecast_vals)
  test_metrics <- calculate_metrics(as.numeric(ts_test_use), test_forecast_vals)
  
  if (any(is.na(unlist(train_metrics))) | any(is.na(unlist(test_metrics)))) next
  
  p <- fit$arma[1]
  q <- fit$arma[2]
  
  all_metrics_detailed <- bind_rows(all_metrics_detailed, tibble(
    split = "80",
    BRAND = brand_i,
    variable = "log_Contacts",
    modelis = "ARIMAX(Contacts~Spend)",
    model_str = paste0("ARIMAX(", p, ",", d_value, ",", q, ")+Spend_lag"),
    
    train_n = length(ts_train_use),
    train_RMSE = round(train_metrics$RMSE, 4),
    train_MAE = round(train_metrics$MAE, 4),
    train_MAPE = round(train_metrics$MAPE, 2),
    
    test_n = length(ts_test_use),
    test_RMSE = round(test_metrics$RMSE, 4),
    test_MAE = round(test_metrics$MAE, 4),
    test_MAPE = round(test_metrics$MAPE, 2),
    
    delta_RMSE = round(test_metrics$RMSE - train_metrics$RMSE, 4),
    delta_MAE = round(test_metrics$MAE - train_metrics$MAE, 4),
    delta_MAPE = round(test_metrics$MAPE - train_metrics$MAPE, 2),
    
    AIC = round(AIC(fit), 2)
  ))
}

# ARIMAX: log_GT ~ log_Spend
for (brand_i in BRANDS) {
  
  brand_data <- panel_clean %>%
    filter(A_BRAND_E_CLEAN == brand_i) %>%
    arrange(YearMonth) %>%
    filter(!is.na(log_GT) & !is.na(log_Spend))
  
  if (nrow(brand_data) < 24) next
  
  n_total <- nrow(brand_data)
  n_train <- floor(n_total * 0.80)
  
  train_data <- brand_data %>% slice(1:n_train)
  test_data <- brand_data %>% slice((n_train + 1):n_total)
  
  xreg_train <- cbind(Spend_lag = train_data$log_Spend[1:(nrow(train_data)-1)])
  xreg_test <- cbind(Spend_lag = test_data$log_Spend[1:(nrow(test_data)-1)])
  
  ts_train_use <- ts(train_data$log_GT[2:nrow(train_data)], frequency = 12)
  ts_test_use <- ts(test_data$log_GT[2:nrow(test_data)], frequency = 12)
  
  if (nrow(xreg_train) != length(ts_train_use) || nrow(xreg_test) != length(ts_test_use)) next
  
  d_value <- get_d_value(brand_i, "log_GT", stationary_choice_summary)
  
  fit <- tryCatch(
    auto.arima(ts_train_use, xreg = xreg_train, d = d_value,
               seasonal = FALSE, stepwise = FALSE),
    error = function(e) NULL
  )
  
  if (is.null(fit)) next
  
  train_forecast_vals <- as.numeric(fitted(fit))
  test_forecast_vals <- tryCatch(
    as.numeric(forecast(fit, xreg = xreg_test, h = length(ts_test_use))$mean),
    error = function(e) NULL
  )
  
  if (is.null(test_forecast_vals)) next
  
  train_metrics <- calculate_metrics(as.numeric(ts_train_use), train_forecast_vals)
  test_metrics <- calculate_metrics(as.numeric(ts_test_use), test_forecast_vals)
  
  if (any(is.na(unlist(train_metrics))) | any(is.na(unlist(test_metrics)))) next
  
  p <- fit$arma[1]
  q <- fit$arma[2]
  
  all_metrics_detailed <- bind_rows(all_metrics_detailed, tibble(
    split = "80",
    BRAND = brand_i,
    variable = "log_GT",
    modelis = "ARIMAX(GT~Spend)",
    model_str = paste0("ARIMAX(", p, ",", d_value, ",", q, ")+Spend_lag"),
    
    train_n = length(ts_train_use),
    train_RMSE = round(train_metrics$RMSE, 4),
    train_MAE = round(train_metrics$MAE, 4),
    train_MAPE = round(train_metrics$MAPE, 2),
    
    test_n = length(ts_test_use),
    test_RMSE = round(test_metrics$RMSE, 4),
    test_MAE = round(test_metrics$MAE, 4),
    test_MAPE = round(test_metrics$MAPE, 2),
    
    delta_RMSE = round(test_metrics$RMSE - train_metrics$RMSE, 4),
    delta_MAE = round(test_metrics$MAE - train_metrics$MAE, 4),
    delta_MAPE = round(test_metrics$MAPE - train_metrics$MAPE, 2),
    
    AIC = round(AIC(fit), 2)
  ))
}

#  ARIMA: z1, z2, z3
Z_VARIABLES <- c("z1", "z2", "z3")

for (brand_i in BRANDS) {
  
  brand_data <- panel_clean %>%
    filter(A_BRAND_E_CLEAN == brand_i) %>%
    arrange(YearMonth) %>%
    filter(!is.na(z1) & !is.na(z2) & !is.na(z3))
  
  if (nrow(brand_data) < 24) next
  
  n_total <- nrow(brand_data)
  n_train <- floor(n_total * 0.80)
  
  train_data <- brand_data %>% slice(1:n_train)
  test_data <- brand_data %>% slice((n_train + 1):n_total)
  
  for (z_var in Z_VARIABLES) {
    
    d_value <- get_d_value(brand_i, z_var, stationary_choice_summary)
    
    ts_train_use <- ts(train_data[[z_var]], frequency = 12)
    ts_test_use <- ts(test_data[[z_var]], frequency = 12)
    
    if (any(is.na(ts_train_use)) | any(!is.finite(ts_train_use))) next
    if (any(is.na(ts_test_use)) | any(!is.finite(ts_test_use))) next
    
    fit <- tryCatch(
      auto.arima(ts_train_use, d = d_value, seasonal = FALSE, stepwise = FALSE),
      error = function(e) NULL
    )
    
    if (is.null(fit)) next
    
    train_forecast_vals <- as.numeric(fitted(fit))
    test_forecast_vals <- tryCatch(
      as.numeric(forecast(fit, h = length(ts_test_use))$mean),
      error = function(e) NULL
    )
    
    if (is.null(test_forecast_vals)) next
    
    train_metrics <- calculate_metrics(as.numeric(ts_train_use), train_forecast_vals)
    test_metrics <- calculate_metrics(as.numeric(ts_test_use), test_forecast_vals)
    
    if (any(is.na(unlist(train_metrics))) | any(is.na(unlist(test_metrics)))) next
    
    p <- fit$arma[1]
    q <- fit$arma[2]
    
    all_metrics_detailed <- bind_rows(all_metrics_detailed, tibble(
      split = "80",
      BRAND = brand_i,
      variable = z_var,
      modelis = paste0("ARIMA(", z_var, ")"),
      model_str = paste0("ARIMA(", p, ",", d_value, ",", q, ")"),
      
      train_n = length(ts_train_use),
      train_RMSE = round(train_metrics$RMSE, 4),
      train_MAE = round(train_metrics$MAE, 4),
      train_MAPE = round(train_metrics$MAPE, 2),
      
      test_n = length(ts_test_use),
      test_RMSE = round(test_metrics$RMSE, 4),
      test_MAE = round(test_metrics$MAE, 4),
      test_MAPE = round(test_metrics$MAPE, 2),
      
      delta_RMSE = round(test_metrics$RMSE - train_metrics$RMSE, 4),
      delta_MAE = round(test_metrics$MAE - train_metrics$MAE, 4),
      delta_MAPE = round(test_metrics$MAPE - train_metrics$MAPE, 2),
      
      AIC = round(AIC(fit), 2)
    ))
  }
}


# TRAIN vs TEST PALYGINIMAS 



all_metrics_sorted <- all_metrics_detailed[order(all_metrics_detailed$test_RMSE), ]

# Top 20
top20_full <- all_metrics_sorted[1:20, c(
  "BRAND", "variable", "modelis", 
  "train_RMSE", "train_MAE", "train_MAPE",
  "test_RMSE", "test_MAE", "test_MAPE",
  "delta_RMSE"
)]

print(top20_full)


#  PAGAL MODELĮ - VIDUTINIAI


unique_models <- unique(all_metrics_detailed$modelis)

by_model_list <- lapply(unique_models, function(mdl) {
  subset_data <- all_metrics_detailed[all_metrics_detailed$modelis == mdl, ]
  
  data.frame(
    modelis = mdl,
    n = nrow(subset_data),
    train_RMSE = round(mean(subset_data$train_RMSE, na.rm = TRUE), 4),
    train_MAE = round(mean(subset_data$train_MAE, na.rm = TRUE), 4),
    train_MAPE = round(mean(subset_data$train_MAPE, na.rm = TRUE), 2),
    test_RMSE = round(mean(subset_data$test_RMSE, na.rm = TRUE), 4),
    test_MAE = round(mean(subset_data$test_MAE, na.rm = TRUE), 4),
    test_MAPE = round(mean(subset_data$test_MAPE, na.rm = TRUE), 2),
    delta_RMSE = round(mean(subset_data$delta_RMSE, na.rm = TRUE), 4),
    delta_MAE = round(mean(subset_data$delta_MAE, na.rm = TRUE), 4),
    delta_MAPE = round(mean(subset_data$delta_MAPE, na.rm = TRUE), 2),
    stringsAsFactors = FALSE
  )
})

by_model <- do.call(rbind, by_model_list)
by_model <- by_model[order(by_model$test_RMSE), ]
rownames(by_model) <- NULL

print(by_model)


#z1 Z2 Z3 BE DEPO  


unique_brands <- unique(all_metrics_detailed$BRAND)

by_brand_list <- lapply(unique_brands, function(brnd) {
  subset_data <- all_metrics_detailed[all_metrics_detailed$BRAND == brnd, ]
  
  data.frame(
    BRAND = brnd,
    n = nrow(subset_data),
    train_RMSE = round(mean(subset_data$train_RMSE, na.rm = TRUE), 4),
    train_MAE = round(mean(subset_data$train_MAE, na.rm = TRUE), 4),
    train_MAPE = round(mean(subset_data$train_MAPE, na.rm = TRUE), 2),
    test_RMSE = round(mean(subset_data$test_RMSE, na.rm = TRUE), 4),
    test_MAE = round(mean(subset_data$test_MAE, na.rm = TRUE), 4),
    test_MAPE = round(mean(subset_data$test_MAPE, na.rm = TRUE), 2),
    delta_RMSE = round(mean(subset_data$delta_RMSE, na.rm = TRUE), 4),
    delta_MAE = round(mean(subset_data$delta_MAE, na.rm = TRUE), 4),
    delta_MAPE = round(mean(subset_data$delta_MAPE, na.rm = TRUE), 2),
    stringsAsFactors = FALSE
  )
})

by_brand <- do.call(rbind, by_brand_list)
by_brand <- by_brand[order(by_brand$test_RMSE), ]
rownames(by_brand) <- NULL

print(by_brand)



# brand + modelis
by_brand_model_list <- list()

for (brnd in unique_brands) {
  for (mdl in unique_models) {
    subset_data <- all_metrics_detailed[
      all_metrics_detailed$BRAND == brnd & all_metrics_detailed$modelis == mdl, 
    ]
    
    if (nrow(subset_data) > 0) {
      by_brand_model_list[[paste0(brnd, "_", mdl)]] <- data.frame(
        BRAND = brnd,
        modelis = mdl,
        n = nrow(subset_data),
        train_RMSE = round(mean(subset_data$train_RMSE, na.rm = TRUE), 4),
        train_MAE = round(mean(subset_data$train_MAE, na.rm = TRUE), 4),
        train_MAPE = round(mean(subset_data$train_MAPE, na.rm = TRUE), 2),
        test_RMSE = round(mean(subset_data$test_RMSE, na.rm = TRUE), 4),
        test_MAE = round(mean(subset_data$test_MAE, na.rm = TRUE), 4),
        test_MAPE = round(mean(subset_data$test_MAPE, na.rm = TRUE), 2),
        delta_RMSE = round(mean(subset_data$delta_RMSE, na.rm = TRUE), 4),
        stringsAsFactors = FALSE
      )
    }
  }
}

by_brand_model <- do.call(rbind, by_brand_model_list)
by_brand_model <- by_brand_model[order(by_brand_model$BRAND, by_brand_model$test_RMSE), ]
rownames(by_brand_model) <- NULL

print(by_brand_model, n = 100)


##########################
unique_vars <- unique(all_metrics_detailed$variable)
unique_models <- unique(all_metrics_detailed$modelis)
unique_brands <- unique(all_metrics_detailed$BRAND)

by_brand_var_model_list <- list()

for (brnd in unique_brands) {
  for (var in unique_vars) {
    for (mdl in unique_models) {
      subset_data <- all_metrics_detailed[
        all_metrics_detailed$BRAND == brnd & 
          all_metrics_detailed$variable == var & 
          all_metrics_detailed$modelis == mdl, 
      ]
      
      if (nrow(subset_data) > 0) {
        by_brand_var_model_list[[paste0(brnd, "_", var, "_", mdl)]] <- data.frame(
          BRAND = brnd,
          variable = var,
          modelis = mdl,
          n = nrow(subset_data),
          
          # RMSE 
          train_RMSE = round(mean(subset_data$train_RMSE, na.rm = TRUE), 4),
          test_RMSE = round(mean(subset_data$test_RMSE, na.rm = TRUE), 4),
          delta_RMSE = round(mean(subset_data$delta_RMSE, na.rm = TRUE), 4),
          
          # MAE 
          train_MAE = round(mean(subset_data$train_MAE, na.rm = TRUE), 4),
          test_MAE = round(mean(subset_data$test_MAE, na.rm = TRUE), 4),
          delta_MAE = round(mean(subset_data$delta_MAE, na.rm = TRUE), 4),
          
          #  MAPE
          train_MAPE = round(mean(subset_data$train_MAPE, na.rm = TRUE), 2),
          test_MAPE = round(mean(subset_data$test_MAPE, na.rm = TRUE), 2),
          delta_MAPE = round(mean(subset_data$delta_MAPE, na.rm = TRUE), 2),
          
          stringsAsFactors = FALSE,
          row.names = NULL
        )
      }
    }
  }
}

by_brand_var_model <- do.call(rbind, by_brand_var_model_list)
by_brand_var_model <- by_brand_var_model[order(by_brand_var_model$BRAND, 
                                               by_brand_var_model$variable,
                                               by_brand_var_model$test_RMSE), ]
rownames(by_brand_var_model) <- NULL


for (i in 1:nrow(by_brand_var_model)) {
  row <- by_brand_var_model[i, ]
  cat(sprintf(
    "%-12s %-15s %-10s | RMSE: %6.4f/%6.4f (delta%6.4f) | MAE: %6.4f/%6.4f (delta%6.4f) | MAPE: %6.2f/%6.2f (delta%6.2f)\n",
    row$BRAND, row$variable, row$modelis,
    row$train_RMSE, row$test_RMSE, row$delta_RMSE,
    row$train_MAE, row$test_MAE, row$delta_MAE,
    row$train_MAPE, row$test_MAPE, row$delta_MAPE
  ))
}
cat("\n")





for (brnd in unique_brands) {
  brand_data <- by_brand_var_model[by_brand_var_model$BRAND == brnd, ]
  
  if (nrow(brand_data) > 0) {
    cat(sprintf(" %s:\n", brnd))

    brand_data_sorted <- brand_data[order(brand_data$test_RMSE), ]
    
    for (j in 1:min(3, nrow(brand_data_sorted))) {
      row <- brand_data_sorted[j, ]
      cat(sprintf(
        "  %d. %s + %s (%s): TEST RMSE %.4f (delta%.4f)\n",
        j, row$variable, row$modelis, 
        if(row$delta_RMSE < 1) "geras" else "netinka",
        row$test_RMSE, row$delta_RMSE
      ))
    }
    cat("\n")
  }
}


for (var in unique_vars) {
  var_data <- by_brand_var_model[by_brand_var_model$variable == var, ]
  
  if (nrow(var_data) > 0) {
    cat(sprintf(" %s:\n", var))

    var_data_sorted <- var_data[order(var_data$test_RMSE), ]
    
    for (j in 1:min(3, nrow(var_data_sorted))) {
      row <- var_data_sorted[j, ]
      cat(sprintf(
        "  %d. %s + %s (%s): TEST RMSE %.4f (delta%.4f)\n",
        j, row$BRAND, row$modelis,
        if(row$delta_RMSE < 1) "geras" else "netinka",
        row$test_RMSE, row$delta_RMSE
      ))
    }
    cat("\n")
  }
}



for (mdl in unique_models) {
  mdl_data <- by_brand_var_model[by_brand_var_model$modelis == mdl, ]
  
  if (nrow(mdl_data) > 0) {
    cat(sprintf("%s:\n", mdl))

    mdl_data_sorted <- mdl_data[order(mdl_data$test_RMSE), ]
    
    for (j in 1:min(3, nrow(mdl_data_sorted))) {
      row <- mdl_data_sorted[j, ]
      cat(sprintf(
        "  %d. %s + %s (%s): TEST RMSE %.4f (delta%.4f)\n",
        j, row$BRAND, row$variable,
        if(row$delta_RMSE < 1) "geras" else "netinka",
        row$test_RMSE, row$delta_RMSE
      ))
    }
    cat("\n")
  }
}







#  GERIAUSI MODELIAI - MAŽIAUSIAS delta


all_metrics_detailed$abs_delta_RMSE <- abs(all_metrics_detailed$delta_RMSE)
all_metrics_sorted_delta <- all_metrics_detailed[order(all_metrics_detailed$abs_delta_RMSE), ]

best_models <- all_metrics_sorted_delta[1:15, c(
  "BRAND", "variable", "modelis",
  "train_RMSE", "test_RMSE", "delta_RMSE",
  "train_MAE", "test_MAE", "delta_MAE",
  "train_MAPE", "test_MAPE", "delta_MAPE"
)]

print(best_models)
cat("\n")


# PRASČIAUSI MODELIAI - DIDŽIAUSIAS delta


worst_models <- all_metrics_sorted_delta[
  (nrow(all_metrics_sorted_delta) - 9):nrow(all_metrics_sorted_delta), 
  c(
    "BRAND", "variable", "modelis",
    "train_RMSE", "test_RMSE", "delta_RMSE",
    "train_MAE", "test_MAE", "delta_MAE",
    "train_MAPE", "test_MAPE", "delta_MAPE"
  )
]

print(worst_models)
cat("\n")


#  STATISTIKA



summary_stats <- data.frame(
  Rodiklis = c("RMSE", "MAE", "MAPE (%)"),
  
  Train_Vidurkis = c(
    round(mean(all_metrics_detailed$train_RMSE, na.rm = TRUE), 4),
    round(mean(all_metrics_detailed$train_MAE, na.rm = TRUE), 4),
    round(mean(all_metrics_detailed$train_MAPE, na.rm = TRUE), 2)
  ),
  
  Train_Mediana = c(
    round(median(all_metrics_detailed$train_RMSE, na.rm = TRUE), 4),
    round(median(all_metrics_detailed$train_MAE, na.rm = TRUE), 4),
    round(median(all_metrics_detailed$train_MAPE, na.rm = TRUE), 2)
  ),
  
  Test_Vidurkis = c(
    round(mean(all_metrics_detailed$test_RMSE, na.rm = TRUE), 4),
    round(mean(all_metrics_detailed$test_MAE, na.rm = TRUE), 4),
    round(mean(all_metrics_detailed$test_MAPE, na.rm = TRUE), 2)
  ),
  
  Test_Mediana = c(
    round(median(all_metrics_detailed$test_RMSE, na.rm = TRUE), 4),
    round(median(all_metrics_detailed$test_MAE, na.rm = TRUE), 4),
    round(median(all_metrics_detailed$test_MAPE, na.rm = TRUE), 2)
  ),
  
  Delta_Vidurkis = c(
    round(mean(all_metrics_detailed$delta_RMSE, na.rm = TRUE), 4),
    round(mean(all_metrics_detailed$delta_MAE, na.rm = TRUE), 4),
    round(mean(all_metrics_detailed$delta_MAPE, na.rm = TRUE), 2)
  ),
  
  stringsAsFactors = FALSE
)

print(summary_stats)


library(ggplot2, warn.conflicts = FALSE)
library(patchwork, warn.conflicts = FALSE)

# Paruošiami duomenys grafikams
mokymo_testavimo_rmse <- rbind(
  data.frame(imtis = "Mokymo", modelis = all_metrics_detailed$modelis, reikšmė = all_metrics_detailed$train_RMSE),
  data.frame(imtis = "Testavimo", modelis = all_metrics_detailed$modelis, reikšmė = all_metrics_detailed$test_RMSE)
)

mokymo_testavimo_mae <- rbind(
  data.frame(imtis = "Mokymo", modelis = all_metrics_detailed$modelis, reikšmė = all_metrics_detailed$train_MAE),
  data.frame(imtis = "Testavimo", modelis = all_metrics_detailed$modelis, reikšmė = all_metrics_detailed$test_MAE)
)

#  RMSE
p1 <- ggplot(mokymo_testavimo_rmse, aes(x = modelis, y = reikšmė, fill = imtis)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = c("Mokymo" = "#2E86AB", "Testavimo" = "#A23B72")) +
  labs(
    title = "Mokymo ir testavimo imčių RMSE palyginimas",
    x = "Modelis",
    y = "RMSE",
    fill = "Imtis"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# MAE
p2 <- ggplot(mokymo_testavimo_mae, aes(x = modelis, y = reikšmė, fill = imtis)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = c("Mokymo" = "#2E86AB", "Testavimo" = "#A23B72")) +
  labs(
    title = "Mokymo ir testavimo imčių MAE palyginimas",
    x = "Modelis",
    y = "MAE",
    fill = "Imtis"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

#  modelio persimokymas pagal Delta RMSE
by_model_delta <- by_model[order(by_model$test_RMSE), ]

p3 <- ggplot(
  by_model_delta,
  aes(
    x = reorder(modelis, delta_RMSE),
    y = delta_RMSE,
    fill = delta_RMSE > 1
  )
) +
  geom_col(alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 1) +
  geom_hline(yintercept = 1, linetype = "dotted", color = "orange", size = 1) +
  scale_fill_manual(values = c("TRUE" = "#C64545", "FALSE" = "#06A77D")) +
  labs(
    title = "Modelių persimokymo vertinimas pagal Delta RMSE",
    x = "Modelis",
    y = "Delta RMSE"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Grafikų sujungimas
bendras_grafikas <- (p1 + p2) / p3
print(bendras_grafikas)





# RMSE pagal train/test split
p1 <- ggplot(all_results, aes(x = factor(split), y = RMSE, fill = modelis)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "RMSE pagal mokymo ir testavimo imties santykį",
    x = "Mokymo imties dalis (%)",
    y = "RMSE",
    fill = "Modelis"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

#  Modelių palyginimas pagal RMSE
p2 <- ggplot(all_results, aes(x = reorder(modelis, RMSE, FUN = median), y = RMSE, fill = modelis)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_brewer(palette = "Dark2") +
  labs(
    title = "Modelių palyginimas pagal RMSE",
    x = "Modelis",
    y = "RMSE"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

#  RMSE ir MAPE ryšys
p3 <- ggplot(all_results, aes(x = RMSE, y = MAPE, color = factor(split), shape = modelis)) +
  geom_point(size = 2.5, alpha = 0.6) +
  scale_color_manual(values = c("80" = "#185FA5", "70" = "#BA7517", "60" = "#C64545")) +
  labs(
    title = "RMSE ir MAPE ryšys pagal imties padalijimą",
    x = "RMSE",
    y = "MAPE (%)",
    color = "Mokymo imtis (%)",
    shape = "Modelis"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

p4 <- all_results %>%
  group_by(split, modelis) %>%
  summarise(kiekis = n(), .groups = "drop") %>%
  ggplot(aes(x = factor(split), y = kiekis, fill = modelis)) +
  geom_col(alpha = 0.7, position = "dodge") +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Sėkmingai įvertintų modelių skaičius",
    x = "Mokymo imties dalis (%)",
    y = "Modelių skaičius",
    fill = "Modelis"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

combined <- (p1 + p2) / (p3 + p4)
print(combined)


plot_data <- all_results_comparison %>%
  filter(!is.na(RMSE), !is.na(MAPE))

p1 <- ggplot(plot_data, aes(x = factor(split), y = RMSE, fill = modelis)) +
  geom_boxplot(alpha = 0.7) +
  labs(
    title = "RMSE pagal mokymo ir testavimo imties santykį",
    x = "Mokymo imties dalis (%)",
    y = "RMSE",
    fill = "Modelis"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

p2 <- ggplot(plot_data, aes(x = reorder(modelis, RMSE, FUN = median), y = RMSE, fill = modelis)) +
  geom_boxplot(alpha = 0.7) +
  labs(
    title = "Modelių palyginimas pagal RMSE",
    x = "Modelis",
    y = "RMSE"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

p3 <- ggplot(plot_data, aes(x = RMSE, y = MAPE, color = factor(split), shape = modelis)) +
  geom_point(size = 2.5, alpha = 0.6) +
  labs(
    title = "RMSE ir MAPE ryšys pagal imties padalijimą",
    x = "RMSE",
    y = "MAPE (%)",
    color = "Mokymo imtis (%)",
    shape = "Modelis"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

p4 <- plot_data %>%
  group_by(split, modelis) %>%
  summarise(kiekis = n(), .groups = "drop") %>%
  ggplot(aes(x = factor(split), y = kiekis, fill = modelis)) +
  geom_col(alpha = 0.7, position = "dodge") +
  labs(
    title = "Sėkmingai įvertintų modelių skaičius",
    x = "Mokymo imties dalis (%)",
    y = "Modelių skaičius",
    fill = "Modelis"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

combined <- (p1 + p2) / (p3 + p4)
print(combined)




# prognoze / realus duomenys 

create_forecast_data <- function(actual, forecast, model_name, brand, variable, split_val, dates) {
  tibble(
    date = as.Date(dates),
    time = 1:length(actual),
    Actual = as.numeric(actual),
    Forecast = as.numeric(forecast),
    Model = model_name,
    Brand = brand,
    Variable = variable,
    Split = split_val,
    Error = as.numeric(actual) - as.numeric(forecast),
    Abs_Error = abs(as.numeric(actual) - as.numeric(forecast))
  )
}

all_forecast_data <- tibble()


#  ARIMA, ARIMAX, SARIMA, SARIMAX


for (split_name in names(data_splits)) {
  for (key in names(data_splits[[split_name]])) {
    
    data <- data_splits[[split_name]][[key]]
    split_val <- gsub("split_", "", split_name)
    
    d_value <- get_d_value(data$brand, data$variable, stationary_choice_summary)
    h <- data$n_test
    
    actual_vals <- as.numeric(data$ts_test)
    dates_test <- data$test_data$YearMonth
    
    if (any(is.na(actual_vals))) next
    if (length(dates_test) != length(actual_vals)) next
    
    # ---------------- ARIMA 
    fit_arima <- tryCatch(
      auto.arima(data$ts_train, d = d_value, seasonal = FALSE, stepwise = FALSE),
      error = function(e) NULL
    )
    
    if (!is.null(fit_arima)) {
      forecast_vals <- tryCatch(
        as.numeric(forecast(fit_arima, h = h)$mean),
        error = function(e) NULL
      )
      
      if (!is.null(forecast_vals) && length(forecast_vals) == length(actual_vals)) {
        all_forecast_data <- bind_rows(
          all_forecast_data,
          create_forecast_data(
            actual_vals, forecast_vals, "ARIMA",
            data$brand, data$variable, split_val,
            dates_test
          )
        )
      }
    }
    
    # ---------------- SARIMA 
    fit_sarima <- tryCatch(
      auto.arima(data$ts_train, d = d_value, seasonal = TRUE, stepwise = FALSE),
      error = function(e) NULL
    )
    
    if (!is.null(fit_sarima)) {
      forecast_vals <- tryCatch(
        as.numeric(forecast(fit_sarima, h = h)$mean),
        error = function(e) NULL
      )
      
      if (!is.null(forecast_vals) && length(forecast_vals) == length(actual_vals)) {
        all_forecast_data <- bind_rows(
          all_forecast_data,
          create_forecast_data(
            actual_vals, forecast_vals, "SARIMA",
            data$brand, data$variable, split_val,
            dates_test
          )
        )
      }
    }
    
    # ---------------- ARIMAX
    if (!is.null(data$xreg_train) && !is.null(data$xreg_test)) {
      
      fit_arimax <- tryCatch(
        auto.arima(
          data$ts_train,
          xreg = data$xreg_train,
          d = d_value,
          seasonal = FALSE,
          stepwise = FALSE
        ),
        error = function(e) NULL
      )
      
      if (!is.null(fit_arimax)) {
        forecast_vals <- tryCatch(
          as.numeric(forecast(fit_arimax, xreg = data$xreg_test, h = h)$mean),
          error = function(e) NULL
        )
        
        if (!is.null(forecast_vals) && length(forecast_vals) == length(actual_vals)) {
          all_forecast_data <- bind_rows(
            all_forecast_data,
            create_forecast_data(
              actual_vals, forecast_vals, "ARIMAX",
              data$brand, data$variable, split_val,
              dates_test
            )
          )
        }
      }
    }
    
    # ---------------- SARIMAX
    if (!is.null(data$xreg_train) && !is.null(data$xreg_test)) {
      
      fit_sarimax <- tryCatch(
        auto.arima(
          data$ts_train,
          xreg = data$xreg_train,
          d = d_value,
          seasonal = TRUE,
          stepwise = FALSE
        ),
        error = function(e) NULL
      )
      
      if (!is.null(fit_sarimax)) {
        forecast_vals <- tryCatch(
          as.numeric(forecast(fit_sarimax, xreg = data$xreg_test, h = h)$mean),
          error = function(e) NULL
        )
        
        if (!is.null(forecast_vals) && length(forecast_vals) == length(actual_vals)) {
          all_forecast_data <- bind_rows(
            all_forecast_data,
            create_forecast_data(
              actual_vals, forecast_vals, "SARIMAX",
              data$brand, data$variable, split_val,
              dates_test
            )
          )
        }
      }
    }
  }
}


#  VAR MODELIS


for (split_name in names(data_splits)) {
  for (brand_i in BRANDS) {
    
    data_keys <- names(data_splits[[split_name]])
    brand_keys <- data_keys[grepl(paste0("^", brand_i), data_keys)]
    
    if (length(brand_keys) < 3) next
    
    data <- data_splits[[split_name]][[brand_keys[1]]]
    split_val <- gsub("split_", "", split_name)
    
    data_train <- cbind(
      log_Spend = data$train_data$log_Spend,
      log_GT = data$train_data$log_GT,
      log_Contacts = data$train_data$log_Contacts
    )
    
    data_test <- cbind(
      log_Spend = data$test_data$log_Spend,
      log_GT = data$test_data$log_GT,
      log_Contacts = data$test_data$log_Contacts
    )
    
    dates_test <- data$test_data$YearMonth
    
    lag_select <- tryCatch(
      VARselect(data_train, lag.max = 3, type = "const"),
      error = function(e) NULL
    )
    
    if (is.null(lag_select)) next
    
    p_optimal <- as.numeric(lag_select$selection[1])
    
    fit_var <- tryCatch(
      VAR(data_train, p = p_optimal, type = "const"),
      error = function(e) NULL
    )
    
    if (is.null(fit_var)) next
    
    forecast_obj <- tryCatch(
      predict(fit_var, n.ahead = nrow(data_test)),
      error = function(e) NULL
    )
    
    if (is.null(forecast_obj)) next
    
    for (var_idx in 1:3) {
      
      var_name <- colnames(data_test)[var_idx]
      actual_vals <- data_test[, var_idx]
      forecast_vals <- forecast_obj$fcst[[var_idx]][, 1]
      
      if (length(actual_vals) != length(forecast_vals)) next
      if (length(dates_test) != length(actual_vals)) next
      
      all_forecast_data <- bind_rows(
        all_forecast_data,
        create_forecast_data(
          actual_vals, forecast_vals, "VAR",
          brand_i, var_name, split_val,
          dates_test
        )
      )
    }
  }
}




#  Contacts~Spend,  GT~Spend,  z1,z2,z3


# ARIMAX: log_Contacts ~ log_Spend_lag
for (brand_i in BRANDS) {
  
  brand_data <- panel_clean %>%
    filter(A_BRAND_E_CLEAN == brand_i) %>%
    arrange(YearMonth) %>%
    filter(!is.na(log_Contacts) & !is.na(log_Spend))
  
  if (nrow(brand_data) < 24) next
  
  n_total <- nrow(brand_data)
  n_train <- floor(n_total * 0.80)
  
  train_data <- brand_data %>% slice(1:n_train)
  test_data <- brand_data %>% slice((n_train + 1):n_total)
  
  xreg_train <- cbind(Spend_lag = train_data$log_Spend[1:(nrow(train_data)-1)])
  xreg_test <- cbind(Spend_lag = test_data$log_Spend[1:(nrow(test_data)-1)])
  
  actual_vals <- as.numeric(test_data$log_Contacts[2:nrow(test_data)])
  dates_test <- test_data$YearMonth[2:nrow(test_data)]
  
  ts_train_use <- ts(train_data$log_Contacts[2:nrow(train_data)], frequency = 12)
  
  d_value <- get_d_value(brand_i, "log_Contacts", stationary_choice_summary)
  
  fit <- tryCatch(
    auto.arima(ts_train_use, xreg = xreg_train, d = d_value,
               seasonal = FALSE, stepwise = FALSE),
    error = function(e) NULL
  )
  
  if (is.null(fit)) next
  
  forecast_vals <- tryCatch(
    as.numeric(forecast(fit, xreg = xreg_test, h = length(actual_vals))$mean),
    error = function(e) NULL
  )
  
  if (is.null(forecast_vals)) next
  
  all_forecast_data <- bind_rows(
    all_forecast_data,
    create_forecast_data(
      actual_vals, forecast_vals,
      "ARIMAX(Contacts~Spend)",
      brand_i, "log_Contacts", "80",
      dates_test
    )
  )
}

#  ARIMAX: log_GT ~ log_Spend_lag
for (brand_i in BRANDS) {
  
  brand_data <- panel_clean %>%
    filter(A_BRAND_E_CLEAN == brand_i) %>%
    arrange(YearMonth) %>%
    filter(!is.na(log_GT) & !is.na(log_Spend))
  
  if (nrow(brand_data) < 24) next
  
  n_total <- nrow(brand_data)
  n_train <- floor(n_total * 0.80)
  
  train_data <- brand_data %>% slice(1:n_train)
  test_data <- brand_data %>% slice((n_train + 1):n_total)
  
  xreg_train <- cbind(Spend_lag = train_data$log_Spend[1:(nrow(train_data)-1)])
  xreg_test <- cbind(Spend_lag = test_data$log_Spend[1:(nrow(test_data)-1)])
  
  actual_vals <- as.numeric(test_data$log_GT[1:nrow(test_data)])
  dates_test <- test_data$YearMonth[1:nrow(test_data)]
  
  ts_train_use <- ts(train_data$log_GT[1:nrow(train_data)], frequency = 12)
  
  d_value <- get_d_value(brand_i, "log_GT", stationary_choice_summary)
  
  fit <- tryCatch(
    auto.arima(ts_train_use, xreg = xreg_train, d = d_value,
               seasonal = FALSE, stepwise = FALSE),
    error = function(e) NULL
  )
  
  if (is.null(fit)) next
  
  forecast_vals <- tryCatch(
    as.numeric(forecast(fit, xreg = xreg_test, h = length(actual_vals))$mean),
    error = function(e) NULL
  )
  
  if (is.null(forecast_vals)) next
  
  all_forecast_data <- bind_rows(
    all_forecast_data,
    create_forecast_data(
      actual_vals, forecast_vals,
      "ARIMAX(GT~Spend)",
      brand_i, "log_GT", "80",
      dates_test
    )
  )
}

#  z1, z2, z3
Z_VARIABLES <- c("z1", "z2", "z3")

for (brand_i in BRANDS) {
  
  brand_data <- panel_clean %>%
    filter(A_BRAND_E_CLEAN == brand_i) %>%
    arrange(YearMonth) %>%
    filter(!is.na(z1) & !is.na(z2) & !is.na(z3))
  
  if (nrow(brand_data) < 24) next
  
  n_total <- nrow(brand_data)
  n_train <- floor(n_total * 0.80)
  
  train_data <- brand_data %>% slice(1:n_train)
  test_data <- brand_data %>% slice((n_train + 1):n_total)
  
  for (z_var in Z_VARIABLES) {
    
    actual_vals <- as.numeric(test_data[[z_var]])
    dates_test <- test_data$YearMonth
    
    d_value <- get_d_value(brand_i, z_var, stationary_choice_summary)
    
    ts_train_use <- ts(train_data[[z_var]], frequency = 12)
    
    fit <- tryCatch(
      auto.arima(ts_train_use, d = d_value, seasonal = FALSE, stepwise = FALSE),
      error = function(e) NULL
    )
    
    if (is.null(fit)) next
    
    forecast_vals <- tryCatch(
      as.numeric(forecast(fit, h = length(actual_vals))$mean),
      error = function(e) NULL
    )
    
    if (is.null(forecast_vals)) next
    
    all_forecast_data <- bind_rows(
      all_forecast_data,
      create_forecast_data(
        actual_vals, forecast_vals,
        paste0("ARIMA(", z_var, ")"),
        brand_i, z_var, "80",
        dates_test
      )
    )
  }
}




all_forecast_data_clean <- all_forecast_data %>%
  filter(
    is.finite(Actual),
    is.finite(Forecast),
    !is.na(date),
    date <= as.Date("2025-06-30")  
  )
print(sort(unique(all_forecast_data_clean$date)))

print(table(all_forecast_data_clean$Model))


format_ym <- function(x) {
  format(x, "%Y-%m")
}


p_gt <- all_forecast_data_clean %>%
  filter(Variable == "log_GT") %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = Actual, color = "Faktinė"), linewidth = 0.9) +
  geom_line(aes(y = Forecast, color = "Prognozuota"), linetype = "dashed", linewidth = 0.9) +
  geom_point(aes(y = Actual, color = "Faktinė"), size = 1.6, alpha = 0.7) +
  facet_grid(Brand ~ Model, scales = "free_y") +
  scale_color_manual(values = c("Faktinė" = "#1f77b4", "Prognozuota" = "#d62728")) +
  scale_x_date(
    breaks = sort(unique(all_forecast_data_clean$date)),
    labels = format_ym
  ) +
  labs(
    title = "log(GT): faktinių ir prognozuotų reikšmių palyginimas",
    x = "Mėnuo",
    y = "log(GT)",
    color = "Tipas"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(p_gt)







p_spend <- all_forecast_data_clean %>%
  filter(Variable == "log_Spend") %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = Actual, color = "Faktinė"), linewidth = 0.9) +
  geom_line(aes(y = Forecast, color = "Prognozuota"), linetype = "dashed", linewidth = 0.9) +
  geom_point(aes(y = Actual, color = "Faktinė"), size = 1.6, alpha = 0.7) +
  facet_grid(Brand ~ Model, scales = "free_y") +
  scale_color_manual(values = c("Faktinė" = "#1f77b4", "Prognozuota" = "#d62728")) +
  scale_x_date(
    breaks = sort(unique(all_forecast_data_clean$date)),
    labels = format_ym
  ) +
  labs(
    title = "log(Išlaidos): faktinių ir prognozuotų reikšmių palyginimas",
    x = "Mėnuo",
    y = "log(Išlaidos)",
    color = "Tipas"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(p_spend)





p_contacts <- all_forecast_data_clean %>%
  filter(Variable == "log_Contacts") %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = Actual, color = "Faktinė"), linewidth = 0.9) +
  geom_line(aes(y = Forecast, color = "Prognozuota"), linetype = "dashed", linewidth = 0.9) +
  geom_point(aes(y = Actual, color = "Faktinė"), size = 1.6, alpha = 0.7) +
  facet_grid(Brand ~ Model, scales = "free_y") +
  scale_color_manual(values = c("Faktinė" = "#1f77b4", "Prognozuota" = "#d62728")) +
  scale_x_date(
    breaks = sort(unique(all_forecast_data_clean$date)),
    labels = format_ym
  ) +
  labs(
    title = "log(Kontaktai): faktinių ir prognozuotų reikšmių palyginimas",
    x = "Mėnuo",
    y = "log(Kontaktai)",
    color = "Tipas"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(p_contacts)


table(all_forecast_data$Variable)
table(all_forecast_data_clean$Variable)

z_data <- all_forecast_data %>%
  filter(
    Variable %in% c("z1", "z2", "z3"),
    is.finite(Actual),
    is.finite(Forecast),
    !is.na(date),
    date <= as.Date("2025-06-30"),
    date >= as.Date("2024-10-01")
  )

print(table(z_data$Variable))
print(range(z_data$date))



p_z <- z_data %>%
  filter(Variable %in% c("z1", "z2", "z3")) %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = Actual, color = "Faktinė"), linewidth = 0.9) +
  geom_line(aes(y = Forecast, color = "Prognozuota"), linetype = "dashed", linewidth = 0.9) +
  geom_point(aes(y = Actual, color = "Faktinė"), size = 1.6, alpha = 0.7) +
  facet_grid(Brand ~ Model + Variable, scales = "free_y") +
  scale_color_manual(values = c("Faktinė" = "#1f77b4", "Prognozuota" = "#d62728")) +
  scale_x_date(
    breaks = sort(unique(z_data$date)),
    labels = format_ym
  ) +
  labs(
    title = "z reikšmės: faktinių ir prognozuotų reikšmių palyginimas",
    x = "Mėnuo",
    y = "z reikšmė",
    color = "Tipas"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(p_z)


