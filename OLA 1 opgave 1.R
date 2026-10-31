# =====================================================================
# OLA 1 – Opgave 1: Data Science Modellen
# =====================================================================
# Opgave 1 står for sig selv: den læser newhomes-data, renser dem og finder boligerne.
# Opgave 2 bygger videre på den med source("OLA 1 opgave 1.R").

library(tidyverse)   # indeholder readr (read_csv), dplyr (filter, mutate ...) og ggplot2

# ---- Læs data ----
# col_character() = læs alt som tekst, så R ikke gætter forkert på tallene
newhomes <- read_csv("newhomes.csv", col_types = cols(.default = col_character()))
nrow(newhomes)
# 11.900 rækker
head(newhomes, 3)
# alt er tekst fra hjemmesiden, fx pris = "3.200.000 kr.", kvm = "126 m²" og vaer = "3 Vær."

# ---- NA-værdier og dubletter ----
colSums(is.na(newhomes))                                                         # antal NA i hver kolonne
# 200 rækker er helt tomme (ingen bolig_id)
sum(duplicated(newhomes$bolig_id[!is.na(newhomes$bolig_id)]))                    # samme bolig flere gange
# 361 dubletter: samme bolig er hentet flere gange
sum(newhomes$liggetid == "-", na.rm = TRUE)
# 82 boliger har "-" som liggetid, altså ingen liggetid på hjemmesiden

# ---- Rens data: lav tekst om til tal ----
# gsub("[^0-9]", "", x) fjerner alt, der ikke er tal: "3.200.000 kr." -> "3200000"
# grepl() tjekker, at teksten er det, vi forventer. Ellers bliver værdien NA, fordi nogle
# boliger har fået oplysningerne i den forkerte kolonne (se opgave 1.3)
boliger <- newhomes |>
  filter(!is.na(bolig_id)) |>                                                    # fjern tomme rækker
  distinct(bolig_id, .keep_all = TRUE) |>                                        # hver bolig kun én gang
  mutate(pris             = as.numeric(gsub("[^0-9]", "", pris)),
         boligareal_m2    = ifelse(grepl("^[0-9.]+ m", kvm),  as.numeric(gsub("[^0-9]", "", kvm)),     NA),   # "126 m²" -> 126
         maanedlig_udgift = ifelse(grepl("kr./md", ejerudg),  as.numeric(gsub("[^0-9]", "", ejerudg)), NA),   # "Ejerudg. 2.967 kr./md" -> 2967
         grund_m2         = ifelse(grepl("^Grund", grund),    as.numeric(gsub("[^0-9]", "", grund)),   NA),   # "Grund 785 m²" -> 785
         vaerelser        = ifelse(grepl("Vær", vaer),        as.numeric(gsub("[^0-9]", "", vaer)),    NA),   # "3 Vær." -> 3
         liggetid_dage    = ifelse(grepl("dag", liggetid),    as.numeric(gsub("[^0-9]", "", liggetid)), NA),  # "11 dage" -> 11
         opfoert_aar      = ifelse(grepl("Opført", alder),    as.numeric(gsub("[^0-9]", "", alder)),   NA),   # "Opført 1974" -> 1974
         postnr           = as.numeric(sub(".*([0-9]{4}) .*", "\\1", zip)),                                  # "Vivild, 8961 Allingåbro" -> 8961
         by               = sub(".*[0-9]{4} ", "", zip),                                                     # -> "Allingåbro"
         kvadratmeterpris = pris / boligareal_m2,                                                            # pris pr. m² (findes ikke i data)
         link             = paste0("https://www.boligsiden.dk", addr)) |>                                    # boligens side på boligsiden.dk
  select(bolig_id, link, by, postnr, type, energi, pris, kvadratmeterpris, boligareal_m2,
         maanedlig_udgift, grund_m2, vaerelser, liggetid_dage, opfoert_aar)
nrow(boliger)
# 11.339 forskellige boliger

colSums(is.na(boliger))
# fx mangler grund for mange ejerlejligheder, og liggetid mangler for boliger med "-"


# =====================================================================
# Opgave 1.1: Find data
# =====================================================================
# De to huse fra billederne i opgaven: Tousvej 106 i Aabyhøj og Egevej 20 i Vejle
boliger |> filter(grepl("tousvej-106", link))
boliger |> filter(grepl("egevej-20-7100", link))
# 0 rækker: de to huse findes ikke i newhomes-filen.
# Søger jeg på deres størrelse og grund (120 m² og 883 m², 170 m² og 737 m²), finder jeg dem heller ikke:
boliger |> filter((boligareal_m2 == 120 & grund_m2 == 883) | (boligareal_m2 == 170 & grund_m2 == 737))
# Billederne passer altså ikke til denne csv-fil. Det viser, hvor vigtigt det er at vide,
# hvornår og hvordan data er hentet.


# =====================================================================
# Opgave 1.2: Vælg
# =====================================================================
# To boliger fra filen. link giver direkte adressen på boligsiden.dk
boliger |> filter(grepl("elmekrogen-19-3200", link)) |> select(link, pris, boligareal_m2, grund_m2, vaerelser)
# Elmekrogen 19, Helsinge: 3.200.000 kr., 126 m², grund 785 m², 3 værelser
boliger |> filter(grepl("platanalle-11-6000", link)) |> select(link, pris, boligareal_m2, grund_m2, vaerelser)
# Platanalle 11, Kolding: 1.245.000 kr., 100 m², grund 979 m², 4 værelser
# Åbn linket og sammenlign med boligsiden.dk (boligen kan være solgt siden)


# =====================================================================
# Opgave 1.3: Beskriv variabler
# =====================================================================
# NA-værdier:
# - 200 rækker er helt tomme: webscrapingen har ikke fået noget med for dem.
# - Nogle boliger har oplysningerne i den forkerte kolonne. En ejerlejlighed har fx ingen grund,
#   så liggetiden ender i kolonnen grund. Webscrapingen har læst felterne i rækkefølge i stedet
#   for efter navn. Derfor tjekker jeg med grepl(), at hver værdi er det, den skal være.
# - Liggetid "-" betyder, at boligsiden.dk ikke viser en liggetid, fx for nye boliger.
# - 361 dubletter: samme bolig er hentet flere gange.
# Manglende variable sammenlignet med boligsiden.dk:
# - Kvadratmeterprisen er ikke hentet med, så den skal regnes ud (pris / m²).
# - Dato for udtrækket: pris og liggetid ændrer sig, så uden dato kan man ikke sammenligne.
# - Boligskat og mægler.
# Unik identifikation: newhomes har bolig_id og adressen (addr), så hver bolig kan findes
# på boligsiden.dk. Men dubletterne viser, at man også skal fjerne gentagelser.


# =====================================================================
# Opgave 1.4: Research goal
# =====================================================================
# Data Science Modellens skridt frem til csv-filen:
# 1. Research goal: et formål med at indsamle data
# 2. Retrieving data: webscraping af boligsiden.dk
# 3. Data preparation: data samlet i en csv-fil (her renser jeg den videre i R)
# Mit research goal: "Som dataanalytiker vil jeg indsamle data om boliger til salg, så jeg kan
# undersøge, hvilke faktorer der påvirker prisen pr. m²."
# Med det mål på forhånd ville man vide, at kvadratmeterpris, dato og boligtype skulle hentes,
# og at hvert felt skulle gemmes efter navn, så værdier ikke ender i den forkerte kolonne.