# =====================================================================
# OLA 1 – Opgave 2: Korrelation og simpel lineær regression
# =====================================================================
# Opgave 2 bygger på de rensede boligdata fra opgave 1 (boliger).
# source() kører hele "OLA 1 opgave 1.R" først, så boliger findes, før opgave 2 starter.
source("OLA 1 opgave 1.R", encoding = "UTF-8")                                  # kør opgave 1 først (læser og renser data)


# =====================================================================
# Opgave 2.1: Beskrivende statistik
# =====================================================================
numeriske_variable <- c("pris", "kvadratmeterpris", "boligareal_m2", "maanedlig_udgift",
                        "grund_m2", "vaerelser", "liggetid_dage", "opfoert_aar")

summary(boliger[numeriske_variable])                                            # min, kvartiler, median, gennemsnit og max

# samme tal samlet i én tabel med standardafvigelse. na.rm = TRUE: spring NA over (fx liggetid)
beskrivende_statistik <- data.frame(
  variabel       = numeriske_variable,
  minimum        = sapply(boliger[numeriske_variable], min,    na.rm = TRUE),
  median         = sapply(boliger[numeriske_variable], median, na.rm = TRUE),
  gennemsnit     = sapply(boliger[numeriske_variable], mean,   na.rm = TRUE),
  maksimum       = sapply(boliger[numeriske_variable], max,    na.rm = TRUE),
  standardafvig  = sapply(boliger[numeriske_variable], sd,     na.rm = TRUE),
  row.names = NULL
)
beskrivende_statistik
# Prisens gennemsnit (3,06 mio. kr.) ligger over medianen (2,20 mio. kr.): nogle meget dyre boliger
# trækker gennemsnittet op. Der er også mærkelige værdier, fx en grund på 4,1 mio. m² og et byggeår på 1200.

# Y = kvadratmeterpris, fordi den tager højde for boligens størrelse og gør boligerne sammenlignelige.
# X'er = variable, der teoretisk kan påvirke prisen pr. m²: værelser, grund, boligareal,
# månedlig udgift, byggeår og postnummer (beliggenhed).


# =====================================================================
# Opgave 2.2: Korrelation
# =====================================================================
cor(boliger$boligareal_m2, boliger$pris, use = "complete.obs")                 # complete.obs: spring boliger uden m² over
# 0,39: en moderat positiv sammenhæng. Større boliger koster typisk mere, men beliggenhed betyder også meget

# Korrelation måler styrken og retningen af den lineære sammenhæng mellem to variable.
# Den ligger mellem -1 og 1: tæt på 1 = stærk positiv sammenhæng, tæt på -1 = stærk negativ,
# og tæt på 0 = ingen lineær sammenhæng. Korrelation siger ikke noget om årsag.


# =====================================================================
# Opgave 2.3: Simple regressioner
# =====================================================================
# 5 simple regressioner med kvadratmeterprisen som y – én x ad gangen i et loop
x_variable <- c("vaerelser", "grund_m2", "boligareal_m2", "maanedlig_udgift", "opfoert_aar")

regressioner <- data.frame()                                                    # tom tabel, som fyldes i loopet
for (x in x_variable) {                                                         # loop: variabel for variabel
  model <- lm(boliger$kvadratmeterpris ~ boliger[[x]])                           # kvadratmeterpris forklaret af én x
  regressioner <- bind_rows(regressioner, data.frame(
    x_variabel = x,
    haeldning  = coef(model)[2],                                                 # hvor meget kvadratmeterprisen ændrer sig, når x stiger 1
    R2         = summary(model)$r.squared,                                       # forklaringsgrad
    p_vaerdi   = summary(model)$coefficients[2, 4],                              # er hældningen signifikant?
    row.names = NULL
  ))
}
regressioner |> arrange(desc(R2))                                                # den model, der forklarer mest, øverst
# månedlig udgift forklarer mest (R2 0,47). De andre forklarer under 2 %.
# Forbehold: den månedlige udgift indeholder ejendomsskat, som afhænger af boligens værdi,
# så den er nærmest en del af prisen selv

# Ekstra: postnummer som kategori (factor), så hvert postnummer får sit eget niveau
model_postnr <- lm(kvadratmeterpris ~ factor(postnr), data = boliger)
summary(model_postnr)$r.squared
# R2 0,70: beliggenhed forklarer mest af alle, men modellen har én variabel pr. postnummer,
# så R2 bliver let for højt (overfitting)

# ---- Ekstra: postnummer opdelt i de 5 regioner ----
# Færre grupper end postnumrene, så modellen ikke overfitter. Opdelingen er en tilnærmelse:
# postnumre følger ikke regionsgrænserne helt (fx Greve/Solrød 2670-2690 hører til Sjælland,
# Hedensted 7130-7171 til Midtjylland)
boliger_med_region <- boliger |>
  mutate(region = case_when(
    between(postnr, 1000, 3799) ~ "Hovedstaden",                                  # København, Nordsjælland og Bornholm (37xx)
    between(postnr, 4000, 4999) ~ "Sjælland",
    between(postnr, 5000, 6899) ~ "Syddanmark",                                   # Fyn og Sønderjylland
    between(postnr, 7000, 7299) ~ "Syddanmark",                                   # Fredericia, Vejle, Billund
    between(postnr, 6900, 6999) ~ "Midtjylland",                                  # Skjern og Ringkøbing
    between(postnr, 7300, 7699) ~ "Midtjylland",                                  # Herning, Holstebro, Struer
    between(postnr, 7800, 8999) ~ "Midtjylland",                                  # Skive, Aarhus, Randers, Viborg
    between(postnr, 7700, 7799) ~ "Nordjylland",                                  # Thisted
    between(postnr, 7900, 9999) ~ "Nordjylland"                                   # Mors, Aalborg, Hjørring
  ))

boliger_med_region |> count(region)                                             # TJEK: hvor mange boliger i hver region, og ingen NA

boliger_med_region |>
  group_by(region) |>
  summarise(median_kvadratmeterpris = median(kvadratmeterpris, na.rm = TRUE)) |>
  arrange(desc(median_kvadratmeterpris))                                         # hvilken region er dyrest pr. m²

model_region <- lm(kvadratmeterpris ~ region, data = boliger_med_region)         # region bliver selv lavet om til factor
summary(model_region)                                                            # Hovedstaden er reference (først alfabetisk)
summary(model_region)$r.squared
# R2 ...: skriv tallet her og sammenlign med 0,70 fra model_postnr.
# Koefficienterne viser, hvor meget lavere kvadratmeterprisen er end i Hovedstaden.
# Region har kun 4 variable i stedet for én pr. postnummer, så modellen er mere robust,
# men vi mister forskellen inde i regionen (København og Bornholm er i samme gruppe)

# ---- Korrelationsmatrix: hænger de 5 x'er sammen med hinanden? ----
korrelationsmatrix <- boliger |>
  select(kvadratmeterpris, all_of(x_variable)) |>
  cor(use = "complete.obs") |>                                                   # complete.obs: brug kun rækker uden NA
  round(2)
korrelationsmatrix
# værelser og boligareal hænger stærkt sammen (0,70): store boliger har flere værelser.
# månedlig udgift og kvadratmeterpris hænger stærkt sammen (0,70), som i regressionen


# =====================================================================
# Opgave 2.4: Korrelation og lineær regression
# =====================================================================
# Begge måler den lineære sammenhæng mellem to variable. Korrelationen (r) giver styrke og retning,
# mens regressionen giver en linje: y = b0 + b1 * x, der viser, hvor meget y ændrer sig, når x stiger 1.
# I en simpel lineær regression er R2 = r^2. Det kan tjekkes:
cor(boliger$kvadratmeterpris, boliger$boligareal_m2, use = "complete.obs")^2
regressioner |> filter(x_variabel == "boligareal_m2") |> pull(R2)
# de to tal er ens