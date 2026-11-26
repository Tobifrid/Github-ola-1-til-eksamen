# =====================================================================
# OLA 1 – Opgave 2: Korrelation og simpel lineær regression (RETTET: landsdele og overfitting-tjek)
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


# ---- Ekstra: postnummer som kategori (factor) ----
# Hvert postnummer får sit eget niveau, altså én dummy-variabel pr. postnummer
model_postnr <- lm(kvadratmeterpris ~ factor(postnr), data = boliger)
length(coef(model_postnr)) - 1                                                   # antal x-variable i modellen (ca. én pr. postnummer)
summary(model_postnr)$r.squared                                                  # R2 ca. 0,70
summary(model_postnr)$adj.r.squared                                              # justeret R2: straffer for mange x'er
# Mange postnumre har kun få boliger. Så tilpasser modellen sig de få boliger (støj) i stedet for
# den generelle sammenhæng: det er overfitting. Det tjekkes med train/test nedenfor.


# ---- Ekstra: postnummer opdelt i DST's 11 landsdele ----
# Landsdele er grupper af kommuner, ikke af postnumre. Opdelingen herunder er derfor en tilnærmelse,
# hvor postnumrene er lagt i den landsdel, som de fleste af dem hører til.
# Først får alle boliger NA, og derefter sætter hver linje en landsdel for et interval af postnumre.
# Hver linje overskriver de tidligere, så den SIDSTE linje, der passer, vinder (omvendt af case_when). Derfor: brede intervaller først, undtagelser til sidst.
boliger_med_landsdel <- boliger                                                  # kopi af boligerne
boliger_med_landsdel$landsdel <- NA                                              # start: ingen landsdel
postnr <- boliger_med_landsdel$postnr                                            # kort navn, så linjerne kan læses
# -- de brede intervaller først --
boliger_med_landsdel$landsdel[postnr >= 1000 & postnr <= 2500] <- "Byen København"        # København, Frederiksberg og Valby
boliger_med_landsdel$landsdel[postnr >= 2600 & postnr <= 2959] <- "Københavns omegn"      # fx Glostrup, Herlev, Lyngby, Gentofte
boliger_med_landsdel$landsdel[postnr >= 3000 & postnr <= 3699] <- "Nordsjælland"          # Helsingør, Hillerød, Frederikssund
boliger_med_landsdel$landsdel[postnr >= 3700 & postnr <= 3799] <- "Bornholm"
boliger_med_landsdel$landsdel[postnr >= 4000 & postnr <= 4999] <- "Vest- og Sydsjælland"  # Sjælland, Lolland og Falster
boliger_med_landsdel$landsdel[postnr >= 4000 & postnr <= 4099] <- "Østsjælland"           # Roskilde (overskriver linjen over)
boliger_med_landsdel$landsdel[postnr >= 4600 & postnr <= 4629] <- "Østsjælland"           # Køge (overskriver)
boliger_med_landsdel$landsdel[postnr >= 5000 & postnr <= 5999] <- "Fyn"
boliger_med_landsdel$landsdel[postnr >= 6000 & postnr <= 6899] <- "Sydjylland"            # Sønderjylland, Esbjerg og Varde
boliger_med_landsdel$landsdel[postnr >= 6900 & postnr <= 6999] <- "Vestjylland"           # Ringkøbing og Skjern
boliger_med_landsdel$landsdel[postnr >= 7000 & postnr <= 7299] <- "Sydjylland"            # Fredericia, Vejle, Billund
boliger_med_landsdel$landsdel[postnr >= 7300 & postnr <= 7699] <- "Vestjylland"           # Brande, Herning, Holstebro, Struer
boliger_med_landsdel$landsdel[postnr >= 7700 & postnr <= 7799] <- "Nordjylland"           # Thisted
boliger_med_landsdel$landsdel[postnr >= 7800 & postnr <= 7899] <- "Vestjylland"           # Skive
boliger_med_landsdel$landsdel[postnr >= 8000 & postnr <= 8999] <- "Østjylland"            # Aarhus, Randers, Horsens, Silkeborg
boliger_med_landsdel$landsdel[postnr >= 9000 & postnr <= 9999] <- "Nordjylland"           # Aalborg, Hjørring, Frederikshavn
# -- undtagelser til sidst, så de overskriver de brede intervaller --
boliger_med_landsdel$landsdel[postnr >= 2670 & postnr <= 2690] <- "Østsjælland"           # Greve og Solrød
boliger_med_landsdel$landsdel[postnr >= 2770 & postnr <= 2791] <- "Byen København"        # Kastrup og Dragør (Tårnby og Dragør)
boliger_med_landsdel$landsdel[postnr %in% c(2700, 2720)]       <- "Byen København"        # Brønshøj og Vanløse
boliger_med_landsdel$landsdel[postnr >= 2960 & postnr <= 2990] <- "Nordsjælland"          # Rungsted, Hørsholm, Kokkedal, Nivå
boliger_med_landsdel$landsdel[postnr == 2765]                  <- "Nordsjælland"          # Smørum (Egedal)
boliger_med_landsdel$landsdel[postnr >= 7130 & postnr <= 7171] <- "Østjylland"            # Hedensted og Juelsminde
boliger_med_landsdel$landsdel[postnr >= 7300 & postnr <= 7329] <- "Sydjylland"            # Jelling, Gadbjerg og Give (Vejle)
boliger_med_landsdel$landsdel[postnr >= 8800 & postnr <= 8850] <- "Vestjylland"           # Viborg og Bjerringbro
boliger_med_landsdel$landsdel[postnr >= 7900 & postnr <= 7999] <- "Nordjylland"           # Mors (Nykøbing M)

boliger_med_landsdel |> count(landsdel, sort = TRUE)                            # TJEK: 11 landsdele og ingen NA
boliger_med_landsdel |> filter(is.na(landsdel)) |> distinct(postnr)             # TJEK: hvilke postnumre fik ingen landsdel? (skal være tom)
boliger_med_landsdel |> filter(postnr %in% c(7900, 7130, 2670)) |>
  distinct(postnr, by, landsdel)                                                 # TJEK: Mors, Hedensted og Greve ligger rigtigt

boliger_med_landsdel |>
  group_by(landsdel) |>
  summarise(antal = n(),
            median_kvadratmeterpris = median(kvadratmeterpris, na.rm = TRUE)) |>
  arrange(desc(median_kvadratmeterpris))                                         # hvilken landsdel er dyrest pr. m²

model_landsdel <- lm(kvadratmeterpris ~ landsdel, data = boliger_med_landsdel)   # landsdel bliver selv lavet om til factor
summary(model_landsdel)                                                          # referencen er den første alfabetisk (Bornholm)
# Koefficienterne viser, hvor meget højere eller lavere kvadratmeterprisen er end på Bornholm.
# Landsdel har kun 10 x-variable i stedet for én pr. postnummer, så modellen er mere robust,
# men vi mister forskellen inde i landsdelen.


# =====================================================================
# Overfitting: sammenlign modellerne
# =====================================================================
# 1) R2 mod justeret R2
# R2 stiger altid, når man tilføjer flere x'er. Justeret R2 trækker fra for hver ekstra x:
#   justeret R2 = 1 - (1 - R2) * (n - 1) / (n - k - 1),   n = antal boliger, k = antal x'er
# Hvis R2 og justeret R2 ligger langt fra hinanden, er der mange x'er, som ikke bidrager.
model_areal <- lm(kvadratmeterpris ~ boligareal_m2, data = boliger)

sammenligning <- data.frame(
  model       = c("boligareal", "landsdel", "postnummer"),
  antal_x     = c(length(coef(model_areal)), length(coef(model_landsdel)), length(coef(model_postnr))) - 1,
  R2          = c(summary(model_areal)$r.squared, summary(model_landsdel)$r.squared, summary(model_postnr)$r.squared),
  justeret_R2 = c(summary(model_areal)$adj.r.squared, summary(model_landsdel)$adj.r.squared, summary(model_postnr)$adj.r.squared)
)
sammenligning |> mutate(across(c(R2, justeret_R2), ~ round(.x, 3)))
# Skriv tallene her, når du har kørt det: R2 og justeret R2 for landsdel og postnummer.

# 2) Train/test: hvor godt rammer modellen boliger, den IKKE har set?
# Data deles tilfældigt: 80 % bruges til at lave modellen (træning), 20 % til at teste den.
# En overfittet model er god på træningsdata, men dårligere på testdata.
model_data <- boliger_med_landsdel |>
  filter(is.finite(kvadratmeterpris), !is.na(landsdel))                          # kun boliger med kvadratmeterpris og landsdel

set.seed(1)                                                                      # samme tilfældige opdeling hver gang
traening_raekker <- sample(nrow(model_data), size = round(0.8 * nrow(model_data)))
traening <- model_data[traening_raekker, ]                                       # 80 % til at lave modellen
test     <- model_data[-traening_raekker, ]                                      # 20 % til at teste den

# Et postnummer, der kun er i testdata, har ingen koefficient i modellen, så predict() fejler.
# Derfor tester vi kun på boliger, hvis postnummer også findes i træningsdata.
test <- test |> filter(postnr %in% traening$postnr)

rmse <- function(faktisk, forudsagt) sqrt(mean((faktisk - forudsagt)^2))         # typisk fejl i kr. pr. m²

tt_areal    <- lm(kvadratmeterpris ~ boligareal_m2,  data = traening)
tt_landsdel <- lm(kvadratmeterpris ~ landsdel,       data = traening)
tt_postnr   <- lm(kvadratmeterpris ~ factor(postnr), data = traening)

train_test <- data.frame(
  model = c("boligareal", "landsdel", "postnummer"),
  RMSE_traening = c(rmse(traening$kvadratmeterpris, predict(tt_areal)),
                    rmse(traening$kvadratmeterpris, predict(tt_landsdel)),
                    rmse(traening$kvadratmeterpris, predict(tt_postnr))),
  RMSE_test     = c(rmse(test$kvadratmeterpris, predict(tt_areal,    newdata = test)),
                    rmse(test$kvadratmeterpris, predict(tt_landsdel, newdata = test)),
                    rmse(test$kvadratmeterpris, predict(tt_postnr,   newdata = test)))
) |>
  mutate(stigning_pct = round((RMSE_test / RMSE_traening - 1) * 100, 1))         # hvor meget værre på testdata
train_test
# Sådan læses tabellen: Hvis postnummer-modellens fejl stiger meget mere fra træning til test end
# landsdel-modellens, er det tegn på overfitting. Den model med lavest RMSE_test er den bedste til
# at forudsige nye boliger, uanset hvilken der har højest R2.
# Skriv tallene og konklusionen her, når du har kørt det.


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