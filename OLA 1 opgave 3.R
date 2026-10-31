# =====================================================================
# OLA 1 – Opgave 3: Tilfældigheder og terninger
# =====================================================================
# Opgave 3 står helt for sig selv.

library(tidyverse)   # ggplot2 til barplots

set.seed(1)          # samme tilfældige tal hver gang, så resultaterne kan genskabes


# =====================================================================
# Opgave 3.1: Funktion til terninger
# =====================================================================
slaa_terninger <- function(antal_terninger) {
  sample(1:6, size = antal_terninger, replace = TRUE)                           # replace = TRUE: samme tal kan komme igen
}

kast_25000 <- slaa_terninger(25000)
antal_femmere <- sum(kast_25000 == 5)                                            # == 5 giver TRUE/FALSE, sum tæller TRUE
antal_femmere
# 4.206 femmere (med set.seed(1))
sandsynlighed_for_femmer <- antal_femmere / 25000
sandsynlighed_for_femmer
# 0,168: tæt på 1/6 = 0,167, som er den teoretiske sandsynlighed for en 5'er


# =====================================================================
# Opgave 3.2: Plot I
# =====================================================================
sum_af_6_terninger <- function() {
  sum(slaa_terninger(6))                                                         # slå 6 terninger og læg dem sammen
}
sum_af_6_terninger()

summer_10000 <- replicate(10000, sum_af_6_terninger())                           # replicate: gentag funktionen 10.000 gange

data.frame(sum = summer_10000) |>
  ggplot(aes(x = sum)) +
  geom_bar(fill = "steelblue") +                                                 # geom_bar tæller, hvor mange gange hver sum kom
  labs(title = "Summen af 6 terninger, 10.000 kast", x = "Sum af 6 terninger", y = "Antal gange")
# Summen kan gå fra 6 til 36 og er oftest omkring 21 (6 * 3,5). Fordelingen ligner en klokke,
# fordi der er mange flere måder at få 21 på end at få 6 (kun 1+1+1+1+1+1).
# Det giver mening, fordi sample() giver hvert tal 1-6 lige stor chance.


# =====================================================================
# Opgave 3.3: Plot II
# =====================================================================
summer_million <- replicate(1000000, sum_af_6_terninger())                       # tager lidt tid

data.frame(sum = summer_million) |>
  ggplot(aes(x = sum)) +
  geom_bar(fill = "darkred") +
  labs(title = "Summen af 6 terninger, 1.000.000 kast", x = "Sum af 6 terninger", y = "Antal gange")
# Med 1.000.000 kast bliver fordelingen meget glattere og mere symmetrisk end med 10.000:
# jo flere kast, jo tættere kommer vi på den teoretiske fordeling (store tals lov).


# =====================================================================
# Opgave 3.4: Lav dine egne data
# =====================================================================
tilfaeldig_raekke <- sample(c(1, 2, 3, 5, 6))                                    # tallene 1, 2, 3, 5, 6 i tilfældig rækkefølge
tilfaeldig_raekke

min_matrix <- cbind(tal_2_til_6 = 2:6, tilfaeldig_raekke)                                      # cbind sætter to lige lange kolonner sammen
min_matrix
dim(min_matrix)
# 5 rækker og 2 kolonner