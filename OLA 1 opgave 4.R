# =====================================================================
# OLA 1 – Opgave 4: Dataframes
# =====================================================================
# Opgave 4 står helt for sig selv.

library(tidyverse)   # tidyr (pivot_wider) og dplyr (bind_rows)


# =====================================================================
# Opgave 4.1: Månedlige observationer
# =====================================================================
klasser <- data.frame(
  Klasse = rep(c("A", "B", "C", "D"), each = 9),                                 # each = 9: 9 A'er, så 9 B'er osv.
  Uge    = rep(seq(1, 9), times = 4),                                            # 1 til 9, gentaget for hvert bogstav
  Score  = c(5, 6, 7, 8, 7, 9, 8, 7, 10,
             4, 5, 6, 6, 7, 8, 7, 9, 8,
             6, 7, 7, 8, 8, 9, 10, 9, 10,
             3, 4, 5, 5, 6, 7, 6, 8, 9)
)
dim(klasser)
# 36 rækker og 3 kolonner


# =====================================================================
# Opgave 4.2: Kvartalsvise observationer
# =====================================================================
# Loop gennem alle rækker. Hver tredje gang (i %% 3 == 0) laves en 1x3 dataframe med
# klassen og ugen fra rækken og gennemsnittet af de tre seneste scores.
kvartaler <- data.frame()                                                        # tom tabel, som fyldes i loopet

for (i in seq(3, nrow(klasser), by = 3)) {                                       # i = 3, 6, 9, ... 36
  en_raekke <- data.frame(
    Klasse = klasser$Klasse[i],
    Uge    = klasser$Uge[i],
    Score  = mean(klasser$Score[(i - 2):i])                                      # gennemsnit af række i-2, i-1 og i
  )
  kvartaler <- bind_rows(kvartaler, en_raekke)                                   # sæt den nye række under de andre
}
kvartaler
# samme 12 rækker som før
# 12 rækker: 3 "kvartaler" (uge 3, 6 og 9) for hver af de 4 klasser.
# Opgaven skriver 9x3, men 36 rækker / 3 giver 12 rækker.


# =====================================================================
# Opgave 4.3: Pivot
# =====================================================================
kvartaler_bred <- kvartaler |>
  pivot_wider(names_from = Klasse, values_from = Score)                          # én kolonne pr. klasse
kvartaler_bred
# kolonnerne Uge, A, B, C og D med gennemsnittene