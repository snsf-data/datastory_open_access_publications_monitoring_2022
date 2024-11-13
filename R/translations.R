#==============================================================================#
# OA status                                                                 ####
#==============================================================================#

oa_gold <-
  switch(
    params$lang,
    en = "gold",
    de = "Gold",
    fr = "voie dorée"
  )

oa_green <-
  switch(
    params$lang,
    en = "green",
    de = "Grün",
    fr = "voie verte"
  )

oa_hybrid <-
  switch(
    params$lang,
    en = "hybrid",
    de = "Hybrid",
    fr = "voie hybride"
  )

oa_other <-
  switch(
    params$lang,
    en = "other OA",
    de = "Anderer OA",
    fr = "autres voies OA"
  )

oa_restricted <-
  switch(
    params$lang,
    en = "restricted",
    de = "Eingeschränkt",
    fr = "accès restreint"
  )

oa_snsf <-
  switch(
    params$lang,
    en = "SNSF OA (gold, green, hybrid)",
    de = "SNF-OA (Gold, Grün, Hybrid)",
    fr = "FNS OA (voies dorée, verte,\nhybride)"
  )
  

#==============================================================================#
# Licences                                                                  ####
#==============================================================================#

lc_unknown <-
  switch(
    params$lang,
    en = "unknown",
    de = "Unbekannt",
    fr = "indéterminé"
  )

lc_other_cc_by <-
  switch(
    params$lang,
    en = "other CC BY",
    de = "Andere CC BY",
    fr = "autre CC BY"
  )

lc_other_oa <-
  switch(
    params$lang,
    en = "other OA/public domain",
    de = "Anderer OA/öffentlich",
    fr = "autre OA/domaine publique"
  )