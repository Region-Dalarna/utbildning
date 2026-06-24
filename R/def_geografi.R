# ============================================================
#  def_geografi.R
#  Geografiska indelningar för appen.
#  - Kommun: Dalarnas 15 kommuner.
#  - Samverkansområde: Gysam-indelningen (gymnasiesamverkan).
# ============================================================

# ---- Dalarnas kommuner -----------------------------------------------------
dalarna_kommuner <- tibble::tribble(
  ~kommkod, ~kommun,
  "2021",   "Vansbro",
  "2023",   "Malung-Sälen",
  "2026",   "Gagnef",
  "2029",   "Leksand",
  "2031",   "Rättvik",
  "2034",   "Orsa",
  "2039",   "Älvdalen",
  "2061",   "Smedjebacken",
  "2062",   "Mora",
  "2080",   "Falun",
  "2081",   "Borlänge",
  "2082",   "Säter",
  "2083",   "Hedemora",
  "2084",   "Avesta",
  "2085",   "Ludvika"
)

# ---- Samverkansområden (Gysam) ---------------------------------------------
# Gysam Västra: Vansbro, Malung-Sälen
# Gysam Södra:  Hedemora, Säter, Falun, Borlänge + VBU (Ludvika, Smedjebacken)
# Gysam Siljan: Leksand, Rättvik, Gagnef
# Avesta:       eget samverkansområde
# Mora, Orsa och Älvdalen: Mora, Orsa, Älvdalen
kommun_samverkan <- tibble::tribble(
  ~kommkod, ~samverkansomrade,
  "2021",   "Gysam Västra",
  "2023",   "Gysam Västra",
  "2083",   "Gysam Södra",
  "2082",   "Gysam Södra",
  "2080",   "Gysam Södra",
  "2081",   "Gysam Södra",
  "2085",   "Gysam Södra",      # VBU
  "2061",   "Gysam Södra",      # VBU
  "2029",   "Gysam Siljan",
  "2031",   "Gysam Siljan",
  "2026",   "Gysam Siljan",
  "2084",   "Avesta",
  "2062",   "Mora, Orsa och Älvdalen",
  "2034",   "Mora, Orsa och Älvdalen",
  "2039",   "Mora, Orsa och Älvdalen"
)

# ---- Färdiga val till väljarna ---------------------------------------------
# Kommun: visa namn, returnera kommkod.
geo_val_kommun <- setNames(dalarna_kommuner$kommkod, dalarna_kommuner$kommun)

# Samverkansområde: visa = returnera namn.
geo_val_samverkan <- sort(unique(kommun_samverkan$samverkansomrade))
geo_val_samverkan <- setNames(geo_val_samverkan, geo_val_samverkan)
