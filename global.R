# ============================================================
#  global.R  –  Utbildning i Dalarna (Samhällsanalys, Region Dalarna)
#  Laddas av både ui.R och server.R, FÖRE filerna i R/.
# ============================================================

# ---- Bibliotek -------------------------------------------------------------
library(shiny)
library(shinyWidgets)
library(dplyr)
library(tidyr)
library(tibble)
library(forcats)
library(readr)
library(ggplot2)
library(ggiraph)

# ---- Delade hjälpfunktioner för Samhällsanalys Shiny-appar -----------------
# Standardrad för Region Dalarnas Shiny-appar (direkt under library()).
source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_shinyappar.R",
       encoding = "utf-8", echo = FALSE)

# ---- Lokala filer ----------------------------------------------------------
# Hjälp- och modulfiler i R/ laddas AUTOMATISKT av Shiny (>= 1.5.0), i
# bokstavsordning och efter denna fil. Inga source()-rader behövs här.
#   R/def_geografi.R            kommuner + Gysam-områden
#   R/func_data.R               databas-/demodataläsning   (beror på def_geografi)
#   R/func_diagram.R            diagramhjälpare (ggiraph)
#   R/mod_gymnasiet.R           modul: skolform Gymnasiet
#   R/mod_skolform_placeholder.R platshållarmodul för övriga skolformer
