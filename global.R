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
library(purrr)
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
#   R/func_data.R               databas-/demodataläsning, gymnasiet   (beror på def_geografi)
#   R/func_data_folkhogskola.R  databasläsning, Folkhögskola
#   R/func_data_forskola.R      databasläsning, Förskola
#   R/func_data_grundskola.R    databasläsning, Grundskola
#   R/func_data_hogskola.R      databasläsning, Högskola
#   R/func_data_komvux.R        databasläsning, Komvux/SFI
#   R/func_data_yh.R            databasläsning, Yrkeshögskola (YH)
#   R/func_diagram.R            diagramhjälpare (ggiraph)
#   R/mod_folkhogskola.R        modul: skolform Folkhögskola
#   R/mod_forskola.R            modul: skolform Förskola
#   R/mod_grundskola.R          modul: skolform Grundskola
#   R/mod_gymnasiet.R           modul: skolform Gymnasiet
#   R/mod_hogskola.R            modul: skolform Högskola
#   R/mod_komvux.R              modul: skolform Komvux (inkl. SFI)
#   R/mod_yh.R                  modul: skolform Yrkeshögskola (YH)
#   R/mod_skolform_placeholder.R platshållarmodul (används inte just nu)
