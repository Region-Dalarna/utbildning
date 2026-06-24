# ============================================================
#  mod_skolform_placeholder.R
#  Enkel platshållare för skolformer som inte är inlagda än.
#  Ingen server behövs – återanvänd mod_gymnasiet-mönstret när det blir dags.
# ============================================================

mod_skolform_placeholder_ui <- function(id, namn) {
  div(
    class = "rd-card",
    h2(namn),
    div(
      class = "rd-info",
      paste0("Statistik f\u00f6r ", namn, " \u00e4r inte inlagd \u00e4n. ",
             "Vyn kommer att f\u00f6lja samma m\u00f6nster som Gymnasiet: ",
             "statistikomr\u00e5den som knapprad, indikatorer i sidopanelen ",
             "och kontextuella filter f\u00f6r kommun/samverkansomr\u00e5de.")
    )
  )
}
