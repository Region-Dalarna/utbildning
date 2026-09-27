shinyServer(function(input, output, session) {

  # Skolform Gymnasiet, Yrkeshögskola, Komvux och Folkhögskola (Högskola
  # m.fl. är fortfarande platshållare utan server).
  mod_gymnasiet_server('gym')
  mod_yh_server('yh')
  mod_komvux_server('komvux')
  mod_folkhogskola_server('folkhogskola')

})
