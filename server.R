shinyServer(function(input, output, session) {

  # Skolform Gymnasiet, Yrkeshögskola och Komvux (Högskola m.fl. är
  # fortfarande platshållare utan server).
  mod_gymnasiet_server('gym')
  mod_yh_server('yh')
  mod_komvux_server('komvux')

})
