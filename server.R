shinyServer(function(input, output, session) {

  # En modul per skolform.
  mod_gymnasiet_server('gym')
  mod_yh_server('yh')
  mod_komvux_server('komvux')
  mod_hogskola_server('hogskola')
  mod_grundskola_server('grundskola')
  mod_folkhogskola_server('folkhogskola')

})
