shinyServer(function(input, output, session) {

  output$example_text <- renderText({
    'Byt ut detta mot din egen serverlogik.'
  })

})

