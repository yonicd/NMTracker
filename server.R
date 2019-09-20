shinyServer(function(input, output, session) {
    
  # shiny::observeEvent(input$run,{
  #   
  #   new_est <- tidynm::read_extensions(file.path(input$path,input$run),
  #                           exts = 'ext',
  #                           est_ind = identity)
  #   if(nrow(new_est)>0){
  #   
  #     new_est %>%
  #       dplyr::pull(estimation_index)%>%
  #       as.numeric()
  #     
  #     shiny::updateSelectInput(session = session,
  #                              inputId = 'est',
  #                              choices = new_est,
  #                              selected = new_est[1])
  #       
  #   }
  #   
  #   
  # })
  
  shiny::observeEvent(c(input$run),{
    output$plot <- shiny::renderPlot({
      ext_status_plot(data()(),stat = input$panel)
    })
  })
    
    data <- shiny::eventReactive(input$run,{
      shiny::reactivePoll(
        1000, session,
        # This function returns the time that log_file was last modified
        checkFunc = function() {
          f <- file.path(input$path,input$run,sprintf('%s.ext',input$run))
          if (file.exists(f))
            file.info(f)$mtime[1]
          else
            ""
        },
        # This function returns the content of log_file
        valueFunc = function() {
          ext_status_read(file.path(input$path,input$run),est_ind = input$est)
        }
      )  
    })
    
    
    shiny::observeEvent(input$path,{
      
      runs <- basename(list.dirs(input$path,recursive = FALSE))
      
      output$run <- renderUI({
        shiny::selectInput(
          inputId = 'run',
          label = 'Select Run',
          choices = runs,
          selected = tail(runs,1)
        )
      })
      
    })
    
    shiny::observeEvent(input$qt, {
      shiny::stopApp()
    })
    
})
  