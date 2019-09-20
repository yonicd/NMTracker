miniUI::miniPage(
  miniUI::gadgetTitleBar(
    title = 'NONMEM RUN Tracker',
    left = miniUI::miniTitleBarButton("qt", "Quit")
  ),
  miniUI::miniContentPanel(
    shiny::sidebarLayout(
      sidebarPanel = shiny::sidebarPanel(
        shiny::textInput('path','Project Path',path,placeholder = 'Enter Path'),
        shiny::uiOutput('run'),
        shiny::selectInput(
          inputId = 'est',
          label = 'Estimation Index',
          choices = 1:5,
          selected = 1
        ),
        shiny::checkboxGroupInput(
          inputId = 'panel',
          label = 'Select Statistic',
          choices = c('SAEMOBJ','OBJ','THETA','OMEGA','SIGMA'),
          selected = c('SAEMOBJ','THETA')
        ),
        width = 3
      ),
      mainPanel = shiny::mainPanel(
        shiny::plotOutput('plot',height = 800),width = 9
      )
    )
  )
)