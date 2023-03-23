shinyUI(
    navbarPage("premessa",
        tabPanel("Panel editor",
            fluidPage(
                fluidRow(
                    fileInput("file", "Choose files", multiple = TRUE),
                    actionButton("submit_analysis", "Submit files"),
                    uiOutput("paneleditorUI")
                )
            )
        )
    )
)



