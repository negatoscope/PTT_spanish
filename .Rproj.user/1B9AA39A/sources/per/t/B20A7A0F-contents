library(shiny)
library(plotrix)
library(bslib)
library(shinyjs)

# Initialize counter
counter <- reactiveVal(1)

# Function for a line breaker in the UI
linebreaks <- function(n){HTML(strrep(br(), n))}

ui <- bootstrapPage(
  useShinyjs(),
  theme = bs_theme(version = 5, bootswatch = "minty"),
  div(class="container-fluid",
      h2("Test de Orientacion Espacial (SOT)"),
      div(class="row",
            div(class="col-lg-6",
                div(class="well",
                  h3("Instrucciones"),
                  tags$style(".well {background: rgb(255, 255, 255);}"),
                  htmlOutput("sideText"),
                  linebreaks(1),
                  uiOutput("sideContent"),
                  actionButton("nextButton", "Siguiente", style = "position: absolute; down: 0; right: 30px;"),
                  linebreaks(2),
                  downloadButton("downloadButton", "Download CSV", disabled = TRUE)
                )
            ),
            div(class="col-lg-6",
                textOutput("savedTime"),
                uiOutput("mainContent"))
      )
    )
  )

server <- function(input, output, session) {
  # Reactive value to store the clicked coordinates
  clickedPoint <- reactiveVal(NULL)
  
  # Reactive values to store the clicked coordinates and angle values
  savedAngles <- reactiveValues(values = numeric())
  answerAngles <- reactiveValues(values = numeric())
  currentAngle <- reactiveValues(values = numeric())
  counterCount <- reactiveValues(values = numeric())
  dataset <- reactiveValues(df = NULL)

  output$circlePlot <- renderPlot({
    # Create a circle with a radius of 0.2 centered at (0,0)
    plot.new()
    radius <- 1
    plot.window(xlim = c(-radius*1.1, radius*1.1), ylim = c(-radius*1.1, radius*1.1))
    
    draw.circle(0, 0, radius)
    
    # Draw a line from center to perimeter at 90 degrees
    arrows(0, 0, 0, radius, col = "black", length = 0.1, angle = 20)
    
    # Draw the stored radius line
    if (!is.null(clickedPoint())) {
      x <- clickedPoint()$x
      y <- clickedPoint()$y
      
      # Calculate the angle of the clicked point
      angle <- atan2(y, x)
      clickedAngles <- angle
      
      # Calculate the coordinates of the end point of the radius line
      
      end_x <- radius * cos(angle) + 0
      end_y <- radius * sin(angle) + 0
      
      lines(c(0, end_x), c(0, end_y), col = "red")
    }
    
    object1_texts <- c("", "", "Objeto 1", "", "Tambor", "Campana", "Bote de basura", "", "Rueda", "Tambor", "Semáforo", "Tambor", "Semáforo", "Semáforo", "Barril", "Bote de basura", "Rueda", "Barril", "Árbol", "Tambor")
    object2_texts <- c("", "", "Objeto 2", "", "Semáforo", "Árbol", "Tambor", "", "Barril",  "Árbol", "Tambor", "Campana", "Árbol", "Campana", "Bote de basura", "Campana", "Semáforo", "Tambor", "Campana", "Bote de basura")
    
    object1_texts[counter() - 1]
  
    
    # Add the text "Tú" beneath the circle's center
    text(0, radius*-0.1, object1_texts[counter()], col = "black", font = 2, cex = 1.5)
    
    # Add the text for each object above the arrowhead
    text(0, radius*1.1, object2_texts[counter()], col = "black", font = 2, cex = 1.5)
  })
  
  observeEvent(input$circleClick, {
    # Store the clicked coordinates
    clickedPoint(input$circleClick)
    
    
    
    # Calculate the angle of the clicked point
    x <- input$circleClick$x
    y <- input$circleClick$y
    angle <- atan2(y, x) * 180 / pi
    
    roundedAngle <- round(angle, 2)
    currentAngle$values <- roundedAngle
    savedAngles$values <- c(savedAngles$values, roundedAngle)
  })
  
  # # Disable/enable the download button based on the counter value
  # observe({
  #   if (counter() >= 5 && currentAngle$values == savedAngles$values[length(savedAngles$values)]) {
  #     shinyjs::enable("nextButton")
  #   } else {
  #     shinyjs::enable("nextButton")
  #   }
  # })
  
  # Define initial and new texts
  initial_side_text <- "Bienvenid@ al Test de Orientación Espacial. <br><br>Esta prueba tendrá una duración aproximada de 10 minutos. Haz clic en el botón Siguiente para continuar. <br><br>Lee atentamente las instrucciones que se te presentarán a continuación."
  initialImage <- ""

  
  instructions1 <- "Esta es una prueba que evalúa la capacidad para imaginar diferentes perspectivas u orientaciones en el espacio. En esta tarea, verás una imagen con una serie de objetos con una indicación debajo, junto con un círculo con una flecha. Se te pedirá que imagines que estás de pie en la posición de uno de los objetos de la serie y justo frente a otro objeto. Tu tarea consiste en señalar la línea que muestre la dirección en que se encuentra un tercer objeto desde esta perspectiva. En cada prueba, se te pedirá que se imagines estar parado sobre un primer objeto diferente, frente a un segundo objeto diferente, y luego que dibujes una línea hacia un tercer objeto diferente. Aquí debajo tienes un ejemplo:"
  instructions2 <- "Para responder podrás utilizar el círculo de respuesta. El centro del círculo representa la localización imaginada (en el primer objeto) y la flecha vertical representa tu perspectiva imaginada (mirando hacia el segundo objeto). <br><br>Utilizando el ratón, haz clic sobre el círculo para trazar una línea en la dirección hacia el tercer objeto desde la dirección en la que estás mirando. Haz una prueba haciendo clic en el círculo ahora. Haz clic en Siguiente cuando termines."
  instructions3 <- "Mira el ejemplo a continuación. En este ejemplo, se pide que imagines que estás de pie sobre la campana mirando hacia el árbol. Tu tarea consiste en colocar una línea que indique la dirección hacia el tambor. En este ejemplo, la línea ya ha sido trazada. En la fase de prueba deberás trazar la línea haciendo clic en el círculo sobre la dirección deseada. Podrás hacerlo varias veces hasta que estés conforme con la dirección de la línea. <br><br>Ahora mira el círculo: ¿notas que si estuvieras sobre la campana mirando hacia al árbol, el tambor estaría localizado en la dirección que muestra la línea roja? Si tienes dudas, puedes preguntar al experimentador sobre los pasos que tienes que seguir."
  instructions4 <- "Ahora realizarás tres ejercicios de práctica. Haz clic en el círculo para trazar la línea que indique tu respuesta."
  instructions5 <- "Ahora realizarás la prueba. Contiene 12 ítems y tendrás un máximo de 5 minutos para completarlos. <br><br>Por favor responde con la mayor precisión posible, pero sin pasar demasiado tiempo en el ítem. <br><br>Si aún tienes dudas sobre la prueba, pregunta al experimentador. <br><br>Para comenzar haz clic en el botón Siguiente"
  farewell <- "La prueba ha finalizado. Muchas gracias por tu tiempo."
  
  
  new_side_texts <- c(instructions1, instructions2, instructions3, instructions4, instructions4, instructions4, instructions5, "", "", "", "", "", "", "", "", "", "", "", "", farewell)

  # Define images (if using tags$img there's no need to add www/ before the file name)
  tutorialImage1 <- "tutorial1.png"
  tutorialImage2 <- "tutorial2.png"
  tutorialImage3 <- "tutorial3.png"
  tutorialImage4 <- "tutorial4.png"
  tutorialImage5 <- "tutorial5.png"
  testImage1 <- "test1.png"
  testImage2 <- "test2.png"
  testImage3 <- "test3.png"
  testImage4 <- "test4.png"
  testImage5 <- "test5.png"
  testImage6 <- "test6.png"
  testImage7 <- "test7.png"
  testImage8 <- "test8.png"
  testImage9 <- "test9.png"
  testImage10 <- "test10.png"
  testImage11 <- "test11.png"
  testImage12 <- "test12.png"
  newImage <- c(tutorialImage1, tutorialImage2, tutorialImage3, tutorialImage4, tutorialImage5, initialImage, testImage1, testImage2, testImage3, testImage4, testImage5, testImage6, testImage7, testImage8, testImage9, testImage10, testImage11, testImage12, initialImage)
    
  # Initialize counter
  counter <- reactiveVal(1)
  
  # Output initial text
  output$sideText <- renderText({
    if (counter() == 1) {
      HTML(initial_side_text)
    } else {
      new_side_texts[counter() - 1]
    }
  })

  output$sideContent <- renderUI({
    if (counter() == 1){
      # Display arrow circle for testing
      tags$img(src="")
    } else if (counter() == 2){
      # Display second tutorial image
      tags$img(src="tutorial1.png")
    } else if (counter() == 4){
      tags$img(src="tutorial1.png")
    } else if (counter() == 5 || counter() == 6 || counter() == 7){
      tags$img(src=newImage[counter() - 2])
    } else if (counter() == 8){
      tags$img(src="")
    } else if (counter() > 8){
      tags$img(src=newImage[counter() - 2])
    }
  })
  
  output$mainContent <- renderUI({
    if (counter() == 3){
      # Display arrow circle for testing
      plotOutput("circlePlot", click = "circleClick", height = "950px")
    } else if (counter() == 4){
      # Display second tutorial image
      tags$img(src="tutorial2.png")
    } else if (counter() == 8 || counter() == 21){
      # Display second tutorial image
      tags$img(src="")
    } else if (counter() > 4){
      plotOutput("circlePlot", click = "circleClick", height = "950px")
    }
  })
  
    # Update counter and text on button click
  observeEvent(input$nextButton, {
    counter(counter() + 1)
    
    if (counter() > length(new_side_texts) + 1) {
      counter(1)
    }
    
    if (counter() < 4) {
      answerAngles$values <- c(answerAngles$values, 1)
    } else {
      answerAngles$values <- c(answerAngles$values, savedAngles$values[length(savedAngles$values)])
    }
    
    # Disable/enable the download button based on the counter value
    observe({
      if ((counter() == 5 || counter() == 6 || counter() == 7 || counter() >= 9) && currentAngle$values == answerAngles$values[length(answerAngles$values)]) {
        shinyjs::disable("nextButton")
      } else {
        shinyjs::enable("nextButton")
      }
    })
    
    counterCount$values <- c(counterCount$values, counter())
    dataset$df <- data.frame(answerAngles$values, counterCount$values)
  })
  
  # Disable/enable the download button based on the counter value
  observe({
    if (counter() >= 21) {
      shinyjs::show("downloadButton")
      shinyjs::hide("nextButton")
    } else {
      shinyjs::hide("downloadButton")
    }
  })
  
  output$downloadButton <- downloadHandler(
    filename = "angle_data.csv",
    content = function(file) {
      write.csv(dataset$df[-c(1, 2, 3, 4, 8),], file, row.names = FALSE) #keeps only practice and test trials
    }
  )
}

shinyApp(ui, server)