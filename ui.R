source('share_load.R')


ui <- fluidPage(
  theme = shinytheme("cerulean"),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "animate.min.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.1.1/css/all.min.css"),
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.5/MathJax.js?config=TeX-MML-AM_CHTML"),
    tags$style(HTML("
      body {
        background-color: #f7f7f7;
        font-family: Arial;
        font-size: 14px;
      }
      .content {
        margin-top: 80px; /* Adjust this value to ensure content is not hidden by navbar */
      }
      /* Other custom CSS */
      #map-row {
        margin-top: 60px;
      }
      .navbar {
        position: fixed; 
        width: 100%; 
        z-index: 100; 
      }
    ")),
    tags$script(HTML("
      $(document).on('shiny:value', function(event) {
        if (event.name === 'predicted_price') {
          window.scrollTo(0, 0);
        }
      });
      $(function () {
        $('[data-toggle=\"tooltip\"]').tooltip();
      });
    "))
  ),
  useShinyjs(), # Initialize shinyjs
  
  navbarPage(
    title = tags$a(tags$img(src = "Logo2.png", height = "20px", width = "auto"), href = "https://www.linkdata.uz/"),
    windowTitle = "MyApp",
    id = "nav",
    tabPanel("Info",
             div(class = "content",  # Add a class to push content down
                 
                 
                 h2(paste0("2024-yil Toshkent shahrida sodir bo'lgan yo'l transport hodisalarning tavsifi")),
                 #p("Click to show information"),
                 fluidRow( h3("Birinchi qator", align = 'center'),
                           column( width = 6, h4("Qonunbuzarlik turlari"), plotlyOutput('plot1')  ),
                           column( width = 6, h4("Yo'llarning holati"), plotlyOutput('plot2')  ) ),
                 fluidRow( h3("Ikkinchi qator", align = 'center'),
                           column( width = 6, h4("Ob-havo sharoiti"), plotlyOutput('plot3')  ),
                           column( width = 6, h4("Ishtirokchilar yoshi"), plotlyOutput('plot4')  ) ),
                 fluidRow( h3("Uchinchi qator", align = 'center'),
                           column( width = 6, h4("Hodisada kuzatilgan qonunbuzarliklar soni"), plotlyOutput('plot5')  ),
                           column( width = 6, h4("Hodisa turlari"), plotlyOutput('plot6')  ) ),
                 fluidRow( h3("To'rtinchi qator", align = 'center'),
                           column( width = 6, h4("Hodisalar sodir bo'lgan vaqtlar"), plotlyOutput('plot7')  ),
                           column( width = 6, h4("Hodisalar sodir bo'lgan vaqtlarning ulushi"), plotlyOutput('plot8')  ) ),
                 fluidRow( h3("Beshinchi qator", align = 'center'),
                           column( width = 6, h4("Hodisalar sodir bo'lgan hafta kunlari"), plotlyOutput('plot9')  ),
                           column( width = 6, h4("Hodisalar sodir bo'lgan hafta kunlari ulushi"), plotlyOutput('plot10')  ) ),
             )
    ),
    tabPanel("To'qnashuvlar", 
             fluidRow(id = "map-row", leafletOutput(outputId = 'tmap', width = "100%", height = "710px"))
    ),
    tabPanel("Piyoda urib yuborishlar", 
             fluidRow(id = "map-row", leafletOutput(outputId = 'pmap', width = "100%", height = "710px"))
    )
  )
)
