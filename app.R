library(shiny)
library(fredr)
library(dplyr)
library(forecast)
library(ggplot2)
library(lubridate)
library(purrr)

Sys.getenv("FRED_KEY")

ui <- fluidPage(
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
  titlePanel("US Inflation Forecasting Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Model Configuration"),
      selectInput("tipo_modelo", "Select model:", 
                  choices = c("ARIMA (Inflation Only)" = "arima", "ARIMAX (With Exogenous)" = "arimax")),
      
      conditionalPanel(
        condition = "input.tipo_modelo == 'arimax'",
        checkboxGroupInput("exogenas", "Exogenous Variables:",
                           choices = c("Interest Rate" = "drate", 
                                       "Unemployment"  = "unrate", 
                                       "M2 Money Supply" = "dm2", 
                                       "Oil Price"     = "doil"),
                           selected = c("drate", "unrate"))
      ),
      
      sliderInput("h_periodos", "Months to forecast:", min = 6, max = 48, value = 24, step = 6),
      hr(),
      uiOutput("date_range_ui"),
      hr(),
      uiOutput("exog_info")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("YoY Chart",          plotOutput("plot_yoy", height = "500px")),
        tabPanel("Statistical Summary", verbatimTextOutput("model_summary"))
      ),
      hr(),
      h4("Forecast Table (Annual Inflation %)"),
      tableOutput("table_pred")
    )
  )
)

server <- function(input, output, session) {
  
  datos_maestros <- reactive({
    
    cpi   <- fredr(series_id = "CPIAUCSL") %>%
      dplyr::arrange(date) %>%
      dplyr::transmute(date,
                       cpi     = value,
                       inf_mom = 100 * (log(cpi) - log(dplyr::lag(cpi))))
    
    rate  <- fredr(series_id = "FEDFUNDS")   %>% dplyr::transmute(date, drate  = c(NA, diff(value)))
    unemp <- fredr(series_id = "UNRATE")     %>% dplyr::transmute(date, unrate = value)
    oil   <- fredr(series_id = "MCOILWTICO") %>% dplyr::transmute(date, doil   = c(NA, diff(value)))
    m2    <- fredr(series_id = "M2SL")       %>% dplyr::transmute(date, dm2    = c(NA, diff(value)))
    
    cpi %>%
      dplyr::left_join(rate,  by = "date") %>%
      dplyr::left_join(unemp, by = "date") %>%
      dplyr::left_join(oil,   by = "date") %>%
      dplyr::left_join(m2,    by = "date") %>%
      dplyr::filter(!is.na(inf_mom)) %>%
      tidyr::fill(drate, unrate, doil, dm2, .direction = "down")
  })
  
  output$date_range_ui <- renderUI({
    df <- datos_maestros()
    dateRangeInput("date_range", "View from:",
                   start = max(df$date) - years(5),
                   end   = max(df$date) + months(input$h_periodos))
  })
  
  output$exog_info <- renderUI({
    if (input$tipo_modelo != "arimax" || is.null(input$exogenas) || length(input$exogenas) == 0) {
      return(NULL)
    }
    
    info_list <- list(
      drate = list(
        title = "Interest Rate (Federal Funds Rate Change)",
        desc = "Changes in the federal funds rate directly influence borrowing costs, consumer spending, and aggregate demand.",
        cite = "\"Monetary policy works primarily through interest rates to affect aggregate demand and inflation.\" — Federal Reserve Board (2023)"
      ),
      unrate = list(
        title = "Unemployment Rate",
        desc = "The Phillips Curve suggests an inverse relationship between unemployment and inflation: lower unemployment typically leads to wage pressures and higher inflation.",
        cite = "\"There is a trade-off between inflation and unemployment in the short run.\" — Phillips (1958), Economica"
      ),
      dm2 = list(
        title = "M2 Money Supply (Change)",
        desc = "According to the Quantity Theory of Money, increases in money supply can lead to inflation if they outpace economic growth.",
        cite = "\"Inflation is always and everywhere a monetary phenomenon.\" — Friedman (1963), American Economic Review"
      ),
      doil = list(
        title = "Oil Price (WTI Crude Change)",
        desc = "Oil price shocks transmit through the economy as energy costs affect production and transportation, creating cost-push inflation.",
        cite = "\"Oil price shocks have been a major source of inflation volatility.\" — Hamilton (2009), Journal of Monetary Economics"
      )
    )
    
    selected_info <- info_list[input$exogenas]
    
    div(
      style = "background-color: #f8f9fa; padding: 12px; border-radius: 5px; margin-top: 10px;",
      h5("Selected Variables:", style = "margin-top: 0;"),
      lapply(selected_info, function(var) {
        div(
          style = "margin-bottom: 15px; padding-bottom: 10px; border-bottom: 1px solid #dee2e6;",
          h6(strong(var$title), style = "color: #0066cc; margin-bottom: 5px;"),
          p(var$desc, style = "margin-bottom: 5px; font-size: 0.9em;"),
          p(em(var$cite), style = "color: #6c757d; font-size: 0.85em; margin-bottom: 0;")
        )
      })
    )
  })
  
  resultados <- reactive({
    df <- datos_maestros()
    h  <- input$h_periodos
    
    if (input$tipo_modelo == "arima" || is.null(input$exogenas) || length(input$exogenas) == 0) {
      
      fit <- forecast::auto.arima(df$inf_mom, seasonal = TRUE)
      fc  <- forecast::forecast(fit, h = h)
      
    } else {
      
      x_train <- as.matrix(df[, input$exogenas, drop = FALSE])
      x_train[is.na(x_train)] <- 0
      
      fit <- forecast::auto.arima(df$inf_mom, xreg = x_train, seasonal = TRUE)
      
      x_future_list <- lapply(input$exogenas, function(var) {
        serie_exog              <- df[[var]]
        serie_exog[is.na(serie_exog)] <- 0
        
        fit_exog  <- forecast::auto.arima(serie_exog, seasonal = TRUE)
        fc_exog   <- forecast::forecast(fit_exog, h = h)
        
        as.numeric(fc_exog$mean)   # vector de longitud h
      })
      
      x_future <- do.call(cbind, x_future_list)
      if (!is.matrix(x_future)) x_future <- matrix(x_future, nrow = h)
      colnames(x_future) <- input$exogenas
      
      fc <- forecast::forecast(fit, xreg = x_future, h = h)
    }
    
    ultima_obs  <- tail(df, 1)
    ultimo_cpi  <- ultima_obs$cpi
    
    df_fc <- data.frame(
      date    = max(df$date) %m+% months(1:h),
      inf_mom = as.numeric(fc$mean),
      lower   = as.numeric(fc$lower[, 2]),
      upper   = as.numeric(fc$upper[, 2])
    ) %>%
      dplyr::mutate(
        cpi   = ultimo_cpi * exp(cumsum(inf_mom / 100)),
        cpi_l = ultimo_cpi * exp(cumsum(lower   / 100)),
        cpi_u = ultimo_cpi * exp(cumsum(upper   / 100))
      )
    
    total <- dplyr::bind_rows(
      df     %>% dplyr::select(date, cpi) %>% dplyr::mutate(tipo = "Historical", cpi_l = NA_real_, cpi_u = NA_real_),
      df_fc  %>% dplyr::select(date, cpi, cpi_l, cpi_u) %>% dplyr::mutate(tipo = "Forecast")
    ) %>%
      dplyr::arrange(date) %>%
      dplyr::mutate(
        yoy   = 100 * (log(cpi)           - log(dplyr::lag(cpi, 12))),
        yoy_l = 100 * (log(pmax(cpi_l, 1)) - log(dplyr::lag(cpi, 12))),
        yoy_u = 100 * (log(cpi_u)          - log(dplyr::lag(cpi, 12)))
      )
    
    list(fit = fit, df = total)
  })
  
  output$plot_yoy <- renderPlot({
    res <- resultados()
    req(input$date_range)
    
    ggplot2::ggplot(res$df, aes(x = date)) +
      ggplot2::geom_ribbon(
        data = dplyr::filter(res$df, tipo == "Forecast"),
        aes(ymin = yoy_l, ymax = yoy_u),
        fill = "red", alpha = 0.15
      ) +
      ggplot2::geom_line(
        data = dplyr::filter(res$df, tipo == "Historical"),
        aes(y = yoy, color = "Historical"), lwd = 1
      ) +
      ggplot2::geom_line(
        data = dplyr::filter(res$df, date >= max(datos_maestros()$date)),
        aes(y = yoy, color = "Forecast"), lwd = 1, linetype = "dashed"
      ) +
      ggplot2::geom_hline(yintercept = 2, linetype = "dashed", color = "darkgreen", lwd = 0.8) +
      ggplot2::annotate("text", x = min(res$df$date, na.rm = TRUE), y = 2.25,
                        label = "Fed Target: 2%", color = "darkgreen", hjust = 0, size = 3.5) +
      ggplot2::scale_color_manual(values = c("Historical" = "blue", "Forecast" = "red")) +
      ggplot2::coord_cartesian(xlim = input$date_range) +
      ggplot2::theme_minimal() +
      ggplot2::labs(title = "Year-over-Year Inflation (%)", y = "%", x = "")
  })
  
  output$model_summary <- renderPrint({ summary(resultados()$fit) })
  
  output$table_pred <- renderTable({
    resultados()$df %>%
      dplyr::filter(tipo == "Forecast") %>%
      dplyr::transmute(
        Date    = format(date, "%Y-%m"),
        `YoY %` = yoy,
        `Min`   = yoy_l,
        `Max`   = yoy_u
      )
  }, digits = 2)
}

shinyApp(ui, server)
