server2 <- function() {
  library(shiny)
  library(dplyr)
  library(ggplot2)

  db <- adminKraken::con_dplyr()

  # Preload distinct instrument types only once
  types <- tbl(db, "wetqc_lot") %>%
    select(cartridgetypeid, Inst) %>%
    distinct() %>%
    collect()

  font_scale <- rev(seq(4, 20, length.out = (300 - 20)))

  function(input, output, session) {
    
    observeEvent(input$CTID, {
      updateSelectInput(
        session,
        inputId = "Inst",
        label = "Instrument",
        choices = types$Inst[types$cartridgetypeid == input$CTID],
        selected = NULL
      )
    })

    # Reactive expression to fetch data from DB only when inputs change
    filtered_data <- reactive({
      req(input$Inst, input$var, input$nlots)
      
      validate(need(input$nlots >= 20, "Number of lots must be at least 20"))
      
      tbl(db, "wetqc_lot") %>%
        filter(Inst == local(input$Inst)) %>%
        select(Lot, y = contains(input$var), n) %>%
        filter(!is.na(y)) %>%
        collect() %>%
        mutate(
          lot = stringr::str_pad(Lot, 5, pad = "0"),
          day = as.numeric(substr(lot, 1, 3)),
          year = as.numeric(substr(lot, 4, 5))
        ) %>%
        arrange(year, day) %>%
        tail(input$nlots) %>%
        mutate(LOT = factor(lot, levels = lot)) %>%
        mutate(z = abs(y - mean(y)) / sd(y),
               violation = z >= 3) %>%
        mutate(N = factor(n))
    })

    output$plot1 <- renderPlot({
      Q <- filtered_data()
      req(nrow(Q) > 0)

      sigma_thresholds <- if (grepl("cv_avg", input$var)) {
        c(3)
      } else {
        3 * c(1, -1)
      }
      
      title_cols <- c("black", "red")[as.numeric(factor(Q$violation, levels = c(F, T)))]

      ggplot(Q, aes(x = LOT, y = y)) +
        geom_hline(yintercept = mean(Q$y), linetype = 3) +
        geom_hline(yintercept = mean(Q$y) + (sigma_thresholds * sd(Q$y)), col = "darkgreen") +
        geom_line(aes(group = 1), alpha = 0.5) +
        geom_point(aes(fill = violation), size = 3, alpha = 0.75, shape = 21, col = rgb(0, 0, 0, 0.2)) +
        theme_minimal() +
        theme(legend.position = 'bottom') +
        scale_fill_manual(values = c("black", "red")) +
        theme(
          axis.text.x = element_text(
            angle = 90, vjust = 0.5, hjust = 1,
            colour = title_cols,
            size = font_scale[input$nlots]
          )
        ) +
        ylab(input$var) +
        ggtitle(paste0("Instrument: ", input$Inst, " , ", input$var))
    })

    output$TBL <- renderDataTable({
      filtered_data() %>%
        filter(violation == TRUE) %>%
        select(LOT, y, z)
    }, options = list(dom = 't'))
  }
}
