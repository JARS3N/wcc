server <- function() {
library(shiny)
library(dplyr)
library(ggplot2)

db <- adminKraken::con_dplyr()
types <- tbl(db, "wetqc_lot") %>%
  select(cartridgetypeid, Inst) %>%
  distinct() %>%
  collect()

font_scale <-
  rev(seq(4, 20, by = (20 - 4) / (300 - 20), step = 10))
#font_scale<-seq(25,6,length.out=(500-20))



function(input, output, session) {
  # output$violation_tbl <- NULL
  observeEvent(input$CTID, {
    print(input$CTID)
    updateSelectInput(
      session,
      inputId = "Inst",
      label = "Instrument",
      choices = types$Inst[types$cartridgetypeid == input$CTID],
      selected = NULL
    )
  })

  # input change instrument
  observeEvent(list(input$Inst, input$var, input$nlots), {
    #so it won't crash if values outside of range

    req(input$nlots >= 20, input$nlots <= 300)
    if (input$nlots < 20) {
      updateNumericInput(session, inputId = input$nlots,  value = 20)
    }

    output$plot1 <- renderPlot({
      NULL
    })
    nsigma <- 3 #input$select_sigma

    Q <-  tbl(db, "wetqc_lot") %>%
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
             violation = z >= nsigma) %>%
      mutate(N = factor(n))

    sigma_thresholds <-
      if (grepl("cv_avg", input$var)) {
        c(nsigma)
      } else{
        nsigma * c(1, -1)
      }
    title_cols <-
      c("black", "red")[as.numeric(factor(Q$violation, levels = c(F, T)))]

    #ggtitles<-generate_ggtitle(input$Variable)
    plot <- Q %>%
      ggplot(., aes(x = LOT , y = y)) +
      geom_hline(yintercept = mean(Q$y), linetype = 3) +
      geom_hline(
        yintercept = mean(Q$y) + (sigma_thresholds * sd(Q$y)),
        col = "darkgreen"
      ) +
      theme_minimal() +
      geom_line(aes(group = 1), alpha = .5) +
      geom_point(
        aes(fill = violation,
            size = 3),
        alpha = .75,
        shape = 21,
        col = rgb(0, 0, 0, .2)
      ) +
      ggtitle(paste0("Instrument: ", input$Inst, " , ", input$var)) +
      theme(legend.position = 'bottom') +
      scale_fill_manual(values = c("black", "red")) +
      theme(
        axis.text.x = element_text(
          angle = 90,
          vjust = 0.5,
          hjust = 1,
          colour  = title_cols,
          size = if (input$nlots > 300) {
            1
          } else{
            font_scale[input$nlots]
          }
        )
      ) +
      ylab(input$var) +
      ggtitle(generate_ggtitle(input$var))




    output$plot1 <- renderPlot({
      plot
    })
    output$TBL <- renderDataTable({
      filter(Q, violation == T) %>%
        select(LOT, y, z)
    }, options = list(dom = 't'))

  })
}
}

# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

hello <- function() {
  print("Hello, world!")
}
