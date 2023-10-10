ui<-function(){
  library(shiny)

  fluidPage(titlePanel("Cartridge QC Control Charts"),
            mainPanel(width="100%",
                      #row 1
                      fluidRow(
                        plotOutput("plot1", click = "plot_click",width="100%"))
                      ,
                      #row 2
                      fluidRow(
                        column(2,
                               selectInput(
                                 label = "Cartridge Type",
                                 inputId = "CTID",
                                 choices =  c("W" = 3, "B" = 5, "C" = 8),
                                 selected = NULL
                               ),
                               selectInput(
                                 label = "Instrument",
                                 inputId = "Inst",
                                 choices = NULL
                               )),
                        column(2,
                               selectInput(
                                 label = 'Variable',
                                 inputId = "var",
                                 choices = c(
                                   "mean_cv_pHLED",
                                   "mean_cv_O2LED",
                                   "mean_cv_Gain" ,
                                   "mean_cv_KSV",
                                   "mean_avg_pHLED",
                                   "mean_avg_O2LED",
                                   "mean_avg_Gain",
                                   "mean_avg_KSV",
                                   "cv_avg_PHLED" ,
                                   "cv_avg_O2LED",
                                   "cv_avg_Gain",
                                   "cv_avg_KSV"
                                 )),
                               numericInput(
                                 inputId ="nlots",
                                 label="n samples",
                                 value =  50,
                                 min = 20,
                                 max = 300,
                                 step = 10,
                                 width = NULL
                               )

                        ),
                        column(6, dataTableOutput("TBL"))

                      )
            ))
}
