app<-function(){
  shiny::shinyApp(wcc::ui(),wcc::server())
}
