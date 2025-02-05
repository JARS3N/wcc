app<-function(){
  shiny::shinyApp(wcc::ui(),wcc::server())
}

app2<-function(){
  shiny::shinyApp(wcc::ui(),wcc::server2())
}
