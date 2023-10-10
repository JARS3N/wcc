generate_ggtitle <- function(u) {

  title1 <-  c(mean_CV = "the average CV of Samples",
               cv_avg = "The %CV of sample means",
               mean_avg = "The Mean of Sample Means")[
                 sapply(c("mean_cv", "cv_avg", "mean_avg"),
                        grepl,
                        x = u)]


  aspect2 <- c("pH", "O2", "Gain", "KSV")
  title2 <- aspect2[sapply(aspect2, grepl, x = u)]

  paste0(title1, " for ", title2)
}
