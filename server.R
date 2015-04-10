################################################################################
### Title: server.R                                              ###
###                                                                          ###
### Project: looming                                                ###
###                                                                          ###
### Version: 0.1 - 08/apr/2015                                    ###
###                                                                          ###
### Description: short description                                           ###
###                                                                          ###
### Authors: Federico Mattiello <Federico.Mattiello@UGent.be>                ###
###                                                                          ###
### Maintainer: Federico Mattiello <Federico.Mattiello@UGent.be>             ###
###                                                                          ###
### Versions:                                                                ###
###     > 0.1 - 08/apr/2015 - 11:57:19:                                      ###
###         creation                                                         ###
###                                                                          ###
################################################################################
###
#' Short Description for documentation
#' 
#' More Details (section Details)
#'
#' @title Title to be displayed in the HTML help file
#' 
#' @param arg1
#'     first argument
#' @param ...
#'     last argument
#' @return 
#'     what the function returns
#' @author Federico Mattiello <Federico.Mattiello@@UGent.be>
#' @export
#' 
shinyServer(
    function(input, output, session) {
      makePatterns <- reactive({
            n <- input$nHeddles
            p <- input$patternLength
            N <- 2^n - 2
            numCombRep <- choose(N + p - 1, p) - N
            numCombNoAdjRep <- N * (N - 1)^(p - 1)
            sequenceMatrix <- matrix(0, nrow = n, ncol = N)
            cc <- 0
            for (i in seq_len(n-1))
            {
              step <- choose(n, i)
              combs <- combn(n, i)
              inds <- cbind(
                  c(combs), 
                  rep(seq_len(step) + cc, each = i)
              )
              sequenceMatrix[inds] <- 1L
              cc <- cc + step
            }
            
            arg <- vector("list", p)
            arg[] <- list(1L:N)
            
            totMat <- as.matrix(expand.grid(arg, KEEP.OUT.ATTRS = FALSE))
            ### tolte le disposizioni tipo (1,1,1) (2,2,2) etc 
            auxM <- array(rowMeans(totMat), dim = dim(totMat))
            indsOk <- rowSums(totMat == auxM) < ncol(totMat) 
            disposition <- totMat[indsOk, ]
            colnames(disposition) <- paste0("Seq", seq_len(p))
            
            list("sequenceMatrix" = sequenceMatrix, "disposition" = disposition)
          })
      
      output$warping1 <- renderUI({
            ### create as much as min input$nHeddles  
            checkboxGroupInput("heddle1", label = "",# paste0("Liccio: ", 1), 
                choices = seq_len(input$warpingLength), inline = TRUE)
          })
      output$warping2 <- renderUI({
            checkboxGroupInput("heddle2", label = "",# paste0("Liccio: ", 2), 
                choices = seq_len(input$warpingLength), inline = TRUE)
          })
      output$warping3 <- renderUI({
            if (input$nHeddles > 2)
              checkboxGroupInput("heddle3", label = "",# paste0("Liccio: ", 3), 
                  choices = seq_len(input$warpingLength), inline = TRUE)
          })
      output$warping4 <- renderUI({
            if (input$nHeddles > 3)
              checkboxGroupInput("heddle4", label = "",# paste0("Liccio: ", 4), 
                  choices = seq_len(input$warpingLength), inline = TRUE)
          })
      output$warping5 <- renderUI({
            if (input$nHeddles > 4)
              checkboxGroupInput("heddle5", label = "",# paste0("Liccio: ", 5), 
                  choices = seq_len(input$warpingLength), inline = TRUE)
          })
      output$warping6 <- renderUI({
            if (input$nHeddles > 5)
              checkboxGroupInput("heddle6", label = "",# paste0("Liccio: ", 6), 
                  choices = seq_len(input$warpingLength), inline = TRUE)
          })
      output$warping7 <- renderUI({
            if (input$nHeddles > 6)
              checkboxGroupInput("heddle7", label = "",# paste0("Liccio: ", 7), 
                  choices = seq_len(input$warpingLength), inline = TRUE)
          })
      output$warping8 <- renderUI({
            if (input$nHeddles > 7)
              checkboxGroupInput("heddle8", label = "",# paste0("Liccio: ", 8), 
                  choices = seq_len(input$warpingLength), inline = TRUE)
          })
      
      
      makeWarping <- reactive({
            warping <- matrix(0L, nrow = input$nHeddles, ncol = input$warpingLength)
            aux <- list(input$heddle1, input$heddle2, input$heddle3, input$heddle4,
                input$heddle5, input$heddle6, input$heddle7, input$heddle8)
            for (i in seq_len(input$nHeddles))
              warping[i, as.integer(aux[[i]])] <- 1L
            warping
          })
      
      
      weave <- reactive({
            matSeq <- makePatterns()$"sequenceMatrix"
            dispRep <-  makePatterns()$"disposition"
            warping <- makeWarping()
            
            res <- lapply(seq_len(nrow(dispRep)), 
                FUN = function(i)
                {
                  one <- crossprod(warping, matSeq[, dispRep[i, ]])
                  two <- matrix(one, nrow = ncol(warping), ncol = 3 * input$patternLength)
                  two[, ncol(two):1L]
                })
            res
          })
      
      
      output$seeWarping1_3 <- renderPlot({
            patterns <- weave()
            warping <- makeWarping()
            rowsSeq <- seq_len(nrow(patterns[[1L]]))
            colsSeq <- seq_len(ncol(patterns[[1L]]))
            
            cont <- 1L
            layout(t(1L:3))
            
            ## first plot
            image(x = rowsSeq, y = colsSeq, patterns[[cont]], 
                col = c("gray80", "blue3"), 
                yaxt = "n", xlab = "", ylab = "",
                main = paste0("pattern: ", cont))
            axis(side = 2, at = colsSeq, labels = rev(colsSeq))
            title(ylab = "sequence", xlab = "Warping", line = 2)
            abline(v = seq_len(ncol(warping)) - .5)
            abline(h = 1:(5 * input$patternLength) - .5)
            cont <- cont + 1L
            
            ## second plot
            image(x = rowsSeq, y = colsSeq, patterns[[cont]], 
                col = c("gray80", "blue3"), 
                yaxt = "n", xlab = "", ylab = "",
                main = paste0("pattern: ", cont))
            axis(side = 2, at = colsSeq, labels = rev(colsSeq))
            title(ylab = "sequence", xlab = "Warping", line = 2)
            abline(v = seq_len(ncol(warping)) - .5)
            abline(h = 1:(5 * input$patternLength) - .5)
            cont <- cont + 1L
            
            ## third plot
            image(x = rowsSeq, y = colsSeq, patterns[[cont]], 
                col = c("gray80", "blue3"), 
                yaxt = "n", xlab = "", ylab = "",
                main = paste0("pattern: ", cont))
            axis(side = 2, at = colsSeq, labels = rev(colsSeq))
            title(ylab = "sequence", xlab = "Warping", line = 2)
            abline(v = seq_len(ncol(warping)) - .5)
            abline(h = 1:(5 * input$patternLength) - .5)
            cont <- cont + 1L
            
          })
      
      
      output$seeWarping4_6 <- renderPlot({
            if (length(weave()) < 6 || input$maxPlots < 6)
            {
              NULL
            } else
            {
              patterns <- weave()
              warping <- makeWarping()
              rowsSeq <- seq_len(nrow(patterns[[1L]]))
              colsSeq <- seq_len(ncol(patterns[[1L]]))
              
              cont <- 4L
              layout(t(1L:3))
              
              ## first plot
              image(x = rowsSeq, y = colsSeq, patterns[[cont]], 
                  col = c("gray80", "blue3"), 
                  yaxt = "n", xlab = "", ylab = "",
                  main = paste0("pattern: ", cont))
              axis(side = 2, at = colsSeq, labels = rev(colsSeq))
              title(ylab = "sequence", xlab = "Warping", line = 2)
              abline(v = seq_len(ncol(warping)) - .5)
              abline(h = 1:(5 * input$patternLength) - .5)
              cont <- cont + 1L
              
              ## second plot
              image(x = rowsSeq, y = colsSeq, patterns[[cont]], 
                  col = c("gray80", "blue3"), 
                  yaxt = "n", xlab = "", ylab = "",
                  main = paste0("pattern: ", cont))
              axis(side = 2, at = colsSeq, labels = rev(colsSeq))
              title(ylab = "sequence", xlab = "Warping", line = 2)
              abline(v = seq_len(ncol(warping)) - .5)
              abline(h = 1:(5 * input$patternLength) - .5)
              cont <- cont + 1L
              
              ## third plot
              image(x = rowsSeq, y = colsSeq, patterns[[cont]], 
                  col = c("gray80", "blue3"), 
                  yaxt = "n", xlab = "", ylab = "",
                  main = paste0("pattern: ", cont))
              axis(side = 2, at = colsSeq, labels = rev(colsSeq))
              title(ylab = "sequence", xlab = "Warping", line = 2)
              abline(v = seq_len(ncol(warping)) - .5)
              abline(h = 1:(5 * input$patternLength) - .5)
              cont <- cont + 1L
            }# END - ifelse
          })
          
          
      output$seeWarping7_9 <- renderPlot({
            if (length(weave()) < 9 || input$maxPlots < 9)
            {
              NULL
            } else
            {
              patterns <- weave()
              warping <- makeWarping()
              rowsSeq <- seq_len(nrow(patterns[[1L]]))
              colsSeq <- seq_len(ncol(patterns[[1L]]))
              
              cont <- 7L
              layout(t(1L:3))
              
              ## first plot
              image(x = rowsSeq, y = colsSeq, patterns[[cont]], 
                  col = c("gray80", "blue3"), 
                  yaxt = "n", xlab = "", ylab = "",
                  main = paste0("pattern: ", cont))
              axis(side = 2, at = colsSeq, labels = rev(colsSeq))
              title(ylab = "sequence", xlab = "Warping", line = 2)
              abline(v = seq_len(ncol(warping)) - .5)
              abline(h = 1:(5 * input$patternLength) - .5)
              cont <- cont + 1L
              
              ## second plot
              image(x = rowsSeq, y = colsSeq, patterns[[cont]], 
                  col = c("gray80", "blue3"), 
                  yaxt = "n", xlab = "", ylab = "",
                  main = paste0("pattern: ", cont))
              axis(side = 2, at = colsSeq, labels = rev(colsSeq))
              title(ylab = "sequence", xlab = "Warping", line = 2)
              abline(v = seq_len(ncol(warping)) - .5)
              abline(h = 1:(5 * input$patternLength) - .5)
              cont <- cont + 1L
              
              ## third plot
              image(x = rowsSeq, y = colsSeq, patterns[[cont]], 
                  col = c("gray80", "blue3"), 
                  yaxt = "n", xlab = "", ylab = "",
                  main = paste0("pattern: ", cont))
              axis(side = 2, at = colsSeq, labels = rev(colsSeq))
              title(ylab = "sequence", xlab = "Warping", line = 2)
              abline(v = seq_len(ncol(warping)) - .5)
              abline(h = 1:(5 * input$patternLength) - .5)
              cont <- cont + 1L
            }# END - ifelse
          })
      
      output$downloadReport <- downloadHandler(
          filename = function() {
            aux <- format(Sys.time(), "%Y-%m-%d_%X")
            for (i in 1L:3)
              aux <- sub(pattern = ":", replacement = ".", x = aux, fixed = TRUE)
            paste0('LoomingRPT_', aux, '.pdf')
          },
          
          content = function(file) {
            src <- normalizePath('templateLoom.Rmd')
            
            # temporarily switch to the temp dir, in case you do not have write
            # permission to the current working directory
            owd <- setwd(tempdir())
            on.exit(setwd(owd))
            file.copy(src, 'templateLoom.Rmd')
            
            require(rmarkdown)
            out <- render('templateLoom.Rmd', pdf_document(highlight = "haddock"))
            file.rename(out, file)
          }
      )
            
      
    })





