#ライブラリ読み込み
library(shiny)
library(DT)
library(rhandsontable)
library(lavaan)
library(DiagrammeR)
library(lavaanPlot)
library(semPlot)


#関連ファイル呼び出し。
source("ezsem.R")

#空の潜在変数のデータフレーム
lv <- data.frame(Latent.Variable = c(NA,NA,NA))
lv[, 1] <- as.character(lv[, 1])


shinyServer(function(input, output, session) {
  
  observeEvent(input$file, {
    
    #ファイルをコンソールに表示(チェック用)
    print(input$file$name)

    #データを読み込み
    raw.data <- reactive({read.csv(input$file$datapath)})
    
    #データを表示
    #https://github.com/rstudio/shinydashboard/issues/22
    output$data <- renderDT(raw.data(), 
                            server = TRUE,
                            filter = "top",
                            selection = "none",
                            options = list(autoWidth = TRUE))
    
    #データが読み込まれてからの処理
    observeEvent(input$data_rows_all, {
      
      #DTで選択した部分と、解析に使用できるデータのみ抜粋。
      proc.data <- reactive({
        as.worth(
          data.rows(raw.data(), input$data_rows_all)
          )
        })
      
      #変数選択(因子はそのまま)
      output$vec.obj <- renderUI({ 
        checkboxGroupInput("vec.obj", "Numeric and character", 
                    choices = names(proc.data()),
                    selected = names(proc.data()))
      })
      
      #潜在変数を記入
      output$latent.variable <- rhandsontable::renderRHandsontable({
        rhandsontable(lv) %>% hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE)
      })
      
      
      #因子を数値に変換して選択
      observeEvent(input$vec.obj,{
        
        #因子を数値に変換したデータ
        num.data <- reactive({
          df.fac.num(df = proc.data(), vec = input$vec.obj, scale = input$data.scale)
          })
        
        #最終的な変数選択
        output$vec.last <- renderUI({ 

          checkboxGroupInput("vec.last", "Numeric", 
                             choices = colnames(num.data()),
                             selected = colnames(num.data()))
        })
        
        #データを表示
        #https://github.com/rstudio/shinydashboard/issues/22
        output$last.data <- renderDT(signif(num.data(),4), 
                                server = TRUE,
                                selection = "none",
                                options = list(autoWidth = TRUE))
        
        
        observeEvent(input$vec.last, {
          #モデルをチェックボックスを使って作成。
          #https://www.youtube.com/watch?v=lzYTDmmCPnY
          output$model <- rhandsontable::renderRHandsontable({
            hot.cb(vec.obj = input$vec.last, vec.con = lv.sel(input$latent.variable))
          })

          
          output$model.text <- renderText({df.model.input(input$model)})
          
          sem.result <- reactive({
            ezsem(df.model.input(input$model), num.data(), 
                  estimator = input$estimator)
            })
          
          output$summary <- renderPrint({
            summary(sem.result(), fit.measures = TRUE)
            })
          
          output$model.fit <- renderPrint({fitMeasures(sem.result())})
          
          #https://rstudio-pubs-static.s3.amazonaws.com/10828_e6380adea6c44ee18358ae42ba0cc89c.html
          output$model.modification <- renderPrint({modificationindices(sem.result())})
          
          output$imp.fit.text <- renderPrint({cat(imp.fit(sem.result())$text)})
          
          output$semplot <- renderPlot(semPlot::semPaths(sem.result(), "std"))
          
          output$plot <- DiagrammeR::renderGrViz({ezsem.plot(sem.result())})

        })
      })
    })
  })
})
