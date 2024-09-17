#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(dplyr)
library(readr)
library(ggplot2)
library(ggcorrplot)
library(tidyr)
library(DT)
library(shinythemes)
library(outliers)


find_outliers_zscore <- function(data, threshold = 3) {
  z_scores <- scale(data)
  outliers <- abs(z_scores) > threshold
  return(data[outliers])
}


# Define UI 
ui <- fluidPage( theme = shinytheme("cerulean"),
                 titlePanel("Цены на жилье в Бостоне"),
                 
                 mainPanel( 
                   h3("Описание датасета"),
                   tags$ul(
                     tags$li(tags$b("crim: "), "уровень преступности на душу населения по городам."),
                     tags$li(tags$b("zn: "),"доля жилых земель, зонированных для участков площадью более 25 000 кв. футов."),
                     tags$li(tags$b("indus: "),"доля площадей под неторговую деятельность на город."),
                     tags$li(tags$b("chas: "),"фиктивная переменная реки Чарльз (= 1, если тракт ограничивает реку; 0 в противном случае)."),
                     tags$li(tags$b("nox: "),"концентрация оксидов азота (частей на 10 миллионов)."),
                     tags$li(tags$b("rm: "),"среднее количество комнат в жилище."),
                     tags$li(tags$b("age: "),"доля квартир, занимаемых владельцами, построенных до 1940 года."),
                     tags$li(tags$b("dis: "),"средневзвешенное значение расстояний до пяти центров занятости Бостона."),
                     tags$li(tags$b("rad: "),"индекс доступности радиальных магистралей."),
                     tags$li(tags$b("tax: "),"ставка налога на имущество на полную стоимость за 10 000 долларов США."),
                     tags$li(tags$b("ptratio: "),"соотношение учеников и учителей по городам."),
                     tags$li(tags$b("black: ")," 1000(Bk - 0,63)^2, где Bk — доля чернокожих по городам."),
                     tags$li(tags$b("lstat: ")," более низкий статус населения (в процентах)."),
                     tags$li(tags$b("medv: ")," средняя стоимость домов, занимаемых владельцами, в 1000 долларов."),
                   ),
                   tabsetPanel(
                     tabPanel("Dataframe", DTOutput("dataset")),
                     tabPanel("Summary", verbatimTextOutput("summary")),
                   ),
                   
                     h3("Гистограммы", align = "center"),
                   
                   sidebarLayout(
                     sidebarPanel(
                       selectInput("feature", "Select Feature",
                                   choices = c("crim", "zn", "indus", 
                                              "nox","rm","age","dis","rad","black","tax","Istat","ptratio", "medv")),
                       sliderInput("bins", "Number of Bins:",
                                   min = 1, max = 50, value = 10),
                       
                     ),
                     mainPanel( 
                       plotOutput("histogram"),
                       textOutput("outliers")
                     )
                   ),
                   h3("Диаграмма рассеяния", align = "center"),
                   sidebarLayout(
                     sidebarPanel(
                       selectInput("x_var", "X-axis Feature:", choices = c("crim", "zn", "indus", 
                                                                            "nox","rm","age","dis","rad","black","Istat","ptratio","tax","medv")),
                       selectInput("y_var", "Y-axis Feature:", choices = c( "crim","zn", "indus", 
                                                                            "nox","rm","age","dis","rad","black","Istat","ptratio","tax","medv"))
                     ),
                     mainPanel(
                       plotOutput("feature_plot"),
                       htmlOutput("t_criteria"),
                       verbatimTextOutput("testAll")
                     )
                   ),
                   h3("Диаграмма коррелации ", align = "center"),
                   
                   sidebarLayout(
                     sidebarPanel (
                       selectInput("x_cor", "First Feature correlation:", choices = c("crim", "zn", 
                                                                           "rm","age","dis","rad","black","Istat","ptratio","medv")),
                       selectInput("y_cor", "second Feature correlation:", choices = c("zn","crim",
                                                                           "rm","age","dis","rad","black","Istat","ptratio","medv"))
                     ),
                     mainPanel( plotOutput("correlation_table"),
                                textOutput("remove_corr")   
                     )
                   ),
                   mainPanel(
                     
                     h4("Интерпретация коррелации", align = "center"),
                     htmlOutput("corr_test_output"),
                   ),
                   
                   
                   mainPanel(
                     tabsetPanel(
                       tabPanel("Linear Model", verbatimTextOutput("linear_model")),
                       tabPanel("Metrics", verbatimTextOutput("evaluation_metrics")),
                       tabPanel("Coefficient", verbatimTextOutput("coef"))
                       ),
                     h3("Качество подгонки модели.",align = "center" ),
                     textOutput("lm_interp")
                     ),
                   mainPanel(
                     h3("Интерпретация коэффициентов линейной регрессии"),
                     tags$ul(
                       tags$li("увеличение показателя crim на 1 влечет снижение medv на 0.09."),
                       tags$li("увеличение показателя zn на 1 влечет увеличение medv на 0.07."),
                       tags$li("увеличение показателя rm на 1 влечет увеличение medv на 0.35."),
                       tags$li("увеличение показателя age на 1 влечет снижение medv на 0.07."),
                       tags$li("увеличение показателя dis на 1 влечет снижение medv на 0.22."),
                       tags$li("увеличение показателя rad на 1 влечет увеличение medv на 0.01."),
                       tags$li("увеличение показателя ptratio на 1 влечет снижение medv на 0.19."),
                       tags$li("увеличение показателя black на 1 влечет увеличение medv на 0.11"),
                       tags$li("увеличение показателя lstat на 1 влечет снижение medv на 0.45."),
                       
                     )
                   ),
                   mainPanel(
                     tabsetPanel(
                       tabPanel("Actual vs Predicted", plotOutput("actual_predicted")),
                       tabPanel("Fitted vs residual",  plotOutput("fitted_residual")),
                       tabPanel("residual vs leverage",  plotOutput("residual_leverage"))
                       
                     ),
                     h3("Интерпретация графиков", align ="center"),
                     textOutput("lm_plot"),
                   ),
                   mainPanel(
                     h3("Заключение", align="center"),
                     textOutput("conclusion")
                   )
                   
                 )
)

# Define server logic
server <- function(input, output) {
  
  df <- read.csv("boston_data.csv")
 
  output$dataset <- renderDT({
    datatable(df)
  })
  # Render summary table
  output$summary <- renderPrint({
    summary(df)
  })
  
  
  # Render histogram
  output$histogram <- renderPlot({
    req(input$feature)
    
    # Extract selected feature
    selected_feature <- df[[input$feature]]
    
    hist(selected_feature, breaks = input$bins, 
         xlab = input$feature, main = paste("Гистограмма", input$feature), col = "blue")
  })
  #выбросы
  output$outliers <- renderText({
    req(input$feature)
    feature_data <- df[[input$feature]]
    outliers <- find_outliers_zscore(feature_data)
    if (length(outliers) == 0) {
      return("Нет выбросов")
    } else {
      out_text <- paste("Выбросы для", input$feature, ":", paste(outliers, collapse = ", "))
      return(out_text)
    }
  })
  output$feature_plot <- renderPlot({
    ggplot(data = df, aes_string(x = input$x_var, y = input$y_var, color = factor(df$chas))) +
      geom_point() +
      labs(x = input$x_var, y = input$y_var, color = "chas") +
      theme_minimal()
  })
  
  output$t_criteria <-renderUI({
    req(input$x_var, input$y_var)
    
    selected_feature <-df[[input$x_var]]
    compare_feature <-df[[input$y_var]]
    
    #calculate t-test
    t_test_result <-t.test(selected_feature, compare_feature)
    
    if(t_test_result$p.value < 0.05){
      HTML ( paste("<b>Нулевая гипотеза: </b> среднее значение" , input$x_var, "равно среднему значению", input$y_var,
            ".<br>T-критерий равен", round(t_test_result$statistic, 2), "и  p-value равно",
            round(t_test_result$p.value, 4), ". P-значение меньше уровня значимости 0.05.Мы можем отвергнуть нулевую гипотезу."))
    }
    else{
     HTML( paste("<b>Нулевая гипотеза:</b>среднее значение" , input$x_var, "равно среднему значению", input$y_var,
            "T-критерий равен", round(t_test_result$statistic, 2), "и  p-value равно",
            round(t_test_result$p.value, 4), ".P-значение больше уровня значимости 0.05.Мы не можем отвергнуть нулевую гипотезу."))
    }
    
  })
  
  
  # Filter out numeric columns
  numeric_columns <- sapply(df, function(x) is.numeric(x) && length(unique(x)) > 5)  
  numeric_data <- df[, numeric_columns]
  
  
  df_scaled <- as.data.frame(scale(numeric_data))
  
  correlation_matrix <- round(cor(df_scaled),1)
  
  
  # Render correlation table
  output$correlation_table <- renderPlot({
    ggcorrplot(correlation_matrix, hc.order = TRUE, 
               type = "upper", 
               lab = TRUE, 
               lab_size = 3, 
               method="circle", 
               colors = c("tomato2", "white", "springgreen3"), 
               title="Таблица корреляции", 
               ggtheme=theme_bw)
  })
  
  #remove column with high correlation
  a<-caret::findCorrelation(correlation_matrix, cutoff = 0.75, verbose = TRUE)
  remove_col <- colnames(df_scaled)[a]
  df_scaled <- as.data.frame(df_scaled) %>% select(-which(colnames(df_scaled) %in% remove_col))

  output$remove_corr <- renderText({
    paste("Признаки с высокой корреляцией, которые необходимо удалить из
           набор данных:", paste(remove_col, collapse = ", "))
  })
 
  # Calculate correlation test 
  output$corr_test_output <- renderUI({
    req(input$x_cor, input$y_cor)
    
    cor_x <- df_scaled[[input$x_cor]]
    cor_y <- df_scaled[[input$y_cor]]
    
  
    corr_test_result <- cor.test(cor_x, cor_y)
    
    
    if (is.na(corr_test_result$estimate)) {
      "Коэффициент корреляции не может быть рассчитан."
    } else {
      if (corr_test_result$p.value < 0.05) {
       HTML( paste("<b>Нулевая гипотеза:</b> коэффициент корреляции между переменными", input$x_cor,
              "и ", input$y_cor, "равен нулю.<br>",
              "Коэффициент корреляции равно ", round(corr_test_result$estimate, 2),
              ",  p-value равно", round(corr_test_result$p.value, 4), "и t равно", round(corr_test_result$statistic, 4), 
              ". Tаким образом, гипотеза о равенстве нулю коэффициента корреляции не принимается на выбранном уровне значимости."))
      } else {
        HTML( paste("<b>Нулевая гипотеза:</b> коэффициент корреляции между переменными", input$cor_x,
              "и ", input$cor_y, "равен нулю.<br>",
              "Коэффициент корреляции равно ", round(corr_test_result$estimate, 2),
              ",  p-value равно", round(corr_test_result$p.value, 4), "и t равно", round(corr_test_result$statistic, 4), 
              ". Таким образом, гипотеза о равенстве нулю коэффициента корреляции не отвергается на выбранном уровне значимости."))
      }
    }
  })
  
  
  #linear regression
  set.seed(123)
  index <- sample(nrow(df_scaled), nrow(df_scaled) * 0.80)
  df_train <- df_scaled[index, ]
  df_test <- df_scaled[-index, ]
  lm_model <- lm(medv ~ ., data = df_train)
  
  predictions <- predict(lm_model, newdata = df_test)
  mse <- mean((df_test$medv - predictions)^2)
  r_squared <- summary(lm_model)$r.squared
  
  output$linear_model <- renderPrint({
    summary(lm_model)
  })
  
  #evaluation metric
  output$evaluation_metrics <- renderPrint({
    lm_pred <- predict(lm_model, newdata = df_test)
    mse <- mean((df_test$medv - lm_pred)^2)
    r_squared <- summary(lm_model)$r.squared
    cat("Mean Squared Error (MSE):", mse, "\n")
    cat("R-squared (R2):", r_squared, "\n")
  })
  
  # coefficients
  output$coef <- renderPrint({
    coefficients <- coef(lm_model)
    print(coefficients)
  })
  
  output$lm_interp <- renderText({
    paste("Для построенной модели коэффициент детерминации 0.72, 
          подогнанная регрессия объясняет основную часть вариабельности 
          ключевой пременной medv. P-value для F-критерия равно 0 означает, 
          что нулевая гипотеза о том, что все коэффициенты независимых переменных 
          равны нулю, отвергается на уровне значимости 0.05. Таким образом регрессия значима.")
  })
  
  # Plot actual vs. predicted values
  output$actual_predicted <- renderPlot({
    lm_pred <- predict(lm_model, newdata = df_test)
    plot(df_test$medv, lm_pred, xlab = "Actual", ylab = "Predicted", main = "Actual vs. Predicted Values")
    abline(0, 1, col = "red")
  })
  
  output$fitted_residual <- renderPlot({
    ggplot(lm_model, aes(.fitted, .resid))+ geom_point() + 
      stat_smooth(method="loess") + 
      geom_hline(yintercept=0, col="red", 
                 linetype="dashed") + 
    labs(x="Подогнанные значения (fitted)", y="Остатки регрессии (residuals)", 
         title = "Residual vs Fitted Plot")+theme_bw() })
  
 
  output$residual_leverage <- renderPlot({ 
    ggplot(lm_model, aes(.hat, .stdresid)) + 
      geom_point(aes(size = .cooksd), na.rm = TRUE) + 
      stat_smooth(method = "loess", na.rm = TRUE) + 
      labs(x = "Leverage", y = "Standardized Residuals", title = "Residual vs Leverage Plot") + 
      scale_size_continuous("Cook's Distance", range = c(1, 5)) + 
      theme_bw() + 
      theme(legend.position = "bottom")
  })
  output$lm_plot <- renderText({
    paste("На графике actual vs predicted мы можем наблюдать сильную взаимосвязь 
          между прогнозируемыми значениями и текущими значениями, что подтверждает 
          хорошое качество нашей модели. На других графиках мы можем 
          более подробно увидеть выбросы модели. предположения о однородности дисперсии 
          остатков в значительной степени выполняются.")
  })
  
  output$conclusion <-renderText({
    paste("Мы проводим статистический анализ набора данных о ценах на жилье в Бостоне и связи 
    между признаками этого набора данных. Чтобы спрогнозировать цену дома в Бостоне, 
    мы использовали линейную регрессию и получили R^2, равный 0,72, что означает, что наша модель 
    делает качественное предсказание. Мы также даем некоторые пояснения по поводу других 
          статистических показателей в модели.")
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)