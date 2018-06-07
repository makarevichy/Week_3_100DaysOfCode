---
  title: "Response to the test task"
author: "Makarevich Y.V."
date: "07 06 2018"
output: html_document
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Task

Необходимо:<br/>
  1. Разработать модель для предсказания события Status <br/>
  2. Рассчитать вероятность наступления события Status<br/>
  3. Построить матрицу распределения, пример есть в файле<br/>
  4. Посчитать AUC и индекс Gini на обучающей и тестовой выборке<br/>
  5. Посчитать IV и WOE для переменных вашей модели или полученных агрегатов<br/>
  6. Кратко, пошагово, описать процесс разработки получившейся модели.<br/>
  
## Solution
  
  1. Загружаем пакеты для работы с нашими данными и открываем таблицу:
  ```{r, message=FALSE}
library(readxl)
library(dplyr)
library(caret)
library(Information)
df <- read_xlsx(path = 'df_test.xlsx') #I do not host this file
```

Смотрим таблицу :
  ```{r}
df

```

Данные в переменной Factor_5 не нормально распределены
```{r}
hist(df$Factor_5, main = 'Гистограмма переменной Factor_5')

```

мы их логарифмируем, также делаем некоторые другие преобразования:
  ```{r}
df$Factor_5 <- log(df$Factor_5 + 1)
df$`Вероятность наступления события status` <- NULL #Удаляем пустую переменную
df$status <- factor(df$status) 
levels(df$status) <- make.names(levels(factor(df$status))) # перекодируем целевую переменную status

```

Что обозначает каждый фактор нам не известно, поэтому добавлять новые переменные путем сложения переменных, перемножения и т.п. друг на друга я не стану (тем более там есть отрицательные значения, в т.ч. отсутствующие, которые закодированы -1 - это предположение). 
В таблице в отдельных факторах присутствуют значения, которые встречаются один или несколько раз, мы их перекодируем (хотя какого-то значимого эффекта это не принесет в итоге):
  ```{r}
df <- df %>%
  mutate(
    Factor_1 = case_when(
      Factor_1 >=2 ~ 2,
      TRUE ~ as.numeric(Factor_1)),
    Factor_2 = case_when(
      Factor_2 < -1 ~ -1,
      Factor_2 > 7 ~ 8,
      TRUE ~ as.numeric(Factor_2)),
    Factor_6 = case_when(
      Factor_6 <= -1 ~ -1,
      Factor_6 >= 2 ~ 2,
      TRUE ~ as.numeric(Factor_6)),
    Factor_7 = case_when(
      Factor_7 <= -1 ~ -1,
      Factor_7 >= 5 ~ 5,
      TRUE ~ as.numeric(Factor_7)),
    Factor_10 = case_when(
      Factor_10 < - 15 ~ -16,
      Factor_10 > 16 ~ 17,
      TRUE ~ as.numeric(Factor_10)),
    Factor_11 = case_when(
      Factor_11 > 2 ~ 3,
      TRUE ~ as.numeric(Factor_11)),
    Factor_12 = case_when(
      Factor_12 > 2 ~ 3,
      TRUE ~ as.numeric(Factor_12))
  )

```

Далее разбиваем выборку на тренировочную и тестовую в пропорции 90/10:
  ```{r}
division <- createDataPartition(df$status, p = 0.9, list = F)
df_train <- df[division,]
df_test <- df[-division,]

```

Проверяем, чтобы пропорции целевой переменной сохранились:
  ```{r}
round(prop.table(table(df_train$status)) * 100, 2) # доля в процентах каждого ответа в train
round(prop.table(table(df_test$status)) * 100, 2) # доля в процентах каждого ответа в test
```

Модели мы будем строить используя 10-кратную кросс-валидацию. Для большей устойчивость модели к реальным данным можно её повторить несколько раз добавив аргумент repeats = 3, но в нашем примере использовать не будем, чтобы ускорить процесс обучения.
```{r, message=FALSE}
ctrl <- trainControl(
  method = "cv",
  number = 10, # 10-кратная кросс-валидация
  verboseIter = FALSE,
  classProbs = T, # оцениваем вероятность принадлежности к классу
  summaryFunction = twoClassSummary)
```

Строим пробную модел методом градиентного спуска, метрикой будет площадь под кривой (ROC).

```{r, message=FALSE, warning=FALSE}
fit_gbm <- train(status ~ .,
                 data = df_train,
                 metric = 'ROC',
                 method = 'gbm',
                 trControl = ctrl)
```

Предсказываем оценки респондентов на тестовой выборке и смотрим метрику:
  ```{r}
pred_gbm <- predict(fit_gbm, df_test[,-13])
confusionMatrix(pred_gbm, df_test$status)
ROSE::roc.curve(df_test$status, pred_gbm, plotit = FALSE)

```

И видим что наша модель, мегко говоря, не очень корректно работает и часто предсказывает 0 вместо 1. Площадь под кривой 0.526 :(
  Но давайте взглянем еще раз на баланс целевой переменной:
    ```{r}
  round(prop.table(table(df_test$status)) * 100, 2)
  
  ```
  
  Видим что целевая переменная имеет некоторый дисбаланс достигающий 3 к 1. Он большой, но не критический. Тем не менее попробуем выровнять баланс и прибегнем к генерации синтетических данных.
  ```{r}
  library(ROSE) #загружаем библиотеку
  df_synth <- ovun.sample(status~., data=df, method = 'over', p = 0.5,seed=42)$data #уравниваем классы в пропорции 0.5
  #смотрим на пропорцию целевой
  round(prop.table(table(df_synth$status)) * 100, 2)
  
  ```
  
  Повторно разбиваем выборку на тренировочную и тестовую в пропорции 90/10:
    ```{r}
  division <- createDataPartition(df_synth$status, p = 0.9, list = F)
  df_train <- df_synth[division,]
  df_test <- df_synth[-division,]
  
  ```
  
  Строим модели методом градиентного спуска, рандом форестом и методом опорных векторов, оцениваем ROC:
    ```{r, message=FALSE, warning=FALSE}
  fit_gbm <- train(status ~ .,
                   data = df_train,
                   metric = 'ROC',
                   method = 'gbm',
                   trControl = ctrl)
  fit_rf <- train(status ~ .,
                  data = df_train,
                  metric = 'ROC',
                  method = 'rf',
                  trControl = ctrl)
  fit_svm <- train(status ~ .,
                   data = df_train,
                   metric = 'ROC',
                   method = 'svmRadial',
                   trControl = ctrl)
  ```
  
  Мы ограничимся стандартными параметрами поиска по сетке, которые высчитываются по умолчанию в пакете (caret) для каждой модели. Подгонять параметры моделей, для повышения их точности - сейчас такой цели у нас нет. 
  Теперь смотрим результаты построеных моделей:
    ```{r}
  resam <- resamples(list(GBM = fit_gbm,
                          RandomForest = fit_rf,
                          SVM = fit_svm))
  dotplot(resam, metric = "ROC")
  ```
  
  Из графика видим что максимально значение ROC получаем в модели построеной методом RandomForest (случайный лес).
  Посмотрим на важность предикторов в модели случайного леса:
    ```{r}
  varImp(fit_rf)
  ```
  
  2. Рассчитываем вероятность наступления события Status:
    ```{r}
  fit_prob <- predict(fit_rf, df[,-13], type = 'prob')
  levels(df$status) <- c("0", "1")
  df_for_write <- data.frame(df, `Вероятность наступления события status` = fit_prob$X1)
  df_for_write$Factor_5 <- exp(df_for_write$Factor_5) - 1 #возвращаем к исходному виду прологарифмированные данные
  openxlsx::write.xlsx(df_for_write, 'test_solutions_1.xlsx') #записываем таблицу в excel
  ```
  
  3. Построить матрицу распределения, пример есть в файле
  
  Строим таблицу распределения:
    ```{r}
  matrix_allocation <- split(df_for_write$Вероятность.наступления.события.status, 
                             cut(df_for_write$Вероятность.наступления.события.status, seq(0, 1, 0.05)))
  matrix_allocation <- data.frame(`Балл` = names(matrix_allocation),`Кол-во наблюдений` = rev(t(purrr::map_df(matrix_allocation, length))))
  matrix_allocation <- matrix_allocation %>% 
    mutate(`Доля наблюдений в сегменте балла` = round(`Кол.во.наблюдений`/sum(`Кол.во.наблюдений`) * 100, 2),
           `Доля наблюдений (накопленная)` = rev(cumsum(`Доля наблюдений в сегменте балла`)))
  matrix_allocation <- df_for_write %>% 
    filter(status == 1) %>%
    split(., cut(.$Вероятность.наступления.события.status, seq(0, 1, 0.05))) %>% 
    purrr::map_df(., nrow) %>% 
    t %>% 
    data.frame(`Число наблюдений, где  status=1` = rev(.)) %>% 
    bind_cols(matrix_allocation) %>% 
    select(names(matrix_allocation), everything(), - `.`) %>% 
    mutate(`Доля наблюдений, где status=1 в сегменте балла` = round(Число.наблюдений..где..status.1 / Кол.во.наблюдений * 100, 0),
           `Доля наблюдений, где status=1 в сегменте балла (накопленная)` = round((cumsum(Число.наблюдений..где..status.1) / cumsum(rev(`Кол.во.наблюдений`))*100), 0))
  head(matrix_allocation) #Смотрим что получилось
  openxlsx::write.xlsx(matrix_allocation, 'test_solutions_2.xlsx') #записываем таблицу в excel
  ```
  
  4. Посчитать AUC и индекс Gini на обучающей и тестовой выборке
  Считаем ROC на обучающей выборке:
    ```{r}
  fit_train <- predict(fit_rf, df_train[,-13])
  roc.curve(df_train$status, fit_train)
  ```
  
  Считаем ROC на тестовой выборке:
    ```{r}
  fit_test <- predict(fit_rf, df_test[,-13])
  roc.curve(df_test$status, fit_test)
  ```
  
  Функция Gini:
    ```{r}
  normalizedGini <- function(aa, pp) {
    Gini <- function(a, p) {
      if (length(a) !=  length(p)) stop("Actual and Predicted need to be equal lengths!")
      temp.df <- data.frame(actual = a, pred = p, range=c(1:length(a)))
      temp.df <- temp.df[order(-temp.df$pred, temp.df$range),]
      population.delta <- 1 / length(a)
      total.losses <- sum(a)
      null.losses <- rep(population.delta, length(a)) # Hopefully is similar to accumulatedPopulationPercentageSum
      accum.losses <- temp.df$actual / total.losses # Hopefully is similar to accumulatedLossPercentageSum
      gini.sum <- cumsum(accum.losses - null.losses) # Not sure if this is having the same effect or not
      sum(gini.sum) / length(a)
    }
    Gini(aa,pp) / Gini(aa,aa)
  }
  ```
  
  Смотрим Gini на обучающей выборке:
    ```{r}
  levels(df_train$status) <- c('0', '1')
  y_train_raw <- as.numeric(levels(df_train$status))[df_train$status]
  fit_train_prob <- predict(fit_rf, df_train[,-13], type = 'prob')
  normalizedGini(y_train_raw, fit_train_prob$X1)
  ```
  
  Смотрим Gini на тестовой выборке:
    ```{r}
  levels(df_test$status) <- c('0', '1')
  y_test_raw <- as.numeric(levels(df_test$status))[df_test$status]
  fit_test_prob <- predict(fit_rf, df_test[,-13], type = 'prob')
  normalizedGini(y_test_raw, fit_test_prob$X1)
  ```
  
  5. Посчитать IV и WOE для переменных вашей модели или полученных агрегатов.
  ```{r}
  df_for_write <- readxl::read_xlsx(path = 'test_solutions_1.xlsx')
  df_for_write$status <- as.numeric(df_for_write$status)
  IV <- Information::create_infotables(data = df_for_write[,-14], y='status', parallel=FALSE)
  
  x <- IV$Tables$Factor_1[,-c(1, 2, 3)]
  for (i in 2:length(IV$Tables)) {
    x <- rbind(x, IV$Tables[[i]][,-c(1, 2, 3)])
  }
  
  nam <- unlist(purrr::map(IV$Tables, nrow))
  nam <- rep(names(nam), nam)
  y <- vector('character', 0)
  for(j in 1:length(IV$Tables)){
    z <- IV$Tables[[j]][,1]
    y <- c(y, z)
  }
  iv_woe <- data.frame(`Название переменной` = nam, `Агрегация` = y, x)
  head(iv_woe)
  openxlsx::write.xlsx(iv_woe, "test_solutions_3.xlsx")
  ```
  
  Спасибо за внимание!