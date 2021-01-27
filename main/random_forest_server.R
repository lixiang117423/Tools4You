# 提交成功显示
observeEvent(input$submit_rf, { 
  if (input$submit_rf>0) {
    sendSweetAlert(
      session = session,
      title = "提交成功!",
      text = "数据上传成功，参数设置正确",
      type = "success")
  }
})
# 导入数据
# 运行正常20210111
user_data_rf <- reactive({
  
  table_in_test <- data.table::fread(input$data_input_rf$datapath,,
                                     header = TRUE,
                                     encoding = 'UTF-8')
  table_in_test <<- table_in_test
})

# 下载示例数据
# 运行正常20210111
output$download_demo_data_rf <- downloadHandler(
  filename = '随机森林示例数据.csv',
  content = function(file){
    file.copy('./demo_data/随机森林示例数据.csv',file)
  }
)


# 分类准确率
output$acc_rf_view <- renderDataTable(df_table_rf())

df_table_rf <- eventReactive(input$submit_rf,{
  if (input$submit_rf > 0) {
    
    user_records <- data.table::fread('./www/user records.txt', encoding = 'UTF-8')
    temp <- as.character(Sys.time())
    temp <- stringr::str_split(temp, ' ')
    date <- temp[[1]][1]
    time <- temp[[1]][2]
    user_record <- data.frame(Cat = 't-test',
                              Date = date,
                              Time = time)
    user_records <- rbind(user_records, user_record)
    data.table::fwrite(user_records,file = './www/user records.txt')
    
    # 开始RF
    df <- user_data_rf()
    colnames(df)[1] <- 'group'
    df$group <- as.factor(df$group)
    
    # 划分数据
    set.seed(123)
    ind <- sample(2, nrow(df), 
                  replace = TRUE, 
                  prob = c(as.numeric(input$train_per_rf), 
                           1-as.numeric(input$train_per_rf)))
    train <- df[ind==1,] %>%
      na.omit()
    
    test <- df[ind==2,] %>%
      na.omit()
    
    # 构建模型
    if (input$ntree_rf == '500') {
      ntree <- 500
    }else{
      ntree <- as.numeric(input$ntree_rf)
    }
    
    if (input$mtry_rf == '0') {
      mtry <- sqrt((ncol(train)-1))
    }else{
      mtry <- as.numeric(input$mtry_rf)
    }
    fit <- randomForest(group ~ .,
                        data = train,
                        ntree = ntree,
                        mtry = mtry,
                        importance = TRUE,
                        proximity = TRUE)
    
    mat <- fit[["confusion"]] %>% as.data.frame()
    col <- colnames(mat)
    
    mat$prediction <- rownames(mat)
    
    mat <- dplyr::select(mat, c('prediction', col))
    
    mat$class.error <- paste(round((1 - mat$class.error)*100,2),
                             '%',sep = '')
    colnames(mat)[ncol(mat)] <- '分类准确率'

  }
  mat
})



# 预测准确率
output$pre_per_rf <- renderDataTable(df_table_rf1())

df_table_rf1 <- eventReactive(input$submit_rf,{
  if (input$submit_rf > 0) {
    
    # 开始RF
    df <- user_data_rf()
    colnames(df)[1] <- 'group'
    df$group <- as.factor(df$group)
    
    # 划分数据
    set.seed(123)
    ind <- sample(2, nrow(df), 
                  replace = TRUE, 
                  prob = c(as.numeric(input$train_per_rf), 
                           1-as.numeric(input$train_per_rf)))
    train <- df[ind==1,] %>%
      na.omit()
    
    test <- df[ind==2,] %>%
      na.omit()
    
    # 构建模型
    # 构建模型
    if (input$ntree_rf == 500) {
      ntree <- 500
    }else{
      ntree <- as.numeric(input$ntree_rf)
    }
    
    if (input$mtry_rf == 1) {
      mtry <- sqrt((ncol(train)-1))
    }else{
      mtry <- as.numeric(input$mtry_rf)
    }
    
    fit <- randomForest(group ~ .,
                        data = train,
                        ntree = ntree,
                        mtry = mtry,
                        importance = TRUE,
                        proximity = TRUE)
    
    pre <- predict(fit, test)
    
    lx <- confusionMatrix(pre, test$group)[["table"]] %>% 
      as.data.frame()
    
    lx <- reshape2::dcast(lx, Prediction ~ Reference)
    lx$cat <- 1
    for (i in 1:nrow(lx)) {
      lx$cat[i] = paste(round(lx[i,i+1]/sum(lx[i,2:(ncol(lx)-1)])*100,2),
                        '%', sep = '')
    }
    
    colnames(lx)[ncol(lx)] <- '分类准确率'
    
  }
  lx
})



# 变量重要性
output$variable_rf <- renderDataTable(df_table_rf2(),options = list(pageLength = 4))

df_table_rf2 <- eventReactive(input$submit_rf,{
  if (input$submit_rf > 0) {
    
    # 开始RF
    df <- user_data_rf()
    colnames(df)[1] <- 'group'
    df$group <- as.factor(df$group)
    
    # 划分数据
    set.seed(123)
    ind <- sample(2, nrow(df), 
                  replace = TRUE, 
                  prob = c(as.numeric(input$train_per_rf), 
                           1-as.numeric(input$train_per_rf)))
    train <- df[ind==1,] %>%
      na.omit()
    
    test <- df[ind==2,] %>%
      na.omit()
    
    # 构建模型
    # 构建模型
    if (input$ntree_rf == 500) {
      ntree <- 500
    }else{
      ntree <- as.numeric(input$ntree_rf)
    }
    
    if (input$mtry_rf == 1) {
      mtry <- sqrt((ncol(train)-1))
    }else{
      mtry <- as.numeric(input$mtry_rf)
    }
    
    fit <- randomForest(group ~ .,
                        data = train,
                        ntree = ntree,
                        mtry = mtry,
                        importance = TRUE,
                        proximity = TRUE)
    
    imp <- as.data.frame(round(importance(fit), 2))
    imp <- imp[order(imp$MeanDecreaseAccuracy,decreasing = TRUE),]
    imp$variable <- rownames(imp)
    imp2 <- dplyr::select(imp, c('variable','MeanDecreaseAccuracy','MeanDecreaseGini'))
    
    # 保存分析结果
    write.csv(imp, file = './results/rest_tab.csv',row.names = FALSE)
    write.table(imp, file = './results/rest_tab.txt',row.names = FALSE)
    xlsx::write.xlsx(imp, file = './results/rest_tab.xlsx',row.names = FALSE)
  }
  imp2
})



# 交叉验证
output$cv_plot <- renderPlot(df_table_rf3(),
                             height = 350,
                             width = 600)

df_table_rf3 <- eventReactive(input$submit_rf,{
  if (input$submit_rf > 0) {
    
    # 开始RF
    df <- user_data_rf()
    colnames(df)[1] <- 'group'
    df$group <- as.factor(df$group)
    
    # 划分数据
    set.seed(123)
    ind <- sample(2, nrow(df), 
                  replace = TRUE, 
                  prob = c(as.numeric(input$train_per_rf), 
                           1-as.numeric(input$train_per_rf)))
    train <- df[ind==1,] %>%
      na.omit()
    
    test <- df[ind==2,] %>%
      na.omit()
    
    # 构建模型
    if (input$ntree_rf == 500) {
      ntree <- 500
    }else{
      ntree <- as.numeric(input$ntree_rf)
    }
    
    if (input$mtry_rf == 1) {
      mtry <- sqrt((ncol(train)-1))
    }else{
      mtry <- as.numeric(input$mtry_rf)
    }
    
    fit <- randomForest(group ~ .,
                        data = train,
                        ntree = ntree,
                        mtry = mtry,
                        importance = TRUE,
                        proximity = TRUE)
    # 交叉验证
    # 交叉验证
    n <- ncol(train)-1
    my.train <- train[1:n]
    set.seed(117)
    result= rfcv(train, train$group, cv.fold=10, scale = "log", step = 0.9)
    with(result, plot(n.var, error.cv, log="x", type="o", lwd=2))
    result1 <- result
    
    error.cv <- data.frame(num = result$n.var, 
                           error.1 =  result$error.cv)
    
    # 用另外4组随机数来看错误率，然后绘图
    for (i in 323:326){
      print(i)
      set.seed(i)
      result <- rfcv(train, 
                     train$group, 
                     cv.fold=10, 
                     scale = "log", 
                     step = 0.9)
      error.cv <- cbind(error.cv, result$error.cv)
    }
    
    ## 绘制交叉验证曲线
    n.var <- error.cv$num
    error.cv <- error.cv[,2:6]
    colnames(error.cv) <- paste('err',1:5,sep = '.')
    err.mean <- apply(error.cv, 1, mean)
    allerr <- data.frame(num = n.var, err.mean = err.mean, error.cv)
    optimal <- allerr[allerr$err.mean == min(allerr$err.mean),1]
    # 绘图
    allerr <- allerr[allerr$num < 100,]
    p <- ggplot() + 
      geom_line(aes(x = allerr$num, y = allerr$err.1), colour = 'grey') + 
      geom_line(aes(x = allerr$num, y = allerr$err.2), colour = 'grey') + 
      geom_line(aes(x = allerr$num, y = allerr$err.3), colour = 'grey') + 
      geom_line(aes(x = allerr$num, y = allerr$err.4), colour = 'grey') + 
      geom_line(aes(x = allerr$num, y = allerr$err.mean), colour = 'red') + 
      geom_vline(xintercept = optimal, colour='black', 
                 lwd=0.36, linetype="dashed") + 
      coord_trans(x = "log2") +
      #scale_x_continuous(breaks = c(1, 2, 5, 10, 20, 30, 50, 100)) + # , max(allerr$num)
      labs(title=paste('Training set (n = ', dim(train)[1],')', sep = ''), 
           x='变量数量', y='交叉验证错误率') + 
      annotate("text", x = optimal, y = max(allerr$err.mean), 
               label=paste("最优值 = ", optimal, sep="")) + 
      theme_classic() +
      theme(legend.position = 'none',
            legend.title = element_blank(),
            legend.text = element_text(color = 'black',size = 12, 
                                       family = 'Arial', face = 'plain'),
            panel.background = element_blank(),
            panel.grid = element_blank(),
            axis.text = element_text(color = 'black',size = 15, 
                                     family = 'Arial', face = 'plain'),
            axis.title = element_text(color = 'black',size = 15, 
                                      family = 'Arial', face = 'plain'),
            axis.ticks = element_line(color = 'black'))
    
    ggsave(p, 
           filename = paste('./results/', '交叉验证结果.pdf', sep = ''),
           width = 6,
           height = 4,
           device = cairo_pdf)

  }
  p
})


# 下载分析结果
# 运行正常20210111
output$taboutput_rf_download <- downloadHandler(
  filename <- function(){
    paste(stringr::str_sub(input$data_input_rf$name,
                           1,
                           (nchar(input$data_input_rf$name) - 4)),
          '_统计分析结果.xlsx',sep = ''
    )
  },
  content <- function(file){
    file.copy('./results/rest_tab.xlsx',file)
  }
)

# 下载图片
output$download_figure__rf <- downloadHandler(
  filename <- function(){
    paste(stringr::str_sub(input$data_input_rf$name,
                           1,
                           (nchar(input$data_input_rf$name) - 4)),
          '_交叉验证结果.pdf',sep = ''
    )
  },
  content <- function(file){
    file.copy('./results/交叉验证结果.pdf',file)
  }
)











