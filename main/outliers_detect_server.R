# 异常值检测服务端设置开始----------------------------------------------------
# 提交成功显示
observeEvent(input$submit_outliers_detect, { 
  if (input$submit_outliers_detect>0) {
    sendSweetAlert(
      session = session,
      title = "提交成功!",
      text = "数据上传成功，参数设置正确",
      type = "success")
  }
})

# 导入数据
# 运行正常20210111
user_data_outliers_detect <- reactive({
  table_in_test <- read.csv(input$data_input_outliers_detect$datapath,
                            header = T, 
                            stringsAsFactors = TRUE,
                            encoding = 'UTF-8')
  colnames(table_in_test)[1] <- 'group'
  table_in_test <- reshape2::melt(table_in_test, id.vars = 1)
  table_in_test <- table_in_test[,c(1,3)]
  colnames(table_in_test) <- c('group','value')
  table_in_test <<- table_in_test
})

# 下载示例数据
# 运行正常20210111
output$download_demo_data_outliers_detect <- downloadHandler(
  filename = '异常值正态性方差齐性示例数据.csv',
  content = function(file){
    file.copy('./demo_data/异常值正态性方差齐性示例数据.csv',file)
  }
)

# 展示求和结果
output$taboutput_tab_outliers_detect_view <- renderDataTable(outliers_detect_temp(),
                                                             options = list(
                                                               pageLength = 4
                                                             ))
outliers_detect_temp <- eventReactive(input$submit_outliers_detect,{
  if (input$submit_outliers_detect > 0) {
    
    user_records <- data.table::fread('./www/user records.txt', encoding = 'UTF-8')
    temp <- as.character(Sys.time())
    temp <- stringr::str_split(temp, ' ')
    date <- temp[[1]][1]
    time <- temp[[1]][2]
    user_record <- data.frame(Cat = 'outlier',
                              Date = date,
                              Time = time)
    user_records <- rbind(user_records, user_record)
    data.table::fwrite(user_records,file = './www/user records.txt')
    
    
    df <- user_data_outliers_detect()
    
    res <- NULL
    
    for (i in unique(df$group)) {
      df_sub <- df[df$group == i,]
      
      mean <- aggregate(df_sub$value, by = list(df_sub$group), FUN = mean)
      colnames(mean) <- c('group','mean')
      
      df_sub <- merge(df_sub,mean, by = 'group')
      pdf('./results/temple.pdf')
      box <- boxplot(df_sub$value)
      dev.off()
      outliers <- box[["out"]]
      df_sub$异常值与否 <- if_else(df_sub$value %in% outliers,
                              '异常值','正常值')
      
      shap <- shapiro.test(df_sub$value)[["p.value"]]
      
      df_sub$正态性 <- ifelse(shap < 0.05,'不服从正态分布','服从正态分布')
      
      res <- rbind(res, df_sub)
    }
    
    if (input$save_or_not_outliers_detect == 'TRUE') {
      res <-res
    }else{
      res$new_data <- ifelse(res$异常值与否 == '异常值',
                             res$mean,
                             res$value)
    }
    
    df <- res
    # 保存分析结果
    write.csv(df, file = './results/res_tab.csv',row.names = FALSE)
    write.table(df, file = './results/res_tab.txt',row.names = FALSE)
    xlsx::write.xlsx(df, file = './results/res_tab.xlsx',row.names = FALSE)
  }
  df
})

# 展示箱线图
output$box_outliers_detect_view <- renderPlot(outliers_detect_temp1(),
                                              height = 330,
                                              width = 600)
outliers_detect_temp1 <- eventReactive(input$submit_outliers_detect,{
  if (input$submit_outliers_detect > 0) {
    
    df <- user_data_outliers_detect()
    
    p <- ggplot(df,aes(group,value,fill = group))+
      geom_boxplot() +
      scale_fill_aaas() +
      theme_bw() +
      theme(legend.position = 'none')
  }
  p
})

# 展示Q-Q图
output$qqplot_outliers_detect_view <- renderPlot(outliers_detect_temp3(),
                                                 height = 330,
                                                 width = 600)

outliers_detect_temp3 <- eventReactive(input$submit_outliers_detect,{
  if (input$submit_outliers_detect > 0) {
    
    df <- user_data_outliers_detect()
    
    opar <- par(no.readonly = TRUE)
    par(mfrow = c(1,length(unique(df$group))))
    
    #pdf('./results/temple.pdf')
    for (i in unique(df$group)) {
      df_sub <- df[df$group == i,]
      qqnorm(df_sub$value,main = unique(df_sub$group)) 
      qqline(df_sub$value)
    }
    par(opar)
    dev.off()
  }
})

# 展示离群值
output$otherinfo_outliers_detect_view <- renderText({
  '暂未上线'
})
#output$other_outliers_detect_view <- renderPlot(outliers_detect_temp4(),
#height = 330,
#width = 600)

#outliers_detect_temp4 <- eventReactive(input$submit_outliers_detect,{
#if (input$submit_outliers_detect > 0) {
#dev.off()
#p1 <- ggplot() +
#labs(title = '暂未上线')
#}
#p1
#})

# 下载分析结果
# 运行正常20210111
output$taboutput_outliers_detect_download <- downloadHandler(
  filename <- function(){
    paste(stringr::str_sub(input$data_input_outliers_detect$name,
                           1,
                           (nchar(input$data_input_outliers_detect$name) - 4)),
          '_计算结果',input$outliers_detect_filetype,sep = ''
    )
  },
  content <- function(file){
    if (input$outliers_detect_filetype == '.csv') {
      file.copy('./results/res_tab.csv',file)
    }else if (input$outliers_detect_filetype == '.txt') {
      file.copy('./results/res_tab.txt',file)
    }else{
      file.copy('./results/res_tab.xlsx',file)
    }
  }
)
# 异常值检测服务端设置开始----------------------------------------------------
