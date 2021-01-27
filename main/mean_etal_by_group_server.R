# 分组求和/平均值/标准差/标准误服务端设置开始---------------------------------
# 提交成功显示
observeEvent(input$submit_mean_etal_by_group, { 
  if (input$submit_mean_etal_by_group>0) {
    sendSweetAlert(
      session = session,
      title = "提交成功!",
      text = "数据上传成功，参数设置正确",
      type = "success")
  }
})

# 导入数据
# 运行正常20210111
user_data_mean_etal_by_group <- reactive({
  table_in_test <- read.csv(input$data_input_mean_etal_by_group$datapath,
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
output$download_demo_data_mean_etal_by_group <- downloadHandler(
  filename = '分组求和均值标准差标准误示例数据.csv',
  content = function(file){
    file.copy('./demo_data/分组求和均值标准差标准误示例数据.csv',file)
  }
)

# 计算上述4个参数
mean_etal_by_group_res <- reactive({
  res <- mean_sd_se_num(user_data_mean_etal_by_group())
  df <- res[,-2]
  df <- df[!duplicated(df),]
})

# 展示求和结果
output$taboutput_sum_mean_etal_by_group_view <- renderDataTable(mean_etal_temp(),
                                                                options = list(
                                                                  pageLength = 5
                                                                ))
mean_etal_temp <- eventReactive(input$submit_mean_etal_by_group,{
  if (input$submit_mean_etal_by_group > 0) {
    
    user_records <- data.table::fread('./www/user records.txt', encoding = 'UTF-8')
    temp <- as.character(Sys.time())
    temp <- stringr::str_split(temp, ' ')
    date <- temp[[1]][1]
    time <- temp[[1]][2]
    user_record <- data.frame(Cat = 'mean_etal',
                              Date = date,
                              Time = time)
    user_records <- rbind(user_records, user_record)
    data.table::fwrite(user_records,file = './www/user records.txt')
    
    
    # 选择求和结果
    res <- mean_sd_se_num(user_data_mean_etal_by_group())
    df <- res[,-2]
    df <- df[,-2]
    df <- df[!duplicated(df),]
    
    # 保存分析结果
    write.csv(df, file = './results/res_tab.csv',row.names = FALSE)
    write.table(df, file = './results/res_tab.txt',row.names = FALSE)
    xlsx::write.xlsx(df, file = './results/res_tab.xlsx',row.names = FALSE)
    
    df <- df[,c('group','sum')]
    colnames(df) <- c('分组','和')
  }
  if (input$sum_or_not_mean_etal_by_group == 'TRUE') {
    return(df)
  }else{
    return(NULL)
  }
})

# 展示均值结果
output$taboutput_mean_mean_etal_by_group_view <- renderDataTable(mean_etal_temp1(),
                                                                 options = list(
                                                                   pageLength = 5
                                                                 ))
mean_etal_temp1 <- eventReactive(input$submit_mean_etal_by_group,{
  if (input$submit_mean_etal_by_group > 0) {
    
    res <- mean_sd_se_num(user_data_mean_etal_by_group())
    df <- res[,-2]
    df <- df[!duplicated(df),]
    df <- df[,c('group','mean')]
    colnames(df) <- c('分组','均值')
  }
  if (input$mean_or_not_mean_etal_by_group == 'TRUE') {
    return(df)
  }else{
    return(NULL)
  }
})

# 展示标准差结果
output$taboutput_sd_mean_etal_by_group_view <- renderDataTable(mean_etal_temp3(),
                                                               options = list(
                                                                 pageLength = 5
                                                               ))
mean_etal_temp3 <- eventReactive(input$submit_mean_etal_by_group,{
  if (input$submit_mean_etal_by_group > 0) {
    
    res <- mean_sd_se_num(user_data_mean_etal_by_group())
    df <- res[,-2]
    df <- df[!duplicated(df),]
    df <- df[,c('group','SD')]
    colnames(df) <- c('分组','标准差')
  }
  if (input$sd_or_not_mean_etal_by_group == 'TRUE') {
    return(df)
  }else{
    return(NULL)
  }
})

# 展示标准误结果
output$taboutput_se_mean_etal_by_group_view <- renderDataTable(mean_etal_temp4(),
                                                               options = list(
                                                                 pageLength = 5
                                                               ))
mean_etal_temp4 <- eventReactive(input$submit_mean_etal_by_group,{
  if (input$submit_mean_etal_by_group > 0) {
    
    res <- mean_sd_se_num(user_data_mean_etal_by_group())
    df <- res[,-2]
    df <- df[!duplicated(df),]
    df <- df[,c('group','SE')]
    colnames(df) <- c('分组','标准误')
  }
  if (input$se_or_not_mean_etal_by_group == 'TRUE') {
    return(df)
  }else{
    return(NULL)
  }
})

# 下载分析结果
# 运行正常20210111
output$taboutput_mean_etal_by_group_download <- downloadHandler(
  filename <- function(){
    paste(stringr::str_sub(input$data_input_mean_etal_by_group$name,
                           1,
                           (nchar(input$data_input_mean_etal_by_group$name) - 4)),
          '_计算结果',input$mean_etal_by_group_filetype,sep = ''
    )
  },
  content <- function(file){
    if (input$mean_etal_by_group_filetype == '.csv') {
      file.copy('./results/res_tab.csv',file)
    }else if (input$mean_etal_by_group_filetype == '.txt') {
      file.copy('./results/res_tab.txt',file)
    }else{
      file.copy('./results/res_tab.xlsx',file)
    }
  }
)

# 分组求和/平均值/标准差/标准误服务端设置结束---------------------------------
