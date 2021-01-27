# 提交成功显示
observeEvent(input$submit_perm, { 
  if (input$submit_perm>0) {
    sendSweetAlert(
      session = session,
      title = "提交成功!",
      text = "数据上传成功，参数设置正确",
      type = "success")
  }
})
# 导入数据
# 运行正常20210111
user_data_perm <- reactive({
  table_in_test <- data.table::fread(input$data_input_perm$datapath)
  table_in_test <<- table_in_test
})

# 下载示例数据
# 运行正常20210111
output$download_demo_data_perm <- downloadHandler(
  filename = 'permanova示例数据.csv',
  content = function(file){
    file.copy('./demo_data/permanova示例数据.csv',file)
  }
)


# 展示统计分析结果
output$taboutput1_perm_view <- renderDataTable(df_table_perm(),
                                               options = list(
                                                 pageLength = 10
                                               ))

df_table_perm <- eventReactive(input$submit_perm,{
  if (input$submit_perm > 0) {
    
    user_records <- data.table::fread('./www/user records.txt', encoding = 'UTF-8')
    temp <- as.character(Sys.time())
    temp <- stringr::str_split(temp, ' ')
    date <- temp[[1]][1]
    time <- temp[[1]][2]
    user_record <- data.frame(Cat = 'permanova',
                              Date = date,
                              Time = time)
    user_records <- rbind(user_records, user_record)
    data.table::fwrite(user_records,file = './www/user records.txt')
    
    # 将数据划分成丰度表和环境文件
    df <- user_data_perm()
    
    # 丰度
    df_1 <- df[,(as.numeric(input$num_factor_perm) + 1):ncol(df)]
    df_2 <- df[,1:as.numeric(input$num_factor_perm)]
    
    group <- data.frame(origin = colnames(df_2),
                        replace = paste('group',1:ncol(df_2), sep = ''))
    
    colnames(df_2) <- paste('group',1:ncol(df_2), sep = '')
    
    # 进行permanova计算
    if (input$num_factor_perm == '1') {
      fit <- adonis(df_1 ~ ., 
                    data = df_2,
                    method = input$select_dist_method_perm,
                    permutations = as.numeric(input$select_number_perm))
    }else if (input$num_factor_perm == '2' & input$factor_inter_perm == 'TRUE') {
      fit <- adonis(df_1 ~ group1*group2, 
                    data = df_2,
                    method = input$select_dist_method_perm,
                    permutations = as.numeric(input$select_number_perm))
    }else if (input$num_factor_perm == '2' & input$factor_inter_perm == 'FALSE') {
      fit <- adonis(df_1 ~ ., 
                    data = df_2,
                    method = input$select_dist_method_perm,
                    permutations = as.numeric(input$select_number_perm))
    }else if (input$num_factor_perm == '3' & input$factor_inter_perm == 'TRUE') {
      fit <- adonis(df_1 ~ group1*group2*group3, 
                    data = df_2,
                    method = input$select_dist_method_perm,
                    permutations = as.numeric(input$select_number_perm))
    }else if (input$num_factor_perm == '3' & input$factor_inter_perm == 'FALSE') {
      fit <- adonis(df_1 ~ ., 
                    data = df_2,
                    method = input$select_dist_method_perm,
                    permutations = as.numeric(input$select_number_perm))
    }else if (input$num_factor_perm == '4' & input$factor_inter_perm == 'TRUE') {
      fit <- adonis(df_1 ~ group1*group2*group3*group4, 
                    data = df_2,
                    method = input$select_dist_method_perm,
                    permutations = as.numeric(input$select_number_perm))
    }else if (input$num_factor_perm == '4' & input$factor_inter_perm == 'FALSE') {
      fit <- adonis(df_1 ~ ., 
                    data = df_2,
                    method = input$select_dist_method_perm,
                    permutations = as.numeric(input$select_number_perm))
    }else if (input$num_factor_perm == '5' & input$factor_inter_perm == 'TRUE') {
      fit <- adonis(df_1 ~ group1*group2*group3*group4*group5, 
                    data = df_2,
                    method = input$select_dist_method_perm,
                    permutations = as.numeric(input$select_number_perm))
    }else {
      fit <- adonis(df_1 ~ ., 
                    data = df_2,
                    method = input$select_dist_method_perm,
                    permutations = as.numeric(input$select_number_perm))
    }
    
    res_fit <- fit[["aov.tab"]] %>% as.data.frame()
    
    res_fit_end  <- res_fit[(nrow(res_fit)-1):nrow(res_fit),]
    coln <- colnames(res_fit_end)
    res_fit_end$variable <- rownames(res_fit_end) 
    coln <- c('variable',coln)
    res_fit_end <- res_fit_end[,coln]
    
    res_fit$temp = rownames(res_fit)
    
    res_fit =res_fit[1:(nrow(res_fit)-2),]
    
    stringr::str_split(res_fit$temp, ':')
    res_fit$cat1 = 1
    res_fit$cat2 = 2
    for (i in 1:nrow(res_fit)) {
      res_fit$cat1[i] = stringr::str_split(res_fit$temp[i], ':')[[1]][1]
    }
    
    for (i in 1:nrow(res_fit)) {
      if (length(stringr::str_split(res_fit$temp[i], ':')[[1]]) == 1) {
        res_fit$cat2[i] = ''
      }else{
        res_fit$cat2[i] = stringr::str_split(res_fit$temp[i], ':')[[1]][2]
      }
    }
    
    
    
    res_fit = merge(res_fit, group, by.x = 'cat1', by.y = 'replace', all.x = TRUE)
    
    res_fit = merge(res_fit, group, by.x = 'cat2', by.y = 'replace', all.x = TRUE, na.)
    res_fit$origin.y = as.character(res_fit$origin.y)
    
    res_fit[is.na(res_fit)] = ''
    
    res_fit$variable = ifelse(res_fit$origin.y == '',
                              as.character(res_fit$origin.x),
                              paste(res_fit$origin.x,':',res_fit$origin.y, sep = ''))
    
    res_fit <- res_fit[,coln]
    res_fit <- rbind(res_fit, res_fit_end)
    
    # 保存分析结果
    write.csv(res_fit, file = './results/rest_tab.csv',row.names = FALSE)
    write.table(res_fit, file = './results/rest_tab.txt',row.names = FALSE)
    xlsx::write.xlsx(res_fit, file = './results/rest_tab.xlsx',row.names = FALSE)
    # 保存分析结果结束
  }
  res_fit # 返回要展示的结果
})



# 下载分析结果
# 运行正常20210111
output$taboutput_perm_download <- downloadHandler(
  filename <- function(){
    paste(stringr::str_sub(input$data_input_perm$name,
                           1,
                           (nchar(input$data_input_perm$name) - 4)),
          '_统计分析结果',input$perm_stat_res_filetype,sep = ''
    )
  },
  content <- function(file){
    if (input$perm_stat_res_filetype == '.csv') {
      file.copy('./results/rest_tab.csv',file)
    }else if (input$perm_stat_res_filetype == '.txt') {
      file.copy('./results/rest_tab.txt',file)
    }else{
      #return(NULL)
      file.copy('./results/rest_tab.xlsx',file)
    }
  }
)