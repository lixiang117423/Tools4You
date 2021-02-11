# 提交成功显示
observeEvent(input$submit_ttest, { 
  if (input$submit_ttest>0) {
    sendSweetAlert(
      session = session,
      title = "提交成功!",
      text = "数据上传成功，参数设置正确",
      type = "success")
  }
})
# 导入数据
# 运行正常20210111
user_data_ttest <- reactive({
  table_in_test <- read.csv(input$data_input_ttest$datapath,
                            header = T, 
                            stringsAsFactors = TRUE,
                            encoding = 'UTF-8')
  colnames(table_in_test)[1] <- 'group'
  table_in_test <- reshape2::melt(table_in_test, id.vars = 1) %>%
    dplyr::select(c(1,3)) %>% 
    na.omit()
  colnames(table_in_test) <- c('group','value')
  table_in_test <<- table_in_test
})

# 下载示例数据
# 运行正常20210111
output$download_demo_data_ttest <- downloadHandler(
  filename = 't检验示例数据.csv',
  content = function(file){
    file.copy('./demo_data/t检验示例数据.csv',file)
  }
)

# 对数据求均值等
# 运行正常20210111
mean_etal_ttest <- reactive({
  mean_ttest <- mean_sd_se_num(user_data_ttest())
  mean_ttest <<- mean_ttest
})

# 展示统计分析结果
output$taboutput_ttest_view <- renderDataTable(df_table_ttest(),
                                               options = list(
                                                 pageLength = 12
                                               ))

df_table_ttest <- eventReactive(input$submit_ttest,{
  if (input$submit_ttest > 0) {
    
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

    t <- t.test(value ~ group,
                data = user_data_ttest(),# 必须要加上()才行
                conf.level = as.numeric(input$select_confidence_interval_ttest),
                alternative = input$select_oneside_or_twoside_ttest,
                paired = ifelse(input$select_paired_or_not_ttest == 'FALSE',FALSE,TRUE),
                var.equal = ifelse(input$var_equal_ttest == 'FALSE',FALSE, TRUE)
    )
    
    df1 <- data.frame(pvalue = round(t$p.value,4),
                      group = unique(user_data_ttest()$group))
    df1$significance <- ifelse(df1$pvalue < 0.001,'***',
                               ifelse(df1$pvalue > 0.001 & df1$pvalue < 0.01,'**',
                                      ifelse(df1$pvalue > 0.05,'NS','*')))
    if (input$ttest_fig_sig == '如：versicolor') {
      df1$significance <- df1$significance
    }else if (input$ttest_fig_sig %in% df1$group) {
      df1[df1$group != input$ttest_fig_sig,]$significance = ''
    }
    
    df1 <- merge(mean_etal_ttest(), df1, by = 'group')
    df1 <- df1[,-2]
    df1 <- df1[!duplicated(df1),]
    
    # 保存分析结果
    write.csv(df1, file = './results/rest_tab.csv',row.names = FALSE)
    write.table(df1, file = './results/rest_tab.txt',row.names = FALSE)
    xlsx::write.xlsx(df1, file = './results/rest_tab.xlsx',row.names = FALSE)
    # 保存分析结果结束
  }
  df1 # 返回要展示的结果
})

# 进行绘图
output$plot_ttest <- renderPlot(plot_res_ttest(),
                                height = 300,
                                width = 600)
plot_res_ttest <- eventReactive(input$submit_ttest,{
  if (input$submit_ttest > 0) {
    p_ttest <- ggplot(df_table_ttest(), 
                      aes(group, mean,fill = group)) +
      geom_bar(stat = 'identity',width = 0.4)
    
    if (input$ttest_err_bar == 'SD') {
      p_ttest <- p_ttest + 
        geom_errorbar(aes(group, 
                          ymin = mean - SD, 
                          ymax = mean + SD),
                      width = 0.1) +
        geom_text(aes(group,(mean + SD)*1.1,
                      label = significance)) +
        geom_hline(yintercept = (max(df_table_ttest()$mean) + max(df_table_ttest()$SD))*1.3, 
                   color = 'white') +
        labs(x = ifelse(input$ttest_fig_x_axis == '','group',input$ttest_fig_x_axis),
             y = ifelse(input$ttest_fig_y_axis == '','group',input$ttest_fig_y_axis),
             title = input$ttest_fig_title) +
        scale_y_continuous(expand = c(0,0)) +
        scale_fill_aaas() +
        theme_prism(base_size = 14) +
        theme(legend.position = 'none',
              panel.background = element_blank(),
              panel.grid = element_blank(),
              axis.text = element_text(color = 'black',size = 10, 
                                       family = 'Arial', face = 'plain'),
              axis.title.x = element_text(color = 'black',size = 10,
                                          family = 'Arial', face = 'plain'),
              axis.ticks = element_line(color = 'black'))
    }else{
      p_ttest <- p_ttest + 
        geom_errorbar(aes(group, 
                          ymin = mean - SE, 
                          ymax = mean + SE),
                      width = 0.1) +
        geom_text(aes(group,(mean + SE)*1.1,
                      label = significance)) +
        geom_hline(yintercept = (max(df_table_ttest()$mean) + max(df_table_ttest()$SE))*1.3, 
                   color = 'white') +
        labs(x = ifelse(input$ttest_fig_x_axis == '','group',input$ttest_fig_x_axis),
             y = ifelse(input$ttest_fig_y_axis == '','group',input$ttest_fig_y_axis),
             title = input$ttest_fig_title) +
        scale_y_continuous(expand = c(0,0)) +
        scale_fill_aaas() +
        theme_prism(base_size = 14) +
        theme(legend.position = 'none',
              panel.background = element_blank(),
              panel.grid = element_blank(),
              axis.text = element_text(color = 'black',size = 10, 
                                       family = 'Arial', face = 'plain'),
              axis.title.x = element_text(color = 'black',size = 10,
                                          family = 'Arial', face = 'plain'),
              axis.ticks = element_line(color = 'black'))
    }
    

    # 保存图片
    filename <- ifelse(input$ttest_fig_res_filetype == '.pdf','res_fig.pdf',
                       ifelse(input$ttest_fig_res_filetype == '.png','res_fig.png',
                              ifelse(input$ttest_fig_res_filetype == '.jpg','res_fig.jpg',
                                     ifelse(input$ttest_fig_res_filetype == '.tiff','res_fig.tiff','res_fig.eps'))))
    
    if (input$ttest_fig_res_filetype == '.pdf') {
      ggsave(p_ttest, 
             filename = paste('./results/', filename, sep = ''),
             width = input$ttest_fig_wdith,
             height = input$ttest_fig_height,
             device = cairo_pdf)
    }else{
      ggsave(p_ttest, 
             filename = paste('./results/', filename, sep = ''),
             width = input$ttest_fig_wdith,
             height = input$ttest_fig_height)
    }
  }
  p_ttest # 返回图
})

# 下载分析结果
# 运行正常20210111
output$taboutput_ttest_download <- downloadHandler(
  filename <- function(){
    paste(stringr::str_sub(input$data_input_ttest$name,
                           1,
                           (nchar(input$data_input_ttest$name) - 4)),
          '_统计分析结果',input$ttest_stat_res_filetype,sep = ''
    )
  },
  content <- function(file){
    if (input$ttest_stat_res_filetype == '.csv') {
      file.copy('./results/rest_tab.csv',file)
    }else if (input$ttest_stat_res_filetype == '.txt') {
      file.copy('./results/rest_tab.txt',file)
    }else{
      #return(NULL)
      file.copy('./results/rest_tab.xlsx',file)
    }
  }
)

# 下载图片
output$download_figure__ttest <- downloadHandler(
  filename <- function(){
    paste(stringr::str_sub(input$data_input_ttest$name,
                           1,
                           (nchar(input$data_input_ttest$name) - 4)),
          '_绘图结果',input$ttest_fig_res_filetype,sep = ''
    )
  },
  content <- function(file){
    if (input$ttest_fig_res_filetype == '.pdf') {
      file.copy('./results/res_fig.pdf',file)
    }else if (input$ttest_fig_res_filetype == '.png') {
      file.copy('./results/res_fig.png',file)
    }else if (input$ttest_fig_res_filetype == '.jpg') {
      file.copy('./results/res_fig.jpg',file)
    }else if (input$ttest_fig_res_filetype == '.tiff') {
      file.copy('./results/res_fig.tiff',file)
    }else{
      file.copy('./results/res_fig.eps',file)
    }
  }
)