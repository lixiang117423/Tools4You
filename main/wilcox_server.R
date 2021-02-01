# 提交成功显示
observeEvent(input$submit_wilcox, { 
  if (input$submit_wilcox>0) {
    sendSweetAlert(
      session = session,
      title = "提交成功!",
      text = "数据上传成功，参数设置正确",
      type = "success")
  }
})
# 导入数据
# 运行正常20210111
user_data_wilcox <- reactive({
  table_in_test <- read.csv(input$data_input_wilcox$datapath,
                            header = T, 
                            stringsAsFactors = TRUE,
                            encoding = 'UTF-8')
  colnames(table_in_test)[1] <- 'group'
  table_in_test <- reshape2::melt(table_in_test, id.vars = 1) %>% na.omit()
  table_in_test <- table_in_test[,c(1,3)]
  colnames(table_in_test) <- c('group','value')
  table_in_test <<- table_in_test
})

# 下载示例数据
# 运行正常20210111
output$download_demo_data_wilcox <- downloadHandler(
  filename = 'Wilcox示例数据.csv',
  content = function(file){
    file.copy('./demo_data/Wilcox示例数据.csv',file)
  }
)

# 对数据求均值等
# 运行正常20210111
mean_etal_wilcox <- reactive({
  mean_wilcox <- mean_sd_se_num(user_data_wilcox())
  mean_wilcox <<- mean_wilcox
})

# 展示统计分析结果
output$taboutput_wilcox_view <- renderDataTable(df_table_wilcox(),
                                               options = list(
                                                 pageLength = 4
                                               ))

df_table_wilcox <- eventReactive(input$submit_wilcox,{
  if (input$submit_wilcox > 0) {
    
    # 记录用户使用记录
    user_records <- data.table::fread('./www/user records.txt', encoding = 'UTF-8')
    temp <- as.character(Sys.time())
    temp <- stringr::str_split(temp, ' ')
    date <- temp[[1]][1]
    time <- temp[[1]][2]
    user_record <- data.frame(Cat = 'wilcox',
                              Date = date,
                              Time = time)
    user_records <- rbind(user_records, user_record)
    data.table::fwrite(user_records,file = './www/user records.txt')
    
    # Wilcox test
    t <- wilcox.test(value ~ group, 
                     data = user_data_wilcox(),
                     conf.level = as.numeric(input$select_confidence_interval_wilcox),
                     alternative = input$select_oneside_or_twoside_wilcox,
                     paired = ifelse(input$select_paired_or_not_wilcox == 'FALSE',FALSE,TRUE),
                     var.equal = ifelse(input$var_equal_wilcox == 'FALSE',FALSE, TRUE)
    )
    
    df1 <- data.frame(pvalue = round(t$p.value,4),
                      group = unique(user_data_wilcox()$group))
    df1$significance <- ifelse(df1$pvalue < 0.001,'***',
                               ifelse(df1$pvalue > 0.001 & df1$pvalue < 0.01,'**',
                                      ifelse(df1$pvalue > 0.05,'NS','*')))
    if (input$wilcox_fig_sig == '如：y') {
      df1$significance <- df1$significance
    }else if (input$wilcox_fig_sig %in% df1$group) {
      df1[df1$group != input$wilcox_fig_sig,]$significance = ''
    }
    
    df1 <- merge(mean_etal_wilcox(), df1, by = 'group')
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
output$plot_wilcox <- renderPlot(plot_res_wilcox(),
                                height = 300,
                                width = 600)
plot_res_wilcox <- eventReactive(input$submit_wilcox,{
  if (input$submit_wilcox > 0) {
    p_wilcox <- ggplot(df_table_wilcox(), 
                      aes(group, mean,fill = group)) +
      geom_bar(stat = 'identity',width = 0.4) +
      geom_errorbar(aes(group, 
                        ymin = mean - ifelse(input$wilcox_err_bar == 'SD',SD,SE), 
                        ymax = mean + ifelse(input$wilcox_err_bar == 'SD',SD,SE)),
                    width = 0.1) +
      geom_text(aes(group,(mean + ifelse(input$wilcox_err_bar == 'SD',SD,SE))*1.1,
                    label = significance)) +
      geom_hline(yintercept = (max(df_table_wilcox()$mean) + 
                                 ifelse(input$wilcox_err_bar == 'SD',
                                        max(df_table_wilcox()$SD),max(df_table_wilcox()$SE)))*1.3, 
                 color = 'white') +
      labs(x = ifelse(input$wilcox_fig_x_axis == '','group',input$wilcox_fig_x_axis),
           y = ifelse(input$wilcox_fig_y_axis == '','group',input$wilcox_fig_y_axis),
           title = input$wilcox_fig_title) +
      scale_y_continuous(expand = c(0,0)) +
      scale_fill_aaas() +
      theme_classic() +
      theme(legend.position = 'none',
            panel.background = element_blank(),
            panel.grid = element_blank(),
            axis.text = element_text(color = 'black',size = 10, 
                                     family = 'Arial', face = 'plain'),
            axis.title.x = element_text(color = 'black',size = 10,
                                        family = 'Arial', face = 'plain'),
            axis.ticks = element_line(color = 'black'))
    
    
    # 保存图片
    filename <- ifelse(input$wilcox_fig_res_filetype == '.pdf','res_fig.pdf',
                       ifelse(input$wilcox_fig_res_filetype == '.png','res_fig.png',
                              ifelse(input$wilcox_fig_res_filetype == '.jpg','res_fig.jpg',
                                     ifelse(input$wilcox_fig_res_filetype == '.tiff','res_fig.tiff','res_fig.eps'))))
    
    if (input$wilcox_fig_res_filetype == '.pdf') {
      ggsave(p_wilcox, 
             filename = paste('./results/', filename, sep = ''),
             width = input$wilcox_fig_wdith,
             height = input$wilcox_fig_height,
             device = cairo_pdf)
    }else{
      ggsave(p_wilcox, 
             filename = paste('./results/', filename, sep = ''),
             width = input$wilcox_fig_wdith,
             height = input$wilcox_fig_height)
    }
  }
  p_wilcox # 返回图
})

# 下载分析结果
# 运行正常20210111
output$taboutput_wilcox_download <- downloadHandler(
  filename <- function(){
    paste(stringr::str_sub(input$data_input_wilcox$name,
                           1,
                           (nchar(input$data_input_wilcox$name) - 4)),
          '_分析结果',input$wilcox_stat_res_filetype,sep = ''
    )
  },
  content <- function(file){
    if (input$wilcox_stat_res_filetype == '.csv') {
      file.copy('./results/rest_tab.csv',file)
    }else if (input$wilcox_stat_res_filetype == '.txt') {
      file.copy('./results/rest_tab.txt',file)
    }else{
      #return(NULL)
      file.copy('./results/rest_tab.xlsx',file)
    }
  }
)

# 下载图片
output$download_figure__wilcox <- downloadHandler(
  filename <- function(){
    paste(stringr::str_sub(input$data_input_wilcox$name,
                           1,
                           (nchar(input$data_input_wilcox$name) - 4)),
          '_绘图结果',input$wilcox_fig_res_filetype,sep = ''
    )
  },
  content <- function(file){
    if (input$wilcox_fig_res_filetype == '.pdf') {
      file.copy('./results/res_fig.pdf',file)
    }else if (input$wilcox_fig_res_filetype == '.png') {
      file.copy('./results/res_fig.png',file)
    }else if (input$wilcox_fig_res_filetype == '.jpg') {
      file.copy('./results/res_fig.jpg',file)
    }else if (input$wilcox_fig_res_filetype == '.tiff') {
      file.copy('./results/res_fig.tiff',file)
    }else{
      file.copy('./results/res_fig.eps',file)
    }
  }
)