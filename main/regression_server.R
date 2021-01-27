# 提交成功显示
observeEvent(input$submit_regression, { 
  if (input$submit_regression>0) {
    sendSweetAlert(
      session = session,
      title = "提交成功!",
      text = "数据上传成功，参数设置正确",
      type = "success")
  }
})
# 导入数据
# 运行正常20210111
user_data_regression <- reactive({
  table_in_test <-data.table::fread(input$data_input_regression$datapath,
                                    encoding = 'UTF-8')
  colnames(table_in_test) <- c('x','y','group')
  table_in_test <<- table_in_test
})

# 下载示例数据
# 运行正常20210111
output$download_demo_data_regression <- downloadHandler(
  filename = '回归分析示例数.csv',
  content = function(file){
    file.copy('./demo_data/回归分析示例数.csv',file)
  }
)



# 展示统计分析结果
output$taboutput_regression_view <- renderDataTable(df_table_regression(),
                                               options = list(
                                                 pageLength = 4
                                               ))

df_table_regression <- eventReactive(input$submit_regression,{
  if (input$submit_regression > 0) {
    
    user_records <- data.table::fread('./www/user records.txt', encoding = 'UTF-8')
    temp <- as.character(Sys.time())
    temp <- stringr::str_split(temp, ' ')
    date <- temp[[1]][1]
    time <- temp[[1]][2]
    user_record <- data.frame(Cat = 'retgression',
                              Date = date,
                              Time = time)
    user_records <- rbind(user_records, user_record)
    data.table::fwrite(user_records,file = './www/user records.txt')
    
    df <- user_data_regression()
    
    regre_res <- NULL
    
    for (i in 1:length(unique(df$group))) {
      
      df_sub <- df[df$group == unique(df$group)[i],]
      colnames(df_sub) <- c('x','y','group')
      fit <- lm(y ~ x, data = df_sub)
      fit <- summary(fit)
      fit <- fit[["coefficients"]]
      fit <- as.data.frame(fit)
      fit$group = unique(df$group)[i]
      fit$variable <- c('截距','斜率')
      fit = fit[,c(5,6,1:4)]
      rownames(fit) = NULL
      regre_res <- rbind(regre_res, fit)
    }
    
    colnames(regre_res)[6] <- 'Pvalue'
    # 保存分析结果
    write.csv(regre_res, file = './results/rest_tab.csv',row.names = FALSE)
    write.table(regre_res, file = './results/rest_tab.txt',row.names = FALSE)
    xlsx::write.xlsx(regre_res, file = './results/rest_tab.xlsx',row.names = FALSE)
    # 保存分析结果结束
  }
  regre_res # 返回要展示的结果
})


# 进行绘图
output$plot_regression <- renderPlot(plot_res_regression(),
                                height = 300,
                                width = 600)
plot_res_regression <- eventReactive(input$submit_regression,{
  if (input$submit_regression > 0) {
    
    
    df <- user_data_regression()
    colnames(df) <- c('x','y','group')
    
    p_regression <- ggplot(df,aes(x, y, 
                             group = group, 
                             color = group,
                             fill = group)) +
      geom_point(aes(shape = group),
                 size = input$point_size_regression,
                 alpha = input$point_transpar_regression) +
      geom_smooth(formula = y ~ x,
                  method = 'lm',
                  se = TRUE,
                  colour = 'black',
                  span = 0.8) +
      stat_poly_eq(aes(label = paste(..eq.label.., 
                                     #..adj.rr.label.., 
                                     ..p.value.label..,
                                     sep = '~~~~')),
                       formula = y ~ x, parse = TRUE) +
      
      scale_fill_aaas() +
      scale_color_aaas() +
      theme_classic() +
      labs(x = input$regression_fig_x_axis,
           y = input$regression_fig_y_axis,
           title = input$regression_fig_title) +
      theme(legend.position = ifelse(input$regression_legend == 'FALSE','none','right'),
            panel.background = element_blank(),
            panel.grid = element_blank(),
            axis.text = element_text(color = 'black',size = 10, 
                                     family = 'Arial', face = 'plain'),
            axis.title.x = element_text(color = 'black',size = 10,
                                        family = 'Arial', face = 'plain'),
            axis.ticks = element_line(color = 'black'))
    
    
    
    
    if (input$point_shape_regression == '') {
      p_regression <- p_regression
    }else{
      p_regression <- p_regression + 
        scale_shape_manual(values = input$point_shape_regression)
    }
    
    if (input$point_color_regression == '') {
      p_regression <- p_regression
    }else{
      p_regression <- p_regression + 
        scale_color_manual(values = input$point_color_regression)
    }
    
    
    # 保存图片
    filename <- ifelse(input$regression_fig_res_filetype == '.pdf','res_fig.pdf',
                       ifelse(input$regression_fig_res_filetype == '.png','res_fig.png',
                              ifelse(input$regression_fig_res_filetype == '.jpg','res_fig.jpg',
                                     ifelse(input$regression_fig_res_filetype == '.tiff','res_fig.tiff','res_fig.eps'))))
    
    if (input$regression_fig_res_filetype == '.pdf') {
      ggsave(p_regression, 
             filename = paste('./results/', filename, sep = ''),
             width = input$regression_fig_wdith,
             height = input$regression_fig_height,
             device = cairo_pdf)
    }else{
      ggsave(p_regression, 
             filename = paste('./results/', filename, sep = ''),
             width = input$regression_fig_wdith,
             height = input$regression_fig_height)
    }
  }
  p_regression # 返回图
})

# 下载分析结果
# 运行正常20210111
output$taboutput_regression_download <- downloadHandler(
  filename <- function(){
    paste(stringr::str_sub(input$data_input_regression$name,
                           1,
                           (nchar(input$data_input_regression$name) - 4)),
          '_分析结果',input$regression_stat_res_filetype,sep = ''
    )
  },
  content <- function(file){
    if (input$regression_stat_res_filetype == '.csv') {
      file.copy('./results/rest_tab.csv',file)
    }else if (input$regression_stat_res_filetype == '.txt') {
      file.copy('./results/rest_tab.txt',file)
    }else{
      #return(NULL)
      file.copy('./results/rest_tab.xlsx',file)
    }
  }
)

# 下载图片
output$download_figure__regression <- downloadHandler(
  filename <- function(){
    paste(stringr::str_sub(input$data_input_regression$name,
                           1,
                           (nchar(input$data_input_regression$name) - 4)),
          '_图片',input$regression_fig_res_filetype,sep = ''
    )
  },
  content <- function(file){
    if (input$regression_fig_res_filetype == '.pdf') {
      file.copy('./results/res_fig.pdf',file)
    }else if (input$regression_fig_res_filetype == '.png') {
      file.copy('./results/res_fig.png',file)
    }else if (input$regression_fig_res_filetype == '.jpg') {
      file.copy('./results/res_fig.jpg',file)
    }else if (input$regression_fig_res_filetype == '.tiff') {
      file.copy('./results/res_fig.tiff',file)
    }else{
      file.copy('./results/res_fig.eps',file)
    }
  }
)