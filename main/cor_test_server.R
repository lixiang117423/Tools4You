# 提交成功显示
observeEvent(input$submit_cor, { 
  if (input$submit_cor>0) {
    sendSweetAlert(
      session = session,
      title = "提交成功!",
      text = "数据上传成功，参数设置正确",
      type = "success")
  }
})
# 导入数据
user_data_cor <- reactive({
  data.table::fread(input$data_input_cor$datapath, encoding = 'UTF-8') %>% as.data.frame()
})

# 下载示例数据
output$download_demo_data_cor <- downloadHandler(
  filename = '相关性分析示例数据.csv',
  content = function(file){
    file.copy('./demo_data/相关性分析示例数据.csv',file)
  }
)


# 相关性图
output$cor_res_plot <- renderPlot(cor_point_plot(),height = 380,width = 700)

cor_point_plot <- eventReactive(input$submit_cor,{
  if (input$submit_cor > 0) {
    
    user_records <- data.table::fread('./www/user records.txt', encoding = 'UTF-8')
    temp <- as.character(Sys.time())
    temp <- stringr::str_split(temp, ' ')
    date <- temp[[1]][1]
    time <- temp[[1]][2]
    user_record <- data.frame(Cat = 'cor-test',
                              Date = date,
                              Time = time)
    user_records <- rbind(user_records, user_record)
    data.table::fwrite(user_records,file = './www/user records.txt')
    
    
    df <- user_data_cor()
    
    cor_res <- WGCNA::cor(df, method = input$method_cor)
    
    # 绘图
    p <- quickcor(cor_res,
             cor.test = TRUE,
             type = ifelse(input$select_plot_type_cor == 1,'full',
                           ifelse(input$select_plot_type_cor == 2, 'upper','lower')))
    # 图形展示
    if (input$select_cir_type_cor == 1) {
      p <- p + geom_circle2()
    }else if (input$select_cir_type_cor == 2) {
      p <- p + geom_square()
    }else if (input$select_cir_type_cor == 3) {
      p <- p + geom_ellipse2()
    }else{
      p <- p + geom_pie2()
    }
    
    # 展示数值
    if (input$num_cort == 'TRUE') {
      p <- p + geom_number(aes(num = r), 
                           colour = ifelse(input$cor_text_color == '',
                                           "grey90",
                                           input$cor_text_color), 
                           size = ifelse(input$cor_taxt_size == '',
                                         3.5,
                                         as.numeric(input$cor_taxt_size)))
    }else{
      p <- p
    }
    
    # 展示显著性
    if (input$signif_cort == 'TRUE') {
      p <- p + geom_mark(data = get_data(show.diag = FALSE),
                         size = 5,r = NA, 
                         color = ifelse(input$cor_text_color == '',
                                        "grey90",
                                        input$cor_text_color), 
                         nudge_y = 0.15)
    }else{
      p <- p
    }
    
    # 颜色设置
    if (input$cor_color_scale_top !=  ''& input$cor_color_scale_but != '') {
      p <- p + scale_fill_gradient(high = input$cor_color_scale_top,
                                   low = input$cor_color_scale_but)
    }else{
      p <- p
    }

    p_cor_2 <- p + labs(x = '') +
      theme(panel.background = element_blank(),
            panel.grid = element_blank(),
            axis.text = element_text(color = 'black',size = 10, 
                                     family = 'Arial', face = 'plain'),
            axis.title.x = element_text(color = 'black',size = 10,
                                        family = 'Arial', face = 'plain'),
            axis.ticks = element_line(color = 'black'))

    
    # 保存图片
    filename <- ifelse(input$cor_fig_res_filetype == '.pdf','res_fig.pdf',
                       ifelse(input$cor_fig_res_filetype == '.png','res_fig.png',
                              ifelse(input$cor_fig_res_filetype == '.jpg','res_fig.jpg',
                                     ifelse(input$cor_fig_res_filetype == '.tiff','res_fig.tiff','res_fig.eps'))))
    
    if (input$cor_fig_res_filetype == '.pdf') {
      ggsave(p_cor_2, 
             filename = paste('./results/', filename, sep = ''),
             width = input$cor_fig_wdith,
             height = input$cor_fig_height,
             device = cairo_pdf)
    }else{
      ggsave(p_cor_2, 
             filename = paste('./results/', filename, sep = ''),
             width = input$cor_fig_wdith,
             height = input$cor_fig_height)
    }
  }
  p_cor_2
})


# 相关性表格
output$cor_res_table <- renderDataTable(cor_table(),
                                        options = list(pageLength = 4)
                                        )

cor_table <- eventReactive(input$submit_cor,{
  if (input$submit_cor > 0) {
    
    df <- user_data_cor()
    
    cor_res <- WGCNA::cor(df, method = input$method_cor) %>% as.data.frame()
    
    # 保存分析结果
    write.csv(cor_res, file = './results/rest_tab.csv',row.names = FALSE)
    write.table(cor_res, file = './results/rest_tab.txt',row.names = FALSE)
    xlsx::write.xlsx(cor_res, file = './results/rest_tab.xlsx',row.names = FALSE)
  }
  cor_res
  })


# 下载图
output$download_figure__cor <- downloadHandler(
  filename <- function(){
    paste(stringr::str_sub(input$data_input_cor$name,
                           1,
                           (nchar(input$data_input_cor$name) - 4)),
          '_相关性图',input$cor_fig_res_filetype,sep = ''
    )
  },
  content <- function(file){
    if (input$cor_fig_res_filetype == '.pdf') {
      file.copy('./results/res_fig.pdf',file)
    }else if (input$cor_fig_res_filetype == '.png') {
      file.copy('./results/res_fig.png',file)
    }else if (input$cor_fig_res_filetype == '.jpg') {
      file.copy('./results/res_fig.jpg',file)
    }else if (input$cor_fig_res_filetype == '.tiff') {
      file.copy('./results/res_fig.tiff',file)
    }else{
      file.copy('./results/res_fig.eps',file)
    }
  }
)


# 下载表格
output$taboutput_cor_download <- downloadHandler(
  filename <- function(){
    paste(stringr::str_sub(input$data_input_cor$name,
                           1,
                           (nchar(input$data_input_cor$name) - 4)),
          '_相关性表',input$cor_stat_res_filetype,sep = ''
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



