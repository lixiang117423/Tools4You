# 提交成功显示
observeEvent(input$submit_oplsda, { 
  if (input$submit_oplsda>0) {
    sendSweetAlert(
      session = session,
      title = "提交成功!",
      text = "数据上传成功，参数设置正确",
      type = "success")
  }
})
# 导入数据
user_data_oplsda <- reactive({
  table_in_test <- read.csv(input$data_input_oplsda$datapath,
                            header = T, 
                            stringsAsFactors = TRUE,
                            encoding = 'UTF-8')
  colnames(table_in_test)[1:2] <- c('sample','group')
  table_in_test <<- table_in_test
})

# 下载示例数据
output$download_demo_data_oplsda <- downloadHandler(
  filename = 'OPLS-DA示例数据.csv',
  content = function(file){
    file.copy('./demo_data/OPLS-DA示例数据.csv',file)
  }
)


# oplsda
oplsda_res <- reactive({
  df_oplsda <- user_data_oplsda()
  
  oplsda <- ropls::opls(df_oplsda[,3:ncol(df_oplsda)],
                        df_oplsda$group,
                        predI = 1, 
                        orthoI = NA)
  oplsda <<- oplsda
})



# 展示统计分析结果
output$taboutput_oplsda_view <- renderDataTable(df_table_oplsda(),
                                               options = list(
                                                 pageLength = 4
                                               ))

df_table_oplsda <- eventReactive(input$submit_oplsda,{
  if (input$submit_oplsda > 0) {
    
    user_records <- data.table::fread('./www/user records.txt', encoding = 'UTF-8')
    temp <- as.character(Sys.time())
    temp <- stringr::str_split(temp, ' ')
    date <- temp[[1]][1]
    time <- temp[[1]][2]
    user_record <- data.frame(Cat = 'oplsda',
                              Date = date,
                              Time = time)
    user_records <- rbind(user_records, user_record)
    data.table::fwrite(user_records,file = './www/user records.txt')
    
    
    vip_score <- as.data.frame(oplsda_res()@vipVn)
    colnames(vip_score) <- 'VIP值'
    vip_score$metabolites <- rownames(vip_score)
    vip_score <- vip_score[,c('metabolites','VIP值')]
    vip_score <- vip_score[order(-vip_score$VIP值),]
    
    # 保存分析结果
    write.csv(vip_score, file = './results/oplsda/rest_tab.csv',row.names = FALSE)
    write.table(vip_score, file = './results/oplsda/rest_tab.txt',row.names = FALSE)
    xlsx::write.xlsx(vip_score, file = './results/oplsda/rest_tab.xlsx',row.names = FALSE)
  }
  vip_score
})

# 棒棒糖图
output$lollipop_plot_oplsda <- renderPlot(plot_lollipop_res_oplsda(),
                                height = 350,
                                width = 600)
plot_lollipop_res_oplsda <- eventReactive(input$submit_oplsda,{
  if (input$submit_oplsda > 0) {
    
    vip_score <- as.data.frame(oplsda_res()@vipVn)
    colnames(vip_score) <- 'VIP值'
    vip_score$metabolites <- rownames(vip_score)
    vip_score <- dplyr::select(vip_score,c('metabolites','VIP值'))
    vip_score = vip_score[vip_score$VIP值 >= 1,]
    vip_score <- vip_score[order(-vip_score$VIP值),][1:as.numeric(input$num_oplsda_show),]
    vip_2 = vip_score[order(vip_score$VIP值),]
    vip_2$metabolites = factor(vip_2$metabolites,levels = unique(vip_2$metabolites))
    
    p_oplsda_lollipop = ggplot(vip_2, aes(metabolites, VIP值)) +
      geom_segment(aes(x = metabolites,xend = metabolites,
                       y = 1, yend = VIP值)) +
      geom_point(aes(size = VIP值), color = '#008000') +
      geom_hline(yintercept = max(vip_2$VIP值*1.02), color = 'white') +
      coord_flip() +
      scale_y_continuous(expand = c(0,0)) +
      labs(x = '',y = 'VIP value') +
      theme_bw() +
      theme(legend.title = element_blank(),
            legend.text = element_text(color = 'black',size = 10, family = 'Arial', face = 'plain'),
            panel.background = element_blank(),
            panel.grid = element_blank(),
            axis.text = element_text(color = 'black',size = 10, family = 'Arial', face = 'plain'),
            axis.title = element_text(color = 'black',size = 10, family = 'Arial', face = 'plain'),
            axis.ticks = element_line(color = 'black'),
            axis.ticks.y = element_blank())

    
    # 保存图片
    filename <- ifelse(input$oplsda_fig_res_filetype == '.pdf','res_fig.pdf',
                       ifelse(input$oplsda_fig_res_filetype == '.png','res_fig.png',
                              ifelse(input$oplsda_fig_res_filetype == '.jpg','res_fig.jpg',
                                     ifelse(input$oplsda_fig_res_filetype == '.tiff','res_fig.tiff','res_fig.eps'))))
    
    if (input$oplsda_fig_res_filetype == '.pdf') {
      ggsave(p_oplsda_lollipop, 
             filename = paste('./results/oplsda/figures/', filename, sep = ''),
             width = 8,
             height = 5,
             device = cairo_pdf)
    }else{
      ggsave(p_oplsda_lollipop, 
             filename = paste('./results/oplsda/figures/', filename, sep = ''),
             width = 8,
             height = 5)
    }
  }
  p_oplsda_lollipop # 返回图
})

# heatmap
output$heatmap_oplsda <- renderPlot(plot_heatmap_res_oplsda(),
                                         height = 350,
                                         width = 600)
plot_heatmap_res_oplsda <- eventReactive(input$submit_oplsda,{
  if (input$submit_oplsda > 0) {
    
    vip_score <- as.data.frame(oplsda_res()@vipVn)
    colnames(vip_score) <- 'VIP值'
    vip_score$metabolites <- rownames(vip_score)
    vip_score <- vip_score[,c('metabolites','VIP值')]
    vip_score <- vip_score[order(-vip_score$VIP值),]
    vip_score <- vip_score[1:as.numeric(input$num_oplsda_show),]
    
    df_sub <- user_data_oplsda()
    rownames(df_sub) <- df_sub[,1]
    df_sub <- df_sub[,-1]
    df_sub <- df_sub[,-1]
    heat = df_sub[,vip_score$meta] %>% t() %>% as.data.frame()
    
    p_heapmap <-  pheatmap::pheatmap(heat,
                       scale = ifelse(input$oplsda_heatmap_scale == 'TRUE',
                                      'row','none'),
                       color = colorRampPalette(colors = c("blue","white","red"))(100),
                       border_color = NA,
                       cluster_rows = ifelse(input$oplsda_heatmap_cluster == 'TRUE',
                                             TRUE,FALSE),
                       cluster_cols = ifelse(input$oplsda_heatmap_cluster == 'TRUE',
                                             TRUE,FALSE))
    
    p_heapmap <- ggplotify::as.ggplot(p_heapmap)
    
    # 保存图片
    filename <- ifelse(input$oplsda_fig_res_filetype == '.pdf','热图.pdf',
                       ifelse(input$oplsda_fig_res_filetype == '.png','热图.png',
                              ifelse(input$oplsda_fig_res_filetype == '.jpg','热图.jpg',
                                     ifelse(input$oplsda_fig_res_filetype == '.tiff','热图.tiff','热图.eps'))))
    
    if (input$oplsda_fig_res_filetype == '.pdf') {
      ggsave(p_heapmap, 
             filename = paste('./results/oplsda/figures/', filename, sep = ''),
             width = 6,
             height = 5,
             device = cairo_pdf)
    }else{
      ggsave(p_heapmap, 
             filename = paste('./results/oplsda/figures/', filename, sep = ''),
             width = 6,
             height = 5)
    }
    
  }
  dev.off()
  pheatmap::pheatmap(heat,
                     scale = ifelse(input$oplsda_heatmap_scale == 'TRUE',
                                    'row','none'),
                     color = colorRampPalette(colors = c("blue","white","red"))(100),
                     border_color = NA,
                     cluster_rows = ifelse(input$oplsda_heatmap_cluster == 'TRUE',
                                           TRUE,FALSE),
                     cluster_cols = ifelse(input$oplsda_heatmap_cluster == 'TRUE',
                                           TRUE,FALSE))
  }
  )


# sample scores plot

output$oplsda_plot <- renderPlot(plot_res_oplsda(),
                                    height = 350,
                                    width = 600)
plot_res_oplsda <- eventReactive(input$submit_oplsda,{
  if (input$submit_oplsda > 0) {
    
    sacurine_model.df <- oplsda_res()@modelDF # 提取解释度
    
    sample.score = oplsda_res()@scoreMN %>% 
      as.data.frame() %>%
      mutate(group.3 = user_data_oplsda()$group,
             o1 = oplsda_res()@orthoScoreMN[,1])
    
    p_score = ggplot(sample.score, aes(p1, o1, color = group.3)) +
      geom_hline(yintercept = 0, linetype = 'dashed', size = 0.5) +
      geom_vline(xintercept = 0, linetype = 'dashed', size = 0.5) +
      geom_point(size = 2) +
      labs(x = paste('P1(',round( sacurine_model.df["p1", "R2X(cum)"]*100, 2),'%)',sep = ''),
           y = 'to1') +
      stat_ellipse(level = 0.95, linetype = 'solid', 
                   size = 1, show.legend = FALSE) +
      scale_color_aaas() +
      theme_bw() +
      theme(legend.title = element_blank(),
            legend.text = element_text(color = 'black',size = 10, family = 'Arial', face = 'plain'),
            panel.background = element_blank(),
            panel.grid = element_blank(),
            axis.text = element_text(color = 'black',size = 10, family = 'Arial', face = 'plain'),
            axis.title = element_text(color = 'black',size = 10, family = 'Arial', face = 'plain'),
            axis.ticks = element_line(color = 'black'))

    # 保存图片
    filename <- ifelse(input$oplsda_fig_res_filetype == '.pdf','oplsda图.pdf',
                       ifelse(input$oplsda_fig_res_filetype == '.png','oplsda图.png',
                              ifelse(input$oplsda_fig_res_filetype == '.jpg','oplsda图.jpg',
                                     ifelse(input$oplsda_fig_res_filetype == '.tiff','oplsda图.tiff','oplsda图.eps'))))
    
    if (input$oplsda_fig_res_filetype == '.pdf') {
      ggsave(p_score, 
             filename = paste('./results/oplsda/figures/', filename, sep = ''),
             width = 5,
             height = 4,
             device = cairo_pdf)
    }else{
      ggsave(p_score, 
             filename = paste('./results/oplsda/figures/', filename, sep = ''),
             width = 5,
             height = 4)
    }
  }
  p_score
})

# 下载分析结果
output$taboutput_oplsda_download <- downloadHandler(
  filename <- function(){
    paste(stringr::str_sub(input$data_input_oplsda$name,
                           1,
                           (nchar(input$data_input_oplsda$name) - 4)),
          '_统计分析结果',input$oplsda_stat_res_filetype,sep = ''
    )
  },
  content <- function(file){
    if (input$oplsda_stat_res_filetype == '.csv') {
      file.copy('./results/oplsda/rest_tab.csv',file)
    }else if (input$oplsda_stat_res_filetype == '.txt') {
      file.copy('./results/oplsda/rest_tab.txt',file)
    }else{
      #return(NULL)
      file.copy('./results/oplsda/rest_tab.xlsx',file)
    }
  }
)

# 下载图片
output$download_figure__oplsda <- downloadHandler(
  filename <- function(){
    paste(stringr::str_sub(input$data_input_oplsda$name,
                           1,
                           (nchar(input$data_input_oplsda$name) - 4)),
          'oplsda图片结果.zip',sep = '')
  },
  content <- function(file){
    zip(file,'./results/oplsda/figures/')
  }
)