# 提交成功显示
observeEvent(input$submit_plsda, { 
  if (input$submit_plsda>0) {
    sendSweetAlert(
      session = session,
      title = "提交成功!",
      text = "数据上传成功，参数设置正确",
      type = "success")
  }
})
# 导入数据
user_data_plsda <- reactive({
  table_in_test <- read.csv(input$data_input_plsda$datapath,
                            header = T, 
                            stringsAsFactors = TRUE,
                            encoding = 'UTF-8')
  colnames(table_in_test)[1:2] <- c('sample','group')
  table_in_test <<- table_in_test
})

# 下载示例数据
output$download_demo_data_plsda <- downloadHandler(
  filename = 'PLS-DA示例数据.csv',
  content = function(file){
    file.copy('./demo_data/PLS-DA示例数据.csv',file)
  }
)


# plsda
plsda_res <- reactive({
  df_plsda <- user_data_plsda()
  
  plsda <- ropls::opls(df_plsda[,3:ncol(df_plsda)],
                        df_plsda$group,
                        predI = 1, 
                        orthoI = NA)
  plsda <<- plsda
})



# 展示统计分析结果
output$taboutput_plsda_view <- renderDataTable(df_table_plsda(),
                                                options = list(
                                                  pageLength = 4
                                                ))

df_table_plsda <- eventReactive(input$submit_plsda,{
  if (input$submit_plsda > 0) {
    user_records <- data.table::fread('./www/user records.txt', encoding = 'UTF-8')
    temp <- as.character(Sys.time())
    temp <- stringr::str_split(temp, ' ')
    date <- temp[[1]][1]
    time <- temp[[1]][2]
    user_record <- data.frame(Cat = 'plsda',
                              Date = date,
                              Time = time)
    user_records <- rbind(user_records, user_record)
    data.table::fwrite(user_records,file = './www/user records.txt')
    
    vip_score <- as.data.frame(plsda_res()@vipVn)
    colnames(vip_score) <- 'VIP值'
    vip_score$metabolites <- rownames(vip_score)
    vip_score <- vip_score[,c('metabolites','VIP值')]
    vip_score <- vip_score[order(-vip_score$VIP值),]
    
    # 保存分析结果
    write.csv(vip_score, file = './results/plsda/rest_tab.csv',row.names = FALSE)
    write.table(vip_score, file = './results/plsda/rest_tab.txt',row.names = FALSE)
    xlsx::write.xlsx(vip_score, file = './results/plsda/rest_tab.xlsx',row.names = FALSE)
  }
  vip_score
})

# 棒棒糖图
output$lollipop_plot_plsda <- renderPlot(plot_lollipop_res_plsda(),
                                          height = 350,
                                          width = 600)
plot_lollipop_res_plsda <- eventReactive(input$submit_plsda,{
  if (input$submit_plsda > 0) {
    
    vip_score <- as.data.frame(plsda_res()@vipVn)
    colnames(vip_score) <- 'VIP值'
    vip_score$metabolites <- rownames(vip_score)
    vip_score <- dplyr::select(vip_score,c('metabolites','VIP值'))
    vip_score = vip_score[vip_score$VIP值 >= 1,]
    vip_score <- vip_score[order(-vip_score$VIP值),][1:as.numeric(input$num_plsda_show),]
    vip_2 = vip_score[order(vip_score$VIP值),]
    vip_2$metabolites = factor(vip_2$metabolites,levels = unique(vip_2$metabolites))
    
    p_plsda_lollipop = ggplot(vip_2, aes(metabolites, VIP值)) +
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
    filename <- ifelse(input$plsda_fig_res_filetype == '.pdf','res_fig.pdf',
                       ifelse(input$plsda_fig_res_filetype == '.png','res_fig.png',
                              ifelse(input$plsda_fig_res_filetype == '.jpg','res_fig.jpg',
                                     ifelse(input$plsda_fig_res_filetype == '.tiff','res_fig.tiff','res_fig.eps'))))
    
    if (input$plsda_fig_res_filetype == '.pdf') {
      ggsave(p_plsda_lollipop, 
             filename = paste('./results/plsda/figures/', filename, sep = ''),
             width = 8,
             height = 5,
             device = cairo_pdf)
    }else{
      ggsave(p_plsda_lollipop, 
             filename = paste('./results/plsda/figures/', filename, sep = ''),
             width = 8,
             height = 5)
    }
  }
  p_plsda_lollipop # 返回图
})

# heatmap
output$heatmap_plsda <- renderPlot(plot_heatmap_res_plsda(),
                                    height = 350,
                                    width = 600)
plot_heatmap_res_plsda <- eventReactive(input$submit_plsda,{
  if (input$submit_plsda > 0) {
    
    vip_score <- as.data.frame(plsda_res()@vipVn)
    colnames(vip_score) <- 'VIP值'
    vip_score$metabolites <- rownames(vip_score)
    vip_score <- vip_score[,c('metabolites','VIP值')]
    vip_score <- vip_score[order(-vip_score$VIP值),]
    vip_score <- vip_score[1:as.numeric(input$num_plsda_show),]
    
    df_sub <- user_data_plsda()
    rownames(df_sub) <- df_sub[,1]
    df_sub <- df_sub[,-1]
    df_sub <- df_sub[,-1]
    heat = df_sub[,vip_score$meta] %>% t() %>% as.data.frame()
    
    p_heapmap <-  pheatmap::pheatmap(heat,
                                     scale = ifelse(input$plsda_heatmap_scale == 'TRUE',
                                                    'row','none'),
                                     color = colorRampPalette(colors = c("blue","white","red"))(100),
                                     border_color = NA,
                                     cluster_rows = ifelse(input$plsda_heatmap_cluster == 'TRUE',
                                                           TRUE,FALSE),
                                     cluster_cols = ifelse(input$plsda_heatmap_cluster == 'TRUE',
                                                           TRUE,FALSE))
    
    p_heapmap <- ggplotify::as.ggplot(p_heapmap)
    
    # 保存图片
    filename <- ifelse(input$plsda_fig_res_filetype == '.pdf','热图.pdf',
                       ifelse(input$plsda_fig_res_filetype == '.png','热图.png',
                              ifelse(input$plsda_fig_res_filetype == '.jpg','热图.jpg',
                                     ifelse(input$plsda_fig_res_filetype == '.tiff','热图.tiff','热图.eps'))))
    
    if (input$plsda_fig_res_filetype == '.pdf') {
      ggsave(p_heapmap, 
             filename = paste('./results/plsda/figures/', filename, sep = ''),
             width = 6,
             height = 5,
             device = cairo_pdf)
    }else{
      ggsave(p_heapmap, 
             filename = paste('./results/plsda/figures/', filename, sep = ''),
             width = 6,
             height = 5)
    }
    
  }
  dev.off()
  pheatmap::pheatmap(heat,
                     scale = ifelse(input$plsda_heatmap_scale == 'TRUE',
                                    'row','none'),
                     color = colorRampPalette(colors = c("blue","white","red"))(100),
                     border_color = NA,
                     cluster_rows = ifelse(input$plsda_heatmap_cluster == 'TRUE',
                                           TRUE,FALSE),
                     cluster_cols = ifelse(input$plsda_heatmap_cluster == 'TRUE',
                                           TRUE,FALSE))
}
)


# sample scores plot

output$plsda_plot <- renderPlot(plot_res_plsda(),
                                 height = 350,
                                 width = 600)
plot_res_plsda <- eventReactive(input$submit_plsda,{
  if (input$submit_plsda > 0) {
    
    sacurine_model.df <- plsda_res()@modelDF # 提取解释度
    
    sample.score = plsda_res()@scoreMN %>% 
      as.data.frame() %>%
      mutate(group.3 = user_data_plsda()$group,
             o1 = plsda_res()@orthoScoreMN[,1])
    
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
    filename <- ifelse(input$plsda_fig_res_filetype == '.pdf','plsda图.pdf',
                       ifelse(input$plsda_fig_res_filetype == '.png','plsda图.png',
                              ifelse(input$plsda_fig_res_filetype == '.jpg','plsda图.jpg',
                                     ifelse(input$plsda_fig_res_filetype == '.tiff','plsda图.tiff','plsda图.eps'))))
    
    if (input$plsda_fig_res_filetype == '.pdf') {
      ggsave(p_score, 
             filename = paste('./results/plsda/figures/', filename, sep = ''),
             width = 5,
             height = 4,
             device = cairo_pdf)
    }else{
      ggsave(p_score, 
             filename = paste('./results/plsda/figures/', filename, sep = ''),
             width = 5,
             height = 4)
    }
  }
  p_score
})

# 下载分析结果
output$taboutput_plsda_download <- downloadHandler(
  filename <- function(){
    paste(stringr::str_sub(input$data_input_plsda$name,
                           1,
                           (nchar(input$data_input_plsda$name) - 4)),
          '_统计分析结果',input$plsda_stat_res_filetype,sep = ''
    )
  },
  content <- function(file){
    if (input$plsda_stat_res_filetype == '.csv') {
      file.copy('./results/plsda/rest_tab.csv',file)
    }else if (input$plsda_stat_res_filetype == '.txt') {
      file.copy('./results/plsda/rest_tab.txt',file)
    }else{
      #return(NULL)
      file.copy('./results/plsda/rest_tab.xlsx',file)
    }
  }
)

# 下载图片
output$download_figure__plsda <- downloadHandler(
  filename <- function(){
    paste(stringr::str_sub(input$data_input_plsda$name,
                           1,
                           (nchar(input$data_input_plsda$name) - 4)),
          'plsda图片结果.zip',sep = '')
  },
  content <- function(file){
    zip(file,'./results/plsda/figures/')
  }
)