# 提交成功显示
observeEvent(input$submit_nmds, { 
  if (input$submit_nmds>0) {
    sendSweetAlert(
      session = session,
      title = "提交成功!",
      text = "数据上传成功，参数设置正确",
      type = "success")
  }
})
# 导入数据
user_data_nmds <- reactive({
  data.table::fread(input$data_input_nmds$datapath, encoding = 'UTF-8') %>% as.data.frame()
})

# 下载示例数据
output$download_demo_data_nmds <- downloadHandler(
  filename = 'NMDS示例数据.csv',
  content = function(file){
    file.copy('./demo_data/NMDS示例数据.csv',file)
  }
)


# 拟合度评估
output$nmds_stone <- renderPlot(nmds_stone_plot(),
                               height = 330,
                               width = 700)

nmds_stone_plot <- eventReactive(input$submit_nmds,{
  if (input$submit_nmds > 0) {
    
    user_records <- data.table::fread('./www/user records.txt', encoding = 'UTF-8')
    temp <- as.character(Sys.time())
    temp <- stringr::str_split(temp, ' ')
    date <- temp[[1]][1]
    time <- temp[[1]][2]
    user_record <- data.frame(Cat = 'nmds',
                              Date = date,
                              Time = time)
    user_records <- rbind(user_records, user_record)
    data.table::fwrite(user_records,file = './www/user records.txt')
    
    
    
    df <- user_data_nmds()
    nmds <- vegan::metaMDS(df[,2:ncol(df)],
                           distance = 'bray')
    
    # 保存图片
    filename <- ifelse(input$nmds_fig_res_filetype == '.pdf','res_fig_1.pdf',
                       ifelse(input$nmds_fig_res_filetype == '.png','res_fig_1.png',
                              ifelse(input$nmds_fig_res_filetype == '.jpg','res_fig_1.jpg',
                                     ifelse(input$nmds_fig_res_filetype == '.tiff','res_fig_1.tiff','res_fig_1.eps'))))
    if (input$nmds_fig_res_filetype == '.pdf') {
      pdf(file = paste('./results/', filename, sep = ''),
          width = input$nmds_fig_wdith,
          height = input$nmds_fig_height)
    }else if (input$nmds_fig_res_filetype == '.png') {
      png(filename = paste('./results/', filename, sep = ''),
          width = input$nmds_fig_wdith,
          height = input$nmds_fig_height)
    }else if (input$nmds_fig_res_filetype == '.jpg') {
      jpeg(filename = paste('./results/', filename, sep = ''),
           width = input$nmds_fig_wdith,
           height = input$nmds_fig_height)
    }else if (input$nmds_fig_res_filetype == '.tiff') {
      tiff(filename = paste('./results/', filename, sep = ''),
           width = input$nmds_fig_wdith,
           height = input$nmds_fig_height)
    }else{
      return(NULL)
    }
    
    stressplot(nmds)
    dev.off()
    
    p = vegan::stressplot(nmds)
  }
  p
})


# 散点图
output$nmds_point <- renderPlot(nmds_point_plot(),height = 380,width = 700)

nmds_point_plot <- eventReactive(input$submit_nmds,{
  if (input$submit_nmds > 0) {
    df <- user_data_nmds()
    nmds <- vegan::metaMDS(df[,2:ncol(df)],
                           distance = 'bray')
    
    plot_df <- nmds[["points"]] %>% as.data.frame()
    colnames(plot_df) <- c('NMDS1','NMDS2')
    
    plot_df <- cbind(df[,1], plot_df)
    colnames(plot_df)[1] <- 'group'
    
    # 绘制基础图形
    p_nmds_2 <- ggplot(plot_df, 
                       aes(NMDS1,NMDS2,color = group)) +
      geom_point(aes_string(shape = input$nmds_point_shape),
                 size = input$nmds_point_size,
                 alpha = input$nmds_point_transparency) +
      scale_y_continuous(expand = c(0,0)) +
      scale_color_aaas() +
      theme_prism(base_size = 14)
    
    
    # 绘图样式设置
    if (input$select_plot_type_nmds == 1) {
      p_nmds_2 <- p_nmds_2 + stat_ellipse(level = 0.68, show.legend = F)
    }else if(input$select_plot_type_nmds == 2){
      # 求均值
      df_cent <- plot_df[,c(2,3,1)]
      colnames(df_cent) <- c('A','B','C')
      nmds_cent <- aggregate(cbind(A,B) ~ C,data = df_cent, FUN = mean)
      colnames(nmds_cent) <- c('group','cent1','cent2')
      
      
      plot_df_2 <- merge(plot_df, nmds_cent, 
                         by = 'group', 
                         all.x = TRUE)
      
      p_nmds_2 <- ggplot(plot_df_2,
                         aes(NMDS1,NMDS2,color = group)) +
        geom_point(aes_string(shape = input$nmds_point_shape),
                   size = input$nmds_point_size,
                   alpha = input$nmds_point_transparency) +
        geom_point(data = nmds_cent, aes(cent1, cent2), size = 5, show.legend = FALSE) +
        geom_segment(aes(xend = cent1, yend = cent2), show.legend = FALSE) +
        scale_y_continuous(expand = c(0,0)) +
        scale_color_aaas() +
        theme_prism(base_size = 14)
    }else{
      border_df <- plot_df[,c(2,3,1)]
      group_border <- ddply(border_df, 'group', 
                            function(df) df[chull(df[[1]], df[[2]]), ])
      
      p_nmds_2 <- ggplot(plot_df,
                         aes(NMDS1,NMDS2,color = group)) +
        geom_point(aes_string(shape = input$nmds_point_shape),
                   size = input$nmds_point_size,
                   alpha = input$nmds_point_transparency) +
        geom_polygon(data = group_border, alpha = 0.3, show.legend = F) +
        scale_y_continuous(expand = c(0,0)) +
        scale_color_aaas() +
        theme_prism(base_size = 14)
    }
    
    # 是否添加横线
    if (input$nmds_hline == 'TRUE') {
      p_nmds_2 <- p_nmds_2 +
        geom_hline(yintercept = 0, linetype = 'dashed')
    }else{
      p_nmds_2 <- p_nmds_2
    }
    
    # 是否添加竖线
    if (input$nmds_vline == 'TRUE') {
      p_nmds_2 <- p_nmds_2 +
        geom_vline(xintercept = 0, linetype = 'dashed')
    }else{
      p_nmds_2 <- p_nmds_2
    }
    
    # 四周坐标
    if (input$nmds_axis == 'TRUE') {
      p_nmds_2 <- p_nmds_2 + theme_bw()
    }else{
      p_nmds_2 <- p_nmds_2
    }
    
    
    # X轴
    if (input$nmds_xaxis == '') {
      p_nmds_2 <- p_nmds_2 + labs(x = 'NMDS1')
    }else{
      p_nmds_2 <- p_nmds_2 + labs(x = input$nmds_xaxis)
    }
    
    # Y轴
    if (input$nmds_yaxis == '') {
      p_nmds_2 <- p_nmds_2 + labs(y = 'NMDS2')
    }else{
      p_nmds_2 <- p_nmds_2 + labs(y = input$nmds_yaxis)
    }
    
    # 标题
    p_nmds_2 <- p_nmds_2 + labs(title = input$nmds_title)
    
    # 图例位置
    p_nmds_2 <- p_nmds_2 + 
      theme(legend.position = ifelse(input$nmds_legend == 'FALSE',
                                     'none',input$nmds_legend_position),
            panel.background = element_blank(),
            panel.grid = element_blank(),
            axis.text = element_text(color = 'black',size = 10, 
                                     family = 'Arial', face = 'plain'),
            axis.title.x = element_text(color = 'black',size = 10,
                                        family = 'Arial', face = 'plain'),
            axis.ticks = element_line(color = 'black'))
    
    
    # 用户输入点的颜色
    color_user <- c()
    color_input <- stringr::str_split(input$nmds_point_color_input,',')
    for (i in 1:length(color_input[[1]])) {
      color_user <- c(color_user,c(color_input[[1]][i]))
    }
    
    if (input$nmds_point_color_input == '') {
      p_nmds_2 <- p_nmds_2
    }else{
      p_nmds_2 <- p_nmds_2 + 
        scale_color_manual(values = color_user)
    }
    
    # 适当扩大坐标轴范围
    p_nmds_2 <- p_nmds_2 + 
      ylim(min(plot_df[,3])*1.3, max(plot_df[,3]*1.3)) +
      xlim(min(plot_df[,2])*1.1, max(plot_df[,2]*1.1))
    
    
    p_nmds_2 <- p_nmds_2 + annotate('text',
                                    max(plot_df[,2])*0.75,
                                    min(plot_df[,3])*1.2,
                                    label = paste('Stress = ', round(nmds$stress,2), sep = ''),
                                    color = 'black',size = 3, family = 'Arial', face = 'plain')
    
    
    
    # 保存图片
    filename <- ifelse(input$nmds_fig_res_filetype == '.pdf','res_fig.pdf',
                       ifelse(input$nmds_fig_res_filetype == '.png','res_fig.png',
                              ifelse(input$nmds_fig_res_filetype == '.jpg','res_fig.jpg',
                                     ifelse(input$nmds_fig_res_filetype == '.tiff','res_fig.tiff','res_fig.eps'))))
    
    if (input$nmds_fig_res_filetype == '.pdf') {
      ggsave(p_nmds_2, 
             filename = paste('./results/', filename, sep = ''),
             width = input$nmds_fig_wdith,
             height = input$nmds_fig_height,
             device = cairo_pdf)
    }else{
      ggsave(p_nmds_2, 
             filename = paste('./results/', filename, sep = ''),
             width = input$nmds_fig_wdith,
             height = input$nmds_fig_height)
    }
    
  }
  p_nmds_2
})


# 下载散点图
output$download_figure__nmds <- downloadHandler(
  filename <- function(){
    paste(stringr::str_sub(input$data_input_nmds$name,
                           1,
                           (nchar(input$data_input_nmds$name) - 4)),
          '_散点图结果',input$nmds_fig_res_filetype,sep = ''
    )
  },
  content <- function(file){
    if (input$nmds_fig_res_filetype == '.pdf') {
      file.copy('./results/res_fig.pdf',file)
    }else if (input$nmds_fig_res_filetype == '.png') {
      file.copy('./results/res_fig.png',file)
    }else if (input$nmds_fig_res_filetype == '.jpg') {
      file.copy('./results/res_fig.jpg',file)
    }else if (input$nmds_fig_res_filetype == '.tiff') {
      file.copy('./results/res_fig.tiff',file)
    }else{
      file.copy('./results/res_fig.eps',file)
    }
  }
)


# 下载散点图
output$taboutput_nmds_download <- downloadHandler(
  filename <- function(){
    paste(stringr::str_sub(input$data_input_nmds$name,
                           1,
                           (nchar(input$data_input_nmds$name) - 4)),
          '_拟合度评估',input$nmds_fig_res_filetype,sep = ''
    )
  },
  content <- function(file){
    if (input$nmds_fig_res_filetype == '.pdf') {
      file.copy('./results/res_fig_1.pdf',file)
    }else if (input$nmds_fig_res_filetype == '.png') {
      file.copy('./results/res_fig_1.png',file)
    }else if (input$nmds_fig_res_filetype == '.jpg') {
      file.copy('./results/res_fig_1.jpg',file)
    }else if (input$nmds_fig_res_filetype == '.tiff') {
      file.copy('./results/res_fig_1.tiff',file)
    }else{
      file.copy('./results/res_fig_1.eps',file)
    }
  }
)





