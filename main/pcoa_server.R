# 提交成功显示
observeEvent(input$submit_pcoa, { 
  if (input$submit_pcoa>0) {
    sendSweetAlert(
      session = session,
      title = "提交成功!",
      text = "数据上传成功，参数设置正确",
      type = "success")
  }
})
# 导入数据
user_data_pcoa <- reactive({
  data.table::fread(input$data_input_pcoa$datapath, encoding = 'UTF-8') %>% as.data.frame()
})

# 下载示例数据
output$download_demo_data_pcoa <- downloadHandler(
  filename = 'pcoa示例数据.csv',
  content = function(file){
    file.copy('./demo_data/pcoa示例数据.csv',file)
  }
)

# 提取分组
pcoa_group <- eventReactive(input$submit_pcoa,{
  if (input$submit_pcoa > 0) {
    group_pcoa <- c()
    
    group_input <- stringr::str_split(input$pcoa_group_input,',')
    
    for (i in 1:length(group_input[[1]])) {
      group_pcoa <- c(group_pcoa,c(group_input[[1]][i]))
    }
    
    group_df <- user_data_pcoa()[,group_pcoa] %>% as.data.frame()
    
    colnames(group_df) <- group_pcoa
  }
  group_df
})


# 提取数字矩阵
pcoa_number <- eventReactive(input$submit_pcoa,{
  if (input$submit_pcoa > 0) {
    group_pcoa <- c()
    
    group_input <- stringr::str_split(input$pcoa_group_input,',')
    
    for (i in 1:length(group_input[[1]])) {
      group_pcoa <- c(group_pcoa,c(group_input[[1]][i]))
    }
    
    group_df <- dplyr::select(user_data_pcoa(),-group_pcoa)
    
  }
  group_df
})

# 进行pcoa计算
pcoa_res <- eventReactive(input$submit_pcoa,{
  if (input$submit_pcoa > 0) {
    
    user_records <- data.table::fread('./www/user records.txt', encoding = 'UTF-8')
    temp <- as.character(Sys.time())
    temp <- stringr::str_split(temp, ' ')
    date <- temp[[1]][1]
    time <- temp[[1]][2]
    user_record <- data.frame(Cat = 'pcoa',
                              Date = date,
                              Time = time)
    user_records <- rbind(user_records, user_record)
    data.table::fwrite(user_records,file = './www/user records.txt')
    
    
    pcoa <- cmdscale(vegdist(pcoa_number(), method = 'bray'), 
                     k = nrow(pcoa_number())-1,eig = T)
  }
  pcoa
})


# 碎石图
output$pcoa_stone <- renderPlot(pcoa_stone_plot(),
                               height = 330,
                               width = 700)

pcoa_stone_plot <- eventReactive(input$submit_pcoa,{
  if (input$submit_pcoa > 0) {
    sde = data.frame(sd = pcoa_res()[["eig"]])
    sde$PC = paste('PCoA',1:nrow(sde), sep = '')
    sde$percent = round(sde$sd/sum(sde$sd)*100,2)
    
    p_pcoa_1 <- ggplot(sde[1:9,], aes(PC,percent,fill = PC)) +
      geom_bar(stat = 'identity',width = 0.5) + 
      geom_text(aes(PC,percent*1.1,label = paste(percent,'%',sep = ''))) +
      geom_hline(yintercept = 100, color = 'white') +
      scale_y_continuous(expand = c(0,0),breaks = seq(0,100,10)) +
      scale_fill_aaas() +
      labs(y = '解释度（%）')+
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
    filename <- ifelse(input$pcoa_fig_res_filetype == '.pdf','res_fig_1.pdf',
                       ifelse(input$pcoa_fig_res_filetype == '.png','res_fig_1.png',
                              ifelse(input$pcoa_fig_res_filetype == '.jpg','res_fig_1.jpg',
                                     ifelse(input$pcoa_fig_res_filetype == '.tiff','res_fig_1.tiff','res_fig_1.eps'))))
    
    if (input$pcoa_fig_res_filetype == '.pdf') {
      ggsave(p_pcoa_1, 
             filename = paste('./results/', filename, sep = ''),
             width = input$pcoa_fig_wdith,
             height = input$pcoa_fig_height,
             device = cairo_pdf)
    }else{
      ggsave(p_pcoa_1, 
             filename = paste('./results/', filename, sep = ''),
             width = input$pcoa_fig_wdith,
             height = input$pcoa_fig_height)
    }
  }
  p_pcoa_1
})


# 散点图
output$pcoa_point <- renderPlot(pcoa_point_plot(),height = 380,width = 700)
#output$pcoa_point <- renderDataTable(pcoa_point_plot())
pcoa_point_plot <- eventReactive(input$submit_pcoa,{
  if (input$submit_pcoa > 0) {
    
    pcoa_point_df <- pcoa_res()[["points"]]  %>% as.data.frame()
    colnames(pcoa_point_df) <- paste('PCoA',1:ncol(pcoa_point_df), sep = '')
    pcoa_point_df <- cbind(pcoa_point_df,pcoa_group())
    
    # 选择数据
    # 分组
    group_pcoa <- c()
    
    group_input <- stringr::str_split(input$pcoa_group_input,',')
    
    for (i in 1:length(group_input[[1]])) {
      group_pcoa <- c(group_pcoa,c(group_input[[1]][i]))
    }
    
    # PC score
    score_pcoa <- c()
    
    score_input <- stringr::str_split(input$select_pcoa_num,',')
    
    for (i in 1:length(score_input[[1]])) {
      score_pcoa <- c(score_pcoa,c(score_input[[1]][i]))
    }
    
    all_column <- c(group_pcoa, score_pcoa)
    
    plot_df <- dplyr::select(pcoa_point_df,all_column)
    
    # 绘制基础图形
    p_pcoa_2 <- ggplot(plot_df, 
                       aes_string(score_input[[1]][1],
                                  score_input[[1]][2],
                                  color = input$pcoa_point_color)) +
      geom_point(aes_string(shape = input$pcoa_point_shape),
                 size = input$pcoa_point_size,
                 alpha = input$pcoa_point_transparency) +
      scale_y_continuous(expand = c(0,0)) +
      scale_color_aaas() +
      theme_classic()
    
    # 绘图样式设置
    if (input$select_plot_type_pcoa == 1) {
      p_pcoa_2 <- p_pcoa_2 + stat_ellipse(level = 0.68, show.legend = F)
    }else if(input$select_plot_type_pcoa == 2){
      # 求均值
      df_cent <- dplyr::select(plot_df, c(score_pcoa,input$pcoa_point_color))
      colnames(df_cent) <- c('A','B','C')
      pcoa_cent <- aggregate(cbind(A,B) ~ C,data = df_cent, FUN = mean)
      colnames(pcoa_cent) <- c(input$pcoa_point_color,'cent1','cent2')
      
      
      plot_df_2 <- merge(plot_df, pcoa_cent, 
                         by = input$pcoa_point_color, all.x = TRUE)
      
      p_pcoa_2 <- ggplot(plot_df_2,
                         aes_string(score_input[[1]][1],
                                    score_input[[1]][2],
                                    color = input$pcoa_point_color)) +
        geom_point(aes_string(shape = input$pcoa_point_shape),
                   size = input$pcoa_point_size,
                   alpha = input$pcoa_point_transparency) +
        geom_point(data = pcoa_cent, aes(cent1, cent2), size = 5, show.legend = FALSE) +
        geom_segment(aes(xend = cent1, yend = cent2), show.legend = FALSE) +
        scale_y_continuous(expand = c(0,0)) +
        scale_color_aaas() +
        theme_classic()
    }else{
      border_df <- dplyr::select(plot_df, c(score_pcoa,input$pcoa_point_color))
      group_border <- ddply(border_df, input$pcoa_point_color, 
                            function(df) df[chull(df[[1]], df[[2]]), ])
      
      p_pcoa_2 <- ggplot(plot_df,
                         aes_string(score_input[[1]][1],
                                    score_input[[1]][2],
                                    color = input$pcoa_point_color)) +
        geom_point(aes_string(shape = input$pcoa_point_shape),
                   size = input$pcoa_point_size,
                   alpha = input$pcoa_point_transparency) +
        geom_polygon(data = group_border, alpha = 0.3, show.legend = F) +
        scale_y_continuous(expand = c(0,0)) +
        scale_color_aaas() +
        theme_classic()
    }
    
    
    # 是否添加横线
    if (input$pcoa_hline == 'TRUE') {
      p_pcoa_2 <- p_pcoa_2 +
        geom_hline(yintercept = 0, linetype = 'dashed')
    }else{
      p_pcoa_2 <- p_pcoa_2
    }
    
    # 是否添加竖线
    if (input$pcoa_vline == 'TRUE') {
      p_pcoa_2 <- p_pcoa_2 +
        geom_vline(xintercept = 0, linetype = 'dashed')
    }else{
      p_pcoa_2 <- p_pcoa_2
    }
    
    # 四周坐标
    if (input$pcoa_axis == 'TRUE') {
      p_pcoa_2 <- p_pcoa_2 + theme_bw()
    }else{
      p_pcoa_2 <- p_pcoa_2
    }
    
    # 坐标轴名称
    sde <- data.frame(sd = pcoa_res()[["eig"]])
    sde$PC <- paste('PCoA',1:nrow(sde), sep = '')
    sde$percent <- round(sde$sd/sum(sde$sd)*100,2)
    
    # X轴
    if (input$pcoa_xaxis == '') {
      p_pcoa_2 <- p_pcoa_2 + labs(x = paste(colnames(plot_df)[2],'(',sde[sde$PC == colnames(plot_df)[2],]$percent,'%)'))
    }else{
      p_pcoa_2 <- p_pcoa_2 + labs(x = input$pcoa_xaxis)
    }
    
    # Y轴
    if (input$pcoa_yaxis == '') {
      p_pcoa_2 <- p_pcoa_2 + labs(y = paste(colnames(plot_df)[3],'(',sde[sde$PC == colnames(plot_df)[3],]$percent,'%)'))
    }else{
      p_pcoa_2 <- p_pcoa_2 + labs(y = input$pcoa_yaxis)
    }
    
    # 标题
    p_pcoa_2 <- p_pcoa_2 + labs(title = input$pcoa_title)
    
    # 图例位置
    p_pcoa_2 <- p_pcoa_2 + 
      theme(legend.position = ifelse(input$pcoa_legend == 'FALSE',
                                     'none',input$pcoa_legend_position),
            panel.background = element_blank(),
            panel.grid = element_blank(),
            axis.text = element_text(color = 'black',size = 10, 
                                     family = 'Arial', face = 'plain'),
            axis.title.x = element_text(color = 'black',size = 10,
                                        family = 'Arial', face = 'plain'),
            axis.ticks = element_line(color = 'black'))
    
    
    # 用户输入点的颜色
    color_user <- c()
    color_input <- stringr::str_split(input$pcoa_point_color_input,',')
    for (i in 1:length(color_input[[1]])) {
      color_user <- c(color_user,c(color_input[[1]][i]))
    }
    
    if (input$pcoa_point_color_input == '') {
      p_pcoa_2 <- p_pcoa_2
    }else{
      p_pcoa_2 <- p_pcoa_2 + 
        scale_color_manual(values = color_user)
    }
    
    # 适当扩大坐标轴范围
    p_pcoa_2 <- p_pcoa_2 + 
      ylim(min(plot_df[,3])*1.3, max(plot_df[,3]*1.1)) +
      xlim(min(plot_df[,2])*1.1, max(plot_df[,2]*1.1))
    
    
    # permanova分析
    if (input$var_perm_ana_pcoa == 'TRUE') {
      per_df <- plot_df
      colnames(per_df)[1] <- 'group'
      per <- vegan::adonis(per_df[,(length(group_pcoa)+1):ncol(per_df)] ~ group,
                           data = per_df,
                           method = 'bray',
                           permutations = 999)
      per_res <- per[["aov.tab"]]
      # 绘图结果加入permanova结果
      p_pcoa_2 <- p_pcoa_2 + annotate('text',
                                      max(plot_df[,2])*0.7,
                                      min(plot_df[,3])*1.2,
                                      label = paste('R2 = ',round(per_res[1,5],4),
                                                    '  P = ',round(per_res[1,6],4),
                                                    sep = ''),
                                      color = 'black',size = 3, family = 'Arial', face = 'plain')
    }else{
      p_pcoa_2 <- p_pcoa_2
    }
    
    # 保存图片
    filename <- ifelse(input$pcoa_fig_res_filetype == '.pdf','res_fig.pdf',
                       ifelse(input$pcoa_fig_res_filetype == '.png','res_fig.png',
                              ifelse(input$pcoa_fig_res_filetype == '.jpg','res_fig.jpg',
                                     ifelse(input$pcoa_fig_res_filetype == '.tiff','res_fig.tiff','res_fig.eps'))))
    
    if (input$pcoa_fig_res_filetype == '.pdf') {
      ggsave(p_pcoa_2, 
             filename = paste('./results/', filename, sep = ''),
             width = input$pcoa_fig_wdith,
             height = input$pcoa_fig_height,
             device = cairo_pdf)
    }else{
      ggsave(p_pcoa_2, 
             filename = paste('./results/', filename, sep = ''),
             width = input$pcoa_fig_wdith,
             height = input$pcoa_fig_height)
    }
    
    
  }
  p_pcoa_2
})


# 下载散点图
output$download_figure__pcoa <- downloadHandler(
  filename <- function(){
    paste(stringr::str_sub(input$data_input_pcoa$name,
                           1,
                           (nchar(input$data_input_pcoa$name) - 4)),
          '_散点图结果',input$pcoa_fig_res_filetype,sep = ''
    )
  },
  content <- function(file){
    if (input$pcoa_fig_res_filetype == '.pdf') {
      file.copy('./results/res_fig.pdf',file)
    }else if (input$pcoa_fig_res_filetype == '.png') {
      file.copy('./results/res_fig.png',file)
    }else if (input$pcoa_fig_res_filetype == '.jpg') {
      file.copy('./results/res_fig.jpg',file)
    }else if (input$pcoa_fig_res_filetype == '.tiff') {
      file.copy('./results/res_fig.tiff',file)
    }else{
      file.copy('./results/res_fig.eps',file)
    }
  }
)


# 下载散点图
output$taboutput_pcoa_download <- downloadHandler(
  filename <- function(){
    paste(stringr::str_sub(input$data_input_pcoa$name,
                           1,
                           (nchar(input$data_input_pcoa$name) - 4)),
          '_主成分解释度',input$pcoa_fig_res_filetype,sep = ''
    )
  },
  content <- function(file){
    if (input$pcoa_fig_res_filetype == '.pdf') {
      file.copy('./results/res_fig_1.pdf',file)
    }else if (input$pcoa_fig_res_filetype == '.png') {
      file.copy('./results/res_fig_1.png',file)
    }else if (input$pcoa_fig_res_filetype == '.jpg') {
      file.copy('./results/res_fig_1.jpg',file)
    }else if (input$pcoa_fig_res_filetype == '.tiff') {
      file.copy('./results/res_fig_1.tiff',file)
    }else{
      file.copy('./results/res_fig_1.eps',file)
    }
  }
)





