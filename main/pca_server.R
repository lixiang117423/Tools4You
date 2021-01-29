# 提交成功显示
observeEvent(input$submit_pca, { 
  if (input$submit_pca>0) {
    sendSweetAlert(
      session = session,
      title = "提交成功!",
      text = "数据上传成功，参数设置正确",
      type = "success")
  }
})
# 导入数据
user_data_pca <- reactive({
  data.table::fread(input$data_input_pca$datapath, encoding = 'UTF-8') %>% as.data.frame()
})

# 下载示例数据
output$download_demo_data_pca <- downloadHandler(
  filename = 'PCA示例数据.csv',
  content = function(file){
    file.copy('./demo_data/PCA示例数据.csv',file)
  }
)

# 提取分组
pca_group <- eventReactive(input$submit_pca,{
  if (input$submit_pca > 0) {
    
    user_records <- data.table::fread('./www/user records.txt', encoding = 'UTF-8')
    temp <- as.character(Sys.time())
    temp <- stringr::str_split(temp, ' ')
    date <- temp[[1]][1]
    time <- temp[[1]][2]
    user_record <- data.frame(Cat = 'pca',
                              Date = date,
                              Time = time)
    user_records <- rbind(user_records, user_record)
    data.table::fwrite(user_records,file = './www/user records.txt')
    
    group_pca <- c()
    
    group_input <- stringr::str_split(input$pca_group_input,',')
    
    for (i in 1:length(group_input[[1]])) {
      group_pca <- c(group_pca,c(group_input[[1]][i]))
    }
    
    
    group_df <- dplyr::select(user_data_pca(),group_pca) %>% as.data.frame()
    
    #group_df <- user_data_pca()[,group_pca] %>% as.data.frame()
    
    colnames(group_df) <- group_pca
  }
  group_df
})


# 提取数字矩阵
pca_number <- eventReactive(input$submit_pca,{
  if (input$submit_pca > 0) {
    group_pca <- c()
    
    group_input <- stringr::str_split(input$pca_group_input,',')
    
    for (i in 1:length(group_input[[1]])) {
      group_pca <- c(group_pca,c(group_input[[1]][i]))
    }
    
    group_df <- dplyr::select(user_data_pca(),-group_pca) %>% as.data.frame()
    
  }
  group_df
})

# 进行PCA计算
pca_res <- eventReactive(input$submit_pca,{
  if (input$submit_pca > 0) {
    pca <- prcomp(pca_number(),scale. = TRUE)
  }
})


# 碎石图
output$pca_stone <- renderPlot(pca_stone_plot(),
                               height = 330,
                               width = 700)

pca_stone_plot <- eventReactive(input$submit_pca,{
  if (input$submit_pca > 0) {
    sde = data.frame(sd = pca_res()[["sdev"]])
    sde$PC = paste('PC',1:ncol(pca_number()), sep = '')
    sde$sed2 = sde$sd^2
    sde$percent = round(sde$sed2/sum(sde$sed2)*100,2)
    
    p_pca_1 <- ggplot(sde, aes(PC,percent,fill = PC)) +
      geom_bar(stat = 'identity',width = 0.5) + 
      geom_text(aes(PC,percent*1.1,label = paste(percent,'%',sep = ''))) +
      geom_hline(yintercept = 100, color = 'white') +
      scale_y_continuous(expand = c(0,0),breaks = seq(0,100,10)) +
      scale_fill_aaas() +
      labs(x = '主成分',
           y = '主成分解释度（%）')+
      theme_classic() +
      theme(legend.position = 'none')
    
    # 保存图片
    filename <- ifelse(input$pca_fig_res_filetype == '.pdf','res_fig_1.pdf',
                       ifelse(input$pca_fig_res_filetype == '.png','res_fig_1.png',
                              ifelse(input$pca_fig_res_filetype == '.jpg','res_fig_1.jpg',
                                     ifelse(input$pca_fig_res_filetype == '.tiff','res_fig_1.tiff','res_fig_1.eps'))))
    
    if (input$pca_fig_res_filetype == '.pdf') {
      ggsave(p_pca_1, 
             filename = paste('./results/', filename, sep = ''),
             width = input$pca_fig_wdith,
             height = input$pca_fig_height,
             device = cairo_pdf)
    }else{
      ggsave(p_pca_1, 
             filename = paste('./results/', filename, sep = ''),
             width = input$pca_fig_wdith,
             height = input$pca_fig_height)
    }
  }
  p_pca_1
})


# 散点图
output$pca_point <- renderPlot(pca_point_plot(),height = 380,width = 700)

pca_point_plot <- eventReactive(input$submit_pca,{
  if (input$submit_pca > 0) {
    pca_point_df <- pca_res()[["x"]] %>% as.data.frame()
    pca_point_df <- cbind(pca_point_df,pca_group())
    
    # 选择数据
    # 分组
    group_pca <- c()
    
    group_input <- stringr::str_split(input$pca_group_input,',')
    
    for (i in 1:length(group_input[[1]])) {
      group_pca <- c(group_pca,c(group_input[[1]][i]))
    }
    
    # PC score
    score_pca <- c()
    
    score_input <- stringr::str_split(input$select_pc_num,',')
    
    for (i in 1:length(score_input[[1]])) {
      score_pca <- c(score_pca,c(score_input[[1]][i]))
    }
    
    all_column <- c(group_pca, score_pca)
    
    plot_df <- dplyr::select(pca_point_df,all_column)
    
    # 绘制基础图形
    p_pca_2 <- ggplot(plot_df, 
                      aes_string(score_input[[1]][1],
                                 score_input[[1]][2],
                                 color = input$pca_point_color)) +
      geom_point(aes_string(shape = input$pca_point_shape),
                 size = input$pca_point_size,
                 alpha = input$pca_point_transparency) +
      scale_y_continuous(expand = c(0,0)) +
      scale_color_aaas() +
      theme_classic()
    
    
    # 绘图样式设置
    if (input$select_plot_type_pca == 1) {
      p_pca_2 <- p_pca_2 + stat_ellipse(level = 0.68, show.legend = F)
    }else if(input$select_plot_type_pca == 2){
      # 求均值
      df_cent <- dplyr::select(plot_df, c(score_pca,input$pca_point_color))
      colnames(df_cent) <- c('A','B','C')
      pca_cent <- aggregate(cbind(A,B) ~ C,data = df_cent, FUN = mean)
      colnames(pca_cent) <- c(input$pca_point_color,'cent1','cent2')
      
      
      plot_df_2 <- merge(plot_df, pca_cent, by = input$pca_point_color, all.x = TRUE)
      
      p_pca_2 <- ggplot(plot_df_2,
                        aes_string(score_input[[1]][1],
                                   score_input[[1]][2],
                                   color = input$pca_point_color)) +
        geom_point(aes_string(shape = input$pca_point_shape),
                   size = input$pca_point_size,
                   alpha = input$pca_point_transparency) +
        geom_point(data = pca_cent, aes(cent1, cent2), size = 5, show.legend = FALSE) +
        geom_segment(aes(xend = cent1, yend = cent2), show.legend = FALSE) +
        scale_y_continuous(expand = c(0,0)) +
        scale_color_aaas() +
        theme_classic()
    }else{
      border_df <- dplyr::select(plot_df, c(score_pca,input$pca_point_color))
      group_border <- ddply(border_df, input$pca_point_color, 
                           function(df) df[chull(df[[1]], df[[2]]), ])
      
      p_pca_2 <- ggplot(plot_df,
                        aes_string(score_input[[1]][1],
                                   score_input[[1]][2],
                                   color = input$pca_point_color)) +
        geom_point(aes_string(shape = input$pca_point_shape),
                   size = input$pca_point_size,
                   alpha = input$pca_point_transparency) +
        geom_polygon(data = group_border, alpha = 0.3, show.legend = F) +
        scale_y_continuous(expand = c(0,0)) +
        scale_color_aaas() +
        theme_classic()
    }
    
    # 是否添加横线
    if (input$pca_hline == 'TRUE') {
      p_pca_2 <- p_pca_2 +
        geom_hline(yintercept = 0, linetype = 'dashed')
    }else{
      p_pca_2 <- p_pca_2
    }
    
    # 是否添加竖线
    if (input$pca_vline == 'TRUE') {
      p_pca_2 <- p_pca_2 +
        geom_vline(xintercept = 0, linetype = 'dashed')
    }else{
      p_pca_2 <- p_pca_2
    }
    
    # 四周坐标
    if (input$pca_axis == 'TRUE') {
      p_pca_2 <- p_pca_2 + theme_bw()
    }else{
      p_pca_2 <- p_pca_2
    }
    
    # 坐标轴名称
    sde = data.frame(sd = pca_res()[["sdev"]])
    sde$PC = paste('PC',1:ncol(pca_number()), sep = '')
    sde$sed2 = sde$sd^2
    sde$percent = round(sde$sed2/sum(sde$sed2)*100,2)
    
    # X轴
    if (input$pca_xaxis == '') {
      p_pca_2 <- p_pca_2 + labs(x = paste(score_pca[1],'(',sde[sde$PC == score_pca[1],]$percent,'%)'))
    }else{
      p_pca_2 <- p_pca_2 + labs(x = input$pca_xaxis)
    }
    
    # Y轴
    if (input$pca_yaxis == '') {
      p_pca_2 <- p_pca_2 + labs(y = paste(score_pca[2],'(',sde[sde$PC == score_pca[2],]$percent,'%)'))
    }else{
      p_pca_2 <- p_pca_2 + labs(y = input$pca_yaxis)
    }
    
    # 标题
    p_pca_2 <- p_pca_2 + labs(title = input$pca_title)
    
    # 图例位置
    p_pca_2 <- p_pca_2 + 
      theme(legend.position = ifelse(input$pca_legend == 'FALSE',
                                     'none',input$pca_legend_position),
            panel.background = element_blank(),
            panel.grid = element_blank(),
            axis.text = element_text(color = 'black',size = 10),
            axis.title.x = element_text(color = 'black',size = 10),
            axis.ticks = element_line(color = 'black'))
    
    
    # 用户输入点的颜色
    color_user <- c()
    color_input <- stringr::str_split(input$pca_point_color_input,',')
    for (i in 1:length(color_input[[1]])) {
      color_user <- c(color_user,c(color_input[[1]][i]))
    }

    if (input$pca_point_color_input == '') {
      p_pca_2 <- p_pca_2
    }else{
      p_pca_2 <- p_pca_2 + 
        scale_color_manual(values = color_user)
    }
    
    # 适当扩大坐标轴范围
    p_pca_2 <- p_pca_2 + 
      ylim(min(plot_df[,ncol(plot_df)])*1.2, max(plot_df[,ncol(plot_df)]*1.2)) +
      xlim(min(plot_df[,ncol(plot_df) - 1])*1.2, max(plot_df[,ncol(plot_df) - 1]*1.2))
    
    
    # permanova分析
    if (input$var_perm_ana_pca == 'TRUE') {
      per_df <- user_data_pca()
      per_df <- per_df[,1:(ncol(pca_number()) + 1)]
      colnames(per_df)[ncol(per_df)] <- 'group'
      per <- vegan::adonis(per_df[,1:(ncol(per_df)-1)] ~ group,
                           data = per_df,
                           method = 'bray',
                           permutations = 999)
      per_res <- per[["aov.tab"]]
      # 绘图结果加入permanova结果
      p_pca_2 <- p_pca_2 + annotate('text',1.8,min(plot_df[,ncol(plot_df)])*1.2,
                                    label = paste('R2 = ',round(per_res[1,5],4),
                                                  '  P = ',round(per_res[1,6],4),
                                                  sep = ''),color = 'black',size = 3)
    }else{
      p_pca_2 <- p_pca_2
    }
    
    
    
    
    # 保存图片
    filename <- ifelse(input$pca_fig_res_filetype == '.pdf','res_fig.pdf',
                       ifelse(input$pca_fig_res_filetype == '.png','res_fig.png',
                              ifelse(input$pca_fig_res_filetype == '.jpg','res_fig.jpg',
                                     ifelse(input$pca_fig_res_filetype == '.tiff','res_fig.tiff','res_fig.eps'))))
    
    if (input$pca_fig_res_filetype == '.pdf') {
      ggsave(p_pca_2, 
             filename = paste('./results/', filename, sep = ''),
             width = input$pca_fig_wdith,
             height = input$pca_fig_height,
             device = cairo_pdf)
    }else{
      ggsave(p_pca_2, 
             filename = paste('./results/', filename, sep = ''),
             width = input$pca_fig_wdith,
             height = input$pca_fig_height)
    }
  }
  p_pca_2
})


# 下载散点图
output$download_figure__pca <- downloadHandler(
  filename <- function(){
    paste(stringr::str_sub(input$data_input_pca$name,
                           1,
                           (nchar(input$data_input_pca$name) - 4)),
          '_散点图结果',input$pca_fig_res_filetype,sep = ''
    )
  },
  content <- function(file){
    if (input$pca_fig_res_filetype == '.pdf') {
      file.copy('./results/res_fig.pdf',file)
    }else if (input$pca_fig_res_filetype == '.png') {
      file.copy('./results/res_fig.png',file)
    }else if (input$pca_fig_res_filetype == '.jpg') {
      file.copy('./results/res_fig.jpg',file)
    }else if (input$pca_fig_res_filetype == '.tiff') {
      file.copy('./results/res_fig.tiff',file)
    }else{
      file.copy('./results/res_fig.eps',file)
    }
  }
)


# 下载散点图
output$taboutput_pca_download <- downloadHandler(
  filename <- function(){
    paste(stringr::str_sub(input$data_input_pca$name,
                           1,
                           (nchar(input$data_input_pca$name) - 4)),
          '_主成分解释度',input$pca_fig_res_filetype,sep = ''
    )
  },
  content <- function(file){
    if (input$pca_fig_res_filetype == '.pdf') {
      file.copy('./results/res_fig_1.pdf',file)
    }else if (input$pca_fig_res_filetype == '.png') {
      file.copy('./results/res_fig_1.png',file)
    }else if (input$pca_fig_res_filetype == '.jpg') {
      file.copy('./results/res_fig_1.jpg',file)
    }else if (input$pca_fig_res_filetype == '.tiff') {
      file.copy('./results/res_fig_1.tiff',file)
    }else{
      file.copy('./results/res_fig_1.eps',file)
    }
  }
)





