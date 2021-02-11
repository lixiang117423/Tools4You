# 提交成功显示
observeEvent(input$submit_anova, { 
  if (input$submit_anova>0) {
    sendSweetAlert(
      session = session,
      title = "提交成功!",
      text = "数据上传成功，参数设置正确",
      type = "success")
  }
})

# 导入数据

user_data_anova <- reactive({
  table_in_test <- read.csv(input$data_input_anova$datapath,
                            header = T, 
                            stringsAsFactors = TRUE,
                            encoding = 'UTF-8')
  colnames(table_in_test)[1] <- 'group'
  table_in_test <- reshape2::melt(table_in_test, id.vars = 1)  %>%
    dplyr::select(c(1,3)) %>% 
    na.omit()
  colnames(table_in_test) <- c('group','value')
  table_in_test <<- table_in_test
})

# 下载示例数据
output$download_demo_data_anova <- downloadHandler(
  filename = 'anova示例数据.csv',
  content = function(file){
    file.copy('./demo_data/anova示例数据.csv',file)
  }
)

# 对数据求均值等
mean_etal_anova <- reactive({
  mean_anova <- mean_sd_se_num(user_data_anova())
  mean_anova <<- mean_anova
})

# 展示统计分析结果
output$taboutput_anova_view <- renderDataTable(df_table_anova(),
                                               options = list(pageLength = 6
                                               ))

df_table_anova <- eventReactive(input$submit_anova,{
  if (input$submit_anova > 0) {
    
    user_records <- data.table::fread('./www/user records.txt', encoding = 'UTF-8')
    temp <- as.character(Sys.time())
    temp <- stringr::str_split(temp, ' ')
    date <- temp[[1]][1]
    time <- temp[[1]][2]
    user_record <- data.frame(Cat = 'anova',
                              Date = date,
                              Time = time)
    user_records <- rbind(user_records, user_record)
    data.table::fwrite(user_records,file = './www/user records.txt')
    
    fit <- aov(value ~ group, data = user_data_anova())
    pvalue <- summary(fit)[[1]][["Pr(>F)"]][1]
    
    # 均值等去重
    df0 <- mean_etal_anova()
    df0 <- df0[,-2]
    df0 <- df0[!duplicated(df0),]
    
    # 判断是否进行多重校验
    if (pvalue < 0.05) {
      tuk <- glht(fit, linfct = mcp(group = 'Tukey'))
      sig <- cld(tuk, 
                 #level = as.numeric(input$level_mult_test_anova), 
                 level = 0.99,
                 ddecreasing = TRUE)[["mcletters"]][["Letters"]] %>%
        as.data.frame()
      colnames(sig) <- 'significance'
      sig$group <- rownames(sig)
      sig$pvalue <- pvalue
      
      df <- merge(df0, sig, by = 'group')
    }else{
      df <- df0
      df$pvalue <- pvalue
      df$significance <- 'NS'
    }
    
    # 保存分析结果
    write.csv(df, file = './results/rest_tab.csv',row.names = FALSE)
    write.table(df, file = './results/rest_tab.txt',row.names = FALSE)
    xlsx::write.xlsx(df, file = './results/rest_tab.xlsx',row.names = FALSE)
    # 保存分析结果结束
  }
  df # 返回要展示的结果
})

# 进行绘图
output$plot_anova <- renderPlot(plot_res_anova(),
                                height = 300,
                                width = 600)
plot_res_anova <- eventReactive(input$submit_anova,{
  if (input$submit_anova > 0) {
    df_plot <- df_table_anova()

    
    # 构建绘图数据
    fit <- aov(value ~ group, data = user_data_anova())
    pvalue <- summary(fit)[[1]][["Pr(>F)"]][1]
    
    # 均值等去重
    df0 <- mean_etal_anova()
    
    # 判断是否进行多重校验
    if (pvalue < 0.05) {
      tuk <- glht(fit, linfct = mcp(group = 'Tukey'))
      sig <- cld(tuk, 
                 #level = as.numeric(input$level_mult_test_anova), 
                 level = 0.99,
                 ddecreasing = TRUE)[["mcletters"]][["Letters"]] %>%
        as.data.frame()
      colnames(sig) <- 'significance'
      sig$group <- rownames(sig)
      sig$pvalue <- pvalue
      
      df <- merge(df0, sig, by = 'group')
    }else{
      df <- df0
      df$pvalue <- pvalue
      df$significance <- 'NS'
    }
    
    # 柱子顺序
    order <- c()
    axis_order <- stringr::str_split(input$axis_order,',')
    for (i in 1:length(axis_order[[1]])) {
      order <- c(order, axis_order[[1]][i])
    }
    
    # 开始绘图
    if (input$point_or_not_anova == 'TRUE') {
      df_plot <- df
      p_anova <- ggplot(df_plot) +
        geom_bar(aes(group, mean/number,fill = group),
                 stat = 'identity',width = input$width_anova_bar)
      
      if (input$anova_err_bar == 'SD') {
        p_anova <- p_anova +
          geom_errorbar(aes(group,
                            ymin = mean - SD, 
                            ymax = mean + SD),
                        width = 0.2) +
          geom_text(aes(group,(mean + SD)*1.15,
                        label = significance)) +
          geom_jitter(aes(group, value),width = 0.05)
      }else{
        p_anova <- p_anova +
          geom_errorbar(aes(group,
                            ymin = mean - SE, 
                            ymax = mean + SE),
                        width = 0.2) +
          geom_text(aes(group,(mean + SE)*1.15,
                        label = significance)) +
          geom_jitter(aes(group, value),width = 0.05)
      }

      if (input$axis_order == '') {
        p_anova <- p_anova
      }else{
        p_anova <- p_anova +
          scale_x_discrete(limit = order)
      }

      
    }else{
      df_plot <- df[,-2]
      df_plot <- df_plot[!duplicated(df_plot),]
      
      p_anova <- ggplot(df_plot) +
        geom_bar(aes(group, mean,fill = group),
                 stat = 'identity',width = input$width_anova_bar) +
        geom_errorbar(aes(group,
                          ymin = mean - ifelse(input$anova_err_bar == 'SD',SD,SE), 
                          ymax = mean + ifelse(input$anova_err_bar == 'SD',SD,SE)),
                      width = 0.2) +
        geom_text(aes(group,(mean + ifelse(input$anova_err_bar == 'SD',SD,SE))*1.1,
                      label = significance))
    }
    
    p_anova <- p_anova +
      geom_hline(yintercept = (max(df_plot$mean) + 
                                 ifelse(input$anova_err_bar == 'SD',
                                        max(df_plot$SD),max(df_plot$SE)))*1.3, 
                 color = 'white') +
      labs(x = ifelse(input$anova_fig_x_axis == '','group',input$anova_fig_x_axis),
           y = ifelse(input$anova_fig_y_axis == '','group',input$anova_fig_y_axis),
           title = input$anova_fig_title) +
      scale_y_continuous(expand = c(0,0)) +
      scale_fill_aaas() +
      theme_prism(base_size = 14) +
      theme(legend.position = 'none',
            panel.background = element_blank(),
            panel.grid = element_blank(),
            axis.text = element_text(color = 'black',size = 10),
            axis.title.x = element_text(color = 'black',size = 10),
            axis.ticks = element_line(color = 'black'))
    
    
    # 用户输入填充色
    if (input$color_bar_anova == '') {
      p_anova <- p_anova
    }else{
      fill_cor <- c()
      
      col_input <- stringr::str_split(input$color_bar_anova,',')
      for (i in 1:length(col_input[[1]])) {
        fill_cor <- c(fill_cor, col_input[[1]][i])
      }
      
      if (length(fill_cor) == 1) {
        fill_cor <- c(rep(fill_cor, nrow(df_plot)))
      }
      
      p_anova <- p_anova +
        scale_fill_manual(values = fill_cor)
    }
    
    p = qplot(iris$Sepal.Length)
    
    # 保存图片
    export::graph2ppt(p, file = './results/res_fig.pptx')
  }
  p_anova # 返回图
})

# 下载分析结果
# 运行正常20210111
output$taboutput_anova_download <- downloadHandler(
  filename <- function(){
    paste(stringr::str_sub(input$data_input_anova$name,
                           1,
                           (nchar(input$data_input_anova$name) - 4)),
          '_统计分析结果',input$anova_stat_res_filetype,sep = ''
    )
  },
  content <- function(file){
    if (input$anova_stat_res_filetype == '.csv') {
      file.copy('./results/rest_tab.csv',file)
    }else if (input$anova_stat_res_filetype == '.txt') {
      file.copy('./results/rest_tab.txt',file)
    }else{
      #return(NULL)
      file.copy('./results/rest_tab.xlsx',file)
    }
  }
)

# 下载图片
output$download_figure__anova <- downloadHandler(
  filename <- function(){
    paste(stringr::str_sub(input$data_input_anova$name,
                           1,
                           (nchar(input$data_input_anova$name) - 4)),
          '_绘图结果',input$anova_fig_res_filetype,sep = ''
    )
  },
  content <- function(file){
    if (input$anova_fig_res_filetype == '.pdf') {
      file.copy('./results/res_fig.pdf',file)
    }else if (input$anova_fig_res_filetype == '.png') {
      file.copy('./results/res_fig.png',file)
    }else if (input$anova_fig_res_filetype == '.jpg') {
      file.copy('./results/res_fig.jpg',file)
    }else if (input$anova_fig_res_filetype == '.tiff') {
      file.copy('./results/res_fig.tiff',file)
    }else if (input$anova_fig_res_filetype == '.pptx') {
      file.copy('./results/res_fig.pptx',file)
    }else{
      file.copy('./results/res_fig.eps',file)
    }
  }
)