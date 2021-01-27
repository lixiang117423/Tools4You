source('./global.R')
################################################################################
######################################server####################################
################################################################################
server <- function(input, output, session){
  
    
    
  
  # t-test服务端设置开始--------------------------------------------------------
  
  
  # t-test服务端设置结束--------------------------------------------------------
  
  
  # wilcox服务端设置开始--------------------------------------------------------
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
    table_in_test <- reshape2::melt(table_in_test, id.vars = 1)
    table_in_test <- table_in_test[,c(1,3)]
    colnames(table_in_test) <- c('group','value')
    table_in_test <<- table_in_test
  })
  
  # 下载示例数据
  # 运行正常20210111
  output$download_demo_data_wilcox <- downloadHandler(
    filename = 'wilcox检验示例数据.xlsx',
    content = function(file){
      file.copy('./demo_data/wilcox检验示例数据.xlsx',file)
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
                                                    pageLength = 12
                                                  ))
  
  df_table_wilcox <- eventReactive(input$submit_wilcox,{
    if (input$submit_wilcox > 0) {
      
      wilcox <- t.test(value ~ group,
                  data = user_data_wilcox(),# 必须要加上()才行
                  conf.level = as.numeric(input$select_confidence_interval_wilcox),
                  alternative = input$select_oneside_or_twoside_wilcox,
                  paired = ifelse(input$select_paired_or_not_wilcox == 'FALSE',FALSE,TRUE),
                  var.equal = ifelse(input$var_equal_wilcox == 'FALSE',FALSE, TRUE)
      )
      
      df2 <- data.frame(pvalue = round(wilcox$p.value,4),
                        group = unique(user_data_wilcox()$group))
      df2$significance <- ifelse(df2$pvalue < 0.001,'***',
                                 ifelse(df2$pvalue > 0.001 & df2$pvalue < 0.01,'**',
                                        ifelse(df2$pvalue > 0.05,'NS','*')))
      
      df2 <- merge(mean_etal_wilcox(), df2, by = 'group')
      df2 <- df2[,-2]
      df2 <- df2[!duplicated(df2),]
      
      # 保存分析结果
      write.csv(df2, file = './results/rest_tab.csv',row.names = FALSE)
      write.table(df2, file = './results/rest_tab.txt',row.names = FALSE)
      xlsx::write.xlsx(df2, file = './results/rest_tab.xlsx',row.names = FALSE)
      # 保存分析结果结束
    }
    df2 # 返回要展示的结果
  })
  
  # 进行绘图
  output$plot_wilcox <- renderPlot(plot_res_wilcox(),
                                   height = 330,
                                   width = 600)
  plot_res_wilcox <- eventReactive(input$submit_wilcox,{
    if (input$submit_wilcox > 0) {
      p_wilcox <- ggplot(mean_etal_wilcox(), aes(group, mean/number, fill = group)) +
        geom_bar(stat = 'identity',width = 0.6) +
        geom_hline(yintercept = max(mean_etal_wilcox()$mean)*1.1, color = 'white') +
        scale_y_continuous(expand = c(0,0)) +
        scale_fill_aaas() +
        theme_bw() +
        theme(legend.position = 'none')
      
      # 保存图片
      filename <- ifelse(input$wilcox_fig_res_filetype == '.pdf','res_fig.pdf',
                         ifelse(input$wilcox_fig_res_filetype == '.png','res_fig.png',
                                ifelse(input$wilcox_fig_res_filetype == '.jpg','res_fig.jpg',
                                       ifelse(input$wilcox_fig_res_filetype == '.tiff','res_fig.tiff','res_fig.eps'))))
      ggsave(p_wilcox, 
             filename = paste('./results/', filename, sep = ''),
             width = input$wilcox_fig_wdith,
             height = input$wilcox_fig_height)
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
            '_统计分析结果',input$wilcox_stat_res_filetype,sep = ''
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
            '_绘图展示结果',input$wilcox_fig_res_filetype,sep = ''
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
  
  # wilcox服务端设置结束--------------------------------------------------------
  
  
  # anova服务端设置开始---------------------------------------------------------
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
  # 运行正常20210111
  user_data_anova <- reactive({
    table_in_test <- read.csv(input$data_input_anova$datapath,
                              header = T, 
                              stringsAsFactors = TRUE,
                              encoding = 'UTF-8')
    colnames(table_in_test)[1] <- 'group'
    table_in_test <- reshape2::melt(table_in_test, id.vars = 1)
    table_in_test <- table_in_test[,c(1,3)]
    colnames(table_in_test) <- c('group','value')
    table_in_test <<- table_in_test
  })
  
  # 下载示例数据
  # 运行正常20210111
  output$download_demo_data_anova <- downloadHandler(
    filename = 'anova分析示例数据.xlsx',
    content = function(file){
      file.copy('./demo_data/anova分析示例数据.xlsx',file)
    }
  )
  
  # 对数据求均值等
  # 运行正常20210111
  mean_etal_anova <- reactive({
    mean_anova <- mean_sd_se_num(user_data_anova())
    mean_anova <<- mean_anova
  })
  
  # 展示统计分析结果
  output$taboutput_anova_view <- renderDataTable(df_table_anova(),
                                                 options = list(
                                                   pageLength = 12
                                                 ))
  
  df_table_anova <- eventReactive(input$submit_anova,{
    
    if (input$submit_anova > 0) {
      if (input$num_factor_anova > 1 & input$factor_act_or_not_anova == TRUE) {
        formula <- paste('value ~ ',input$factor_name_anova,sep = '')
      }else {
        formula <- 'value ~ group'
      }
      
      #aov <- aov(formula,data = user_data_anova())
      aov <- aov(value ~ group, data = user_data_anova())
      
      pvalue <- summary(aov)[[1]][["Pr(>F)"]][1]
      
      if (pvalue < 0.05) {
        tuk <-  glht(aov, linfct = mcp(group = 'Tukey'))
        sig <- cld(tuk, level = as.numeric(input$level_mult_test_anova), ddecreasing = TRUE)[["mcletters"]][["Letters"]] %>%
          as.data.frame()
        colnames(sig) <- 'signif'
        sig$group <- rownames(sig)
        sig$pvalue <- pvalue
        
        df3 <- merge(mean_etal_anova(), sig, by = 'group')
      }
 
      df3 <- df3
      df3 <- df3[,-2]
      df3 <- df3[!duplicated(df3),]
      
      # 保存分析结果
      write.csv(df3, file = './results/rest_tab.csv',row.names = FALSE)
      write.table(df3, file = './results/rest_tab.txt',row.names = FALSE)
      xlsx::write.xlsx(df3, file = './results/rest_tab.xlsx',row.names = FALSE)
      # 保存分析结果结束
    }
    df3 # 返回要展示的结果
  })
  
  # 进行绘图
  output$plot_anova <- renderPlot(plot_res_anova(),
                                  height = 330,
                                  width = 600)
  plot_res_anova <- eventReactive(input$submit_anova,{
    if (input$submit_anova > 0) {
      p_anova <- ggplot(mean_etal_anova(), aes(group, mean/number, fill = group)) +
        geom_bar(stat = 'identity',width = 0.6) +
        geom_hline(yintercept = max(mean_etal_anova()$mean)*1.1, color = 'white') +
        scale_y_continuous(expand = c(0,0)) +
        scale_fill_aaas() +
        theme_bw() +
        theme(legend.position = 'none')
      
      # 保存图片
      filename <- ifelse(input$anova_fig_res_filetype == '.pdf','res_fig.pdf',
                         ifelse(input$anova_fig_res_filetype == '.png','res_fig.png',
                                ifelse(input$anova_fig_res_filetype == '.jpg','res_fig.jpg',
                                       ifelse(input$anova_fig_res_filetype == '.tiff','res_fig.tiff','res_fig.eps'))))
      ggsave(p_anova, 
             filename = paste('./results/', filename, sep = ''),
             width = input$anova_fig_wdith,
             height = input$anova_fig_height)
    }
    p_anova# 返回图
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
            '_绘图展示结果',input$anova_fig_res_filetype,sep = ''
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
      }else{
        file.copy('./results/res_fig.eps',file)
      }
    }
  )
  
  # anova服务端设置结束---------------------------------------------------------
  
  
  # kruskal服务端设置开始-------------------------------------------------------
  
  
  # kruskal服务端设置结束-------------------------------------------------------
  
  
  # regerssion服务端设置开始----------------------------------------------------
  
  
  # regression服务端设置结束----------------------------------------------------
  
  
  # cor_test服务端设置开始------------------------------------------------------
  
  
  # cortest服务端设置结束-------------------------------------------------------
  
  
  # pca服务端设置开始-----------------------------------------------------------
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
  # 运行正常20210111
  user_data_pca <- reactive({
    table_in_test <- read.csv(input$data_input_pca$datapath,
                              header = T, 
                              stringsAsFactors = TRUE,
                              encoding = 'UTF-8')
    colnames(table_in_test)[ncol(table_in_test)] <- 'group'
    table_in_test <<- table_in_test
  })
  
  # 下载示例数据
  # 运行正常20210111
  output$download_demo_data_pca <- downloadHandler(
    filename = 'PCA示例数据.xlsx',
    content = function(file){
      file.copy('./demo_data/PCA示例数据.xlsx',file)
    }
  )
  
  # 统计分析
  pca_res <- reactive({
    prcomp(user_data_pca()[,1:(as.numeric(ncol(user_data_pca())-1))])
  })
  
  # 展示统计分析结果
  output$taboutput_pca_view <- renderDataTable(df_table_pca(),
                                               options = list(
                                                 pageLength = 10
                                               ))
  
  df_table_pca <- eventReactive(input$submit_pca,{
    
    if (input$submit_pca > 0) {
      if (as.numeric(ncol(user_data_pca())) > 5) {
        pca_df <- as.data.frame(pca_res()[["x"]])[,1:4]
      }else{
        pca_df <- as.data.frame(pca_res()[["x"]])
      }
      pca_df$group <- user_data_pca()$group
      df4 <- pca_df
      
      # 保存分析结果
      write.csv(df4, file = './results/rest_tab.csv',row.names = FALSE)
      write.table(df4, file = './results/rest_tab.txt',row.names = FALSE)
      xlsx::write.xlsx(df4, file = './results/rest_tab.xlsx',row.names = FALSE)
      # 保存分析结果结束
    }
    df4 # 返回要展示的结果
  })
  
  # 进行绘图
  output$plot_pca <- renderPlot(plot_res_pca())
  plot_res_pca <- eventReactive(input$submit_pca,{
    if (input$submit_pca > 0) {
      
      pca_var <- pca_res()$sdev^2 # 计算原始数据中的每个数据在每个 PC 上的比重
      
      pca_var <- round(pca_var/sum(pca_var)*100,2) 
      
      p_pca <- ggplot(df_table_pca(), aes(PC1, PC2, color = group)) +
        geom_vline(xintercept = 0, linetype = 'dashed') +
        geom_hline(yintercept = 0, linetype = 'dashed') +
        geom_point(size =1.5) +
        labs(x = paste('PC1(',pca_var[1],'%)',sep = ''),
             y = paste('PC2(',pca_var[2],'%)',sep = '')) +
        scale_fill_aaas() +
        theme_bw() +
        theme(legend.title = element_blank())
      
      # 保存图片
      filename <- ifelse(input$pca_fig_res_filetype == '.pdf','res_fig.pdf',
                         ifelse(input$pca_fig_res_filetype == '.png','res_fig.png',
                                ifelse(input$pca_fig_res_filetype == '.jpg','res_fig.jpg',
                                       ifelse(input$pca_fig_res_filetype == '.tiff','res_fig.tiff','res_fig.eps'))))
      ggsave(p_pca, 
             filename = paste('./results/', filename, sep = ''),
             width = input$pca_fig_wdith,
             height = input$pca_fig_height)
    }
    p_pca# 返回图
  })
  
  # 下载分析结果
  # 运行正常20210111
  output$taboutput_pca_download <- downloadHandler(
    filename <- function(){
      paste(stringr::str_sub(input$data_input_pca$name,
                             1,
                             (nchar(input$data_input_pca$name) - 4)),
            '_统计分析结果',input$pca_stat_res_filetype,sep = ''
      )
    },
    content <- function(file){
      if (input$pca_stat_res_filetype == '.csv') {
        file.copy('./results/rest_tab.csv',file)
      }else if (input$pca_stat_res_filetype == '.txt') {
        file.copy('./results/rest_tab.txt',file)
      }else{
        #return(NULL)
        file.copy('./results/rest_tab.xlsx',file)
      }
    }
  )
  
  # 下载图片
  output$download_figure__pca <- downloadHandler(
    filename <- function(){
      paste(stringr::str_sub(input$data_input_pca$name,
                             1,
                             (nchar(input$data_input_pca$name) - 4)),
            '_绘图展示结果',input$pca_fig_res_filetype,sep = ''
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
  
  # pca服务端设置结束---------------------------------------------------------

    # pcoa服务端设置开始----------------------------------------------------------
  
  
  # apcoa服务端设置结束---------------------------------------------------------
  
  
  # rda服务端设置开始-----------------------------------------------------------
  
  
  # rda服务端设置结束-----------------------------------------------------------
  
  
  # nmds服务端设置开始----------------------------------------------------------
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
  # 运行正常20210111
  user_data_nmds <- reactive({
    table_in_test <- read.csv(input$data_input_nmds$datapath,
                              header = T, 
                              stringsAsFactors = TRUE,
                              encoding = 'UTF-8')
    colnames(table_in_test)[ncol(table_in_test)] <- 'group'
    table_in_test <<- table_in_test
  })
  
  # 下载示例数据
  # 运行正常20210111
  output$download_demo_data_nmds <- downloadHandler(
    filename = 'nmds示例数据.xlsx',
    content = function(file){
      file.copy('./demo_data/nmds示例数据.xlsx',file)
    }
  )
  
  # 统计分析
  nmds_res <- reactive({
    prcomp(user_data_nmds()[,1:(as.numeric(ncol(user_data_nmds())-1))])
  })
  
  # 展示统计分析结果
  output$taboutput_nmds_view <- renderDataTable(df_table_nmds(),
                                                options = list(
                                                  pageLength = 10
                                                ))
  
  df_table_nmds <- eventReactive(input$submit_nmds,{
    
    if (input$submit_nmds > 0) {
      if (as.numeric(user_data_nmds()) > 5) {
        nmds_df <- as.data.frame(nmds_res()[["x"]])[,1:4]
      }
      nmds_df$group <- user_data_nmds()$group
      df4 <- nmds_df
      
      # 保存分析结果
      write.csv(df4, file = './results/rest_tab.csv',row.names = FALSE)
      write.table(df4, file = './results/rest_tab.txt',row.names = FALSE)
      xlsx::write.xlsx(df4, file = './results/rest_tab.xlsx',row.names = FALSE)
      # 保存分析结果结束
    }
    df4 # 返回要展示的结果
  })
  
  # 进行绘图
  output$plot_nmds <- renderPlot(plot_res_nmds())
  plot_res_nmds <- eventReactive(input$submit_nmds,{
    if (input$submit_nmds > 0) {
      
      nmds_var <- nmds_res()$sdev^2 # 计算原始数据中的每个数据在每个 PC 上的比重
      
      nmds_var <- round(nmds_var/sum(nmds_var)*100,2) 
      
      p_nmds <- ggplot(df_table_nmds(), aes(PC1, PC2, color = group)) +
        geom_vline(xintercept = 0, linetype = 'dashed') +
        geom_hline(yintercept = 0, linetype = 'dashed') +
        geom_point(size =1.5) +
        labs(x = paste('PC1(',nmds_var[1],'%)',sep = ''),
             y = paste('PC2(',nmds_var[2],'%)',sep = '')) +
        scale_fill_aaas() +
        theme_bw() +
        theme(legend.title = element_blank())
      
      # 保存图片
      filename <- ifelse(input$nmds_fig_res_filetype == '.pdf','res_fig.pdf',
                         ifelse(input$nmds_fig_res_filetype == '.png','res_fig.png',
                                ifelse(input$nmds_fig_res_filetype == '.jpg','res_fig.jpg',
                                       ifelse(input$nmds_fig_res_filetype == '.tiff','res_fig.tiff','res_fig.eps'))))
      ggsave(p_nmds, 
             filename = paste('./results/', filename, sep = ''),
             width = input$nmds_fig_wdith,
             height = input$nmds_fig_height)
    }
    p_nmds# 返回图
  })
  
  # 下载分析结果
  # 运行正常20210111
  output$taboutput_nmds_download <- downloadHandler(
    filename <- function(){
      paste(stringr::str_sub(input$data_input_nmds$name,
                             1,
                             (nchar(input$data_input_nmds$name) - 4)),
            '_统计分析结果',input$nmds_stat_res_filetype,sep = ''
      )
    },
    content <- function(file){
      if (input$nmds_stat_res_filetype == '.csv') {
        file.copy('./results/rest_tab.csv',file)
      }else if (input$nmds_stat_res_filetype == '.txt') {
        file.copy('./results/rest_tab.txt',file)
      }else{
        #return(NULL)
        file.copy('./results/rest_tab.xlsx',file)
      }
    }
  )
  
  # 下载图片
  output$download_figure__nmds <- downloadHandler(
    filename <- function(){
      paste(stringr::str_sub(input$data_input_nmds$name,
                             1,
                             (nchar(input$data_input_nmds$name) - 4)),
            '_绘图展示结果',input$nmds_fig_res_filetype,sep = ''
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
  
  # nmds服务端设置结束----------------------------------------------------------
  
  
  # oplsda服务端设置开始--------------------------------------------------------
  
  
  # oplsda服务端设置结束--------------------------------------------------------
  
  
  # permanova服务端设置开始-----------------------------------------------------
  
  
  # permanova服务端设置结束-----------------------------------------------------
  
  
  
  
  
  
} # server端大括号
##################################### server端设置结束 #########################
