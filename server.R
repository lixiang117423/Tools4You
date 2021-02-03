source('./main/global.R')

shinyServer(function(input, output, session){
  
  # 异常值检测
  # 调试成功
  source('./main/outliers_detect_server.R',local = TRUE,encoding = 'UTF-8')
  
  # 分组求均值
  # 调试成功
  source('./main/mean_etal_by_group_server.R',local = TRUE,encoding = 'UTF-8')
  
  # t检验
  # 调试成功
  source('./main/ttest_server.R', local = TRUE, encoding = 'UTF-8')
  
  # pca分析
  # 调试成功
  source('./main/pca_server.R', local = TRUE, encoding = 'UTF-8')
  
  # pcoa
  # 调试成功
  source('./main/pcoa_server.R', local = TRUE, encoding = 'UTF-8')
  
  # nmds
  # 暂未调试
  source('./main/nmds_server.R', local = TRUE, encoding = 'UTF-8')
  
  # plsda
  # 调试成功
  #source('./main/plsda_server.R', local = TRUE, encoding = 'UTF-8')
  
  # oplsda
  # 调试成功
  source('./main/oplsda_server.R', local = TRUE, encoding = 'UTF-8')
  
  # anova
  # 调试成功
  source('./main/anova_server.R', local = TRUE, encoding = 'UTF-8')
  
  # regression
  # 暂未调试
  source('./main/regression_server.R', local = TRUE, encoding = 'UTF-8')
  
  # cor_test
  # 暂未调试
  source('./main/cor_test_server.R', local = TRUE, encoding = 'UTF-8')
  
  # random_forest
  # 暂未调试
  source('./main/random_forest_server.R', local = TRUE, encoding = 'UTF-8')
  
  # wilcox
  # 暂未调试
  source('./main/wilcox_server.R', local = TRUE, encoding = 'UTF-8')
  
  # kruskal
  # 暂未调试
  #source('./main/kruskal_server.R', local = TRUE, encoding = 'UTF-8')
  
  # permanova
  # 暂未调试
  source('./main/permanova_server.R', local = TRUE, encoding = 'UTF-8')
  
  # pheatmap
  # 暂未调试
  #source('./main/pheatmap_server.R', local = TRUE, encoding = 'UTF-8')
  
  # point plot
  # 暂未调试
  #source('./main/point_plot_server.R', local = TRUE, encoding = 'UTF-8')
  
  # line plot
  # 暂未调试
  #source('./main/line_plot_server.R', local = TRUE, encoding = 'UTF-8')
  
  # regression plot
  # 暂未调试
  #source('./main/regression_plot_server.R', local = TRUE, encoding = 'UTF-8')
  
  # box plot 
  # 暂未调试
  #source('./main/box_plot_server.R', local = TRUE, encoding = 'UTF-8')
  
  # volvanic plt
  # 暂未调试
  #source('./main/volcanic_plot_server.R', local = TRUE, encoding = 'UTF-8')
  
  # cenn plot
  # 暂未调试
  #source('./main/venn_plot_server.R', local = TRUE, encoding = 'UTF-8')
  
  # slope_plot
  # 暂未调试
  #source('./main/slope_plot_server.R', local = TRUE, encoding = 'UTF-8')
  
  # wrodplot
  # 暂未调试
  #source('./main/word_plot_server.R', local = TRUE, encoding = 'UTF-8')
  
  # sacnkey plot
  # 暂未调试
  #source('./main/sankey_plot_server.R', local = TRUE, encoding = 'UTF-8')
  
  # violin plot
  # 暂未调试
  # source('./main/violin_plot_server.R', local = TRUE, encoding = 'UTF-8')
  
  # circie_stack_plot
  # 暂未调试
  #source('./main/circle_stack_plot_server.R', local = TRUE, encoding = 'UTF-8')
  
  # cor plot
  # 暂未调试
  #source('./main/cor_plot_server.R', local = TRUE, encoding = 'UTF-8')
  
  # lollipop plot
  # 暂未调试
  # source('./main/lollipop_plot_server.R', local = TRUE, encoding = 'UTF-8')
  
  # simple bar
  # 暂未调试
  #source('./main/simple_bar_server.R', local = TRUE, encoding = 'UTF-8')
  
  # stacked bar
  # 暂未调试
  #source('./main/stacked_bar_server.R', local = TRUE, encoding = 'UTF-8')
  
  # percent bar
  # 暂未调试
  #source('./main/percent_bar_server.R', local = TRUE, encoding = 'UTF-8')
})