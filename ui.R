#######################################
#################  ui  ################
#######################################
source('./main/stat4xiang.r')$value
source('./main/mean_sd_se_num.R')$value
source('./main/global.R')$value

style <- "color: #fff; background-color: 	#B22222; border-color: #2e6da4"

#########################################定义选卡###############################
if (TRUE) {
  sidebar = dashboardSidebar(width = 230,
                             sidebarMenu(
                               id = 'tabs',
                               color = 'olive',
                               menuItem('Welcome',
                                        tabName = 'welcome',
                                        icon = icon('bullhorn')),
                               menuItem('常用小工具',
                                        tabName = 'useful_tools',
                                        icon = icon('list'),
                                        menuSubItem('异常值/正态性/方差齐性',
                                                    tabName = 'outliers_detect',
                                                    icon = icon('search')),
                                        menuSubItem('分组求和/均值/标准差(误)',
                                                    tabName = 'mean_etal_by_group',
                                                    icon = icon('ruler-vertical'))),
                               menuItem('统计&分析',
                                        icon = icon('list'),
                                        menuSubItem('t检验',
                                                    tabName = 'ttest', 
                                                    icon = icon('paper-plane')),
                                        menuSubItem('Anova',
                                                    tabName = 'anova',
                                                    icon = icon('paper-plane')),
                                        menuSubItem('PCA',
                                                    tabName = 'pca',
                                                    icon = icon('paper-plane')),
                                        
                                        menuSubItem('PCoA',
                                                    tabName = 'pcoa',
                                                    icon = icon('paper-plane')),
                                        #menuSubItem('NMDS',
                                        #tabName = 'nmds',
                                        #icon = icon('paper-plane')),
                                        menuSubItem('PLS-DA',
                                                    tabName = 'plsda',
                                                    icon = icon('paper-plane')),
                                        menuSubItem('OPLS-DA',
                                                    tabName = 'oplsda',
                                                    icon = icon('paper-plane')),
                                       
                                        menuSubItem('相关分析',
                                                    tabName = 'cor_test',
                                                    icon = icon('paper-plane')),
                                        menuSubItem('回归分析',
                                                    tabName = 'regression',
                                                    icon = icon('paper-plane')),
                                        menuSubItem('随机森林',
                                                    tabName = 'random_forest',
                                                    icon = icon('tree')),
                                        menuSubItem('Wilcox检验',
                                                    tabName = 'wilcox',
                                                    icon = icon('paper-plane')),
                                        menuSubItem('PERMANOVA',
                                                    tabName = 'permanova',
                                                    icon = icon('paper-plane'))
                                        ),
                               menuItem('数据可视化',icon = icon('list')
                                        
                               )))
  
}




body <- dashboardBody(
  tabItems(
    # 调试成功
    tabItem(tabName = 'welcome',source('./main/welcome_ui.R',local = TRUE,encoding = 'UTF-8')$value),
    # 调试成功
    tabItem(tabName = 'mean_etal_by_group',source('./main/mean_etal_by_group_ui.R',local = TRUE,encoding = 'UTF-8')$value),
    # 调试成功
    tabItem(tabName = 'outliers_detect',source('./main/outliers_detect_ui.R',local = TRUE,encoding = 'UTF-8')$value),
    # 调试成功
    tabItem(tabName = 'ttest',source('./main/ttest_ui.R',local = TRUE,encoding = 'UTF-8')$value),
    # 暂未调试
    tabItem(tabName = 'pca',source('./main/pca_ui.R',local = TRUE,encoding = 'UTF-8')$value),
    # 暂未调试
     tabItem(tabName = 'pcoa',source('./main/pcoa_ui.R',local = TRUE,encoding = 'UTF-8')$value),
    # 暂未调试
    tabItem(tabName = 'nmds',source('./main/nmds_ui.R',local = TRUE,encoding = 'UTF-8')$value),
    # 暂未调试
    #tabItem(tabName = 'plsda',source('./main/plsda_ui.R',local = TRUE,encoding = 'UTF-8')$value),
    # 暂未调试
    tabItem(tabName = 'oplsda',source('./main/oplsda_ui.R',local = TRUE,encoding = 'UTF-8')$value),
    # 暂未调试
    tabItem(tabName = 'anova',source('./main/anova_ui.R',local = TRUE,encoding = 'UTF-8')$value),
    # 暂未调试
    tabItem(tabName = 'regression',source('./main/regression_ui.R',local = TRUE,encoding = 'UTF-8')$value),
    # 暂未调试
    tabItem(tabName = 'cor_test',source('./main/cor_test_ui.R',local = TRUE,encoding = 'UTF-8')$value),
    # 暂未调试
    tabItem(tabName = 'random_forest',source('./main/random_forest_ui.R',local = TRUE,encoding = 'UTF-8')$value),
    # 暂未调试
    tabItem(tabName = 'wilcox',source('./main/wilcox_ui.R', local = TRUE,encoding = 'UTF-8')$value),
    # 暂未调试
    # tabItem(tabName = 'kruskal',source('./main/kruskal_ui.R',local = TRUE,encoding = 'UTF-8')$value),
    # 暂未调试
    tabItem(tabName = 'permanova',source('./main/permanova_ui.R',local = TRUE,encoding = 'UTF-8')$value)
    # 暂未调试
    #tabItem(tabName = 'pheatmap',source('./main/pheatmap_ui.R',local = TRUE,encoding = 'UTF-8')$value),
    # 暂未调试
    #tabItem(tabName = 'point_plot',source('./main/point_plot_ui.R',local = TRUE,encoding = 'UTF-8')$value),
    # 暂未调试
    #tabItem(tabName = 'line_plot',source('./main/line_plot_ui.R',local = TRUE,encoding = 'UTF-8')$value),
    # 暂未调试
    #tabItem(tabName = 'regression_plot',source('./main/regression_plot_ui.R',local = TRUE,encoding = 'UTF-8')$value),
    # 暂未调试
    #tabItem(tabName = 'box_plot',source('./main/box_plot_ui.R',local = TRUE,encoding = 'UTF-8')$value),
    # 暂未调试
    #tabItem(tabName = 'volvanic_plot',source('./main/volcanic_plot_ui.R',local = TRUE,encoding = 'UTF-8')$value),
    # 暂未调试
    #tabItem(tabName = 'venn_plot',source('./main/venn_plot_ui.R',local = TRUE,encoding = 'UTF-8')$value),
    # 暂未调试
    #tabItem(tabName = 'slope_plot',source('./main/slope_plot_ui.R',local = TRUE,encoding = 'UTF-8')$value),
    # 暂未调试
    #tabItem(tabName = 'word_plot',source('./main/word_plot_ui.R',local = TRUE,encoding = 'UTF-8')$value),
    # 暂未调试
    #tabItem(tabName = 'sankey_plot',source('./main/sankey_plot_ui.R',local = TRUE,encoding = 'UTF-8')$value),
    # 暂未调试
    #tabItem(tabName = 'violin_plot',source('./main/violin_plot_ui.R',local = TRUE,encoding = 'UTF-8')$value),
    # 暂未调试
    #tabItem(tabName = 'circle_stack_plot',source('./main/circle_stack_plot_ui.R',local = TRUE,encoding = 'UTF-8')$value),
    # 暂未调试
    #tabItem(tabName = 'cor_plot',source('./main/cor_plot_ui.R',local = TRUE,encoding = 'UTF-8')$value),
    # 暂未调试
    #tabItem(tabName = 'lollipop_plot',source('./main/lollipop_plot_ui.R',local = TRUE,encoding = 'UTF-8')$value),
    # 暂未调试
    #tabItem(tabName = 'simple_bar',source('./main/simple_bar_ui.R',local = TRUE,encoding = 'UTF-8')$value),
    # 暂未调试
    #tabItem(tabName = 'stacked_bar',source('./main/stacked_bar_ui.R',local = TRUE,encoding = 'UTF-8')$value),
    # 暂未调试
    #tabItem(tabName = 'percent_bar',source('./main/percent_bar_ui.R',local = TRUE,encoding = 'UTF-8')$value)
    
    ))

# 组合
ui <- dashboardPage(
  skin = 'green',
  dashboardHeader(title = "Tools4You"),
  sidebar = sidebar,
  body
)













