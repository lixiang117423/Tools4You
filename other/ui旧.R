
################################################################################
#########################################定义选卡###############################
################################################################################

# welcome
tab_welcome <- fluidRow(
  includeMarkdown('./sources/welcome.md') # 嵌入markdown文件
)





# 定义t-test选项卡结束----------------------------------------------------------


# 定义wilcox选项卡开始----------------------------------------------------------
tab_wilcox <- fluidRow(
  # 使用box后就不能再用column
  # 第一个box
  column(width = 2,
         box(width = NULL,
             height = 870,
             title = '数据上传与参数设置',
             status = "danger", 
             solidHeader = TRUE,
             collapsible = TRUE, 
             background = "navy",
             #"随便打的文本", # 直接插入文本
             #br(), # 换行符
             # 上传数据
             fileInput("data_input_wilcox",
                       label = h4("上传数据"),
                       accept = ".csv",
                       buttonLabel = "浏览..."),
             # 下载示例数据
             downloadLink('download_demo_data_wilcox',
                          label = h4('下载示例数据')),
             
             # 选择置信区间
             selectInput('select_confidence_interval_wilcox',
                         label = h4('置信区间'),
                         choices = list('0.95' = 0.95,
                                        '0.99' = 0.99),
                         selected = 0.99),
             # 选择单尾or双尾检验
             selectInput('select_oneside_or_twoside_wilcox',
                         label = h4('单尾/双尾'),
                         choices = list('双尾' = "two.sided",
                                        '单尾' = "less")),
             # 选择是否配对数据
             selectInput('select_paired_or_not_wilcox',
                         label = h4('配对数据'),
                         choices = list('否' = 'FALSE',
                                        '是' = 'TRUE')),
             # 选择方差齐性
             selectInput('var_equal_wilcox',
                         label = h4('方差相等'),
                         choices = list('否' = 'FALSE',
                                        '是' = 'TRUE')),
             br(),
             br(),
             
             # 点击提交
             actionButton('submit_wilcox',
                          label = h4('点击提交'),
                          width = 230,
                          icon = NULL),
             
             br(),
             br(),
             br(),
             br(),
             
             fluidRow(
               column(width = 2,
                      downloadButton('taboutput_wilcox_download',
                                     label = h4('下载结果'),
                                     width = 230)),
               
               column(width = 2,
                      offset = 4,# 偏移量
                      downloadButton('download_figure__wilcox',
                                     label = h4('下载图片'),
                                     width = 230))
             )
         )),# 第一个box完结
  
  # 第二列
  column(width = 5,
         # 第二列上面的box展示统计分析结果
         box(width = NULL,
             height = 700,
             title = '统计分析结果预览',
             status = "warning", 
             solidHeader = TRUE,
             collapsible = TRUE, 
             background = NULL,
             #"随便打的文本", # 直接插入文本
             br(),
             dataTableOutput('taboutput_wilcox_view')),
         
         # 第二列下面的box下载统计分析参数
         box(width = NULL,
             height = 150,
             title = '统计分析结果下载参数设置',
             status = "info", 
             solidHeader = TRUE,
             collapsible = TRUE, 
             background = NULL,
             #"随便打的文本", # 直接插入文本
             #br(),
             selectInput('wilcox_stat_res_filetype',
                         label = h5('选择数据下载格式'),
                         choices = list('Excel文件' = '.xlsx',
                                        'txt文件' = '.txt',
                                        'csv文件' = '.csv'),
                         selected = '.csv'))
  ),
  
  # 第三列
  column(width = 5,
         # 第一个box用于绘图
         box(width = NULL,
             height = 500,
             title = '绘图结果展示',
             status = "success", 
             solidHeader = TRUE,
             collapsible = TRUE, 
             #background = "navy",
             #"随便打的文本", # 直接插入文本
             br(),
             plotOutput('plot_wilcox')),
         
         # 第二个box用于下载图片
         box(width = NULL,
             height = 350,
             title = '绘图结果下载参数设置',
             status = "primary", 
             solidHeader = TRUE,
             collapsible = TRUE, 
             background = NULL,
             #"随便打的文本", # 直接插入文本
             #br(),
             # 图片格式
             selectInput('wilcox_fig_res_filetype',
                         label = h5('选择图片下载格式'),
                         choices = list('PDF文件' = '.pdf',
                                        'PNG文件' = '.png',
                                        'JPG文件' = '.jpg',
                                        'TIFF文件' = '.tiff',
                                        'EPS文件' = '.eps'),
                         selected = '.pdf'),
             
             # 图片宽度
             sliderInput('wilcox_fig_wdith',
                         label = '图片宽度（单位：cm）',
                         min = 1,max = 20,
                         value = 5,step = 0.5),
             
             # 图片高度
             sliderInput('wilcox_fig_height',
                         label = '图片高度（单位：cm）',
                         min = 1,max = 20,
                         value = 5,step = 0.5))
  )
  
)
# 定义wilcox选项卡结束----------------------------------------------------------


# 定义anova选项卡开始-----------------------------------------------------------
tab_anova <- fluidRow(
  # 使用box后就不能再用column
  # 第一个box
  column(width = 2,
         box(width = NULL,
             height = 870,
             title = '数据上传与参数设置',
             status = "danger", 
             solidHeader = TRUE,
             collapsible = TRUE, 
             background = "navy",
             #"随便打的文本", # 直接插入文本
             #br(), # 换行符
             # 上传数据
             fileInput("data_input_anova",
                       label = h4("上传数据"),
                       accept = ".csv",
                       buttonLabel = "浏览..."),
             # 下载示例数据
             downloadButton('download_demo_data_anova',
                            label = h4('点击 此 处下 载 示 例 数据')),
             
             # 选择因素数量
             sliderInput('num_factor_anova',
                         label = h4('因素个数'),
                         min = 1,
                         max = 5,
                         value = 1),
             # 多重检验水平
             selectInput('level_mult_test_anova',
                         label = h4('多重检验水平'),
                         choices = list('0.95' = 0.95,
                                        '0.99' = 0.99)),
             
             # 选择因素是否交互
             selectInput('factor_act_or_not_anova',
                         label = h4('因素是否交互'),
                         choices = list('否' = 'FALSE',
                                        '是' = 'TRUE')),
             br(),
             # 输入交互因素名称
             textInput('factor_name_anova',
                       label = h4('输入交互因素'),
                       value = '如：group + site'),
             br(),
             br(),
             
             # 点击提交
             actionButton('submit_anova',
                          label = h4('点击提交'),
                          width = 230,
                          icon = NULL),
             
             br(),
             br(),
             
             fluidRow(
               column(width = 2,
                      downloadButton('taboutput_anova_download',
                                     label = h4('下载结果'),
                                     width = 230)),
               
               column(width = 2,
                      offset = 4,# 偏移量
                      downloadButton('download_figure__anova',
                                     label = h4('下载图片'),
                                     width = 230))
             )
         )),# 第一个box完结
  
  # 第二列
  column(width = 5,
         # 第二列上面的box展示统计分析结果
         box(width = NULL,
             height = 700,
             title = '统计分析结果预览',
             status = "warning", 
             solidHeader = TRUE,
             collapsible = TRUE, 
             background = NULL,
             #"随便打的文本", # 直接插入文本
             br(),
             dataTableOutput('taboutput_anova_view')),
         
         # 第二列下面的box下载统计分析参数
         box(width = NULL,
             height = 150,
             title = '统计分析结果下载参数设置',
             status = "info", 
             solidHeader = TRUE,
             collapsible = TRUE, 
             background = NULL,
             #"随便打的文本", # 直接插入文本
             #br(),
             selectInput('anova_stat_res_filetype',
                         label = h5('选择数据下载格式'),
                         choices = list('Excel文件' = '.xlsx',
                                        'txt文件' = '.txt',
                                        'csv文件' = '.csv'),
                         selected = '.csv'))
  ),
  
  # 第三列
  column(width = 5,
         # 第一个box用于绘图
         box(width = NULL,
             height = 500,
             title = '绘图结果展示',
             status = "success", 
             solidHeader = TRUE,
             collapsible = TRUE, 
             #background = "navy",
             #"随便打的文本", # 直接插入文本
             br(),
             plotOutput('plot_anova')),
         
         # 第二个box用于下载图片
         box(width = NULL,
             height = 350,
             title = '绘图结果下载参数设置',
             status = "primary", 
             solidHeader = TRUE,
             collapsible = TRUE, 
             background = NULL,
             #"随便打的文本", # 直接插入文本
             #br(),
             # 图片格式
             selectInput('anova_fig_res_filetype',
                         label = h5('选择图片下载格式'),
                         choices = list('PDF文件' = '.pdf',
                                        'PNG文件' = '.png',
                                        'JPG文件' = '.jpg',
                                        'TIFF文件' = '.tiff',
                                        'EPS文件' = '.eps'),
                         selected = '.tiff'),
             
             # 图片宽度
             sliderInput('anova_fig_wdith',
                         label = '图片宽度（单位：cm）',
                         min = 1,max = 20,
                         value = 5,step = 0.5),
             
             # 图片高度
             sliderInput('anova_fig_height',
                         label = '图片高度（单位：cm）',
                         min = 1,max = 20,
                         value = 5,step = 0.5))
  )
  
)
# 定义anova选项卡结束-----------------------------------------------------------


# 定义kruskal选项卡开始---------------------------------------------------------
tab_kruskal <- fluidRow(
  textOutput('temp_plot')
)
# 定义kruskal选项卡结束---------------------------------------------------------


# 定义refression选项卡开始------------------------------------------------------
tab_regression <- fluidRow(
  textOutput('temp_plot')
)
# 定义regression选项卡结束------------------------------------------------------


# 定义cor选项卡开始-------------------------------------------------------------
tab_cor <- fluidRow(
  textOutput('temp_plot')
)
# 定义cor选项卡结束-------------------------------------------------------------


# 定义pca选项卡开始-------------------------------------------------------------
tab_pca <- fluidRow(
  column(width = 2,
         box(width = NULL,
             height = 870,
             title = '数据上传与参数设置',
             status = "danger", 
             solidHeader = TRUE,
             collapsible = TRUE, 
             background = "navy",
             #"随便打的文本", # 直接插入文本
             #br(), # 换行符
             # 上传数据
             fileInput("data_input_pca",
                       label = h4("上传数据"),
                       accept = ".csv",
                       buttonLabel = "浏览..."),
             # 下载示例数据
             downloadLink('download_demo_data_pca',
                          label = h4('下载示例数据')),
             
             # 选择展示的主成分数量
             selectInput('input_group_name_pca',
                         label = h4('主成分数量'),,
                         choices = list('2' = 2,
                                        '3' = 3,
                                        '4' = 4,
                                        '5' = 5,
                                        '6' = 6,
                                        '7' = 7,
                                        '8' = 8,
                                        '9' = 9,
                                        '10' = 10),
                         selected = 2),
             # 是否导出解释度
             selectInput('select_interpretation_or_not_pca',
                         label = h4('导出主成分解释度'),
                         choices = list('是' = "TRUE",
                                        '否' = "FALSE"),
                         selected = 'FALSE'),
             # 设置绘图样式
             selectInput('select_plot_type_pca',
                         label = h4('选择绘图样式'),
                         choices = list('椭圆' = 1,
                                        '边框' = 2,
                                        '放射' = 3),
                         selected = 3),
             # 是否进行多元方差分析
             selectInput('var_perm_ana_pca',
                         label = h4('多元方差分析'),
                         choices = list('否' = 'FALSE',
                                        '是' = 'TRUE'),
                         selected = 'FALSE'),
             br(),
             br(),
             
             # 点击提交
             actionButton('submit_pca',
                          label = h4('点击提交'),
                          width = 230,
                          icon = NULL),
             
             br(),
             br(),
             br(),
             br(),
             
             fluidRow(
               column(width = 2,
                      downloadButton('taboutput_pca_download',
                                     label = h4('下载结果'),
                                     width = 230)),
               
               column(width = 2,
                      offset = 4,# 偏移量
                      downloadButton('download_figure__pca',
                                     label = h4('下载图片'),
                                     width = 230))
             )
         )),# 第一个box完结
  
  # 第二列
  column(width = 5,
         # 第二列上面的box展示统计分析结果
         box(width = NULL,
             height = 700,
             title = '统计分析结果预览',
             status = "warning", 
             solidHeader = TRUE,
             collapsible = TRUE, 
             background = NULL,
             #"随便打的文本", # 直接插入文本
             br(),
             dataTableOutput('taboutput_pca_view')),
         
         # 第二列下面的box下载统计分析参数
         box(width = NULL,
             height = 150,
             title = '统计分析结果下载参数设置',
             status = "info", 
             solidHeader = TRUE,
             collapsible = TRUE, 
             background = NULL,
             #"随便打的文本", # 直接插入文本
             #br(),
             selectInput('pca_stat_res_filetype',
                         label = h5('选择数据下载格式'),
                         choices = list('Excel文件' = '.xlsx',
                                        'txt文件' = '.txt',
                                        'csv文件' = '.csv'),
                         selected = '.csv'))
  ),
  
  # 第三列
  column(width = 5,
         # 第一个box用于绘图
         box(width = NULL,
             height = 500,
             title = '绘图结果展示',
             status = "success", 
             solidHeader = TRUE,
             collapsible = TRUE, 
             #background = "navy",
             #"随便打的文本", # 直接插入文本
             br(),
             plotOutput('plot_pca')),
         
         # 第二个box用于下载图片
         box(width = NULL,
             height = 350,
             title = '绘图结果下载参数设置',
             status = "primary", 
             solidHeader = TRUE,
             collapsible = TRUE, 
             background = NULL,
             #"随便打的文本", # 直接插入文本
             #br(),
             # 图片格式
             selectInput('pca_fig_res_filetype',
                         label = h5('选择图片下载格式'),
                         choices = list('PDF文件' = '.pdf',
                                        'PNG文件' = '.png',
                                        'JPG文件' = '.jpg',
                                        'TIFF文件' = '.tiff',
                                        'EPS文件' = '.eps'),
                         selected = '.pdf'),
             
             # 图片宽度
             sliderInput('pca_fig_wdith',
                         label = '图片宽度（单位：cm）',
                         min = 1,max = 20,
                         value = 5,step = 0.5),
             
             # 图片高度
             sliderInput('pca_fig_height',
                         label = '图片高度（单位：cm）',
                         min = 1,max = 20,
                         value = 5,step = 0.5))
  )
)
# 定义pca选项卡结束-------------------------------------------------------------

# 定义pcoa选项卡开始-------------------------------------------------------------
tab_pcoa <- fluidRow(
  textOutput('temp_plot')
)
# 定义pcoa选项卡结束-------------------------------------------------------------


# 定义rda选项卡开始-------------------------------------------------------------
tab_rda <- fluidRow(
  textOutput('temp_plot')
)
# 定义rda选项卡结束-------------------------------------------------------------


# 定义nmds选项卡开始-------------------------------------------------------------
tab_nmds <- fluidRow(
  # 定义nmds选项卡开始-------------------------------------------------------------
  tab_nmds <- fluidRow(
    column(width = 2,
           box(width = NULL,
               height = 870,
               title = '数据上传与参数设置',
               status = "danger", 
               solidHeader = TRUE,
               collapsible = TRUE, 
               background = "navy",
               #"随便打的文本", # 直接插入文本
               #br(), # 换行符
               # 上传数据
               fileInput("data_input_nmds",
                         label = h4("上传数据"),
                         accept = ".csv",
                         buttonLabel = "浏览..."),
               # 下载示例数据
               downloadLink('download_demo_data_nmds',
                            label = h4('下载示例数据')),
               
               # 输入分组名称
               textInput('input_group_name_nmds',
                         label = h4('输入分组名称'),,
                         value = '如：海拔'),
               # 是否导出解释度
               selectInput('select_dist_method_nmds',
                           label = h4('距离算法'),
                           choices = list("manhattan" = "manhattan", 
                                          "euclidean" = "euclidean", 
                                          "canberra" = "canberra", 
                                          "clark" = "clark", 
                                          "bray" = "bray", 
                                          "kulczynski" = "kulczynski", 
                                          "jaccard" = "jaccard", 
                                          "gower" = "gower", 
                                          "altGower" = "altGower", 
                                          "morisita" = "morisita", 
                                          "horn" = "horn", 
                                          "mountford" = "mountford", 
                                          "raup" = "raup", 
                                          "binomial" = "binomial", 
                                          "chao" = "chao", 
                                          "cao" = "cao", 
                                          "mahalanobis" = "mahalanobis", 
                                          "chisq" = "chisq", 
                                          "chord" = "chord"),
                           selected = 'bray'),
               # 设置绘图样式
               selectInput('select_plot_type_nmds',
                           label = h4('选择绘图样式'),
                           choices = list('椭圆' = 1,
                                          '边框' = 2,
                                          '放射' = 3),
                           selected = 3),
               # 是否进行多元方差分析
               selectInput('var_perm_ana_nmds',
                           label = h4('多元方差分析'),
                           choices = list('否' = 'FALSE',
                                          '是' = 'TRUE'),
                           selected = 'FALSE'),
               br(),
               br(),
               
               # 点击提交
               actionButton('submit_nmds',
                            label = h4('点击提交'),
                            width = 230,
                            icon = NULL),
               
               br(),
               br(),
               br(),
               br(),
               
               fluidRow(
                 column(width = 2,
                        downloadButton('taboutput_nmds_download',
                                       label = h4('下载结果'),
                                       width = 230)),
                 
                 column(width = 2,
                        offset = 4,# 偏移量
                        downloadButton('download_figure__nmds',
                                       label = h4('下载图片'),
                                       width = 230))
               )
           )),# 第一个box完结
    
    # 第二列
    column(width = 5,
           # 第二列上面的box展示统计分析结果
           box(width = NULL,
               height = 700,
               title = '统计分析结果预览',
               status = "warning", 
               solidHeader = TRUE,
               collapsible = TRUE, 
               background = NULL,
               #"随便打的文本", # 直接插入文本
               br(),
               dataTableOutput('taboutput_nmds_view')),
           
           # 第二列下面的box下载统计分析参数
           box(width = NULL,
               height = 150,
               title = '统计分析结果下载参数设置',
               status = "info", 
               solidHeader = TRUE,
               collapsible = TRUE, 
               background = NULL,
               #"随便打的文本", # 直接插入文本
               #br(),
               selectInput('nmds_stat_res_filetype',
                           label = h5('选择数据下载格式'),
                           choices = list('Excel文件' = '.xlsx',
                                          'txt文件' = '.txt',
                                          'csv文件' = '.csv'),
                           selected = '.csv'))
    ),
    
    # 第三列
    column(width = 5,
           # 第一个box用于绘图
           box(width = NULL,
               height = 500,
               title = '绘图结果展示',
               status = "success", 
               solidHeader = TRUE,
               collapsible = TRUE, 
               #background = "navy",
               #"随便打的文本", # 直接插入文本
               br(),
               plotOutput('plot_nmds')),
           
           # 第二个box用于下载图片
           box(width = NULL,
               height = 350,
               title = '绘图结果下载参数设置',
               status = "primary", 
               solidHeader = TRUE,
               collapsible = TRUE, 
               background = NULL,
               #"随便打的文本", # 直接插入文本
               #br(),
               # 图片格式
               selectInput('nmds_fig_res_filetype',
                           label = h5('选择图片下载格式'),
                           choices = list('PDF文件' = '.pdf',
                                          'PNG文件' = '.png',
                                          'JPG文件' = '.jpg',
                                          'TIFF文件' = '.tiff',
                                          'EPS文件' = '.eps'),
                           selected = '.pdf'),
               
               # 图片宽度
               sliderInput('nmds_fig_wdith',
                           label = '图片宽度（单位：cm）',
                           min = 1,max = 20,
                           value = 5,step = 0.5),
               
               # 图片高度
               sliderInput('nmds_fig_height',
                           label = '图片高度（单位：cm）',
                           min = 1,max = 20,
                           value = 5,step = 0.5))
    )
  )
)
# 定义nmds选项卡结束-------------------------------------------------------------


# 定义plsda选项卡开始-------------------------------------------------------------
tab_plsda <- fluidRow(
  textOutput('temp_plot')
)


# 定义RF选项卡开始-------------------------------------------------------------
tab_rf <- fluidRow(
  textOutput('temp_plot')
)

# 定义oplsda选项卡开始-------------------------------------------------------------
tab_oplsda <- fluidRow(
  textOutput('temp_plot')
)
# 定义oplsda选项卡结束-------------------------------------------------------------


# 定义permanova选项卡开始-------------------------------------------------------------
tab_permanova <- fluidRow(
  textOutput('temp_plot')
)
# 定义permanova选项卡结束-------------------------------------------------------------

# 定义heatmap选项卡开始-------------------------------------------------------------
tab_heatmap <- fluidRow(
  infoBox(title = 'heatmap',
          value = 10,
          icon = icon('credit-card'),
          fill = TRUE)
)
# 定义heatmap选项卡结束-------------------------------------------------------------

# 散点图设置开始------------------------------------------------------------
tab_point_plot <- fluidRow(
  column(width = 2,
         box(width = NULL,
             height = 350,
             title = '数据上传与格式设置',
             status = "danger", 
             solidHeader = TRUE,
             collapsible = TRUE, 
             background = "navy",
             # 上传数据
             fileInput("data_input_point_plot",
                       label = h4("上传数据"),
                       accept = ".csv",
                       buttonLabel = "浏览..."),
             # 下载示例数据
             downloadLink('download_demo_data_point_plot',
                          label = h4('下载示例数据')),
             actionButton('submit_point_plot',
                          label = h4('点击提交'),
                          width = 230,
                          icon = NULL),
             br(),
             br(),
             actionButton('submit_point_plot_to_plot',
                          label = h4('点击更新绘图'),
                          style = style,
                          width = 230,
                          icon = NULL)),
         box(width = NULL,
             height = 500,
             title = '图片下载设置',
             status = 'danger',
             solidHeader = TRUE,
             collapsible = TRUE,
             background = 'navy',
             # 选择图片宽度
             sliderInput('point_plot_fig_width',
                         label = h4('图片宽度（单位：cm）'),
                         min = 2,max = 15,
                         step = 1,value = 5),
             
             # 选择图片高度
             sliderInput('point_plot_fig_height',
                         label = h4('图片高度（单位：cm）'),
                         min = 2,max = 15,
                         step = 1,value = 5),
             # 选择图片下载格式
             selectInput('point_plot_fig_res_filetype',
                         label = h4('选择图片下载格式'),
                         choices = list('PDF文件' = '.pdf',
                                        'PNG文件' = '.png',
                                        'JPG文件' = '.jpg',
                                        'TIFF文件' = '.tiff',
                                        'EPS文件' = '.eps'),
                         selected = '.pdf'),
             br(),
             # 下载图片
             column(width = 1,
                    offset = 3,
                    downloadButton('download_figure__point_plot',
                                   label = h4('下载图片'),
                                   width = '100%')))
  ),
  
  # 第二列散点的颜色等
  column(width = 2,
         box(width = NULL,
             height = 870,
             title = '点的颜色、形状、大小等',
             status = "danger", 
             solidHeader = TRUE,
             collapsible = TRUE, 
             background = "navy",
             # 变量个数
             sliderInput('point_variable_num',
                         label = h4('变量个数'),
                         min = 1, max = 5,
                         sep = 1, value = 1),
             br(),
             # 点的形状赋值
             textInput('point_shape_variable',
                       label = h4('赋值到形状的变量'),
                       value = '如：海拔'),
             br(),
             # 点的形状
             textInput('point_shape',
                       label = h4('点的形状'),
                       value = '单一分组用形状，多个分组以,分隔'),
             br(),
             # 点的形状赋值
             textInput('point_shape_variable',
                       label = h4('赋值到形状的变量'),
                       value = '如：海拔'),
             br(),
             # 点的大小
             textInput('point_size',
                       label = h4('输入点的大小或分组变量'),
                       value = '单一分组用数字，多个分组用分组名称'),
             br(),
             # 点的颜色赋值
             textInput('point_color_variable',
                       label = h4('赋值到颜色的变量'),
                       value = '如：地点'),
             br(),
             # 点的颜色
             textInput('point_color',
                       label = h4('点的颜色'),
                       value = '如：red(多个分组时,分隔)')
         )),
  
  # 第三列:展示图
  column(width = 5,
         offset = 0.5,
         box(width = NULL,
             height = 500,
             title = '绘图结果',
             status = "warning", 
             solidHeader = TRUE,
             collapsible = TRUE, 
             background = NULL,
             br(),
             plotOutput('point_plot_view')),
         
         # 第二列下面的box下载统计分析参数
         box(width = NULL,
             height = 350,
             title = '坐标轴设置',
             status = "info", 
             solidHeader = TRUE,
             collapsible = TRUE, 
             background = 'navy',
             # 横坐标名称
             textInput('point_plot_xaxis_name',
                       label = h4('X轴名称'),
                       value = '如：处理'),
             # 纵坐标名称
             textInput('point_plot_yaxis_name',
                       label = h4('Y轴名称'),
                       value = '相对含量'),
             # 标题
             textInput('point_plot_title',
                       label = h4('图片标题'),
                       value = '这是我的第一个图')
         )),
  # 第三列
  column(width = 3,
         offset = 0.5,
         box(width = NULL,
             height = 875,
             title = '其他参数设置',
             status = "success", 
             background = 'navy',
             solidHeader = TRUE,
             collapsible = TRUE, 
             br(),
             selectInput('point_plot_axis_num',
                         label = h4('坐标轴数量'),
                         choices = list('左+下' = 1,
                                        '全部' = 2),
                         selected = 1),
             sliderInput('point_plot_stick_length',
                         label = h4('刻度长度'),
                         min = -5, max = 5,
                         step = 0.1, value = 0.5),
             textInput('point_plot_ticks_step',
                       label = h4('刻度间隔'),
                       value = '如：0.5'),
             selectInput('point_plot_legend_or_not',
                         label = h4('是否展示图例'),
                         choices = list('是' = 'TRUE',
                                        '否' = 'FALSE'),
                         selected = 'TRUE')
         )))
# 散点图设置开始------------------------------------------------------------


# 定义line_plot选项卡开始-------------------------------------------------------------
tab_line_plot <- fluidRow(
  textOutput('temp_plot')
)
# 定义line_plot选项卡结束-------------------------------------------------------------

# 定义regression_plot选项卡开始-------------------------------------------------------------
tab_regression_plot <- fluidRow(
  textOutput('temp_plot')
)
# 定义regression_plot选项卡结束-------------------------------------------------------------

# 定义box_plot选项卡开始-------------------------------------------------------------
tab_box_plot <- fluidRow(
  textOutput('temp_plot')
)
# 定义box_plot选项卡结束-------------------------------------------------------------

# 定义volcanic_plot选项卡开始-------------------------------------------------------------
tab_volcanic_plot <- fluidRow(
  textOutput('temp_plot')
)
# 定义volcanic_plot选项卡结束-------------------------------------------------------------

# 定义venn_plot选项卡开始-------------------------------------------------------------
tab_venn_plot <- fluidRow(
  textOutput('temp_plot')
)
# 定义venn_plot选项卡结束-------------------------------------------------------------

# 定义slope_plot选项卡开始-------------------------------------------------------------
tab_slope_plot <- fluidRow(
  textOutput('temp_plot')
)
# 定义slope_plot选项卡结束-------------------------------------------------------------

# 定义word_plot选项卡开始-------------------------------------------------------------
tab_word_plot <- fluidRow(
  textOutput('temp_plot')
)
# 定义word_plot选项卡结束-------------------------------------------------------------

# 定义sankey选项卡开始-------------------------------------------------------------
tab_sankey <- fluidRow(
  textOutput('temp_plot')
)
# 定义sankey选项卡结束-------------------------------------------------------------

# 定义violin选项卡开始-------------------------------------------------------------
tab_violin <- fluidRow(
  textOutput('temp_plot')
)
# 定义violin选项卡结束-------------------------------------------------------------

# 定义circle_stack_plot选项卡开始-------------------------------------------------------------
tab_circle_stack_plot <- fluidRow(
  textOutput('temp_plot')
)
# 定义circle_stack_plot选项卡结束-------------------------------------------------------------

# 定义cor_plot选项卡开始-------------------------------------------------------------
tab_cor_plot <- fluidRow(
  textOutput('temp_plot')
)
# 定义cor_plot选项卡结束-------------------------------------------------------------

# 定义lollipop_plot选项卡开始-------------------------------------------------------------
tab_lollipop_plot <- fluidRow(
  textOutput('temp_plot')
)
# 定义lollipop_plot选项卡结束-------------------------------------------------------------

# 定义simple_bar选项卡开始-------------------------------------------------------------
tab_simple_bar <- fluidRow(
  textOutput('temp_plot')
)
# 定义simple_bar选项卡结束-------------------------------------------------------------

# 定义stacked_bar选项卡开始-------------------------------------------------------------
tab_stacked_bar <- fluidRow(
  textOutput('temp_plot')
)
# 定义stacked_bar选项卡结束-------------------------------------------------------------

# 定义percent_plot选项卡开始-------------------------------------------------------------
tab_percent_plot <- fluidRow(
  textOutput('temp_plot')
)
# 定义percent_plot选项卡结束-------------------------------------------------------------



################################################################################
#########################################定义主体###############################
################################################################################
body <- dashboardBody(
  tabItems(
    tabItem(tabName = 'welcome',
            tab_welcome),
    tabItem(tabName = 'outliers_detect',
            tab_outliers_detect),
    tabItem(tabName = 'mean_etal_by_group',
            tab_mean_etal_by_group),
    tabItem(tabName = 't_test',
            tab_for_ttest),
    tabItem(tabName = 'wilcox',
            tab_wilcox),
    tabItem(tabName = 'anova',
            tab_anova),
    tabItem(tabName = 'kruskal',
            tab_kruskal),
    tabItem(tabName = 'regression',
            tab_regression),
    tabItem(tabName = 'cor_test',
            tab_cor),
    tabItem(tabName = 'pca',
            tab_pca),
    tabItem(tabName = 'pcoa',
            tab_pcoa),
    tabItem(tabName = 'rda',
            tab_rda),
    tabItem(tabName = 'nmds',
            tab_nmds),
    tabItem(tabName = 'plsda',
            tab_plsda),
    tabItem(tabName = 'oplsda',
            tab_oplsda),
    tabItem(tabName = 'permanova',
            tab_permanova),
    tabItem(tabName = 'random_forest',
            tab_rf),
    tabItem(tabName = 'heatmap',
            tab_heatmap),
    tabItem(tabName = 'point_plot',
            tab_point_plot),
    tabItem(tabName = 'line_plot',
            tab_line_plot),
    tabItem(tabName = 'regression_plot',
            tab_regression_plot),
    tabItem(tabName = 'box_plot',
            tab_box_plot),
    tabItem(tabName = 'volcanic_plot',
            tab_volcanic_plot),
    tabItem(tabName = 'venn_plot',
            tab_venn_plot),
    tabItem(tabName = 'slope_plot',
            tab_slope_plot),
    tabItem(tabName = 'word_plot',
            tab_word_plot),
    tabItem(tabName = 'sankey_plot',
            tab_sankey),
    tabItem(tabName = 'circle_stack_plot',
            tab_circle_stack_plot),
    tabItem(tabName = 'cor_plot',
            tab_cor_plot),
    tabItem(tabName = 'lollipop_plot',
            tab_lollipop_plot),
    tabItem(tabName = 'simple_bar',
            tab_simple_bar),
    tabItem(tabName = 'stacked_bar',
            tab_stacked_bar),
    tabItem(tabName = 'percent_bar',
            tab_percent_plot),
    tabItem(tabName = 'ciolin_plot',
            tab_violin)
  )
)





