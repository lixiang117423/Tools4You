# 定义t-test选项卡开始----------------------------------------------------------
fluidRow(
  # 使用box后就不能再用column
  # 第一个box
  column(width = 2,
         box(width = NULL,
             height = 875,
             title = '数据上传与主要设置',
             status = "danger", 
             solidHeader = TRUE,
             collapsible = TRUE, 
             background = "navy",
             # 上传丰度数据
             fileInput("data_input_perm",
                       label = h4("上传数据"),
                       accept = ".csv",
                       buttonLabel = "浏览..."),
             # 下载示例丰度数据
             downloadLink('download_demo_data_perm',
                          label = h4('下载示例数据')),
             
             # 选择置信区间
             selectInput('select_dist_method_perm',
                         label = h4('选择距离算法'),
                         choices = list("manhattan" = "manhattan",
                                        "euclidean" = "euclidean",
                                        "canberra" = "canberra",
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
                                        "mahalanobis" = "mahalanobis"),
                         selected = 'bray'),
             # 因素数量
             sliderInput('num_factor_perm',
                         label = h4('因素数量'),
                         min = 1, max = 5,
                         step = 1, value = 1),
             
             # 因素交互与否
             selectInput('factor_inter_perm',
                         label = h4('因素是否交互'),
                         choices = list('是' = 'TRUE',
                                        '否' = 'FALSE'),
                         selected = 'TRUE'),
             
             # 置换检验次数
             sliderInput('select_number_perm',
                         label = h4('置换检验次数'),
                         min = 500, max = 9999,
                         step = 1, value = 999),
             selectInput('perm_stat_res_filetype',
                         label = h4('选择数据下载格式'),
                         choices = list('Excel文件' = '.xlsx',
                                        'txt文件' = '.txt',
                                        'csv文件' = '.csv'),
                         selected = '.xlsx'),
             
             # 点击提交
             actionButton('submit_perm',
                          label = h4('点击提交'),
                          width = 230,
                          icon = NULL),
             br(),
             br(),
             fluidRow(
               column(width = 4,
                      offset = 3,
                      downloadButton('taboutput_perm_download',
                                     label = h4('下载结果'),
                                     width = 300))
             )
         )),# 第一个box完结
  
  # 第二列
  column(width = 10,
         # 第二列上面的box展示统计分析结果
         box(width = NULL,
             height = 875,
             title = '结果预览',
             status = "warning", 
             solidHeader = TRUE,
             collapsible = TRUE, 
             background = NULL,
             br(),
             dataTableOutput('taboutput1_perm_view'))
         
         
  ))
