library(shiny)
library(munsell)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(DT)
library(scales)
library(dplyr)

# ---------------------------------------------------------------------------
# 0. 數據載入
# ---------------------------------------------------------------------------
file_path <- "realistic_kraljic_dataset.csv"
raw_data <- read.csv(file_path, stringsAsFactors = FALSE)

diagnosis_data <- raw_data %>%
  mutate(
    Total_Spend = Order_Volume_Units * Cost_per_Unit,
    Supplier_Region = as.factor(Supplier_Region)
  )

# ---------------------------------------------------------------------------
# 1. 完整語系字典 (包含彈出視窗內容)
# ---------------------------------------------------------------------------
i18n <- list(
  "zh" = list(
    "title" = "供應鏈採購風險決策建議", "sim_h" = "【 情境模擬控制中心 】",
    "lock_lbl" = "區域封鎖模擬 (Region)", "ttr_lbl" = "交期恢復壓力 (TTR Days)", "tco_lbl" = "總採購成本敏感度 (%)",
    "btn_ai" = "啟動 AI 最佳路徑分析",
    "g1" = "TCO 總成本壓力", "g2" = "TTR 恢復天數預警", "g3" = "SP 關係熱度指數",
    "g1_d" = "代表「隱性成本」。上升代表風險與管理成本正在吞噬利潤。",
    "g2_d" = "代表「生存防線」。數字越大，斷鏈後恢復供應所需時間越長。",
    "g3_d" = "代表「談判籌碼」。越高代表您在供應商心中越重要，具優先權。",
    "box1" = "1. 核心韌性指標 (SRI 指數分析)", "box2" = "2. 優先執行採購商品明細 (按優先順序排序)",
    "rose" = "區域策略強度分佈", "ai_h" = "AI 啟發式決策建議",
    "th" = c("品項ID", "名稱", "地區", "優先級", "曝險金額"),
    "plan_a" = "方案 A: 韌性策略", "plan_b" = "方案 B: 成本優化",
    "desc_a" = "「預則立，不預則廢」。風險高於閥值，建議增加庫存緩衝。",
    "desc_b" = "「凡事先勝而後求戰」。利用合約避險鎖定高總額品項之成本。",
    # 彈出視窗專用
    "opt_title" = "AI 戰略最佳化路徑報告", "opt_close" = "關閉報告",
    "opt_head" = "根據目前數據演算，建議立即處理以下品項：",
    "opt_rank" = "優先建議", "opt_analysis" = "【現況分析】該品項來自 ",
    "opt_spend" = "，曝險金額為 ", "opt_action" = "【建議動作】啟動第二供應商認證，預計可降低 TTR 恢復時間約 45%。",
    "opt_footer" = "※ 此分析基於 TCO 與 TTR 交叉權重演算。"
  ),
  "en" = list(
    "title" = "Supply Chain Risk Decision Support", "sim_h" = "[ Simulation Console ]",
    "lock_lbl" = "Region Lockdown Sim", "ttr_lbl" = "Lead Time Recovery (TTR)", "tco_lbl" = "Total Spend Sensitivity (%)",
    "btn_ai" = "Activate AI Optimization",
    "g1" = "TCO Cost Pressure", "g2" = "TTR Recovery Days", "g3" = "SP Relationship Index",
    "g1_d" = "Hidden Costs. Rising levels mean overhead is eating your margins.",
    "g2_d" = "Survival Buffer. Higher days mean longer downtime during disruptions.",
    "g3_d" = "Negotiation Leverage. Higher means you are a priority client in crisis.",
    "box1" = "1. Core Resilience (SRI Index Analysis)", "box2" = "2. Priority Procurement List (Sorted)",
    "rose" = "Regional Strategy Rose Plot", "ai_h" = "AI Strategic Recommendations",
    "th" = c("ID", "Name", "Region", "Priority", "Spend"),
    "plan_a" = "Plan A: Resilience", "plan_b" = "Plan B: Costing",
    "desc_a" = "Forewarned is forearmed. High risk detected; increase buffer stocks.",
    "desc_b" = "Win before fighting. Lock forward contracts for high-spend items.",
    # 彈出視窗專用
    "opt_title" = "AI Strategic Optimization Report", "opt_close" = "Close Report",
    "opt_head" = "Based on current simulation, the following items require immediate action:",
    "opt_rank" = "Priority", "opt_analysis" = "【Analysis】Sourced from ",
    "opt_spend" = ", with a total exposure of ", "opt_action" = "【Action】Initiate dual-sourcing certification to reduce TTR recovery by 45%.",
    "opt_footer" = "* This analysis is based on TCO and TTR weighted cross-calculations."
  )
)

colors <- list(bg = "#0b1426", box = "#152039", danger = "#ff4b2b", warning = "#ffaa00", safe = "#00e676", text = "#ffffff", gold = "#fbbf24", sub_text = "#a0aec0")

# ---------------------------------------------------------------------------
# 2. UI 介面
# ---------------------------------------------------------------------------
ui <- dashboardPage(
  dashboardHeader(title = uiOutput("header_ui"), titleWidth = 400),
  dashboardSidebar(
    width = 300,
    radioButtons("lang", "Language / 語系", choices = c("中文" = "zh", "English" = "en"), inline = TRUE),
    hr(),
    uiOutput("sidebar_ui"),
    hr(),
    uiOutput("btn_ui")
  ),
  dashboardBody(
    tags$head(tags$style(HTML(paste0("
      .content-wrapper { background-color: ", colors$bg, " !important; }
      .box { background: ", colors$box, " !important; border: 1px solid #2d3748 !important; border-radius: 10px !important; color: white !important; }
      .main-header .logo { background-color: #1e293b !important; color: #3b82f6 !important; font-weight: 800 !important; font-size: 20px !important; }
      .main-header .navbar { background-color: #1e293b !important; }
      .box-title { font-size: 18px !important; font-weight: 800 !important; color: #3b82f6 !important; }
      .gauge-title { font-size: 17px !important; text-align: center; color: #ffffff !important; font-weight: 800 !important; margin-bottom: -15px; }
      .gauge-desc { font-size: 13px; text-align: center; color: #cbd5e1; padding: 10px 20px; font-style: italic; min-height: 70px; }
      .ai-card { border-left: 4px solid ", colors$warning, "; background: #1e293b; padding: 12px; margin-bottom: 10px; border-radius: 4px; }
      .dataTables_wrapper { color: white !important; font-size: 14px; }
      table.dataTable thead th { color: #ffffff !important; font-weight: 800 !important; background-color: #1e293b !important; border-bottom: 2px solid #3b82f6 !important;}
      table.dataTable tbody td { color: #ffffff !important; font-weight: 600 !important; border-bottom: 1px solid #2d3748 !important;}
      .control-label { color: #ffffff !important; font-weight: 700 !important; font-size: 15px !important; }
      .main-sidebar h4 { color: #3b82f6 !important; font-weight: 900 !important; }
    ")))),
    
    fluidRow(
      column(width = 9,
             box(width = 12, height = "550px", title = uiOutput("box1_title_ui"),
                 fluidRow(
                   column(4, div(class="gauge-title", uiOutput("g1_l")), plotlyOutput("gauge1", height = "300px"), div(class="gauge-desc", uiOutput("g1_d_ui"))),
                   column(4, div(class="gauge-title", uiOutput("g2_l")), plotlyOutput("gauge2", height = "300px"), div(class="gauge-desc", uiOutput("g2_d_ui"))),
                   column(4, div(class="gauge-title", uiOutput("g3_l")), plotlyOutput("gauge3", height = "300px"), div(class="gauge-desc", uiOutput("g3_d_ui")))
                 )
             ),
             box(width = 12, title = uiOutput("box2_title_ui"), DTOutput("priority_table"))
      ),
      column(width = 3,
             box(width = 12, title = uiOutput("rose_title_ui"), height = "350px", plotOutput("rose_plot", height = "280px")),
             box(width = 12, title = uiOutput("ai_title_ui"), height = "400px", uiOutput("ai_cards_ui"))
      )
    )
  )
)

# ---------------------------------------------------------------------------
# 3. Server 邏輯 (所有 output 指令都必須在大括號內！)
# ---------------------------------------------------------------------------
server <- function(input, output, session) {
  
  # 語系與數據流定義
  L <- reactive({ i18n[[input$lang]] })
  
  # 反應式數據集：根據選單動態過濾
  filtered_df <- reactive({
    data <- diagnosis_data # 使用原始載入的數據
    if (!is.null(input$lockdown) && input$lockdown != "None") {
      data <- data[data$Supplier_Region == input$lockdown, ]
    }
    return(data)
  })
  
  # UI 翻譯與側邊欄連動 (必須在這裡面！)
  output$header_ui <- renderUI({ L()$title })
  output$box1_title_ui <- renderUI({ L()$box1 })
  output$box2_title_ui <- renderUI({ L()$box2 })
  output$g1_l <- renderUI({ L()$g1 })
  output$g2_l <- renderUI({ L()$g2 })
  output$g3_l <- renderUI({ L()$g3 })
  output$g1_d_ui <- renderUI({ L()$g1_d })
  output$g2_d_ui <- renderUI({ L()$g2_d })
  output$g3_d_ui <- renderUI({ L()$g3_d })
  output$rose_title_ui <- renderUI({ L()$rose })
  output$ai_title_ui <- renderUI({ L()$ai_h })
  
  output$sidebar_ui <- renderUI({
    tagList(
      h4(L()$sim_h),
      selectInput("lockdown", L()$lock_lbl, choices = c("None", "Asia", "Europe", "Africa", "South America", "North America")),
      sliderInput("ttr_v", L()$ttr_lbl, 0, 180, 45, post = " d"),
      sliderInput("tco_v", L()$tco_lbl, 0, 100, 50, post = " %")
    )
  })
  
  output$btn_ui <- renderUI({
    actionButton("optimize", L()$btn_ai, class = "btn-primary", style = "width: 85%; margin-left: 20px; border:none; background:#3b82f6; font-weight:bold;")
  })
  
  # 運算引擎
  sri_engine <- reactive({
    df_calc <- filtered_df() # 直接使用反應式數據
    tco <- min(100, (mean(df_calc$Supply_Risk_Score) * 12) + (input$tco_v/2))
    ttr <- min(100, (input$ttr_v / 180 * 100) + (ifelse(input$lockdown != "None", 20, 0)))
    sp <- max(0, 100 - (tco * 0.4 + ttr * 0.2))
    list(tco=tco, ttr=ttr, sp=sp, data=df_calc)
  })
  
  # 圖表渲染函數
  render_g <- function(v, clr) {
    plot_ly(domain = list(x=c(0,1), y=c(0,1)), value = v, type = "indicator", mode = "gauge+number",
            gauge = list(axis = list(range = list(NULL, 100), tickcolor="white"), bar = list(color = clr), bgcolor = colors$box, bordercolor = "#2d3748")) %>%
      layout(paper_bgcolor = "transparent", font = list(color = "white"))
  }
  
  output$gauge1 <- renderPlotly({ render_g(sri_engine()$tco, colors$danger) })
  output$gauge2 <- renderPlotly({ render_g(sri_engine()$ttr, colors$warning) })
  output$gauge3 <- renderPlotly({ render_g(sri_engine()$sp, colors$safe) })
  
  output$rose_plot <- renderPlot({
    # 修正點：使用 raw_data 或 diagnosis_data (全局)，而不是 sri_engine()$data (過濾後)
    rose_df <- diagnosis_data %>% 
      group_by(Supplier_Region) %>% 
      summarise(count = n(), .groups = "drop") %>%
      mutate(
        label = paste0(round(count/sum(count)*100, 1), "%"), 
        is_h = (Supplier_Region == input$lockdown) # 標記選中的地區
      )
    ggplot(rose_df, aes(x = Supplier_Region, y = count, fill = is_h)) +
      geom_bar(stat = "identity", width = 1, color = colors$box, size = 0.5) +
      geom_text(aes(label = label), position = position_stack(vjust = 0.8), color = "white", fontface = "bold", size = 4) +
      coord_polar(clip = "off") +
      scale_fill_manual(values = c("FALSE" = colors$gold, "TRUE" = colors$danger)) +
      theme_void() + 
      theme(plot.background = element_rect(fill = colors$box, color = NA), panel.background = element_rect(fill = colors$box, color = NA), plot.margin = margin(15, 15, 15, 15), axis.text.x = element_text(color = "white", size = 11, face="bold", vjust = 5), legend.position = "none")
  }, bg = "transparent")
  
  output$ai_cards_ui <- renderUI({
    tagList(
      div(class="ai-card", strong(L()$plan_a), p(L()$desc_a)),
      div(class="ai-card", style="border-left-color:#3b82f6", strong(L()$plan_b), p(L()$desc_b))
    )
  })
  
  output$priority_table <- renderDT({
    datatable(sri_engine()$data %>% arrange(desc(Supply_Risk_Score)) %>% head(10) %>% select(Product_ID, Product_Name, Supplier_Region, Supply_Risk_Score, Total_Spend),
              colnames = L()$th, options = list(dom = 't', pageLength = 5), rownames = FALSE) %>% formatCurrency("Total_Spend", "$", digits = 0)
  })
  
  # --- 彈出視窗：AI 戰略報告 (修復變數名稱錯誤) ---
  observeEvent(input$optimize, {
    best_path <- filtered_df() %>%
      arrange(desc(Supply_Risk_Score)) %>% 
      head(3)
    
    showModal(modalDialog(
      title = L()$opt_title,
      size = "l",
      easyClose = TRUE,
      footer = modalButton(L()$opt_close),
      tagList(
        h3(L()$opt_head, style="color: #3b82f6; font-weight: bold;"),
        hr(),
        lapply(1:nrow(best_path), function(i) {
          div(style="background: #f1f5f9; padding: 15px; border-radius: 8px; margin-bottom: 10px; color: #1e293b;",
              h4(paste(L()$opt_rank, i, ":", best_path$Product_Name[i]), style="font-weight: bold; color: #ef4444;"),
              p(paste(L()$opt_analysis, best_path$Supplier_Region[i], L()$opt_spend, dollar(best_path$Total_Spend[i]))),
              p(L()$opt_action)
          )
        }),
        p(L()$opt_footer, style="font-size: 12px; color: #94a3b8; margin-top: 20px;")
      )
    ))
  })
} # <--- 這裡才是真正的 Server 結尾！

# 發動
shinyApp(ui, server)