#' 处理逻辑
#'
#' @param input 输入
#' @param output 输出
#' @param conn_cfg 连接文件
#' @param session 会话
#'
#' @return 返回值
#' @export
#'
#' @examples
#' prdSelServer()
prdSelServer <- function(input,output,session,conn_cfg ='config/conn_cfg.R'){




  var_prdSel_FNumber_txt <- tsui::var_text('prdSel_FNumber_txt')
  shiny::observeEvent(input$prdSel_query_btn,{

    FNumbe =var_prdSel_FNumber_txt()
    data = prdSel_query( conn_cfg = conn_cfg,FNumber = FNumbe)
    output$prdSel_query_dataview <- DT::renderDataTable(data)
    file_name = paste0("物料列表下载",tsdo::getDate(),".xlsx")
    tsui::run_download_xlsx(id = 'prdSel_dl_btn',data = data,filename = file_name)



  })

}

#' 属性类别查询
#'
#' @param FNumber 代码
#' @param conn_cfg 连接文件
#'
#' @return 返回值
#'
#' @examples
#' propCategory_query()
prdSel_query <- function(conn_cfg ='config/conn_cfg.R'
                         ,FNumber='') {
  conn_info = tsda::conn_config(config_file = conn_cfg)
conn = tsda::conn_open(conn_config_info = conn_info)

  if(FNumber == ''){
    sql <- paste0("SELECT

       [物料代码]
      ,[物料名称]
      ,[规格型号]
      ,[产品分组代码]
      ,[产品分组名称]
      ,[选配结果代码]
      ,[选配置结果名称]
      ,[物料短代码]

  FROM   vw_rds_mtrl_prdGenList  ")
  }else{
    sql <- paste0("SELECT

       [物料代码]
      ,[物料名称]
      ,[规格型号]
      ,[产品分组代码]
      ,[产品分组名称]
      ,[选配结果代码]
      ,[选配置结果名称]
      ,[物料短代码]

  FROM   vw_rds_mtrl_prdGenList  where 物料代码 = '",FNumber,"'")
  }


  data = tsda::sql_select(conn,sql)
  tsda::conn_close(conn)

  return(data)

}
