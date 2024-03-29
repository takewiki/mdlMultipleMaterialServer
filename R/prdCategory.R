#' 读取产品大类数据
#'
#' @param file_name 文件名
#' @param sheet_name 页签表
#' @param conn_cfg 连接设置
#'
#' @return 返回值
#' @export
#'
#' @examples
#' prdCategory_read()
prdCategory_read <- function(conn_cfg = 'config/conn_cfg.R',
                             file_name ='data-raw/data/rds_mtrl_productCategory.xlsx',
                             sheet_name ="产品大类") {
  conn_info = tsda::conn_config(config_file = conn_cfg)
  conn = tsda::conn_open(conn_config_info = conn_info)
  res <- data_read(conn = conn,
                   file_name = file_name,
                   sheet_name = sheet_name,
                   table_name = 'rds_mtrl_prdCategory',)
  tsda::conn_close(conn)
  return(res)
}


#' 产品大类查询
#'
#' @param conn_cfg 连接文本
#'
#' @return 返回值
#'
#' @examples
#' prdCategory_query()
prdCategory_query <- function(conn_cfg ='config/conn_cfg.R') {
  conn_info = tsda::conn_config(config_file = conn_cfg)
conn = tsda::conn_open(conn_config_info = conn_info)


  sql <- paste0("SELECT  产品大类代码,产品大类名称,上级产品大类
  FROM  vw_rds_mtrl_prdCategory
  where 是否明细 =1 and 是否禁用 =0")
  data = tsda::sql_select(conn,sql)
  tsda::conn_close(conn)

  return(data)

}

#' 按上级组进行查询产品大类
#'
#' @param FParentPrdCategory 上级组
#' @param conn_cfg 连接配置
#'
#' @return 返回值
#' @export
#'
#' @examples
#' prdCategory_queryByParent()
prdCategory_queryByParent <- function(conn_cfg ='config/conn_cfg.R',FParentPrdCategory ='实验耗材') {

  conn_info = tsda::conn_config(config_file = conn_cfg)
conn = tsda::conn_open(conn_config_info = conn_info)

  sql <- paste0("SELECT
  FPrdCategoryNumber as 产品大类代码
  ,FPrdCategoryName  as 产品大类名称
  FROM  rds_mtrl_prdCategory
  where  Fdetail =1 and Fdeleted =0 and FParentPrdCategory ='",FParentPrdCategory,"'")
  data = tsda::sql_select(conn,sql)
  tsda::conn_close(conn)

  return(data)

}





#' 产品大类查询
#'
#' @param input 输入
#' @param output 输出
#' @param conn_cfg 连接配置
#' @param session 会话
#'
#'@import tsdo
#' @return 返回值
#'
#' @examples
#' prdCategoryServer_query()
prdCategoryServer_query <- function(input,output,session,conn_cfg){

  shiny::observeEvent(input$btnPrdCategory_query,{
    data = prdCategory_query(conn_cfg = conn_cfg)
    file_name = paste0('产品大类下载_',getDate(),".xlsx")
    tsui::run_dataTable2(id = 'dataviewPrdCategory_query',data = data)
    tsui::run_download_xlsx(id = 'btnPrdCategory_dl',data = data,filename = file_name)

  })


}




#' 文本选择
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#'
#'@import tsdo
#' @return 返回值
#' @export
#'
#' @examples
#' prdCategoryUIServer_sheet()
prdCategoryUIServer_sheet <- function(input,output,session){

  var_prdCategory_file <- tsui::var_file('filePrdCategory_upload')

  output$placeholderPrdCategory_sheets <- shiny::renderUI({

    file_name = var_prdCategory_file()

    if (is.null(file_name)){
      res = NULL
    }else{
      sheetNames =  tsdo::vect_as_list(  openxlsx::getSheetNames(file=file_name))
      res = tsui::mdl_ListChoose1(id = 'lc1PrdCategory_sheets',label = '请选择一下产品大类所在页签',
                            choiceNames = sheetNames,choiceValues = sheetNames,selected = sheetNames[[1]])
    }

    res


  })


}





#' 处理内容
#'
#' @param input 输入
#' @param output 输出
#' @param conn_cfg 连接
#' @param session 会议
#'
#' @return 返回值
#' @export
#'
#' @examples
#' file_deal()
prdCategoryServer_read <- function(input,output,session,conn_cfg) {
  var_file_name  = tsui::var_file('filePrdCategory_upload')
  var_sheet_name =tsui::var_ListChoose1('lc1PrdCategory_sheets')
  shiny::observeEvent(input$btnprdCategory_upload,{
    file_name = var_file_name()
    print(file_name)
    sheet_name = var_sheet_name()
    print(sheet_name)
    data = prdCategory_read(conn_cfg = conn_cfg,file_name = file_name ,sheet_name = sheet_name )
     print(data)

    ncount =nrow(data)
    if(ncount >0){
      tsui::pop_notice('上传成功')
    }else{
      tsui::pop_notice('上传失败')
    }






  })

}




#' 产品大娄的处理函数
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#' @param conn 连接
#'
#' @return 返回值
#' @export
#'
#' @examples
#' prdCategoryServer()
prdCategoryServer <- function(input,output,session,conn_cfg){

  prdCategoryServer_query(input,output,session,conn_cfg = conn_cfg)
  prdCategoryUIServer_sheet(input,output,session)
  prdCategoryServer_read(input,output,session,conn_cfg = conn_cfg)



}



