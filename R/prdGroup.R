#' 读取产品分组数据
#'
#' @param conn  连接
#' @param file_name 文件名
#' @param sheet_name 页签表
#'
#' @return 返回值
#' @export
#'
#' @examples
#' prdGroup_read()
prdGroup_read <- function(conn=tsda::conn_rds('cprds'),
                             file_name ='data-raw/data/产品分组模板.xlsx',
                             sheet_name ="产品大类") {
  res <- data_read(conn=conn,
                   file_name =file_name,
                   sheet_name =sheet_name,
                   table_name = 'rds_mtrl_prdGroup',
                   table_key = 'FPrdGroupNumber',
                   table_caption = '产品分组代码',
                   pageCount=500 )
  return(res)
}


#' 产品大类查询
#'
#' @param conn 连接
#'
#' @return 返回值
#'
#' @examples
#' prdGroup_query()
prdGroup_query <- function(conn=tsda::conn_rds('cprds')) {

  sql <- paste0("SELECT
      [产品分组代码]
      ,[产品分组名称]
      ,[产品大类名称]
      ,[产品大类代码]
      ,[级次]
	  ,是否明细

  FROM [vw_rds_mtrl_prdGroup]
    where   是否禁用 =0")
  data = tsda::sql_select(conn,sql)
  return(data)

}





#' 产品大类查询
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#'
#'@import tsdo
#' @return 返回值
#'
#' @examples
#' prdGroupServer_query()
prdGroupServer_query <- function(input,output,session){

  shiny::observeEvent(input$btnPrdGroup_query,{
    data = prdGroup_query()
    file_name = paste0('产品分组下载_',getDate(),".xlsx")
    tsui::run_dataTable2(id = 'dataviewPrdGroup_query',data = data)
    tsui::run_download_xlsx(id = 'btnPrdGroup_dl',data = data,filename = file_name)

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
#' prdGroupUIServer_sheet()
prdGroupUIServer_sheet <- function(input,output,session){

  var_prdGroup_file <- tsui::var_file('filePrdGroup_upload')

  output$placeholderPrdGroup_sheets <- shiny::renderUI({

    file_name = var_prdGroup_file()

    if (is.null(file_name)){
      res = NULL
    }else{
      sheetNames =  tsdo::vect_as_list(  openxlsx::getSheetNames(file=file_name))
      res = tsui::mdl_ListChoose1(id = 'lc1PrdGroup_sheets',label = '请选择一下产品分组所在页签',
                                  choiceNames = sheetNames,choiceValues = sheetNames,selected = sheetNames[[1]])
    }

    res


  })


}





#' 处理内容
#'
#' @param input 输入
#' @param output 输出
#' @param session 会议
#'
#' @return 返回值
#' @export
#'
#' @examples
#' file_deal()
prdGroupServer_read <- function(input,output,session) {
  var_file_name  = tsui::var_file('filePrdGroup_upload')
  var_sheet_name =tsui::var_ListChoose1('lc1PrdGroup_sheets')
  shiny::observeEvent(input$btnprdGroup_upload,{
    file_name = var_file_name()
    print(file_name)
    sheet_name = var_sheet_name()
    print(sheet_name)
    data = prdGroup_read(file_name = file_name ,sheet_name = sheet_name )
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
#'
#' @return 返回值
#' @export
#'
#' @examples
#' prdGroupServer()
prdGroupServer <- function(input,output,session){

  prdGroupServer_query(input,output,session)
  prdGroupUIServer_sheet(input,output,session)
  prdGroupServer_read(input,output,session)



}



