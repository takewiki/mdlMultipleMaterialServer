#' 上传数据
#'
#' @param conn 连接
#' @param file_name 文件名
#' @param sheet_name 页签名
#' @param table_name 表名
#' @param pageCount  分页数
#' @param table_key  表主键
#' @param table_caption 表标题
#'
#' @return 返回值
#'
#' @examples
#' data_read()
data_read <- function(conn=tsda::conn_rds('cprds'),
                             file_name ='data-raw/data/产品大类模板.xlsx',
                             sheet_name ="产品大类",
                             table_name = 'rds_mtrl_prdCategory',
                             table_key = 'FPrdCategoryNumber',
                             table_caption = '产品大类代码',
                              pageCount=500) {


  data <- readxl::read_excel(file_name, sheet = sheet_name)
  ncount = nrow(data)
  if (ncount >0){
    key_str = data[ ,table_caption,drop=TRUE]
    key_str =  tsdo::sql_str(key_str)
    sql_del = paste0(" delete  from  ",table_name,"  where  ",table_key," in (",key_str,")")
    print(sql_del)
    tsda::sql_update(conn,sql_del)

    if(ncount>pageCount){
      pageInfo = tsdo::paging_setting(ncount,pageCount)
      ncount_page = nrow(pageInfo)
      lapply(1:ncount_page, function(i){
        FStart = pageInfo$FStart[i]
        FEnd = pageInfo$FEnd[i]
        item = data[FStart:FEnd, ]
        tsda::db_writeTable(conn = conn,table_name = table_name,r_object = item,append = T)
      })

    }else{
      tsda::db_writeTable(conn = conn,table_name = table_name,r_object = data,append = T)
    }

  }


  return(data)

}


#' 上传数据
#'
#' @param conn 连接
#' @param file_name 文件名
#' @param sheet_name 页签名
#' @param table_name 表名
#' @param pageCount  分页数
#' @param table_key  表主键
#' @param table_caption 表标题
#' @param table_key2  主键2
#' @param table_caption2 标题2
#'
#' @return 返回值
#' @export
#'
#' @examples
#' data_read()
data_read2 <- function(conn=tsda::conn_rds('cprds'),
                      file_name ='data-raw/data/属性类别配置模板.xlsx',
                      sheet_name ="属性类别配置",
                      table_name = 'rds_mtrl_propCategoryConfig',
                      table_key = 'FPrdCategoryNumber',
                      table_key2 = 'FPropCategoryNumber',
                      table_caption = '产品大类代码',
                      table_caption2 = '物料属性类别代码',
                      pageCount=500) {


  data <- readxl::read_excel(file_name, sheet = sheet_name)
  ncount = nrow(data)
  if (ncount >0){
    key_str = data[ ,c(table_caption,table_caption2)]
    lapply(1:ncount, function(i){
      value1 = data[i,table_caption]
      value2 = data[i,table_caption2]

      sql_del = paste0(" delete  from  ",table_name,"  where  ",table_key," ='",value1,"' and ",table_key2," = '",value2,"'  ")
      print(sql_del)
      tsda::sql_update(conn,sql_del)


    })



    if(ncount>pageCount){
      pageInfo = tsdo::paging_setting(ncount,pageCount)
      ncount_page = nrow(pageInfo)
      lapply(1:ncount_page, function(i){
        FStart = pageInfo$FStart[i]
        FEnd = pageInfo$FEnd[i]
        item = data[FStart:FEnd, ]
        tsda::db_writeTable(conn = conn,table_name = table_name,r_object = item,append = T)
      })

    }else{
      tsda::db_writeTable(conn = conn,table_name = table_name,r_object = data,append = T)
    }

  }


  return(data)

}