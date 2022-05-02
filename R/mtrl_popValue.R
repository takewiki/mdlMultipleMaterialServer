#' 读取属性选项数据
#'
#' @param conn  连接
#' @param file_name 文件名
#' @param sheet_name 页签表
#'
#' @return 返回值
#' @export
#'
#' @examples
#' propValue_read()
propValue_read <- function(conn=tsda::conn_rds('cprds'),
                              file_name ='data-raw/data/rds_mtrl_propValue.xls',
                              sheet_name ="属性选项") {
  res <- data_read(conn = conn,file_name = file_name,sheet_name = sheet_name,table_name = 'rds_mtrl_propValue' )
  return(res)
}
