#' 读取属性选项配置数据
#'
#' @param conn  连接
#' @param file_name 文件名
#' @param sheet_name 页签表paa
#'
#' @return 返回值
#' @export
#'
#' @examples
#' propValueConfig_read()
propValueConfig_read <- function(conn=tsda::conn_rds('cprds'),
                           file_name ='data-raw/data/rds_mtrl_propValueConfig.xls',
                           sheet_name ="属性选项配置") {
  res <- data_read(conn = conn,file_name = file_name,sheet_name = sheet_name,table_name = 'rds_mtrl_propValueConfig' )

  return(res)
}
