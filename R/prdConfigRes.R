#' 判断是否是最新
#'
#' @param FPrdGroupNumber 产品分组代码
#' @param FPropValueNumberFlag 选配结果代码
#' @param conn_cfg 连接配置
#'
#' @return 返回值
#' @export
#'
#' @examples
#' prdConfigRes_isNew()
prdConfigRes_isNew <- function(conn_cfg = 'config/conn_cfg.R',
                               FPrdGroupNumber ='1.1.1.004.002',
                               FPropValueNumberFlag ='C000001_AC0001_AD0003_AF0001_AH0002_AJ0011_AM0001_AS0001_AT0001'

                               ) {
conn_info = tsda::conn_config(config_file = conn_cfg)
conn = tsda::conn_open(conn_config_info = conn_info)

sql <- paste0("SELECT  1
  FROM [cprds].[dbo].[rds_mtrl_prdConfigRes]
  where FPrdGroupNumber ='",FPrdGroupNumber,"' and FPropValueNumberFlag ='",FPropValueNumberFlag,"'")
data <- tsda::sql_select(conn,sql)
ncount = nrow(data)
if(ncount >0){
  res <- FALSE
}else{
  res<- TRUE
}
tsda::conn_close(conn)

return(res)

}



