#' 获取最大号ID
#'
#' @param conn 连接
#'
#' @return 返回值
#' @export
#'
#' @examples
#' prdGenList_maxId()
prdGenList_maxId <- function(conn=tsda::conn_rds('cprds')) {
  res = tsda::db_maxId(conn = conn,FTableName = 'rds_mtrl_prdGenList')
  return(res)

}
#' 判断是新的数据
#'
#' @param conn 连接
#' @param FPrdGroupNumber 产品分组代码
#' @param FPropValueNumberFlag  产品属性标签
#'
#' @return 返回值
#' @export
#'
#' @examples
#' prdGenList_isNew()
prdGenList_isNew <- function(conn=tsda::conn_rds('cprds'),
                             FPrdGroupNumber = '1.1.1.004.002',
                             FPropValueNumberFlag ='C000001_AC0001_AD0003_AF0001_AH0002_AJ0011_AM0001_AS0001_AT0001'

                             ) {
sql <- paste0("SELECT
1
FROM rds_mtrl_prdGenList
where FPrdGroupNumber = '",FPrdGroupNumber,"' and FPropValueNumberFlag ='",FPropValueNumberFlag,"'")
data = tsda::sql_select(conn,sql)
ncount =nrow(data)
if(ncount >0){
  res = FALSE
}else{
  res = TRUE
}

return(res)
}


#' 产品结果查看
#'
#' @param conn 连接
#' @param FPrdGroupNumber  产品分组
#' @param FPropValueNumberFlag 选配结果代码
#'
#' @return 返回值
#' @export
#'
#' @examples
#' prdGenList_query()
prdGenList_query <- function(conn=tsda::conn_rds('cprds'),
                             FPrdGroupNumber = '1.1.1.004.002',
                             FPropValueNumberFlag ='C000001_AC0001_AD0003_AF0001_AH0002_AJ0011_AM0001_AS0001_AT0001'

) {
  sql <- paste0("SELECT
       [FNumber] as  物料代码
      ,[FName] as 物料名称
      ,[FModel] as 规格型号
      ,[FPrdGroupNumber] as 产品分组代码
      ,[FPrdGroupName] as 产品分组名称
      ,[FPropValueNumberFlag] as 选配结果代码


  FROM rds_mtrl_prdGenList
where FPrdGroupNumber = '",FPrdGroupNumber,"' and FPropValueNumberFlag ='",FPropValueNumberFlag,"'")
  data = tsda::sql_select(conn,sql)

  return(data)
}


#' 产品结果查看
#'
#' @param conn 连接
#' @param FPrdGroupNumber  产品分组
#' @param FPropValueNumberFlag 选配结果代码
#' @param FPrdGroupName  产品分组名称
#' @param FPropValueNameFlag 选配结果名称
#'
#' @return 返回值
#' @export
#'
#' @examples
#' prdGenList_upload()
prdGenList_upload <- function(conn=tsda::conn_rds('cprds'),
                             FPrdGroupNumber = '1.1.1.004.002',
                             FPrdGroupName ='PCR板无群边',

                             FPropValueNumberFlag ='C000001_AC0001_AD0003_AF0001_AH0002_AJ0011_AM0001_AS0001_AT0001',
                             FPropValueNameFlag ='赛普_透明色_耐高温_袋装_40ul_96孔_单色注塑_R_印刷'

) {
  flag_new = prdGenList_isNew(conn = conn,FPrdGroupNumber = FPrdGroupNumber,FPropValueNumberFlag = FPropValueNumberFlag)
  if(flag_new){
    FInterId = prdGenList_maxId(conn = conn) + 1
    FShortNumber =prdGenRule_autoNumber(conn = conn,FPrdGroupNumber = FPrdGroupNumber,FNumberLength = 6)
    FNumber = paste0(FPrdGroupNumber,".",FShortNumber)
    FName = FPrdGroupName
    FModel = FPropValueNameFlag
    data = data.frame(FInterId,FPrdGroupNumber,FPrdGroupName,FPropValueNumberFlag,FPropValueNameFlag,FShortNumber,FNumber,FName,FModel,stringsAsFactors = F)
    tsda::db_writeTable(conn = conn,table_name = 'rds_mtrl_prdGenList',r_object = data,append = T)
    res = prdGenList_query(conn = conn,FPrdGroupNumber = FPrdGroupNumber,FPropValueNumberFlag = FPropValueNumberFlag)




  }else{
    res = prdGenList_query(conn = conn,FPrdGroupNumber = FPrdGroupNumber,FPropValueNumberFlag = FPropValueNumberFlag)
  }
  return(res)

}
