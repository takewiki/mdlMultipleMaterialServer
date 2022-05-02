
#' 查询表的最大值
#'
#' @param conn 连接
#' @param FTableName  表名
#'
#' @return 返回值
#' @export
#'
#' @examples
#' meta_maxId()
meta_maxId <- function(conn=tsda::conn_rds('metadata'),FTableName ='t_dict_multipleMaterial'){
  sql <- paste0("select isnull(max(FInterId),0) as FInterId  from ",FTableName)
  data = tsda::sql_select(conn,sql)
  res = data$FInterId
  return(res)
}

#' 上传元数据
#'
#' @param file_name 数据表
#'
#' @return 返回值
#' @export
#'
#' @examples
#' meta_upload()
meta_upload <- function(conn=tsda::conn_rds('metadata'),file_name ="data-raw/data/rds_mtrl_productCategory.xlsx") {
  #library(readxl)
  data <- readxl::read_excel(file_name, sheet = "数据字典")
  ncount <- nrow(data)
  max_Id = meta_maxId(conn = conn)
  data_id = data.frame(FInterId = 1:ncount+max_Id)
  data2 = cbind(data_id,data)
  tsda::db_writeTable(conn = conn,table_name = 't_dict_multipleMaterial',r_object = data2,append = T)
  return(data2)

}


#' 创建表格
#'
#' @param conn 连接
#' @param FTableCaption  表名
#'
#' @return 返回值
#' @export
#'
#' @examples
#' meta_createTable()
meta_createTable <-function(conn=tsda::conn_rds('metadata'),FTableCaption ='产品大类'){

sql <- paste0("
select  FTableName,FFieldName,FSqlDataType,FAccessToken   from  vw_dict_multipleMaterial
where FTableCaption ='",FTableCaption,"'")
data = tsda::sql_select(conn,sql)
ncount =nrow(data)
if(ncount >0){

  FTableName = data$FTableName[1]
  FFieldName = data$FFieldName
  FSqlDataType = data$FSqlDataType
  FAccessToken = data$FAccessToken[1]

  db_name = metadatapkg::metadata_getDbName(FToken = FAccessToken)

  conn_db = tsda::conn_rds(db_name)

  table_is_new =metadatapkg::metadata_objectIsNew(FToken = FAccessToken,objectName =FTableName )

  if(table_is_new){
    sql_head = paste0("create table ",FTableName,"  ")
    #做了更加精细化的管理
    #取代原来的根据FDataType + FDataLength的判断方式
    sql_body =  paste0(FFieldName,' ',FSqlDataType,collapse = ",")
    sql_all = paste0(sql_head,"(",sql_body,")")
    print(sql_all)
    tsda::sql_update(conn = conn_db,sql_all)
  }








}

}


#' 创建视图
#'
#' @param conn 连接
#' @param FTableCaption  表名
#'
#' @return 返回值
#' @export
#'
#' @examples
#' meta_createView()
meta_createView <-function(conn=tsda::conn_rds('metadata'),FTableCaption ='产品大类'){

  sql <- paste0("
select  FTableName,FFieldName,FFieldCaption,FAccessToken   from  vw_dict_multipleMaterial
where FTableCaption ='",FTableCaption,"'")
  data = tsda::sql_select(conn,sql)
  ncount =nrow(data)
  if(ncount >0){

    FViewName = paste0('vw_',data$FTableName[1])
    FTableName = data$FTableName[1]
    FFieldName = data$FFieldName
    FFieldCaption = data$FFieldCaption
    FAccessToken = data$FAccessToken[1]

    db_name = metadatapkg::metadata_getDbName(FToken = FAccessToken)

    conn_db = tsda::conn_rds(db_name)

    table_is_new =metadatapkg::metadata_objectIsNew(FToken = FAccessToken,objectName =FViewName )

    if(table_is_new){
      sql_head = paste0("create view ",FViewName," as  select   ")
      #做了更加精细化的管理
      #取代原来的根据FDataType + FDataLength的判断方式
      sql_body =  paste0(FFieldName,' as    ',FFieldCaption,collapse = ",")
      sql_all = paste0(sql_head,"",sql_body,"  from    ",FTableName)
      print(sql_all)
      tsda::sql_update(conn = conn_db,sql_all)
    }








  }

}




