meta_upload()
meta_upload(file_name = 'data-raw/data/rds_mtrl_productGroup.xlsx')


meta_upload(file_name = 'data-raw/data/rds_mtrl_propCategory.xls')

meta_upload(file_name = 'data-raw/data/rds_mtrl_propCategoryConfig.xls')


meta_upload(file_name = 'data-raw/data/rds_mtrl_propValue.xls')


meta_upload(file_name = 'data-raw/data/rds_mtrl_propValueConfig.xls')


metadatapkg::metadata_getDbName(FToken = '9B6F803F-9D37-41A2-BDA0-70A7179AF0F3')


meta_createTable(FTableCaption = '产品大类')

meta_createTable(FTableCaption = '产品分组')


meta_createTable(FTableCaption = '属性类别')

meta_createTable(FTableCaption = '属性类别配置')

meta_createTable(FTableCaption = '属性选项')

meta_createTable(FTableCaption = '属性选项配置')


meta_createView(FTableCaption = '产品大类')

meta_createView(FTableCaption = '产品分组')


meta_createView(FTableCaption = '属性类别')

meta_createView(FTableCaption = '属性类别配置')

meta_createView(FTableCaption = '属性选项')

meta_createView(FTableCaption = '属性选项配置')


library(mdlMultipleMaterialServer)

prdCategory_read()

prdGroup_read()

propCategory_read()

propCategoryConfig_read()

propValue_read()

propValueConfig_read()



meta_createTable(FTableCaption = '产品选配结果')
meta_createView(FTableCaption = '产品选配结果')








