# read clean dataset
df = read.csv('clean_vehicles.csv')
# drop description
df = dplyr::select(df,-description)
# add a new column
df['make_model'] <- paste(df$manufacturer,df$model,sep=':')
# save dataset
write.csv(df,'vehicles_make_model.csv',row.names = F)
