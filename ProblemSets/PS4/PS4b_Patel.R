# PS4b R
# Worked with Jordan and Donald

df1<-(iris)
df<- createDataFrame(df1)
class(df1)
class(df)
head(select(df, df$Sepal_Length, df$Species))
head(filter(df, df$Sepal_Length>5.5))
head(select(filter(df, df$Sepal_Length>5.5), df$Sepal_Length, df$Species))
head(summarize(groupBy(df, df$Species), mean=mean(df$Sepal_Length), count=n(df$Sepal_Length)
