#majority of code borrowed from:
#https://github.com/josephsdavid/teachR/blob/master/R/EDAreg.R
plotAllNumeric <- function(df){
  df%>%keep(is.numeric) %>%
    gather() %>%
    ggplot(aes(value)) +
    facet_wrap(~ key, scales = "free") +
    geom_density()+geom_histogram() 
}

my_palette <- colorRampPalette(c("red", "white", "black"))
heatmapper=function(df)
{df %>% 
    keep(is.numeric) %>% 
    discard(function(x) sd(x)==0) %>%
    tidyr::drop_na() %>%
    cor() %>%
    heatmap.2(col = my_palette ,
              density.info = "none", trace = "none",
              dendogram = c("both"), symm = F,
              symkey = T, symbreaks = T, scale = "none",
              key = T)}
dotplot=function(df,explan,response){
  ggplot(df,aes_string(explan,response))+geom_dotplot(binaxis = "y",
                                                      stackdir="center",method="histodot",binwidth = 0.03)+
    stat_summary(fun.data = function(x) {data.frame(y=mean(x), ymin=mean(x)-2*(sd(x)/sqrt(length(x))), ymax=mean(x)+2*(sd(x)/sqrt(length(x))))},
                 geom="errorbar",color="red")+ theme(axis.text.x=element_text(angle = 10))
}
scatterplots=function(df,explan,response){
  ggplot(df,aes_string(explan,response))+geom_point(position = "jitter"
  )+geom_smooth(method="lm")
  
}
numdotplots= function(df,col,resp){
  ggplot(df,aes_string(col,y=resp))+geom_dotplot(binaxis = "y",
                                                 stackdir="center",method="histodot",binwidth = 0.05)+
    stat_summary(fun.data = function(x) {data.frame(y=mean(x), ymin=mean(x)-2*(sd(x)/sqrt(length(x))), ymax=mean(x)+2*(sd(x)/sqrt(length(x))))},
                 geom="errorbar",color="red")
}
scatterplots=function(df,col,response)
{ggplot(df,aes_string(col,response))+geom_point(position=position_jitter(width=0.5))
}
catplot <- function(df, x,y){
  ggplot(data = df, aes_string(x = x, fill = y)) + 
    geom_bar(position = "fill", alpha = 0.9) + 
    coord_flip()+ylab("Proportion")
}
numplot <- function(df, explan, resp) {
  ggplot(data = df) + geom_density(aes_string(x = explan, fill = resp), alpha = 0.5)+ylab("Proportion")
}