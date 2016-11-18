# Template Editor
# Gender extraction from citation data
# @Sidahmed Benabderrahmane
# Univ. Paris-Dauphine, PSL
# GA Project, 2016. GPL.

library("rjson")
library("stringi")
library("genderizeR")
library("gender")
library("dplyr")
library("ggplot2")
library("dplyr")
library(gdata) # for the trim function
library("plyr")
retrievegender <-function(){
result <- fromJSON(file = "ollion_200160601_2_correc.json")
# json_data_frame <- as.data.frame(result);
author_list=list();
topic_list=list();
mydataframe=data.frame();
article_list=list();
stop_list=list();
working_list=list(); 
working_list2=list(); 
#cleaning the data 
#deleting items without authors' names
stop_list= lapply(result , function(x){
  # if(length(x)<=9)as.character(x) 
 
  if( is.null(x[["authors"]])){x ;}
                                           }
                   ) 

stop_list=Filter(Negate(function(x) is.null(unlist(x))), stop_list)

#deleting the abstract information since it is not important for us !!?
working_list= lapply(result , function(x){
  # if(length(x)<=9)as.character(x) 
  
  if( !is.null(x[["authors"]])){x ;}
}
) 
working_list=Filter(Negate(function(x) is.null(unlist(x))), working_list)


working_list2=lapply(working_list , function(x){
  # if(length(x)<=9)as.character(x) 
  
  working_list[["abstract"]]=NULL 
})
working_list2=Filter(Negate(function(x) is.null(unlist(x))), working_list)
save(working_list2, file="working_List2.RData");

n.obs <- sapply(result, length)
seq.max <- seq_len(max(n.obs))
mat <- t(sapply(result, "[", i = seq.max))
write.table(mat, file="MyDataOriginal.csv", na="")
 
 
n.obs <- sapply(working_list2, length)
seq.max <- seq_len(max(n.obs))
mat2 <- t(sapply(working_list2, "[", i = seq.max))
write.table(mat2, file="MyDataOriginal.csv", na="")


json_data_frame=ldply (working_list2, data.frame)
json_data_frame= data.frame(matrix(unlist(working_list2), ncol = 10))
#from the stop list export the items with the entry article , research-article, in the article-type 





# f=function(x){
#   # if(length(x)<=9)as.character(x) 
#   if(is.null(x[["authors"]])){stop_list=c(stop_list, as.character(x) );}
#   else {as.character(x);print (x);}
# }
# 
# do.call(f,result[1:10])
# grabInfo<-function(var){
#   print(paste("Variable", var, sep=" "))  
#   sapply(foodMarkets, function(x) returnData(x, var)) 
# }
# 
# returnData<-function(x, var){
#   if(!is.null( x[[var]])){
#     return( trim(x[[var]]))
#   }else{
#     return(NA)
#   }
# }


# rm(result)
for(i in 1:as.numeric(length(result))){
 #for authors
   tmp=result[[i]][["authors"]];
    # unlist(result[i]);
  author=as.character(tmp);
  author=stri_trim(author, side = c("both"));
 if(!is.null(author)){
      if(! (length(author) == 0L)){
      if(stri_length(author)>=1){
       if(!(is.element(author , author_list)))
         {author_list=append(author_list,author) }}} }
 
 #for journal
    tmp=result[[i]][["journal"]];
   # unlist(result[i]);
   topic=as.character(tmp);
   topic=stri_trim(topic, side = c("both"));
   if(!is.null(topic)){ 
        if(! (length(topic) == 0L)){ 
        if(stri_length(topic)>=1){ 
         if(!(is.element(topic , topic_list))) 
           {topic_list=append(topic_list,topic)}}} }
        
}
#saving objects in the memory
save (author_list,file="authorList.RData")
save (topic_list,file="topicList.RData")

#extracting the gender of the authors  library("genderizeR")

genderDB <- findGivenNames(author_list, progress = FALSE);

#extracting the gender of the authors  library("gender ")
# k=list()
# firstnamelist=  lapply(  author_list[1:10] ,function(x){strsplit(as.character(x), " ")})
firstnamelist=strsplit(as.character(author_list)," ",fixed=TRUE)
finalfirstnamelist=as.character(lapply(firstnamelist,FUN=function(x){ as.character(x[[1]])}))
rm(firstnamelist);
#remove duplications from firstnamelist
finalfirstnamelist=unique(finalfirstnamelist);

genderDB2 = gender(finalfirstnamelist,
                   method = c("ssa", "ipums", "napp","kantrowitz", "genderize", "demo"),
                   years = c(1900, 2012), countries = c("United States", "Canada","United Kingdom", "Germany", "Iceland", "Norway", "Sweden"))
 

print(gederDB2);

male=length(which(genderDB2$gender=="male"));
female=length(which(genderDB2$gender=="female"))
data=data.frame(group=c("male ","female ") , value=c(male,female) )
ggplot(data, aes(x = group, y = value ,fill = group )) + 
  geom_bar(width = 0.85, stat="identity")

hist(genderDB2$name[which(genderDB2$gender=="male")], breaks=12, col="red")

plot(genderDB2$name[which(genderDB2$gender=="male")],  
                    genderDB2$name[which(genderDB2$gender=="female")], 
                    main="Scatterplot Example", xlab="male ", ylab="female");


save(genderDB2,file="GenderDB2.Rdata");
save(finalfirstnamelist,file="finalfirstnamelist.RData");
# for (i in 1:13){k=append(k , text[i])}
# y=strsplit(as.character(k)," ",fixed=TRUE)
# 
# h=as.character(lapply(y,FUN=function(x){ as.character(x[[1]])}))
# 
# 
#  # firstnamelist=unlist(lapply(author_list[1:10],function(x){strsplit(as.character(x), " ")})[[1]])
# firstnamelist=  lapply(  author_list[1:10] ,function(x){strsplit(as.character(x), " ")})
# install.packages("stringdist","textcat")
# # firstnamelist=lapply(firstnamelist,unlist);
# finalfirstnamelist=list();
# f=lapply(firstnamelist, function(x){
#   
#                                     y=unlist(x);
#                                     finalfirstnamelist=append(finalfirstnamelist,y[1]);
#                                     }
#          )

for (i in 1:length(author_list) )
{
  firstname=unlist(strsplit(as.character(author_list[i]), " "))[1];
  
  genderDB2 = gender(unlist(strsplit(as.character(author_list[1:100]), " "))[1],
                     method = c("ssa", "ipums", "napp","kantrowitz", "genderize", "demo"),
                     years = c(1900, 2012), countries = c("United States", "Canada","United Kingdom", "Germany", "Iceland", "Norway", "Sweden"))
  
  }







}

