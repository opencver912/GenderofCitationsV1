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
retrievegender <-function(){
result <- fromJSON(file = "ollion_200160601_2_correc.json")
json_data_frame <- as.data.frame(result);
author_list=list();
topic_list=list();

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
# for (i in 1:13){k=append(k , text[i])}
# y=strsplit(as.character(k)," ",fixed=TRUE)
# 
# h=as.character(lapply(y,FUN=function(x){ as.character(x[[1]])}))
# 
# 
#  # firstnamelist=unlist(lapply(author_list[1:10],function(x){strsplit(as.character(x), " ")})[[1]])
# firstnamelist=  lapply(  author_list[1:10] ,function(x){strsplit(as.character(x), " ")})
# 
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

