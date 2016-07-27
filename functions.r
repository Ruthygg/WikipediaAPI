#'Functions that use the MediaWiki API
#'by www.ruthygarcia.com
#'last documented: March 22-2016
#' \code{getTimestampFirstRevision}: FirstTimeStamp of an article, controlled that it correspond to the first redirect 
#' \code{solveRedirect}: Gets the links that correspond to user page links
#' \code{getLanguageLinks}: Gets the wikipedia language editions available for an article
#' \code{getPageTitleinOtherLanguage}: Returns the title in the language required if available
#' Edits
#' \code{getTimeStampsForAllRevisions}: Returns timestamps of all revisions

library(rjson)
library(RCurl) #important to parse wikitext http://stackoverflow.com/questions/21977480/problems-with-urlencode-in-r
library(stringr)
getPropinWikiText <- function (text, prop, wiki="en"){
  

    
  print(sprintf("Before curl %s",nchar(text)))
  text<-curlEscape(text)
  print(sprintf("After curl %s",nchar(text)))
  url<-paste("https://", wiki,".wikipedia.org/w/api.php?action=parse&text=",text,
             "&contentmodel=wikitext&format=json&prop=", prop,
             sep="")
  #print(url)
  json_data <- rjson::fromJSON(file=url)


  
  return(json_data$parse)
}
  


getLinksfromRevision <- function(links){
  
  
  content<-NULL
  if(length(links) >0) {
    for (i in 1:length(links))
    {
      if (links[[i]]$ns==0 )
        content <- c(content,gsub(" ", "_", links[[i]]$'*'))
    }
    
    content=paste(content, collapse=" ") 
  }
  
  return(content)
}  

getRevisionLinks <- function (page.title, timestamp2, wiki="en", rvcontinue="")
{
  # Returns the timestamp and links of each revision from start up to a timestamp2
  #' Args:
  #'  page.title: The page title in wikipedia
  #'  wiki: language of the page
  #'  timestamp2: the limiting timestamps2
  #' Returns:
  #'  A data frame of two columns (timestamp and all the links of ns 0 for each timestamp of a revision)
  page.title<- URLencode(page.title, reserved = TRUE)
  

  
  
  if(nchar(rvcontinue) > 0){
    url<-paste("https://", wiki,".wikipedia.org/w/api.php?action=query&titles=",page.title,
               "&prop=revisions&rvlimit=500&format=json&rvend=", timestamp2, 
               "&rvprop=timestamp|content&rvdir=newer&rvcontinue=", rvcontinue,
               sep="")
  }else
    url<-paste("https://", wiki,".wikipedia.org/w/api.php?action=query&titles=",page.title,
               "&prop=revisions&rvlimit=500&format=json&rvend=", timestamp2, 
               "&rvprop=timestamp|content&rvdir=newer",
               sep="")
  
  
  json_data <- rjson::fromJSON(file=url)
  
  df.content<-data.frame(timestamp=as.character(), content=as.character(), stringsAsFactors=FALSE)
  textchar.limit<-5000
                         
  
 # cc<- 0
  for (revision in json_data$query$pages[[1]]$revisions){
  #  cc<-cc+1
    text<- revision$'*'
    if(!length(text) ) next
    print( sprintf("rvcontinue= %s ---nchar(text)=%s",rvcontinue, nchar(text) ))
    content <- NULL
    while(nchar(text) > 0)
    {
      result <- tryCatch({
      part.text<- substring(text,0, textchar.limit)
      if(tail(gregexpr("\\[", part.text)[[1]],1)> tail(gregexpr("\\]", part.text)[[1]],1)){
        part.text<- substring(text,0, tail(gregexpr("\\[", part.text)[[1]],1)-2)
      }else if (str_sub(part.text, start= -1)=="]" & str_sub(part.text, start= -2)!="]]" ){
        part.text<- substring(text,0, textchar.limit+1)
      }
      
      if(nchar(part.text)<=0)
        part.text <- substring(text,0, textchar.limit)
      links<-getPropinWikiText(part.text, "links")$links
      content=c(content,getLinksfromRevision(links) )
      },error = function(e){
        print("There was an error, divide text")
        e
      })
      
      if(inherits(result, "error")){
        textchar.limit<-textchar.limit/2
      }else{
        textchar.limit<-5000
        text <- substring(text, nchar(part.text)+1)
      }
    }
   
 
    if(!is.null(content)){
      content=paste(unique(unlist(strsplit(content,' ', fixed = TRUE))), collapse = " ")
      df.content<-rbind(df.content, data.frame(timestamp=revision$timestamp, content=content, stringsAsFactors=FALSE) )
    }else{
      df.content<-rbind(df.content, data.frame(timestamp=revision$timestamp, content=NA, stringsAsFactors=FALSE) )
    }
    
  
    }
  
  rvcontinue <- json_data$continue$rvcontinue
  
  if(is.null(rvcontinue) )
  {
    return(df.content)
  }
  
  return(rbind(data.frame(df.content, stringsAsFactors=FALSE), getRevisionLinks(page.title,  timestamp2, wiki, rvcontinue) ) )
  
  
  
  
}


getTimestampFirstRevision <- function (page.title, wiki)
{
  # Returns the timestamp of the first edit of an article in the format of yyyy-mm-dd and correspond to the very first redirect
  #' Args:
  #'  page.title: The page title in wikipedia
  #'  wiki: language of the page
  #' Returns:
  #'  Timstamp of the first revision
  page.title<- URLencode(page.title, reserved = TRUE)
  url<-paste("https://", wiki,".wikipedia.org/w/api.php?action=query&titles=",page.title,
             "&prop=revisions&rvdir=newer&rvlimit=1&format=json", sep="")
  json.data <- rjson::fromJSON(file=url)
  
  timestamp<- json.data$query$pages[[1]]$revisions[[1]]$timestamp
  timestamp <- substr(timestamp, 1, 10)
  
  return(timestamp)
  
}


divideRedirects <- function (items)
{
 
 total_items <-NULL
 while(length(items) > 200) {
   flights<- solveRedirectGroup(paste(items[1:200], collapse="|"))
   total_items <- paste (total_items, flights, collapse = "|")
   items<-items[200:length(items)]
 }
 
flights<- solveRedirectGroup(paste(items, collapse="|"))
total_items <- paste (total_items, flights, collapse = "|")

 return(total_items)
}

 
solveRedirectGroup <- function (original_title, wiki="en") {
   # Returns the current titles pages for a group of strings
  #' Args:
  #'  title: The page title in wikipedia
  #'  wiki: language of the page
  #' Returns:
  #'  The current title of the page 
  library(rjson)
  original_title <-  gsub(" ","_",original_title)
  title<- URLencode(original_title, reserved = TRUE)
  page_title_url<- paste("https://", wiki,".wikipedia.org/w/api.php?action=query&titles=",title, "&redirects&format=json", sep="")
  json_data <- rjson::fromJSON(file=page_title_url)
  
  if (length(json_data$query$redirects) >= 1){
    print(sprintf("Fournd %s redirects", length(length(json_data$query$redirects))))
    for(i in 1: length(json_data$query$redirects)){
    redirect <-  gsub(" ","_",json_data$query$redirects[[1]]$to)
    from_title <- gsub(" ","_",json_data$query$redirects[[1]]$from)
    original_title<-gsub(from_title, redirect, original_title)
    }
  }
  
  
  return(original_title)
}

getLanguageLinks <- function (page.title, wiki) {
  #  Returns the language codes separated by a comma that available for a page.title
  #' Args:
  #'  title: The page title in wikipedia
  #'  wiki: language of the page
  #' Returns:
  #'  The current title of the page 
  #' Example: 
  #' wiki<-"en"
  #' page.title<-"Germanwings_Flight_9525"
  #' returns : "af,ar,av,az,bg,bs,ca,cs,cy,da,de,el,eo,es,et,eu,fa,fi,fr,ga,gl,he,
  #'            hr,hu,hy,id,it,ja,ko,lo,lt,lv,ml,mr,ms,nds,ne,nl,no,oc,pl,pt,ro,ru,sh,simple,sk,sv,ta,th,tr,uk,ur,uz,vi,zh,zh-yue"
  
  page.title<- URLencode(page.title, reserved = TRUE)
  json_url<-paste("https://",wiki,".wikipedia.org/w/api.php?action=query&titles=",page.title,"&prop=langlinks&lllimit=500&format=json", sep="")
  json_data <- rjson::fromJSON(file=json_url)
  
  result_langs<- ""
  for(i in 1: length(json_data[['query']][['pages']][[1]][['langlinks']])  ) {
    print(sprintf("%s of %s", i ,length(json_data[['query']][['pages']][[1]][['langlinks']])    ))
    tryCatch( {
      lang <- json_data[['query']][['pages']][[1]][['langlinks']][[i]][['lang']]
      if(!is.null(lang)  ) 
          result_langs<-paste(result_langs, lang,  sep=",")
    }, warning = function(w) w, error = function(e) e )
  }
  
  if(nchar(result_langs) <= 0)
    return(NA)
  else
    return(substr(result_langs,2,nchar(result_langs) ))
    
}


getPageTitleinOtherLanguage <- function (page.title, wiki1, wiki2) {
  #  Returns the title in the language required if available
  #' Args:
  #'  page.title: The page title in wikipedia
  #'  wiki1: the wikipedia version where we want to find the link in other language
  #'  wiki2: the wikipedia language we are looking to find
  #' Returns:
  #'  The title in the Wikipedia language wiki2 if available, NA otherwise
  #' Example: 
  #' wiki<-"en"
  #' page.title<-"Germanwings_Flight_9525"
  #' returns : "af,ar,av,az,bg,bs,ca,cs,cy,da,de,el,eo,es,et,eu,fa,fi,fr,ga,gl,he,
  #'            hr,hu,hy,id,it,ja,ko,lo,lt,lv,ml,mr,ms,nds,ne,nl,no,oc,pl,pt,ro,ru,sh,simple,sk,sv,ta,th,tr,uk,ur,uz,vi,zh,zh-yue"
  
  page.title<- URLencode(page.title, reserved = TRUE)
  json_url<-paste("https://", wiki1, ".wikipedia.org/w/api.php?action=query&titles=",page.title,"&prop=langlinks&lllang=",wiki2,"&format=json", sep="")
  json_data <- rjson::fromJSON(file=json_url)
  
  result <- json_data[['query']][['pages']][[1]][['langlinks']][[1]][['*']]
  
  
  if(is.null(result))
    return(NA)
  else
    return(result)
  
}

getPageLinks <- function(title, plcontinue=NULL, namespace=0, wiki="en") {
  if (!is.null(plcontinue)){
    url<-paste("https://", wiki,".wikipedia.org/w/api.php?action=query&titles=", title, "&prop=links&plnamespace=",namespace, 
               "&plcontinue=",plcontinue,"&pllimit=500&format=json", sep="")
  }else
    url<-paste("https://", wiki,".wikipedia.org/w/api.php?action=query&titles=", title, "&prop=links&plnamespace=",namespace, 
               "&pllimit=500&format=json", sep="")
  
  json_data <- rjson::fromJSON(file=url)
  plcontinue <- json_data$continue$plcontinue
  
  titles<-NULL
  for (link in json_data$query$pages[[1]]$links)
    titles<-data.frame(rbind(titles,solveRedirect(link$title, wiki)), stringsAsFactors=FALSE)
  
  if(is.null(plcontinue) )
  {
    return(titles)
  }
  
  return(rbind(data.frame(titles, stringsAsFactors=FALSE), getCategoryLinks(title, plcontinue, namespace, wiki) ) )
  
  
}




getLinksInPage <-function(page_name, plcontinue=NULL,  wiki="en") {
  #  Returns the not hidden links of a page
  #' Args:
  #'  page_name: The page title in wikipedia
  #'  clcontinue: The parameter to go to the next page in case there are 
  #'              more than 500 revisions
  #'  wiki: language of the page
  #' Returns:
  #'  The categories of a page
  page.title<- URLencode(page_name, reserved = TRUE)
  
  print("call to function")
  
  if(!is.null(plcontinue) > 0){

        url<-paste("https://",wiki,".wikipedia.org/w/api.php?action=query&titles=", page.title,
               "&prop=links&plnamespace=0&plcontinue=",plcontinue,"pllimit=500&format=json", sep="")
    
  }else{
    url<-paste("https://",wiki,".wikipedia.org/w/api.php?action=query&titles=", page.title,
                "&prop=links&plnamespace=0&pllimit=500&format=json", sep="")
  }
  
  json_data <- rjson::fromJSON(file=url)
  plcontinue <- json_data$continue$plcontinue
  
  titles<-NULL
  for (link in json_data$query$pages[[1]]$links)
    titles<-rbind(titles,gsub(" ", "_", link$title) )
  
  if(is.null(plcontinue) )
  {
    return(titles)
  }
  
  return(rbind(data.frame(titles=titles, stringsAsFactors=FALSE), 
               data.frame(titles=getLinksInPage(page_name, plcontinue, wiki) , stringsAsFactors = FALSE) ) )
    
  
}


getCategoryLinks <- function(category_name, cmcontinue, cmtype, wiki) {
  
  
  url<-paste("https://", wiki,".wikipedia.org/w/api.php?action=query&cmtitle=",category_name,
             "&list=categorymembers&cmtype=",cmtype,"&cmcontinue=",cmcontinue,"&cmlimit=500&format=json", sep="")
  
  json_data <- rjson::fromJSON(file=url)
  cmcontinue <- json_data$continue$cmcontinue
  
  titles<-NULL
  for (category in json_data$query$categorymembers)
    titles<-data.frame(rbind(titles,solveRedirect(category$title, wiki)), stringsAsFactors=FALSE)
  
  if(is.null(cmcontinue) )
  {
    return(titles)
  }
  
  return(rbind(data.frame(titles, stringsAsFactors=FALSE), getCategoryLinks(category_name, cmcontinue, cmtype, wiki) ) )
  
  
}

#Hello hellod

###### Editions 

getTimeStampsForAllRevisions <- function (page.title, rvcontinue, wiki)
{
  #  Returns the timestamps of the revisions for each page
  #' Args:
  #'  page.title: The page title in wikipedia
  #'  rvcontinue: The parameter to go to the next page in case there are 
  #'              more than 500 revisions
  #'  wiki: language of the page
  #' Returns:
  #'  The timestamps of all revisions
  
  page.title<- URLencode(page.title, reserved = TRUE)
  if(nchar(rvcontinue) > 0){
    url<-paste("https://",  wiki, ".wikipedia.org/w/api.php?action=query&titles=", page.title, 
              "&prop=revisions&rvprop=timestamp&rvdir=newer&rvlimit=max&rvcontinue=",rvcontinue,"&format=json", sep="")
  }else
    url<-paste("https://",  wiki, ".wikipedia.org/w/api.php?action=query&titles=", page.title, 
               "&prop=revisions&rvprop=timestamp&rvdir=newer&rvlimit=max&format=json", sep="")
  json_data <- rjson::fromJSON(file=url)
  rvcontinue <- json_data$continue$rvcontinue
  
  timestamps<-NULL
  for (revision in json_data$query$pages[[1]]$revisions)
    timestamps<-data.frame(timestamp=rbind(timestamps, substr(revision$timestamp,1, 10)), stringsAsFactors=FALSE)
  
  if(is.null(rvcontinue) )
  {
    return(timestamps)
  }
  
  return(rbind(data.frame(timestamps, stringsAsFactors=FALSE), getTimeStampsForAllRevisions(page.title, rvcontinue,  wiki) ) )
  
  
  
  
}



#get_redirects_from_wikipedia_page <- function(page)
getAllRedirects<- function(page, wiki="en")
{
  json_url<-paste("https://", wiki,".wikipedia.org/w/api.php?action=query&list=backlinks&blfilterredir=redirects&bltitle=",page,"&bllimit=max&format=json", sep="")
  json_data <- rjson::fromJSON(file=json_url)
  redirects<-json_data[['query']][['backlinks']]
  temp <-  rep(NA, length(redirects))
  if (length(redirects) >0 )
    for (i in 1: length(redirects))
      temp[i]<-redirects[[i]][['title']]


  return(temp)
}



getCoordinatesFromWikipediaPage<- function(page, wiki="en")
{
  json_url<-paste("https://",wiki,".wikipedia.org/w/api.php?action=query&prop=revisions&rvprop=content&rvsection=0&titles=",page, "&prop=coordinates&format=json", sep="")
  json_data <- rjson::fromJSON(file=json_url)
  lat<-json_data[['query']][['pages']][[1]][['coordinates']][[1]][['lat']]
  lon<-json_data[['query']][['pages']][[1]][['coordinates']][[1]][['lon']]
  if(length(lat)>0)
    return(paste(lat,lon,sep=','))
  else
    return(NA)
}



get_coordinates_from_wikipedia_page <- function(page)
{
  json_url<-paste("https://en.wikipedia.org/w/api.php?action=query&prop=revisions&rvprop=content&rvsection=0&titles=",page, "&prop=coordinates&format=json", sep="")
  json_data <- rjson::fromJSON(file=json_url)
  lat<-json_data[['query']][['pages']][[1]][['coordinates']][[1]][['lat']]
  lon<-json_data[['query']][['pages']][[1]][['coordinates']][[1]][['lon']]
  if(length(lat)>0)
  return(paste(lat,lon,sep=','))
  else
  return(NA)
}








get_pagelinks<- function (json_category)
{

json_data <- rjson::fromJSON(file=json_category)
category_members<-json_data$query$category
links_df<-NULL
for (category in category_members)
{
  page_title=category$title
  page_title<- gsub(" ","_",page_title)
  tryCatch( {

        page_title<-solve_redirect(page_title)
        date_firstrevision <- get_timestamp_firstrevision(page_title)
        links_df <- rbind(links_df, data.frame(flight=page_title, start.date=as.Date(date_firstrevision), stringsAsFactors=FALSE))
  }
  , warning = function(w) w, error = function(e) e )


}
return(links_df)

}




