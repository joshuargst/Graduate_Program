library(stringr)


raw.data <-"555-1239Moe Szyslak(636) 555-0113Burns, C. Montgomery555-6542Rev. Timothy Lovejoy555 8904Ned Flanders636-555-3226Simpson, Homer5553642Dr. Julius Hibbert"

fullname<- unlist(str_extract_all(raw.data,"[[:alpha:]., ]{2,}"))

  
Areacodes<-unlist(str_extract_all(raw.data,"[[:digit:]-() ]{2,}")) %>%
  str_replace_all("\\D","") %>%
  str_extract("[:digit:]{8,}") %>% 
  substr(1,3)

#extract phone numbers
Phone<-unlist(str_extract_all(raw.data,"[[:digit:]-() ]{2,}")) %>%
  str_replace_all("\\D","") %>%
  str_extract("[:digit:]{1,7}")

#remove initials
fullname<-unlist(str_extract_all(raw.data,"[[:alpha:]., ]{2,}")) %>%
  str_replace_all(" [:alpha:]{1}\\.","")


#extract titles
Titles<-unlist(str_extract_all(fullname,"[[:alpha:]., ]{2,}")) %>%
  str_extract_all("[:alpha:]{1,}\\.") %>%
  lapply(function(x) if(identical(x, character(0))) NA_character_ else x) %>% unlist

#extracting names without titles and 
FnameLname<-str_replace_all(fullname,"\\w+\\.\\s","")%>%
  #and switching names around so that first name is always first
  str_replace_all("(\\w+),\\s(\\w+)","\\2 \\1")

#extract first names
first_name<-str_extract_all(FnameLname,"\\w+\\s+")%>%str_replace_all("\\s+","")

#extract last names
last_name<-str_extract_all(FnameLname,"\\s+\\w+")%>%str_replace_all("\\s+","")

#create table
extracted.info<-cbind.data.frame(Titles, first_name,last_name,Areacodes,Phone)

#3-1. Use the tools of this chapter to rearrange the vector so that all elements conform to the standard first_name last_name

#3-2. construct a logical vector indicating whether a character has a title i.e., Rev. and Dr. 
#since this type of information was extracted into our dataframe using the stringr package as shown above, we can now construct the vector simply by checking which values are not NA within our dataframe

!is.na(extracted.info$Titles)


#if we want to do this directly on the strings we could use the str_detect function as shown below
unlist(str_extract_all(fullname,"[[:alpha:]., ]{2,}")) %>%
  str_detect("[:alpha:]{1,}\\.") 

#3-3. Again, since we have already created a dataframe summarizing all of our relevant information, we can create a logical vector indicating who has a second name by looking at our "last_name" column as shown below which shows that all individuals have a second name

!is.na(extracted.info$last_name)


#4. Describe the types of strings that conform to the following regular expressions and construct an example that is matched by the regular expression

#4-1. [0-9]+\\$

#this expression describes a search for any number between 0 and 9 that occur sequenctially from 0 to infinite amount of times, followed by a dollar sign. an example of this is shown below. The following shows a vector with true and false examples


str_detect(c("123$","sdf544$","werh","32432"),"[0-9]+\\$")

#4-2. \\b[a-z]{1,4}\\b

#this expression will return word boundaries followed by  1-4 character words with lower case letters a-z followed by anouther word boundary. The following shows a vector with true and false examples

str_detect(c("four","hi","Hi","hello"),"\\b[a-z]{1,4}\\b")

#4-3. .*?\\.txt$

#this expression returns any characters that end in .txt. The following shows a vector with true and false examples


str_detect(c("@#%f.txt","a d 4.txt","Hi.tx","hell"),".*?\\.txt$")


#4-4. \\d{2}/\\d{2}/\\d{4}

#This expression returns any string matching a date format like 09/14/2019. The following shows a vector with true and false examples

str_detect(c("09/14/2019","05/02/1992","9/14/2019","09/14/19"),"\\d{2}/\\d{2}/\\d{4}")

#4-5. <(.+?)>.+?</\\1>

#this expression returns any string value containing < followed by any number of sequential characters, folled by the less than sign. This is presumably used for HTML (or similarly formated) documents. The following shows a vector with true and false examples


str_detect(c("<html>This Should Return Truet</html>","<html>thisShouldReturnFalse"),"<(.+?)>.+?</\\1>")





#9

challenge_problem<- "clcopCow1zmstc0d87wnkig7OvdicpNuggvhryn92Gjuwczi8hqrfpRxs5Aj5dwpn0Tanwo
Uwisdij7Lj8kpf03AT5Idr3coc0bt7yczjatOaootj55t3Nj3ne6c4Sfek.r1w1YwwojigO
d6vrfUrbz2.2bkAnbhzgv4R9i05zEcrop.wAgnb.SqoU65fPa1otfb7wEm24k6t3sR9zqe5
fy89n6Nd5t9kc4fE905gmc4Rgxo5nhDk!gr"

#going down the list of regular expressions
#stesting for character only
unlist(str_extract_all(challenge_problem,"[:alpha:]+")) %>%
str_c(collapse= " ")


#testing for numeric only
unlist(str_extract_all(challenge_problem,"[:digit:]+")) %>%
  str_c(collapse= " ")

#testing for lowercase only
unlist(str_extract_all(challenge_problem,"[:lower:]+")) %>%
  str_c(collapse= " ")

#testing for uppercase only
unlist(str_extract_all(challenge_problem,"[:upper:]+")) %>%
  str_c(collapse= " ")

#this seems to provide an actual message, lets try cleaning this up!
unlist(str_extract_all(challenge_problem,"[[:upper:].]")) %>%
  str_c(collapse= "")%>%str_replace_all("\\."," ")

#The puzzle is solved!
