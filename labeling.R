
data.clean<-read.csv("Review_GWK_Clean.csv",header=T)

#skoring
kata.positif <- scan("positive-words.txt",what="character",comment.char=";")
kata.negatif <- scan("negative-words.txt",what="character",comment.char=";")
score.sentiment = function(sentence, positif, negatif,
                           .progress='none')
{
  require(plyr)
  require(stringr)
  scores = laply(sentence, function(kalimat, positif,
                                    negatif) {
    kalimat = gsub('[[:punct:]]', '', kalimat)
    kalimat = gsub('[[:cntrl:]]', '', kalimat)
    kalimat = gsub('\\d+', '', kalimat)
    kalimat = tolower(kalimat)
    list.kata = str_split(kalimat, '\\s+')
    kata2 = unlist(list.kata)
    positif.matches = match(kata2, kata.positif)
    negatif.matches = match(kata2, kata.negatif)
    positif.matches = !is.na(positif.matches)
    negatif.matches = !is.na(negatif.matches)
    score = sum(positif.matches) - (sum(negatif.matches))
    return(score)
  }, kata.positif, kata.negatif, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentence)
  return(scores.df)}

hasil = score.sentiment(data.clean$text, kata.positif, kata.negatif)

#CONVERT SCORE TO SENTIMENT
hasil$klasifikasi<- ifelse(hasil$score<0, "Negatif",ifelse(hasil$score==0,"Netral","Positif"))
hasil$klasifikasi
View(hasil)

#EXCHANGE ROW SEQUENCE
data <- hasil[c(3,1,2)] #ubah urutan kolom
View(data)
write.csv(data, file = "labeling.csv")
