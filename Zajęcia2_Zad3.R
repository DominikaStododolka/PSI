install.packages("qdap")

library(qdap)

#1 Tworzenie chmury słów za pomocą pakietu wordcloud 

install.packages("wordcloud") 

library(wordcloud) 



#2 Wczytaj dane tekstowe 

# Wczytaj plik tekstowy z lokalnego dysku 

text2021 <- readLines(file.choose()) 
text2021 

text2024 <- readLines(file.choose())
text2024



frequent_terms2021 <- freq_terms(text2021) 
frequent_terms2021 
frequent_terms2021 <- freq_terms(text2021, stopwords = Top200Words) 
plot(frequent_terms2021) 


frequent_terms2024 <- freq_terms(text2024) 
frequent_terms2024 
frequent_terms2024 <- freq_terms(text2024, stopwords = Top200Words) 
plot(frequent_terms2024) 


#3 Utwórz chmurę słów 

wordcloud2021 <- wordcloud(frequent_terms2021$WORD, frequent_terms2021$FREQ) 

wordcloud2024 <- wordcloud(frequent_terms2024$WORD, frequent_terms2024$FREQ) 


#4 Opcje chmury słów 

?wordcloud 

# Zmiana wartości min.freq i max.words w celu wyświetlenia mniejszej/większej liczby słów. 

# min.freq: słowa o częstości poniżej tej wartości nie będą wyświetlane 

# max.words: maksymalna liczba słów do wyświetlenia 



#5 Ograniczenie liczby słów w chmurze poprzez określenie minimalnej częstości 

wordcloud(frequent_terms2021$WORD, frequent_terms2021$FREQ, min.freq = 4) 

wordcloud(frequent_terms2024$WORD, frequent_terms2024$FREQ, min.freq = 4) 



#6 Ograniczenie liczby słów w chmurze poprzez określenie maksymalnej liczby słów 

wordcloud(frequent_terms2021$WORD, frequent_terms2021$FREQ, max.words = 5) 

wordcloud(frequent_terms2024$WORD, frequent_terms2024$FREQ, max.words = 5) 


#7 Optymalizacja i dostosowanie wyników 

# Dodanie koloru do chmury słów dla lepszej wizualizacji 

# Dodanie koloru 

wordcloud(frequent_terms2021$WORD, frequent_terms2021$FREQ, min.freq = 4, colors = brewer.pal(8,"Dark2")) 

wordcloud(frequent_terms2024$WORD, frequent_terms2024$FREQ, min.freq = 4, colors = brewer.pal(8,"Dark2")) 

# Dodanie koloru 

wordcloud(frequent_terms2021$WORD, frequent_terms2021$FREQ, max.words = 5, colors = brewer.pal(8,"Accent")) 

wordcloud(frequent_terms2024$WORD, frequent_terms2024$FREQ, max.words = 5, colors = brewer.pal(8,"Accent")) 


?brewer.pal 

brewer.pal.info 


#8 Dodanie różnych palet kolorystycznych 

wordcloud(frequent_terms2021$WORD, frequent_terms2021$FREQ, min.freq = 4, colors = brewer.pal(9,"Blues")) 
wordcloud(frequent_terms2021$WORD, frequent_terms2021$FREQ, min.freq = 4, colors = brewer.pal(9,"Reds")) 
wordcloud(frequent_terms2021$WORD, frequent_terms2021$FREQ, min.freq = 4, colors = brewer.pal(9,"Greens")) 

wordcloud(frequent_terms2024$WORD, frequent_terms2024$FREQ, min.freq = 4, colors = brewer.pal(9,"Blues")) 
wordcloud(frequent_terms2024$WORD, frequent_terms2024$FREQ, min.freq = 4, colors = brewer.pal(9,"Reds")) 
wordcloud(frequent_terms2024$WORD, frequent_terms2024$FREQ, min.freq = 4, colors = brewer.pal(9,"Greens")) 

#Wnioski: Oba teksty skupiają się na słowie american, więc można wnioskować, że był on skierowany do Amerykanów. W tekście z 2024 najwięcej razy użyto słowa president. Słowo future również często pojawiło się w tym tekście.  
