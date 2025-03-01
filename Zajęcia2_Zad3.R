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



#3 Opcje chmury słów 

?wordcloud 

# Zmiana wartości min.freq i max.words w celu wyświetlenia mniejszej/większej liczby słów. 

# min.freq: słowa o częstości poniżej tej wartości nie będą wyświetlane 

# max.words: maksymalna liczba słów do wyświetlenia 


frequent_terms <- freq_terms(text2021) 

frequent_terms 

frequent_terms <- freq_terms(text2021, stopwords = Top200Words) 

plot(frequent_terms) 


#4 Utwórz chmurę słów 

wordcloud(frequent_terms$WORD, frequent_terms$FREQ) 


#5 Ograniczenie liczby słów w chmurze poprzez określenie minimalnej częstości 

wordcloud(frequent_terms$WORD, frequent_terms$FREQ, min.freq = 4) 





#6 Ograniczenie liczby słów w chmurze poprzez określenie maksymalnej liczby słów 

wordcloud(frequent_terms$WORD, frequent_terms$FREQ, max.words = 5) 


#7 Optymalizacja i dostosowanie wyników 

# Dodanie koloru do chmury słów dla lepszej wizualizacji 

# Dodanie koloru 

wordcloud(frequent_terms$WORD, frequent_terms$FREQ, min.freq = 4, colors = brewer.pal(8,"Dark2")) 

# Dodanie koloru 

wordcloud(frequent_terms$WORD, frequent_terms$FREQ, max.words = 5, colors = brewer.pal(8,"Accent")) 

?brewer.pal 

brewer.pal.info 


#8 Dodanie różnych palet kolorystycznych 

wordcloud(frequent_terms$WORD, frequent_terms$FREQ, min.freq = 4, colors = brewer.pal(9,"Blues")) 

wordcloud(frequent_terms$WORD, frequent_terms$FREQ, min.freq = 4, colors = brewer.pal(9,"Reds")) 

wordcloud(frequent_terms$WORD, frequent_terms$FREQ, min.freq = 4, colors = brewer.pal(9,"Greens")) 




