
## Kod do replikacji eksperymentu


Kod został napisany w języku R. Do uruchomienia poszczególnych części skryptu potrzebne jest wcześniejsze zainstalowanie bibliotek `udpipe`, `ldatuning`, `topicmodels`, `stylo`.



### NLP

W pierwszym kroku poszczególne teksty są wczytywane, analizowane pod wględem rozpoznania klas gramatycznych, nazw własnych etc. Jest to standardowy krok w automatycznym przetwarzaniu języka naturalnego (NLP), tutaj realizowany przy użyciu modelu Universal Dependencies oraz biblioteki `udpipe`. Dość istotne jest właściwe ustawienie ścieżek dostępu do plików tesktowych. 

Po analizie gramatycznej i zmianie form wyrazowych na lematy każdy tekst wejściowy zostaje podzielony na próbki długości 1000 słów. Usuwane są wszystkie znaki interpunkcyjne oraz nazwy własne. Wszystkie próbki zapisywane są do pliku wynikowego `texts_sliced.txt`. Plik ów został zamieszczony w niniejszym repozytorium, nie potrzeba więc uruchamiać poniższego pierwszego kroku:



``` R
library(udpipe)
udmodel = udpipe_download_model(language = "polish-pdb", model_dir = tempdir())

chunk_size = 1000


# set the path where your texts were put:
#setwd("Downloads/TMPolCze-master/plain_texts/Pol")

text_files = list.files()
counter = 0

for(current_file in text_files) {

	message(current_file)

    # starting an counter of iterations
    counter = counter + 1
    # say something on screen
    message(".", appendLF = FALSE)
    # lead the next text from file, keep it split into pars
    current_text = readLines(current_file, warn = FALSE)
    current_text = paste(current_text, collapse = " ")

    # run udpipe
    parsed_text = udpipe(x = current_text, object = udmodel)
    lemmatized_text = parsed_text$lemma

    # get rid of NAs
    lemmatized_text = lemmatized_text[!is.na(lemmatized_text)]

    # ged rid of proper nouns and punctuation
    proper_nouns = grep("[A-ZĘÓĄŚŁŻŹĆŃ]", lemmatized_text)
    punctuation = grep("[;:.?!,\"\'—-]", lemmatized_text)
    lemmatized_clean = lemmatized_text[-c(proper_nouns, punctuation)]


    no_of_chunks = floor(length(lemmatized_clean) / chunk_size)

    for(i in 0:(no_of_chunks -1)) {
    
	    start_point = (i * chunk_size) + 1
	    current_chunk = lemmatized_clean[start_point : (start_point + chunk_size)]
	    current_chunk = paste(current_chunk, collapse = " ")
        write(current_chunk, file = "../texts_sliced.txt", sep = "\n", append = TRUE)

        chunk_ID = paste(gsub(".txt", "", current_file), sprintf("%03d", i), sep = "_")
        write(chunk_ID, file = "../chunk_IDs.txt", sep = "\n", append = TRUE)

    }

}
message("NLP done!")

```



## Tabela frekwencji 

Następnym krokiem jest obliczenie frekwencji wszystkich wyrazów w korpusie i ułożenie tych frekwencji w postaci tabeli, zwanej czasem _document/term matrix_. Dla każdego dokumentu (tekstu, próbki tekstowej) należy bowiem pokazać frekwencje wyrazów ułożone w tej samej kolejności.

Posłużymy się wygenerowanym w poprzednim kroku plikiem `texts_sliced.txt` zawierającym wszystkie próbki tekstowe (w jednym wierszu jedna próbka) w postaci lematów. Wczytujemy ów plik i później przy pomocy funkcji z pakietu `stylo` tworzymy tabelę frekwencji:


``` R
corpus = readLines("texts_sliced.txt")
corpus = as.list(corpus)


library(stylo)

parsed_corpus = parse.corpus(corpus, corpus.lang = "Polish", encoding = "UTF-8")


freqlist = make.frequency.list(parsed_corpus, value = TRUE, relative = FALSE)
# trim the list at the freqnency >5
freqlist = names(freqlist)[freqlist > 5]
frequencies = make.table.of.frequencies(parsed_corpus, freqlist, relative = FALSE)

#save(frequencies, file = "word_raw_frequencies.RData")
```






``` R
stopwords = readLines("stoplist_pl.txt", warn = FALSE)
stopwords = unlist(strsplit(stopwords, " "))

filter_words = !(colnames(frequencies) %in% stopwords)
dtm_pruned = frequencies[ , filter_words]

#save(dtm_pruned, file = "dtm_pruned_for_topicmodels.RData")
```



``` R
library(ldatuning)

results = FindTopicsNumber(
  dtm_pruned,
  topics = seq(from = 10, to = 300, by = 30),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 1234),
  #mc.cores = 2L,
  verbose = TRUE
)

save(results, file = "LDA_k_optimize_for_10-300_topics.RData")
```


``` R
FindTopicsNumber_plot(results)
```







Typowe użycie: model LDA, Gibbs sampling, 100 słowozbiorów (1000 + 2000 iteracji, tak wiele iteracji na rozgrzewkę chyba nie jest zresztą potrzebne):

``` R
load("dtm_pruned_for_topicmodels.RData")

topic_model = LDA(dtm_pruned, k = 100, method = "Gibbs", control = list(seed = 1234, burnin = 1000, thin = 100, iter = 1000, verbose = 1)) # also: 'alpha = 0.1' etc. 

save(topic_model, file = "topic_model_k-100.RData")
```

Model zapisany do pliku `topic_model_k-100.RData`. Dostęp do wag poszczególnych słów w poszczególnych słowozbiorach jest nieco bardziej skomplikowany, ale można tę informację wydobyć przez sięgnięcie po funkcję `posterior`:


``` R
model_weights = posterior(topic_model)
topic_words = model_weights$terms
doc_topics = model_weights$topics
```


``` R
chunk_IDs = readLines("../chunk_IDs.txt")
```


``` R
book_IDs = unique(gsub("_[0-9]{3}$", "", chunk_IDs))

for(book in 1 : 100) {

    current_book = as.numeric(simpson_index_per_book[[book]])
    book_name = book_IDs[book]
    png(file = paste(book_name, ".png", sep = ""))
        plot(current_book, main = book_name)
        #model_gam = gam((current_book) ~ s(c(1:length(current_book))))
        model_gam = gam((current_book) ~ s(c(1:length(current_book)), bs = "cr") )
        lines(model_gam$fitted.values, col = rgb(1, 0, 0, 0.6), lwd = 3)
    dev.off()

}
```



