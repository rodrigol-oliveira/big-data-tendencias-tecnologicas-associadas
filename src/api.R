
# libs 
library(stringr)
library(quanteda)
library(dplyr)
library(neuralnet)
library(aws.s3)

Sys.setenv("AWS_ACCESS_KEY_ID" = "AKIAXSWHGJTYRXFWKE6F", "AWS_SECRET_ACCESS_KEY" = "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", "AWS_DEFAULT_REGION" = "us-east-2")


#* Return 
#* @post /treinar
function(){


            rm_accent <- function(str,pattern="all") {
            # Rotinas e funções úteis V 1.0
            # rm.accent - REMOVE ACENTOS DE PALAVRAS
            # Função que tira todos os acentos e pontuações de um vetor de strings.
            # Parâmetros:
            # str - vetor de strings que terão seus acentos retirados.
            # patterns - vetor de strings com um ou mais elementos indicando quais acentos deverão ser retirados.
            #            Para indicar quais acentos deverão ser retirados, um vetor com os símbolos deverão ser passados.
            #            Exemplo: pattern = c("´", "^") retirará os acentos agudos e circunflexos apenas.
            #            Outras palavras aceitas: "all" (retira todos os acentos, que são "´", "'", "^", "~", "¨", "ç")
            if(!is.character(str))
                str <- as.character(str)

            pattern <- unique(pattern)

            if(any(pattern=="Ç"))
                pattern[pattern=="Ç"] <- "ç"

            symbols <- c(
                acute = "áéíóúÁÉÍÓÚýÝ",
                grave = "àèìòùÀÈÌÒÙ",
                circunflex = "âêîôûÂÊÎÔÛ",
                tilde = "ãõÃÕñÑ",
                umlaut = "äëïöüÄËÏÖÜÿ",
                cedil = "çÇ"
            )

            nudeSymbols <- c(
                acute = "aeiouAEIOUyY",
                grave = "aeiouAEIOU",
                circunflex = "aeiouAEIOU",
                tilde = "aoAOnN",
                umlaut = "aeiouAEIOUy",
                cedil = "cC"
            )

            accentTypes <- c("´","'","^","~","¨","ç")

            if(any(c("all","al","a","todos","t","to","tod","todo")%in%pattern)) # opcao retirar todos
                return(chartr(paste(symbols, collapse=""), paste(nudeSymbols, collapse=""), str))

            for(i in which(accentTypes%in%pattern))
                str <- chartr(symbols[i],nudeSymbols[i], str)

            return(str)
            }

            clean_string <- function(string){
                # Lowercase
                temp <- tolower(string)
                # Remove everything that is not a number or letter (may want to keep more 
                # stuff in your actual analyses). 
                temp <- rm_accent(temp)
                temp <- stringr::str_replace_all(temp,"r\\$", " ")
                temp <- stringr::str_replace_all(temp,"[^a-zA-Z\\s]", " ")
                # Shrink down to just one white space
                temp <- stringr::str_replace_all(temp,"[\\s]+", " ")
                # Split it
                return(temp)
            }


            vectorize_sequences <- function(sequences, corpus_dfm, top_terms) {
            # Creates an all-zero matrix of shape (length(sequences), dimension)
                results <- matrix(0, nrow = length(sequences), ncol = length(top_terms)) 
                for (i in 1:length(sequences)){
                    splitted_sequence <- stringr::str_split(sequences[[i]], " ")[[1]]
                    for (j in 1:length(splitted_sequence)){
                        term <- dfm_select(corpus_dfm, splitted_sequence[j])
                        if(! length(featnames(term))==0){
                            term_rank <- match(featnames(term), top_terms)
                            if(! is.na(term_rank)){
                                # Sets specific indices of results[i] to 1s
                                results[i, term_rank] <- 1 
                            }    
                        }
                    }
                }
                results
            }

            # Leitura e processamento dos dados
            
            #busca dados do S3 bucket mayk
            save_object("dataset.csv", file = "dataset.csv",bucket = "mayk")
            review_data <- read.csv(file = "dataset.csv")
            #Fim busca dados
            
            #review_data <- read.csv(file = "./dataset/b2w-10k.csv") codigo caso nao for AWS
            head(review_data)

            # separando os campos contendo o texto do review e a coluna com o dado sobre recomendacao
            filtered_data <- review_data[,c(11,10)]

            head(filtered_data)

            # substituindo o valor yes da coluna por 1, caso o cliente recomende o produto e 0 caso contrario
            filtered_data$score<-ifelse(filtered_data$recommend_to_a_friend=="Yes", 1,0)

            # filtra 2k de registro 
            filtered_data <- filtered_data[1:2000,c(1,3)]


            # tratamento para limpar o texto
            filtered_data$review_text <- clean_string(filtered_data$review_text)

            dim(filtered_data)

            head(filtered_data)

            ## construcao do corpus a partir de todo o texto limpo
            # concatenando o texto de todaas as observacoes em um unico fragmento
            df_corpus <- filtered_data %>% summarise(text = paste0(review_text, sep = "", collapse = ". "))

            dim(df_corpus)

            # criando um corpus a partir do texto concatenado
            my_corpus <- corpus(df_corpus$text)

            summary(my_corpus)

            # criando a matriz de frequencia documento-termo
            corpus_dfm <- dfm(my_corpus, remove_punct = TRUE,  remove = quanteda::stopwords("portuguese"))

            # selecionando os 2000 termos mais frequentes
            sorted_dfm <- dfm_sort(corpus_dfm)[, 1:2000]
            top_terms <- featnames(sorted_dfm)

            # processo 
            #feature_matrix <- vectorize_sequences(filtered_data$review_text, corpus_dfm, top_terms)

            ## usar ja processado 
            
            #verificar upload
            #busca dados do S3 bucket mayk
            #save_object("feature_matrix.txt", file = "feature_matrix.txt",bucket = "mayk")
            #feature_matrix <- read.table(file = "feature_matrix.txt", header = T)
            ##put_object(file = "feature_matrix.txt", object = "feature_matrix.txt", bucket ="mayk")
            #Fim busca dados
            
            feature_matrix <-  read.table("https://raw.githubusercontent.com/raphaelmcobe/r-text-analysis/master/feature_matrix.txt", header = T) 


            feature_matrix <- cbind(feature_matrix, filtered_data$score)

            names(feature_matrix) <- c(top_terms,"score")

            head(feature_matrix)

            # criando rede neural para fazer predicoes
            index <- sample(1:nrow(feature_matrix), .8*nrow(feature_matrix))

            train_split <- feature_matrix[index,]
            test_split <- feature_matrix[-index,]


            # rede com 2 camadas e cada com 64 neuronios
            nn <- neuralnet(score ~ ., data = train_split, hidden = c(64,64), linear.output=FALSE, stepmax = 100, lifesign = "full")

            # avaliando o modelo
            test_X <- test_split[,1:2000]
            test_Y <- test_split[,2001]

            dim(test_split)

            prediction <- compute(nn, test_X)

            head(cbind(test_Y, round(prediction$net.result, 4)))

            print(head(cbind(test_Y, round(prediction$net.result, 4))))

            #Salva os metodos na maquina local
            save(nn, file="modelo.rdata")
            save(corpus_dfm, file="corpus_dfm.rdata")
            save(top_terms, file="top_terms.rdata")

            #salva os metodos na AWS s3
            put_object(file = "rdata/modelo.rdata", object = "rdata/modelo.rdata", bucket = "mayk")
            put_object(file = "rdata/corpus_dfm.rdata", object = "rdata/corpus_dfm.rdata", bucket = "mayk")
            put_object(file = "rdata/top_terms.rdata", object = "rdata/top_terms.rdata", bucket = "mayk")


            paste("treinamento finalizado com sucesso")
}


#* Return 
#* @post /predicao
function( texto ){

            rm_accent <- function(str,pattern="all") {
            # Rotinas e funções úteis V 1.0
            # rm.accent - REMOVE ACENTOS DE PALAVRAS
            # Função que tira todos os acentos e pontuações de um vetor de strings.
            # Parâmetros:
            # str - vetor de strings que terão seus acentos retirados.
            # patterns - vetor de strings com um ou mais elementos indicando quais acentos deverão ser retirados.
            #            Para indicar quais acentos deverão ser retirados, um vetor com os símbolos deverão ser passados.
            #            Exemplo: pattern = c("´", "^") retirará os acentos agudos e circunflexos apenas.
            #            Outras palavras aceitas: "all" (retira todos os acentos, que são "´", "'", "^", "~", "¨", "ç")
            if(!is.character(str))
                str <- as.character(str)

            pattern <- unique(pattern)

            if(any(pattern=="Ç"))
                pattern[pattern=="Ç"] <- "ç"

            symbols <- c(
                acute = "áéíóúÁÉÍÓÚýÝ",
                grave = "àèìòùÀÈÌÒÙ",
                circunflex = "âêîôûÂÊÎÔÛ",
                tilde = "ãõÃÕñÑ",
                umlaut = "äëïöüÄËÏÖÜÿ",
                cedil = "çÇ"
            )

            nudeSymbols <- c(
                acute = "aeiouAEIOUyY",
                grave = "aeiouAEIOU",
                circunflex = "aeiouAEIOU",
                tilde = "aoAOnN",
                umlaut = "aeiouAEIOUy",
                cedil = "cC"
            )

            accentTypes <- c("´","'","^","~","¨","ç")

            if(any(c("all","al","a","todos","t","to","tod","todo")%in%pattern)) # opcao retirar todos
                return(chartr(paste(symbols, collapse=""), paste(nudeSymbols, collapse=""), str))

            for(i in which(accentTypes%in%pattern))
                str <- chartr(symbols[i],nudeSymbols[i], str)

            return(str)
            }

            clean_string <- function(string){
                # Lowercase
                temp <- tolower(string)
                # Remove everything that is not a number or letter (may want to keep more 
                # stuff in your actual analyses). 
                temp <- rm_accent(temp)
                temp <- stringr::str_replace_all(temp,"r\\$", " ")
                temp <- stringr::str_replace_all(temp,"[^a-zA-Z\\s]", " ")
                # Shrink down to just one white space
                temp <- stringr::str_replace_all(temp,"[\\s]+", " ")
                # Split it
                return(temp)
            }


            vectorize_sequences <- function(sequences, corpus_dfm, top_terms) {
            # Creates an all-zero matrix of shape (length(sequences), dimension)
                results <- matrix(0, nrow = length(sequences), ncol = length(top_terms)) 
                for (i in 1:length(sequences)){
                    splitted_sequence <- stringr::str_split(sequences[[i]], " ")[[1]]
                    for (j in 1:length(splitted_sequence)){
                        term <- dfm_select(corpus_dfm, splitted_sequence[j])
                        if(! length(featnames(term))==0){
                            term_rank <- match(featnames(term), top_terms)
                            if(! is.na(term_rank)){
                                # Sets specific indices of results[i] to 1s
                                results[i, term_rank] <- 1 
                            }    
                        }
                    }
                }
                results
            }

            #Faz o download dos metodos da S3 para a maquina local
            # save_object("rdata/modelo.rdata", file = "rdata/modelo.rdata", bucket = "mayk")
            # save_object("rdata/corpus_dfm.rdata", file = "rdata/corpus_dfm.rdata", bucket = "mayk")
            # save_object("rdata/top_terms.rdata", file = "rdata/top_terms.rdata", bucket = "mayk")

            #Carrega os metodos
            s3load("modelo.rdata", bucket = "mayk")
            s3load("corpus_dfm.rdata", bucket = "mayk")
            s3load("top_terms.rdata", bucket = "mayk")

            get_object("modelo.rdata", bucket = "mayk")
            get_object("corpus_dfm.rdata", bucket = "mayk")
            get_object("top_terms.rdata", bucket = "mayk")

            ### utilizando o modleo treinado com frase fora do corpus
            texto <- clean_string(texto)

            print(texto)

            texto_features <- vectorize_sequences(texto, corpus_dfm, top_terms)

            dim(texto_features)

            resultado = compute(nn, texto_features)$net.result

            print(resultado)    

            if ( resultado  > 0.5){
                "Frase positiva"
            }else{
                "Frase negativa"
            }

}
