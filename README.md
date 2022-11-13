# Talking-Deputies
Text analysis of deputies' rhetoric in terms of 2022 Spring session of the State Duma's work
Lemmatization has been provided in a Python by 'pymorphy2' library
Visualizations have been done in Tableau, Infogram and DataWrapper
The full research can be found here: https://cpkr.ru/issledovaniya/gosudarstvennaya-duma/govoryashchie-deputaty/

## Structure of a script is the following:

### 1) Detecting the most discussed projects in the State Duma

### 2) Rating of deputies by an amount of statements
### 3) General Word Cloud 
### 4) Word Cloud for each party (TF-IDF is applied)
### 5) Building a model which allows to reveal a context of word which we are interested in (e.g. 'война').
Despite the fact that this model has not been published, you can test it here: https://public.tableau.com/app/profile/pavel4366/viz/Context_16602202517270/Dashboard1
### 6) Revealing how often the President of Russia Putin is mentioned by each candidate and party 
### 7) Extracting of sentences which has provoked applauses in a hall
### 8) Rating of deputies by lexical richness (TTR metric is applied)
### 9) Rating of deputies by pithiness of statements (= share of stopwords)
