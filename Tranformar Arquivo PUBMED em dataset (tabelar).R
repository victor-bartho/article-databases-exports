library(dplyr)

# Ler o arquivo de texto do PubMed
pubmed_txt <- readLines("resultados pubmed.txt")

# Separar o texto por artigos: cada artigo começa com a listagem PMID
articles <- split(pubmed_txt, cumsum(grepl("^PMID-", pubmed_txt)))

# Função para extrair informações de um artigo
extract_article_info <- function(article_lines) {
  article <- paste(article_lines, collapse = "\n")
  
  # Extrair PMIDs
  pmid <- sub("PMID- ", "", grep("^PMID-", article_lines, value = TRUE))
  if (length(pmid) == 0) pmid <- NA
  
  # Extrair título
  title_lines <- grep("^TI  -", article_lines)
  if (length(title_lines) > 0) {
    title <- article_lines[title_lines[1]]
    for (i in (title_lines[1] + 1):length(article_lines)) {
      if (grepl("^[A-Z]{2}  -", article_lines[i]) ||
          grepl("^[A-Z]{3}", article_lines[i]) ||
          grepl("\\[doi\\]", article_lines[i]) ||
          grepl("\\[pii\\]", article_lines[i])) break
      title <- paste(title, article_lines[i], sep = " ")
    }
    title <- sub("TI  - ", "", title)
    title <- gsub("^\\s+", "", title)  # Remover espaços iniciais
    title <- gsub("\\s+", " ", title) # Remover espaços duplos
  } else {
    title <- NA
  }
  
  # Extrair resumo (abstract)
  abstract_lines <- grep("^AB  -", article_lines)
  if (length(abstract_lines) > 0) {
    abstract <- article_lines[abstract_lines[1]]
    for (i in (abstract_lines[1] + 1):length(article_lines)) {
      if (grepl("^[A-Z]{2}  -", article_lines[i])) break
      abstract <- paste(abstract, article_lines[i], sep = " ")
    }
    abstract <- sub("AB  - ", "", abstract)
    abstract <- gsub("^\\s+", "", abstract)  # Remover espaços iniciais
    abstract <- gsub("\\s+", " ", abstract) # Remover espaços duplos
  } else {
    abstract <- NA
  }
  
  # Extrair ano (year)
  year <- sub("DP  - ", "", grep("^DP  -", article_lines, value = TRUE))
  if (length(year) == 0) year <- NA
  
  # Extrair periódico (journal)
  journal <- sub("TA  - ", "", grep("^TA  -", article_lines, value = TRUE))
  if (length(journal) == 0) journal <- NA
  
  # Extrair autores
  authors <- grep("^AU  -", article_lines, value = TRUE)
  authors <- paste(sub("AU  - ", "", authors), collapse = "; ")
  if (length(authors) == 0) authors <- NA
  
  # Extrair MeSH terms (MH)
  mesh_terms <- grep("^MH  - ", article_lines, value = TRUE)
  if (length(mesh_terms) > 0) {
    mesh_terms <- gsub("^MH  - ", "", mesh_terms)
    mesh_terms <- paste(mesh_terms, collapse = ", ")
  } else {
    mesh_terms <- NA
  }
  
  # Extrair Other terms (OT)
  other_terms <- grep("^OT  - ", article_lines, value = TRUE)
  if (length(other_terms) > 0) {
    other_terms <- gsub("^OT  - ", "", other_terms)
    other_terms <- paste(other_terms, collapse = ", ")
  } else {
    other_terms <- NA
  }
  
  # Garantir que todos os campos estejam presentes, mesmo que vazios
  if (is.na(pmid)) pmid <- NA
  if (is.na(title)) title <- NA
  if (is.na(abstract)) abstract <- NA
  if (is.na(year)) year <- NA
  if (is.na(journal)) journal <- NA
  if (is.na(authors)) authors <- NA
  
  # Criar o dataframe
  data.frame(PMID = pmid, Title = title, Abstract = abstract, Year = year, Journal = journal, Authors = authors,
             `MeSH Terms` = mesh_terms, `Other Terms` = other_terms, stringsAsFactors = FALSE)
}

# Aplicar a função a todos os artigos
article_list <- lapply(articles, extract_article_info)

# Combinar todos os resultados em um único data frame
pubmed_data <- do.call(rbind, article_list)

head(pubmed_data)
