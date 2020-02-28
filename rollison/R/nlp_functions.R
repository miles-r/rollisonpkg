#Miles Rollison
#last updated 18-Feb-2020
#functions for text mining and nlp

ngrams = function(text, n = 1, filter = 0.001){
    text %>%
    as_tibble() %>%
    na.omit %>%
    tidytext::unnest_tokens(ngram, value, token = "ngrams", n = n) %>%
    plyr::count() %>%
    arrange(freq %>% desc) %>%
    mutate(prob = freq / sum(freq)) %>%
    filter(prob >= filter) %>%
    return
}

get_dtm = function(df, column, ngrams, join = F){
    column = deparse(substitute(column))
    if("id" %in% colnames(df)){dtm = data.frame(id = df["id"], text = df[[column]])}
    else{ dtm = data.frame(id = seq(1:nrow(df)), text = df[[column]])}
    for (i in ngrams) {
        cn = gsub(' ', '_', i)
        dtm[[cn]] = stringr::str_count(df[[column]], i) %>% as.integer
    }
    if(join){
        colnames(dtm)[1] = column
        dtm = full_join(df, dtm)
    }
    return(dtm)
}
