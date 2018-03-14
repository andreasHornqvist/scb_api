
scb_api <- function(url, body = '{ "query": [], "response": { "format": "json" } }', ua = ""){
  # Hämtar data från SCBs statistikdatabas. 
  #
  # Argument:
  #  url: Url till den tabell i SSDn som skall hämtas. 
  #    datatyp: sträng
  #  body: Fråga i json-format (Kan hämtas från aktuell tabell i SSDn).
  #    datatyp: sträng (använd single quotes)
  #  ua: User agent. Text som identifierar användaren  
  #    datatyp: sträng
  #
  # Returnerar:
  #  Dataframe innehållandes data från SSD-tabellen.
  #
  # Använda paket:
  #   jsonlite
  #   httr
  #   plyr
  #   stringr

  if (missing(url)){
    stop('argument "url" is missing, with no default')}
  
  if (!missing(body)){
    body <- str_replace(body, '"format": "px"', '"format": "json"')}
  
  response <- POST(url, user_agent(ua), body = body)
  stop_for_status(response)
  json <- content(response, "text")
  
  if (!validate(json)){
    if(attr(validate(json),"err") == "JSON string contains UTF8 byte-order-mark."){
      json <- substring(json, 2)
    } else {
      stop(attr(validate(json),"err"))}}

  values_with_metadata <- fromJSON(json)
  keys <- ldply(values_with_metadata$data$key, rbind)
  values <- ldply(values_with_metadata$data$values, rbind)
  
  keys_and_values <- cbind(keys, values) 
  names(keys_and_values) <- values_with_metadata$columns$text
  keys_and_values
}
