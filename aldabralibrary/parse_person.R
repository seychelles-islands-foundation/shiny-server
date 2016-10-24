parse_person <- function(x, type = "Author"){
	y <- stringr::str_split(x, "\\|")[[1]] %>%
		lapply(parse_contributor) %>%
		do.call(rbind, .)
	
	y[y$role == type, "name"] %>% 
		as.character() %>% 
		as.list() %>% 
		append(values = list(sep = " and ")) %>%
		do.call(paste, .)
}

parse_contributor <- function(z){
	
	# get name
	name <- stringr::str_replace(z, "\\[(.*?)\\]", "")
	name <- stringr::str_split(name, ",")[[1]]
	
	if(length(name) >= 2){
		name <- paste(trimws(name[1]), trimws(name[2]), sep = ", ")
	} else {
		name <- trimws(name[1])
	}
	
	# get role
	roles <- stringr::str_extract(z, "(?<=\\[).+?(?=\\])")
	if(is.na(roles)) {
		role <- "Author"
	} else{
		role <- stringr::str_split(roles, ";")[[1]][1]
	}
	
	data.frame(name = name, role = role)
}