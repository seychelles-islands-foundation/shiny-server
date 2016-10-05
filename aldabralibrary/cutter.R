cc <- readr::read_csv("cutter_codes.csv")

get_cutter <- function(x){
	require(stringr)
	x <- str_to_lower(x)
	x <- gsub("[^a-z0-9.]+", '', x)
	if(grepl("^s", x)){
		cutter <- 'S'
		over <- 1
		nex <- cc[cc$st == 's', ] %>%
			plyr::ddply('ne', find_code, x)
	} else if(grepl("^qu", x)){
		cutter <- 'Q'
		over <- 2
		nex <- cc[cc$st == 'qu', ] %>%
			plyr::ddply('ne', find_code, x, "^..")
	} else if(grepl("^q", x)){
		cutter <- 'Q'
		over <- 1
		nex <- cc[cc$st == 'q', ] %>%
			plyr::ddply('ne', find_code, x, "^.")
	} else if(grepl("^[0-9]", x)){
		
	} else { # starts with something different
		cutter <- substr(x, 1, 1) %>% str_to_upper()
		over <- 1
		if(grepl("^[a,e,i,o,u]",x)) {
			nex <- cc[cc$st == 'VO', ] %>%
				plyr::ddply('ne', find_code, x, "^.")
		}
		else {  # starts with other consonant
			nex <- cc[cc$st == 'OC', ] %>%
				plyr::ddply('ne', find_code, x, "^.")
		}
	}
	if(nrow(nex) == 0) nex <- rbind(nex, data.frame(code = '1', over = 0))
	cutter <- paste0(cutter, nex$code[1])
	over <- over + nex$over[1]
	paste0(cutter, additional(gsub("[^a-z]", "", x), over))
}

find_code <- function(y, x, pattern = "^.") {
	if(grepl(paste0(pattern, y$ne), x)){
		data.frame(code = y$co, over = nchar(y$ne))
	} else {
		NULL
	}
}

get_additional <- function(x, p){
	nex <- cc[cc$st == 'AD', ] %>%
		plyr::ddply('ne', find_code, x, 
								pattern = paste0("^", do.call(paste0, as.list(rep('.', p)))))
	if(p < nchar(x)){
		rbind(nex, additional(x, p + nex$over))
	} else {
		return(NULL)
	}
}

additional <- function(x, p) {
	get_additional(x,p)$code %>%
		as.list() %>% 
		do.call(paste0, .)
}
