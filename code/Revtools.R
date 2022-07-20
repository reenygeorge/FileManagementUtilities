# All code in this file are from https://github.com/mjwestgate/revtools
# Code is extracted here for RIS-CSV conversion and CSV-RIS conversion

####### from revtools_csv.R  ######

# import csv in a format suitable for revtools apps
revtools_csv <- function(filename){
  data <- read.csv(filename, stringsAsFactors = FALSE)
  colnames(data) <- clean_names(colnames(data))
  if(colnames(data)[1] != "label"){
    if(
      length(unique(data[, 1])) < nrow(data) |
      any(c("author", "title", "year", "journal") == colnames(data)[1])
    ){
      data <- data.frame(
        label = create_index("ref", nrow(data)),
        data,
        stringsAsFactors = FALSE
      )
    }
  }
  if(any(colnames(data) == "author")){
    data$author <- clean_author_delimiters(data$author)
  }
  return(data)
}

# detect author delimiters
clean_author_delimiters <- function(x){
  if(all(grepl("\\sand\\s|\\sAND\\s|\\s&\\s", x))){
    x <- gsub("\\sAND\\s|\\s&\\s", "\\sand\\s", x)
  }else{
    x <- gsub(",(?=\\s[[:alpha:]]{2,})", " and", x, perl = TRUE)
  }
  return(x)
}

# function to create a string of named length in format "string_number" that sorts in correct order
create_index <- function(string, n, sep = "_"){
  if(missing(string)){
    string <- "V"
  }
  if(missing(n)){
    stop("n is missing from create_index with no default")
  }
  if(n < 1){
    stop("n must be > 0 for create_index to function")
  }
  if(length(n) > 1){
    n <- length(n)
  }
  size <- log10(n) + 1
  vector <- seq_len(n)
  return(
    paste(
      string,
      formatC(vector, width  = size, format = "d", flag = 0),
      sep = sep
    )
  )
}

# clean up column names
clean_names <- function(
  x # colnames
){
  x <- sub("^(X|Y|Z)\\.+", "", x) # remove leading X
  x <- sub("^[[:punct:]]*", "", x) # leading punctuation
  x <- sub("[[:punct:]]*$", "", x) # trailing punctuation
  x <- gsub("\\.+", "_", x) # replace 1 or more dots with underscore
  # for ris tags consisting only of upper case letters, keep upper case. Otherwise lower.
  not_tags <- !grepl("^[[:upper:]]{2,4}$", x)
  x[not_tags] <- tolower(x[not_tags])
  x <- sub("authors", "author", x) # remove plural authors
  x <- make.unique(x, sep = "_") # ensure uniqueness
  return(x)
}

####### from bibliography_methods.R  ######


as.bibliography <- function(x, ...){
  
  if(class(x) != "data.frame"){
    stop("as.bibliography can only be called for objects of class 'data.frame'")
  }
  
  # get labels for each entry
  x_cols <- colnames(x)
  if(any(x_cols == "label")){
    label_col <- which(x_cols == "label")
    label_vec <- x[, label_col]
    x <- x[, -label_col]
  }else{
    label_vec <- create_index("ref", nrow(x))
  }
  
  x_list <- lapply(
    split(x, seq_len(nrow(x))),
    function(a){
      a <- as.list(a)
      if(any(names(a) == "author")){
        a$author <- strsplit(a$author, " and ")[[1]]
      }
      if(any(names(a) == "keywords")){
        a$keywords <- strsplit(a$keywords, " and ")[[1]]
      }
      return(a)
    }
  )
  names(x_list) <- label_vec
  class(x_list) <- "bibliography"
  return(x_list)
}


# function to convert an object of class 'bibliography' into a data.frame
as.data.frame.bibliography <- function(x, ...){
  
  cols <- unique(unlist(lapply(x, names)))
  
  # fix bug where ris tags get placed first if they appear before bib tags
  col_n <- nchar(cols)
  if(any(col_n < 3)){
    cols <- cols[c(which(col_n >= 3), which(col_n < 3))]
  }
  
  x_list <- lapply(x, function(a, cols){
    result <- lapply(cols, function(b, lookup){
      if(any(names(lookup) == b)){
        data_tr <- lookup[[b]]
        if(length(data_tr) > 1){
          data_tr <- paste0(data_tr, collapse = " and ")
        }
        return(data_tr)
      }else{
        return(NA)
      }
    },
    lookup = a)
    names(result) <- cols
    return(
      as.data.frame(
        result,
        stringsAsFactors=FALSE
      )
    )
  },
  cols = cols
  )
  
  x_dframe <- data.frame(
    label = make.names(names(x_list), unique = TRUE),
    do.call(rbind, x_list),
    stringsAsFactors = FALSE
  )
  rownames(x_dframe) <- NULL
  colnames(x_dframe) <- clean_names(colnames(x_dframe))
  return(x_dframe)
}

#################

####### from read_bibliography.R  ######

# user-accessible function
read_bibliography <- function(
  filename,
  return_df = TRUE
	){

  invisible(Sys.setlocale("LC_ALL", "C"))
  on.exit(invisible(Sys.setlocale("LC_ALL", "")))

  if(missing(filename)){
    stop("filename is missing with no default")
  }
  file_check <- unlist(lapply(filename, file.exists))
  if(any(!file_check)){
    stop("file not found")
  }

  if(length(filename) > 1){
    result_list <- lapply(filename, function(a, df){
      read_bibliography_internal(a, df)
    },
    df = return_df
    )
    names(result_list) <- filename
    if(return_df){
      result <- merge_columns(result_list)
      result$filename <- unlist(
        lapply(seq_len(length(result_list)),
        function(a, data){
          rep(names(data)[a], nrow(data[[a]]))
        },
        data = result_list
      ))
      if(any(colnames(result) == "label")){
        result$label <- make.unique(result$label)
      }
      return(result)
    }else{
      result <- do.call(c, result_list)
      return(result)
    }
  }else{
    return(
      read_bibliography_internal(filename, return_df)
    )
  }

}


# underlying workhorse function
read_bibliography_internal <- function(
  filename,
  return_df = TRUE
	){

  if(grepl(".csv$", filename)){
    result <- revtools_csv(filename)
    if(!return_df){
      result <- as.bibliography(result)
    }
  }else{
    # import x
    z <- tryCatch(
      {
        scan(filename,
          sep = "\t",
          what = "character",
          quote = "",
          quiet = TRUE,
          blank.lines.skip = FALSE
        )
      },
      warning = function(w){
        stop(
          "file import failed: data type not recognized by read_bibliography",
          call. = FALSE
        )
      },
      error = function(e){
        stop(
          "file import failed: data type not recognized by read_bibliography",
          call. = FALSE
        )
      }
    )
    Encoding(z) <- "latin1"
    z <- gsub("<[[:alnum:]]{2}>", "", z) # remove errors from above process

    # detect whether file is bib-like or ris-like via the most common single characters
    nrows <- min(c(200, length(z)))
    zsub <- z[seq_len(nrows)]
    n_brackets <- length(grep("\\{", zsub))
    n_dashes <- length(grep(" - ", zsub))
    if(n_brackets >  n_dashes){
      result <- read_bib(z)  # simple case - no further work needed
    }else{  #  ris format can be inconsistent; custom code needed
      if(grepl(".ciw$", filename)){
        tag_type <- "wos"
      }else{
        tag_type <- "ris"
      }
      z_dframe <- prep_ris(z, detect_delimiter(zsub))
      # import appropriate format
      if(any(z_dframe$ris == "PMID")){
        result <- read_medline(z_dframe)
      }else{
        result <- read_ris(z_dframe, tag_type)
      }
    }
    if(return_df){
      result <- as.data.frame.bibliography(result)
    }
  }
  return(result)
}


rollingsum <- function(a, n = 2L){
  tail(cumsum(a) - cumsum(c(rep(0, n), head(a, -n))), -n + 1)
}

# detect delimiters between references, starting with strings that start with "ER"
detect_delimiter <- function(x){
  if(any(grepl("^ER", x))){
    delimiter <- "endrow"
  }else{
    # special break: same character repeated >6 times, no other characters
    char_list <- strsplit(x, "")
    char_break_test <- unlist(
      lapply(char_list,
        function(a){length(unique(a)) == 1 & length(a > 6)}
      )
    )
    if(any(char_break_test)){
      delimiter <- "character"
    }else{
      # use space as a ref break (last choice)
      space_break_check <- unlist(lapply(
        char_list,
        function(a){all(a == "" | a == " ")}
      ))
      if(any(space_break_check)){
        delimiter <- "space"
      }else{
        stop("import failed: unknown reference delimiter")
      }
    }
  }
  return(delimiter)
}


prep_ris <- function(
  z,
  delimiter
){
	# detect tags
  tags <- regexpr(
    "^([[:upper:]]{2,4}|[[:upper:]]{1}[[:digit:]]{1})\\s{0,}-{0,2}\\s{0,}",
    perl = TRUE,
    z
  )

  z_dframe <- data.frame(
    text = z,
    row = seq_along(z),
    match_length = attr(tags, "match.length"),
    stringsAsFactors = FALSE
  )

  z_list <- split(z_dframe, z_dframe$match_length)
  z_list <- lapply(z_list, function(a){
    n <- a$match_length[1]
    if(n < 0){
      result <- data.frame(
        ris = "",
        text = a$text,
        row_order = a$row,
        stringsAsFactors = FALSE
      )
    }else{
      result <- data.frame(
        ris = sub("\\s{0,}-\\s{0,}|^\\s+|\\s+$", "", substr(a$text, 1, n)),
        text = gsub("^\\s+|\\s+$", "", substr(a$text, n+1, nchar(a$text))),
        row_order = a$row,
        stringsAsFactors = FALSE
      )
    }
    return(result)
  })
  z_dframe <- do.call(rbind, z_list)
  z_dframe <- z_dframe[order(z_dframe$row), ]

	# replace tag information for delimiter == character | space
	if(delimiter == "character"){ # i.e. a single character repeated many times
		z_dframe$ris[which(
			unlist(lapply(
        strsplit(z, ""),
        function(a){
          length(unique(a)) == 1 & length(a > 6)
        }
      ))
		)] <- "ER"
  }
	if(delimiter == "space"){
    z_dframe$ris[which(z_dframe$ris == "" & z_dframe$text == "")] <- "ER"
		# ensure multiple consecutive empty rows are removed
		z_rollsum <- rollingsum(z_dframe$ris == "ER")
		if(any(z_rollsum > 1)){
      z_dframe <- z_dframe[which(z_rollsum <= 1), ]
    }
	}
	if(delimiter == "endrow"){
    # work out what most common starting tag is
    z_dframe$ref <- c(0, cumsum(z_dframe$ris == "ER")[
      seq_len(nrow(z_dframe)-1)]
    ) # split by reference

    start_tags <- unlist(lapply(
      split(z_dframe$ris, z_dframe$ref),
      function(a){a[which(a != "")[1]]}
    ))
    start_tag <- names(which.max(xtabs(~ start_tags )))

    
    # continue old code
    print(which(z_dframe$ris == start_tag))
    print(which(z_dframe$ris == "ER"))
		row_df <- data.frame(
			start = which(z_dframe$ris == start_tag),
			end = which(z_dframe$ris == "ER")
			)
		
		z_list <- apply(
      row_df,
      1,
      function(a){c(a[1]:a[2])}
    )
		z_list <- lapply(
      z_list,
      function(a, lookup){lookup[a, ]},
      lookup = z_dframe
    )
		z_dframe <- as.data.frame(
      do.call(rbind, z_list)
    )
	}

	# cleaning
	z_dframe$ref <- c(0, cumsum(z_dframe$ris == "ER")[
    seq_len(nrow(z_dframe)-1)]
  ) # split by reference
	z_dframe <- z_dframe[which(z_dframe$text != ""), ] # remove empty rows
	z_dframe <- z_dframe[which(z_dframe$ris != "ER"), ] # remove end rows
  z_dframe$text <- trimws(z_dframe$text)

	# fill missing tags
	z_split <- split(z_dframe, z_dframe$ref)
	z_split <- lapply(z_split, function(a){
		if(a$ris[1] == ""){
      a$ris[1] <- "ZZ"
    }
		accum_ris <- Reduce(c, a$ris, accumulate = TRUE)
		a$ris <- unlist(lapply(
      accum_ris,
      function(b){
  			good_vals <- which(b != "")
  			b[good_vals[length(good_vals)]]
			}))
		return(a)
  })
	z_dframe <- as.data.frame(
    do.call(rbind, z_split)
  )

  return(z_dframe)
}



read_medline <- function(x){

	x_merge <- merge(x,
    tag_lookup(type = "medline"),
    by = "ris",
    all.x = TRUE,
    all.y = FALSE
  )
	x_merge <- x_merge[order(x_merge$row_order), ]

	# convert into a list, where each reference is a separate entry
	x_split <- split(x_merge[c("bib", "text")], x_merge$ref)
	x_final <- lapply(x_split, function(a){
		result <- split(a$text, a$bib)
		if(any(names(result) == "abstract")){
			result$abstract <- paste(result$abstract, collapse = " ")
    }
		if(any(names(result) == "title")){
			if(length(result$title) > 1){
        result$title <- paste(result$title, collapse = " ")
      }
    }
		if(any(names(result) == "term_other")){
			names(result)[which(names(result) == "term_other")] <- "keywords"
    }
		if(any(names(result) == "date_published")){
			result$year <- substr(result$date_published, start = 1, stop = 4)
    }
		if(any(names(result) == "article_id")){
			doi_check <- grepl("doi", result$article_id)
			if(any(doi_check)){
				result$doi <- strsplit(result$article_id[which(doi_check)], " ")[[1]][1]
      }
    }
		return(result)
	})

	names(x_final) <- unlist(lapply(x_final, function(a){a$pubmed_id}))
	class(x_final) <- "bibliography"
	return(x_final)
}


# generate unique label for entries, using as much author & year data as possible
generate_bibliographic_names <- function(x){
	nonunique_names <- unlist(lapply(x, function(a){
		name_vector <- rep("", 3)
		if(any(names(a) == "author")){
			name_vector[1] <- strsplit(a$author[1], ",")[[1]][1]
    }
		if(any(names(a) == "year")){
      name_vector[2] <- a$year[1]
    }
		if(any(names(a) == "journal")){
			journal_info <- strsplit(a$journal, " ")[[1]]
			name_vector[3] <- paste(
        substr(journal_info, 1, min(nchar(journal_info), 4)),
        collapse = "")
			}
		name_vector <- name_vector[which(name_vector != "")]
		if(length(name_vector) == 0){
      return("ref")
		}else{
      return(paste(name_vector, collapse = "_"))
    }
	}))

	# where this is not possible, give a 'ref1' style result
	if(any(nonunique_names == "ref")){
		rows_tr <- which(nonunique_names == "ref")
		nonunique_names[rows_tr] <- create_index("ref", length(rows_tr))
	}

	# ensure names are unique
	if(length(unique(nonunique_names)) < length(nonunique_names)){
    nonunique_names <- make.unique(nonunique_names, sep = "_")
  }

	return(nonunique_names)
}


# RIS
read_ris <- function(x, tag_type = "ris"){

	# merge data with lookup info, to provide bib-style tags
	x_merge <- merge(x, tag_lookup(type = tag_type),
    by = "ris",
    all.x = TRUE,
    all.y = FALSE)
	x_merge <- x_merge[order(x_merge$row_order), ]

	# find a way to store missing .bib data rather than discard
	if(any(is.na(x_merge$bib))){
		rows_tr <- which(is.na(x_merge$bib))
    x_merge$bib[rows_tr] <- x_merge$ris[rows_tr]
    if(all(is.na(x_merge$row_order))){
      start_val <- 0
    }else{
      start_val <- max(x_merge$row_order, na.rm = TRUE)
    }
    x_merge$row_order[rows_tr] <- as.numeric(
      as.factor(x_merge$ris[rows_tr])
    ) + start_val
	}

	# method to systematically search for year data
  year_check <- regexpr("^\\d{4}$", x_merge$text)
  if(any(year_check > 0)){
    check_rows <- which(year_check > 0)
    year_strings <- as.numeric(x_merge$text[check_rows])

    # for entries with a bib entry labelled year, check that there arent multiple years
		if(any(x_merge$bib[check_rows] == "year", na.rm = TRUE)){
      # check for repeated year information
      year_freq <- xtabs(~ ref, data = x_merge[which(x_merge$bib == "year"), ])
      if(any(year_freq > 1)){
        year_df <- x_merge[which(x_merge$bib == "year"), ]
        year_list <- split(nchar(year_df$text), year_df$ris)
        year_4 <- sqrt((4 - unlist(lapply(year_list, mean))) ^ 2)
        # rename bib entries that have >4 characters to 'year_additional'
        incorrect_rows <- which(
          x_merge$ris != names(which.min(year_4)[1]) &
          x_merge$bib == "year"
        )
        x_merge$bib[incorrect_rows] <- "year_additional"
      }
		}else{
			possible_rows <- which(
        year_strings > 0 &
        year_strings <= as.numeric(format(Sys.Date(), "%Y")) + 1
      )
			tag_frequencies <- as.data.frame(
				xtabs(~ x_merge$ris[check_rows[possible_rows]]),
				stringsAsFactors = FALSE
      )
			colnames(tag_frequencies) <- c("tag", "n")
			# now work out what proportion of each tag contain year data
			# compare against number of references to determine likelihood of being 'the' year tag
			tag_frequencies$prop <- tag_frequencies$n/(max(x_merge$ref)+1) # number of references
			if(any(tag_frequencies$prop > 0.9)){
				year_tag <- tag_frequencies$tag[which.max(tag_frequencies$prop)]
				rows.tr <- which(x_merge$ris == year_tag)
				x_merge$bib[rows.tr] <- "year"
				x_merge$row_order[rows.tr] <- 3
			}
		}
	}

	# ensure author data from a single ris tag
	if(any(x_merge$bib == "author")){
		lookup.tags <- xtabs( ~ x_merge$ris[which(x_merge$bib == "author")])
		if(length(lookup.tags) > 1){
      replace_tags <- names(which(lookup.tags < max(lookup.tags)))
      replace_rows <- which(x_merge$ris %in% replace_tags)
      x_merge$bib[replace_rows] <- x_merge$ris[replace_rows]
      if(all(is.na(x_merge$row_order))){
        start_val <- 0
      }else{
        start_val <- max(x_merge$row_order, na.rm = TRUE)
      }
      x_merge$row_order[replace_rows] <- start_val + as.numeric(
        as.factor(x_merge$ris[replace_rows])
      )
		}
	}

	# convert into a list, where each reference is a separate entry
	x_split <- split(x_merge[c("bib", "ris", "text", "row_order")], x_merge$ref)

	# convert to list format
	x_final <- lapply(x_split, function(a){
		result <- split(a$text, a$bib)
		# YEAR
		if(any(names(result) == "year")){
			if(any(nchar(result$year) >= 4)){
				year_check <- regexpr("\\d{4}", result$year)
				if(any(year_check > 0)){
					result$year <- substr(
            x = result$year[which(year_check>0)],
            start = year_check[1],
            stop = year_check[1]+3
          )
				}else{
          result$year <- ""
        }
			}else{
        result$year <- ""
      }
		}
		# TITLE
		if(any(names(result) == "title")){
			if(length(result$title) > 1){
				if(result$title[1] == result$title[2]){
          result$title <- result$title[1]
				}else{
          result$title <- paste(result$title, collapse = " ")
        }
      }
			result$title <- gsub("\\s+", " ", result$title) # remove multiple spaces
			result$title <- sub("\\.$", "", result$title) # remove final full stops
		}
		# JOURNAL
		if(any(names(result) == "journal")){
			unique_journals <- unique(result$journal)
			if(length(unique_journals)>1){
				unique_journals <- unique_journals[order(
          nchar(unique_journals),
          decreasing = FALSE
        )]
				result$journal <- unique_journals[1]
				result$journal_secondary <- paste(
          unique_journals[c(2:length(unique_journals))],
          collapse = "; "
        )
			}else{
        result$journal <- unique_journals
      }
			result$journal <-gsub("  ", " ", result$journal)
			result$journal <-sub("\\.$", "", result$journal)
		}
		# ABSTRACT
		if(length(result$abstract > 1)){
			result$abstract <- paste(result$abstract, collapse = " ")
			result$abstract <- gsub("\\s+", " ", result$abstract) # remove multiple spaces
		}
		# PAGE NUMBER
		if(any(names(result) == "pages")){
			if(length(result$pages) > 1){
        result$pages <- paste(sort(result$pages), collapse = "-")
      }
    }
		entry_order <- unlist(lapply(
      names(result),
      function(b, initial){
				initial$row_order[which(a$bib == b)[1]]
      },
      initial = a
    ))
		final_result <- result[order(entry_order)]

		return(final_result)
	})

	names(x_final) <- generate_bibliographic_names(x_final)
	#class(x_final) <- "bibliography"
	return(x_final)
	}



# BIB
read_bib <- function(x){

  # which lines start with @article?
  group_vec <- rep(0, length(x))
  row_id <- which(regexpr("^@", x) == 1)
  group_vec[row_id] <- 1
  group_vec <- cumsum(group_vec)

  # work out row names
  ref_names <- gsub(".*\\{|,$", "", x[row_id])
  ref_type <- gsub(".*@|\\{.*", "", x[row_id])

  # split by reference
  x_split <- split(x[-row_id], group_vec[-row_id])
  length_vals <- unlist(lapply(x_split, length))
  x_split <- x_split[which(length_vals > 3)]

  x_final <- lapply(x_split, function(z){

    # first use a stringent lookup term to locate only tagged rows
  	delimiter_lookup <- regexpr(
      "^[[:blank:]]*([[:alnum:]]|[[:punct:]])+[[:blank:]]*=[[:blank:]]*\\{+",
      z
    )
    delimiter_rows <- which(delimiter_lookup != -1)
    other_rows <- which(delimiter_lookup == -1)
    delimiters <- data.frame(
      row = delimiter_rows,
      location = regexpr("=", z[delimiter_rows])
    )
    split_tags <- apply(delimiters, 1, function(a, lookup){
      c(
        row = as.numeric(a[1]),
        tag = substr(
          x = lookup[a[1]],
          start = 1,
          stop = a[2] - 1
        ),
        value = substr(
          x = lookup[a[1]],
          start = a[2] + 1,
          stop = nchar(lookup[a[1]])
        )
      )
      },
      lookup = z
    )
    entry_dframe <- rbind(
      as.data.frame(
        t(split_tags),
        stringsAsFactors = FALSE
      ),
      data.frame(
        row = other_rows,
        tag = NA,
        value = z[other_rows],
        stringsAsFactors = FALSE
      )
    )
    entry_dframe$row <- as.numeric(entry_dframe$row)
    entry_dframe <- entry_dframe[order(entry_dframe$row), c("tag", "value")]

  	if(any(entry_dframe$value == "}")){
  		entry_dframe <- entry_dframe[seq_len(which(entry_dframe$value == "}")[1]-1), ]
  	}
    if(any(entry_dframe$value == "")){
  		entry_dframe <- entry_dframe[-which(entry_dframe$value == ""), ]
  	}

    # remove whitespace
    entry_dframe <- as.data.frame(
      lapply(entry_dframe, trimws),
      stringsAsFactors = FALSE
    )
    # remove 1 or more opening brackets
    entry_dframe$value <- gsub("^\\{+", "", entry_dframe$value)
    # remove 1 or more closing brackets followed by zero or more punctuation marks
    entry_dframe$value <- gsub("\\}+[[:punct:]]*$", "", entry_dframe$value)

    # convert each entry to a list
    label_group <- rep(0, nrow(entry_dframe))
    tag_rows <- which(entry_dframe$tag != "")
    label_group[tag_rows] <- 1
    tag_names <- entry_dframe$tag[tag_rows]
    entry_list <- split(
      entry_dframe$value,
      cumsum(label_group)+1
    )
    names(entry_list) <- tolower(
      gsub("^\\s+|\\s+$",  "", tag_names)
    )
    entry_list <- lapply(entry_list,
      function(a){paste(a, collapse = " ")}
    )
    if(any(names(entry_list) == "author")){
      if(length(entry_list$author) == 1){
    		entry_list$author <- strsplit(entry_list$author, " and ")[[1]]
      }
    }
    return(entry_list)
  })

  # add type
  x_final <- lapply(
    seq_len(length(x_final)),
    function(a, type, data){
      c(type = type[a], data[[a]])
    },
    type = ref_type,
    data = x_final
  )

  names(x_final) <- ref_names
  class(x_final) <- "bibliography"
  return(x_final)

}

####### from tag_lookup.R  ######

tag_lookup <- function(
  type = "ris"
){
  if(!any(c("ris", "ris_write", "medline", "wos") == type)){
    stop("tag_lookup only accepts types 'wos', 'ris', 'ris_write' or 'medline'")
  }
  ris_list <- switch(type,
    "ris" = {
      list(
        "type" = "TY",
        "author" = c("AU", paste0("A", c(1:5))),
        "author_full" = "AF",
        "year" = c("PY", "Y1"),
        "title" = c("TI", "T1"),
        "journal" = c("JO", "T2", "T3", "SO", "JT", "JF", "JA"),
        "volume" = "VL",
        "issue" = "IS",
        "pages" = c("EP", "BP", "SP"),
        "abstract" = c("AB", "N2"),
        "keywords" = c("KW", "DE"),
        "doi" = c("DO", "DI"),
        "call_number" = "CN",
        "issn" = "SN",
        "url" = "UR",
        "accession" = "AN",
        "institution" = "CY",
        "publisher" = "PB",
        "pubplace" = "PP",
        "address" = "AD",
        "editor" = "ED",
        "edition" = "ED",
        "language" = "LA",
        "eppi_id" = "U1"
      )
    },
    "wos" = { # https://images.webofknowledge.com/images/help/WOK/hs_alldb_fieldtags.html
      list(   # accessed 26/11/2019
        "file_name" = c("FN", "N"),
        "version" = "VN",
        "publication_type" = "PT",
        "author" = "AU",
        "author_full" = "AF",
        "year" = "PY",
        "date_published" = "PD",
        "early_access_year" = "EY",
        "early_access_date" = "EA",
        "book_author" = "BA",
        "book_author_full" = "BF",
        "group_author" = "CA",
        "group_book_author" = "GP",
        "author_other_lang" = "Z2",
        "editor" = "BE",
        "title" = "TI",
        "title_other_lang" = "Z1",
        "title_foreign" = "FT",
        "book_series_title" = "SE",
        "book_series_subtitle" = "BS",
        "source" = "SO",
        "source_other_lang" = "Z3",
        "source_abbreviation_29char" = "J9",
        "source_abbreviation_iso" = "JI",
        "volume" = "VL",
        "issue" = "IS",
        "pages" = c("BP", "EP"),
        "n_pages" = "PG",
        "n_chapters" = "P2",
        "doi" = "DI",
        "doi_book" = "D2",
        "author_keywords" = "DE",
        "keywords_plus" = "ID",
        "abstract" = "AB",
        "abstract_other_lang" = "Z4",
        "author_address" = "C1",
        "reprint_address" = "RP",
        "email" = "EM",
        "orcid_id" = "OI",
        "researcher_id" = "RI",
        "special_issue" = "SI",
        "publisher" = "PU",
        "publisher_city" = "PI",
        "publisher_address" = "PA",
        "conference_title" = "CT",
        "conference_location" = "CL",
        "conference_date" = "CY",
        "conference_host" = "HO",
        "conference_sponsor" = "SP",
        "meeting_abstract" = "MA",
        "funding_agency" = "FA",
        "funding_text" = "FX",
        "patent_assignee" = "AE",
        "patent_number" = "PN",
        "article_number" = "AR",
        "supplement" = "SU",
        "language" = "LA",
        "document_type" = "DT",
        "issn" = "SN",
        "eissn" = "EI",
        "isbn" = "BN",
        "accession_number" = "UT",
        "document_delivery_id" = "GA",
        "pubmed_id" = "PM",
        "open_access" = "OA",
        "wos_cagegories" = "WC",
        "research_areas" = "SC",
        "cited_references" = "CR",
        "n_cited_references" = "NR",
        "n_cited_woscc" = "TC",
        "n_cited_csc" = "Z8",
        "n_cited_biosis" = "ZB",
        "n_cited_allwos" = "Z9",
        "esi_hot_paper" = "HP",
        "esi_highly_cited" = "HC",
        "usage_180_days" = "U1",
        "usage_since_2013" = "U2",
        "date_generated" = "DA",
        "end_record" = "ER",
        "end_file" = "EF"
      )
    },
    "ris_write" = {
      list(
        "type" = "TY",
        "author" = "AU",
        "year" = "PY",
        "title" = "TI",
        "journal" = "JO",
        "volume" = "VL",
        "number" = "IS",
        "startpage" = "SP",
        "endpage" = "EP",
        "abstract" = "AB",
        "keywords" = "KW",
      	"doi" = "DO",
        "call" = "CN",
      	"issn" = "SN",
        "url" = "UR",
        "accession" = "AN",
        "institution" = "CY",
        "publisher" = "PB",
      	"pubplace" = "PP",
        "address" = "AD",
        "editor" = "ED",
        "edition" = "ET",
        "language" = "LA",
        "eppi_id" = "U1",
        "end" = "ER"
      )
    },
    "medline" = {
      list(
        "abstract" = "AB",
        "copyright_info" = "CI",
        "affiliation" = "AD",
        "investigator_affiliation" = "IRAD",
        "article_id" = "AID",
        "author" = "AU",
        "author_id" = "AUID",
        "author_full" = "FAU",
        "book_title" = "BTI",
        "collection_title" = "CTI",
        "conflict_of_interest" = "COI",
        "author_corporate" = "CN",
        "date_created" = "CRDT",
        "date_completed" = "DCOM",
        "date_created" = "DA",
        "date_revised" = "LR",
        "date_published_elec" = "DEP",
        "date_published" = "DP",
        "edition" = "EN",
        "editor" = "ED",
        "editor_full" = "FED",
        "date_added" = "EDAT",
        "gene_symbol" = "GS",
        "general_note" = "GN",
        "grant_number" = "GR",
        "investigator" = "IR",
        "investigator_full" = "FIR",
        "isbn" = "ISBN",
        "issn" = "IS",
        "issue" = "IP",
        "journal_abbreviated" = "TA",
        "journal" = "JT",
        "language" = "LA",
        "location_id" = "LID",
        "manuscript_id" = "MID",
        "mesh_date" = "MHDA",
        "mesh_terms" = "MH",
        "nlm_id" = "JID",
        "references_n" = "RF",
        "abstract_other" = "OAB",
        "copyright_info_other" = "OCI",
        "id_other" = "OID",
        "term_other" = "OT",
        "term_owner_other" = "OTO",
        "owner" = "OWN",
        "pages" = "PG",
        "personal_name_as_subject" = "PS",
        "personal_name_as_subject_full" = "FPS",
        "place_published" = "PL",
        "publication_history_status" = "PHST",
        "publication_status" = "PST",
        "publication_type" = "PT",
        "publishing_model" = "PUBM",
        "pubmed_central_identitfier" = "PMC",
        "pubmed_central_release" = "PMCR",
        "pubmed_id" = "PMID",
        "registry_number" = "RN",
        "substance_name" = "NM",
        "secondary_source_id" = "SI",
        "source" = "SO",
        "space_flight_mission" = "SFM",
        "status" = "STAT",
        "subset" = "SB",
        "title" = "TI",
        "title_transliterated" = "TT",
        "volume" = "VI",
        "volume_title" = "VTI"
      )
    }
  )

  # convert this to a data.frame
  length_list <- lapply(ris_list, length)
  ris_dframe <- data.frame(
    ris = do.call(c, ris_list),
    bib = unlist(lapply(names(ris_list), function(a, lookup){
      rep(a, lookup[[a]])
      },
      lookup = length_list
    )),
    stringsAsFactors = FALSE
  )
  rownames(ris_dframe) <- NULL

  if(type == "ris"){
    ris_dframe$order <- unlist(lapply(
      seq_len(length(ris_list)),
      function(a, lookup){
        rep(a, lookup[[a]])
      },
      lookup = length_list
    ))
    # if(!duplicates){
    #   ris_dframe <- do.call(rbind, lapply(
    #     split(ris_dframe, ris_dframe$order),
    #     function(a){a[1, ]}
    #   ))
    # }
  }

  return(ris_dframe)
}


####### from write_bibliography.R  ######

# function to export data in .bib format
write_bibliography <- function(x, filename, format = "ris"){
  
  if(missing(filename)){
    stop("argument 'filename' is missing, with no default")
  }
  if(!any(c("bibliography", "data.frame") == class(x))){
    stop("write_bibliography only accepts objects of class 'data.frame' or 'bibliography'")
  }
  if(class(x) == "data.frame"){
    x <- as.bibliography(x)
  }
  
  if(format == "bib"){
    # process basic text
    result <- lapply(x, function(a){
      if(any(names(a) == "author")){
        a$author <- paste(a$author, collapse=" and ")
      }
      a <- lapply(a, function(b){ 	# ensure only one entry per value
        if(length(b) > 1){
          paste(b, collapse = "; ")
        }else{
          b
        }
      })
      paste0(names(a), "={", a, "},") # format as text
    })
    
    # add article identifier info
    export <- unlist(lapply(
      seq_len(length(result)),
      function(a, source, entry_names){
        c(
          paste0("@ARTICLE{", entry_names[a], ","),
          source[a],
          "}",
          ""
        )
      },
      source = result,
      entry_names = names(x))
    )
    names(export) <- NULL
    
  }
  
  if(format == "ris"){
    
    result <- lapply(x, function(a, lookup){
      
      # convert to tagged vector
      b <- do.call(c, a)
      b <- data.frame(
        tag = c(names(b), "end"),
        entry = c(b, ""),
        stringsAsFactors = FALSE
      )
      rownames(b) <- NULL
      b$tag <- gsub("[[:digit:]]", "", b$tag)
      
      # page information needs to be treated separately
      if(any(b$tag == "pages")){
        page_row <- which(b$tag == "pages")
        page_sep <- strsplit(b$entry[page_row], "-")[[1]]
        page_sep <- page_sep[grepl("^[[:digit:]]+$", page_sep)]
        if(length(page_sep) > 1){
          new_rows <- data.frame(
            tag = c("startpage", "endpage"),
            entry = page_sep[1:2],
            stringsAsFactors = FALSE
          )
          b <- as.data.frame(rbind(
            b[c(1:(page_row-1)), ],
            new_rows,
            b[c((page_row+1):nrow(b)), ])
          )
        }}
      b$order <- seq_len(nrow(b))
      
      # substitute tags for ris format versions
      b <- merge(lookup, b,
                 by.x = "bib",
                 by.y = "tag",
                 all.x = FALSE,
                 all.y = TRUE
      )
      b <- b[order(b$order), 2:3]
      b <- b[which(!is.na(b$ris)), ]
      
      # concatenate rows, return a vector of strings
      c(paste(b$ris, b$entry, sep = "  - "), "")
      
    },
    lookup = tag_lookup(type = "ris_write")[, 1:2]
    )
    
    export <- do.call(c, result)
  } # end ris
  
  # export
  write.table(
    export,
    filename,
    quote = FALSE,
    row.names = FALSE,
    col.names = FALSE)
  
} #  end function

