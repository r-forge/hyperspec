###-----------------------------------------------------------------------------
###
### raw.split.nul - rawToChar conversion, splitting at \0
###
###

.nul <- as.raw (0)

raw.split.nul <- function (raw, trunc = c (TRUE, TRUE)) {
	# todo make better truncation
	trunc <- rep (trunc, length.out = 2)
	
	if (trunc [1] && raw [1] == .nul)
		raw <- raw [-1]
	if (trunc [2]) {
		tmp <- which (raw > .nul)
		if (length (tmp) == 0) 
			return ("")
		raw <- raw [1 : tail (tmp, 1)]	
	} 
	if (raw [length (raw)] != .nul)
		raw <- c (raw , .nul)
	
	tmp <- c (0, which (raw == .nul))
	
	out <- character (length (tmp) - 1)
	for (i in 1 : (length (tmp) - 1))
		if (tmp [i] + 1 < tmp [i + 1] - 1)
			out [i] <- rawToChar (raw [(tmp [i] + 1)  : (tmp [i + 1] - 1)])
	
	out
}

