#' Function to get the significance or not of a variable when given the the appropriate glms
#'
#' @param \code{aGlm1} the glm that contains the rating factor to be analysed
#' @param \code{aGlm2} the glm without the rating factor to be analysed
#' @param \code{selType} This is the selection criterion type, either "AIC", "BIC", "Chisq", or "F"
#' @return The output is logical, TRUE means that the variable is significant, FALSE means that the variable is not significant
#' @note This is an internal function
#'
#' @keywords glm consistency significance
#' @author Chibisi Chima-Okereke \email{cchima-okereke@@mango-solutions.com}

.getVarSignificance <- function(aGlm1 = glm1, aGlm2 = glm2, selType = "AIC"){
	
	handicap <- get("handicap", envir = funcEnv)
	
	if(selType == "AIC"){
		ICglm1 <- extractAIC(aGlm1)[2]
		ICglm2 <- extractAIC(aGlm2)[2]
		cat("AIC is being used for model selection\n\n")
		cat("Factor Model: ", paste(as.character(aGlm1$formula)[c(2,1,3)], collapse = " "), "\n")
		cat(paste("AIC of candidate model is ", round(ICglm1), "\n", sep = ""))
		cat("Sub model: ", paste(as.character(aGlm2$formula)[c(2,1,3)], collapse = " "), "\n")
		cat(paste("AIC of sub model is ", round(ICglm2), "\n\n\n", sep = ""))
		output <- round(ICglm1) + handicap < round(ICglm2)
	}
	
	if(selType == "BIC"){
		ICglm1 <- BIC(aGlm1)
		ICglm2 <- BIC(aGlm2)
		cat("BIC is being used for model selection\n\n")
		cat("Factor Model: ", paste(as.character(aGlm1$formula)[c(2,1,3)], collapse = " "), "\n")
		cat(paste("BIC of candidate model is ", round(ICglm1), "\n\n", sep = ""))
		cat("Sub model: ", paste(as.character(aGlm2$formula)[c(2,1,3)], collapse = " "), "\n")
		cat(paste("BIC of sub model is ", round(ICglm2), "\n\n\n", sep = ""))
		output <- round(ICglm1) + handicap < round(ICglm2)
	}
	
	if(selType == "Chisq"){
		anovaTable <- anova(aGlm2, aGlm1, test = "Chisq")
		cat("Chi-Squared Test is being used for model selection\n\n")
		cat("ANOVA output for Chi-Squared Test comparing the models\n\n")
		print(anovaTable)
		output <- (anovaTable[2 ,"Pr(>Chi)"] < 0.05 & anovaTable[2, "Deviance"] > 0)
	}
	
	if(selType == "F"){
		anovaTable <- anova(aGlm2, aGlm1, test = "F")
		cat("F-Test is being used for model selection\n\n")
		cat("ANOVA output for F-Test comparing the models\n\n")
		print(anovaTable)
		output <- (anovaTable[2 ,"Pr(>F)"] < 0.05 & anovaTable[2, "Deviance"] > 0)
	}
	
	return(output)
	
}
