# Prepare data
# Export data from Google Form to *.xlsx file
# Use find and replace to ensure every value of categorical variable start with number and dot.
# Example:
# Variable gender contains 2 value formatted as "1. Male", "2. Female"
#--------------------

# Library requires: readxl, haven, stringr, labelled, dplyr
#--------------------

# Load data
df = readxl::read_xlsx(file.choose())
#--------------------

# After load data, see the order number of each column you want to recode
# Then paste it into c() below
# The vector of new variable names to change here
newVarName = c() # paste list of name here
  
# The vector of column number of multiple-choice variables
multiChoiceVar = c() # paste list of number here

# The vecter of column number of checkbox variables
checkBoxVar = c() # paste list of number here
#--------------------

# Store variable labels -> varLab
varLab = names(df)
#--------------------

# Functions declare
# recode var
tospss <- function(var) {
  message(paste("### Đã mã hoá:"))
  value <- data.frame(table(var))[, 1]
  for (i in seq_len(length(value))) {
    var[var == value[i]] <- i
    message(paste("#", i, "-", value[i]))
  }

  labels <- seq_len(length(value))
  class(labels) <- "character"
  names(labels) <- value
  var <- haven::labelled(var, labels = labels)
  return(var)
}
#--------------------

# Script begin
# Rename variable names
names(df) = newVarName

# Recode multiple-choice variables
for (i in multiChoiceVar) {
  df[[i]] = tospss(df[[i]])
}

# Generate new child variables from checkbox variables
newVarLab = c()
for (l in checkBoxVar) {
  pattern = df[[l]] %>% str_split(', ') %>% unlist() %>% unique()
  pattern = pattern[which(pattern %>% str_detect('[:digit:]. '))]
  for (i in pattern) {
    newCol = ncol(df)+1
    df[[newCol]] = rep("0. Không", nrow(df))
    df[[newCol]][str_detect(df[[l]], i)] = '1. Có'
    colnames(df)[newCol] <- paste0(names(df[,l]), which(pattern==i))
    newVarLab = append(newVarLab, i)
  }
}

# Recode those new child variables
recodename = (length(varLab)+1):ncol(df)
for (i in recodename) {
  df[[i]] = tospss(df[[i]])
}

# Set whole variables labels (both old & new) 
df <- labelled::set_variable_labels(df, .labels = append(varLab, newVarLab))

# Export to *.sav file
write_sav(df, 'df.sav')
