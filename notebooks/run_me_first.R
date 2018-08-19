### run_me_first.R --- 
## 
## Filename: run_me_first.R
## Description: 
## Author: 
## Maintainer: 
## Created: Di Jul 10 15:12:47 2018 (+0200)
## Version: 
## Package-Requires: ()
## Last-Updated: Sa Aug 18 06:59:59 2018 (+0200)
##           By: ntuser
##     Update #: 23
## URL: 
## Doc URL: 
## Keywords: 
## Compatibility: 
## 
######################################################################
## 
### Commentary: 
## 
## 
## 
######################################################################
## 
### Change Log:
## 
## 
######################################################################
## 
## This program is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or (at
## your option) any later version.
## 
## This program is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
## 
## You should have received a copy of the GNU General Public License
## along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
## 
######################################################################
## 
### Code:





#library(sodium)
#library(yaml)
#library(readr)
library(metafor)
library(repr)

# Change plot size to 4 x 3
## options(repr.plot.width=4, repr.plot.height=3)

## other.mode1 – als Beispiel für eine kategoriale Variable -> mit welchem Modus web vergleichen wurde

## ·         pub.year- Publikationsjahr

## ·         mod.incentive.web – 


## head(df_wrr)

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title 
##' @param input_file 
##' @param output_file 
##' @param key 
##' @return 
##' @author 
exercise_decrypt <- function(input_file, output_file, key){

    ## Package sodium expects key to be in raw format.
    key_raw <- hash(charToRaw(key))

    input_encrypted <- readRDS(input_file)

    ## Decrypt file.
    input_decrypted <- data_decrypt(input_encrypted, key = key_raw)

    output_decrypted  <- unserialize(input_decrypted)

    write_file(output_decrypted, path = output_file)
}


decryption_key_works <- function(input_file, key){
    input_encrypted <- readRDS(input_file)
    decrypted_file <- tryCatch(data_decrypt(bin = input_encrypted,
                                            key = key),
                               error = function(error){FALSE}
                               )
    status <- !is.logical(decrypted_file) & (length(decrypted_file) > 1)
    return(status)
}


## decryption_key_works(input_file = "solutions/encrypted/e1_1.R",
##                      key = hash(charToRaw("12")))


## exercise_decrypt(input_file = "solutions/encrypted/e1_1.R",
##                  output_file = "solutions/decrypted/e1_1.R",
##                  key = as.character(1234))


## Naming scheme for exercise names:
## e{d}_{#}-{*}, with d = day and # = exercise number and * = part.
## E.g., e1_2, exercise 2 of day 1 
show_solution <- function(exercise, show_always_solution = TRUE){## Filename the keys list.

    if(show_always_solution){
        keyfile <- "keys.yaml"
        ## Folder that holds keys.yaml.
        keypath <- "keys"
        
        ## Flag variable that indicates that user input is required. Default value
        ## is: User needs to type-in the key (password).
        key_ask_for_input <- TRUE
        key_is_true <- NA
        
        ## Create empty list that holds the key(s).
        keys <- list()
        
        ## Construct (relative) path for key file.
        keypath <- file.path(keypath, keyfile)

        ## Construct exercise filename to be encrypted. 
        exercise_file <- paste0(exercise, ".R")
        exercise_path <- file.path("solutions", "encrypted", exercise_file)
        output_path <- file.path("solutions", "decrypted", exercise_file)

        ## Print solution.
        cat(paste0(rep("=", 80),collapse=""))
        cat("\n")
        cat(read_file(output_path))
        cat(paste0(rep("=", 80),collapse=""))     
        cat("\n")

        ## Execute (source) solution. 
        source(output_path, echo = TRUE, max.deparse.length=500)
                
    }else{
         
        keyfile <- "keys.yaml"
        ## Folder that holds keys.yaml.
        keypath <- "keys"
        
        ## Flag variable that indicates that user input is required. Default value
        ## is: User needs to type-in the key (password).
        key_ask_for_input <- TRUE
        key_is_true <- NA
        
        ## Create empty list that holds the key(s).
        keys <- list()
        
        ## Construct (relative) path for key file.
        keypath <- file.path(keypath, keyfile)

        ## Construct exercise filename to be encrypted. 
        exercise_file <- paste0(exercise, ".R")
        exercise_path <- file.path("solutions", "encrypted", exercise_file)
        output_path <- file.path("solutions", "decrypted", exercise_file)

        ## First, check if entry in key file exists (keys/keys.yaml).
        ## Case 1: Entry in key file exists.
        if(file.exists(keypath)){
            keys <- read_yaml(keypath)
            if(exercise %in% names(keys)){
                key <- as.character(keys[exercise])
                ## Check if key is correct.
                key_is_true <- decryption_key_works(exercise_path, key)
                if(key_is_true){
                    key_ask_for_input <- FALSE                
                }
            }else{
                key_ask_for_input <- TRUE 
            }
        }
        ## Case 2: Key file does not exist.
        else{
            key_ask_for_input <- TRUE
        }
        
        
        if(key_ask_for_input | !key_is_true){
            key <- readline(paste0("Please enter key for exercise ",
                                   exercise, ": "))
        }
        
        ## Second, try to decrypt encrypted file.
        ## Tf decryption fails, then file_decrypt_status has value
        ## "Key is not correct!".
        file_decrypt_status <-
            tryCatch(exercise_decrypt(input_file = exercise_path,
                                      output_file = output_path,
                                      key = key),
                     error = function(error){"Key is not correct!"}
                     )
        
        ## Third, either print correct solution or...
        if(file_decrypt_status != "Key is not correct!"){

            cat(paste0("The solution can also be found in ", output_path, "\n\n"))

            ## Print solution.
            cat(paste0(rep("=", 80),collapse=""))
            cat("\n")
            cat(read_file(output_path))
            cat("\n")
            cat(paste0(rep("=", 80),collapse=""))     

            ## Execute (source) solution. 
            source(output_path, echo = TRUE, max.deparse.length=500)

            keys[exercise] <- as.character(key)
            keys <- keys[order(names(keys))]
            keys <- lapply(keys, as.character)
            write_yaml(keys, file = keypath)
            ## ... ask for another key.
        }else{
            cat("\n")
            cat(paste(file_decrypt_status, "Please try again!"))
            key_ask_for_input <- TRUE
            key_is_true <- FALSE
        }
        print(key_ask_for_input)

    }
}



update_keyfile <- function(exercise, key){
    keypath <- "keys"
    
    
}

######################################################################
### run_me_first.R ends here
