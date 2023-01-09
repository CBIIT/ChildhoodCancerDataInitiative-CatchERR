#!/usr/bin/env Rscript

#Childhood Cancer Data Initiative - CatchERR.R

#This script will take a CCDI metadata manifest file and try to blindly fix the most common errors before the validation step.

##################
#
# USAGE
#
##################

#Run the following command in a terminal where R is installed for help.

#Rscript --vanilla CCDI-CatchERR.R --help


##################
#
# Env. Setup
#
##################

#List of needed packages
list_of_packages=c("readr","dplyr","openxlsx","stringi","readxl","janitor","optparse","tools")

#Based on the packages that are present, install ones that are required.
new.packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
suppressMessages(if(length(new.packages)) install.packages(new.packages, repos = "http://cran.us.r-project.org"))

#Load libraries.
suppressMessages(library(readr,verbose = F))
suppressMessages(library(dplyr,verbose = F))
suppressMessages(library(readxl,verbose = F))
suppressMessages(library(openxlsx, verbose = F))
suppressMessages(library(stringi,verbose = F))
suppressMessages(library(janitor,verbose = F))
suppressMessages(library(optparse,verbose = F))
suppressMessages(library(tools,verbose = F))


#remove objects that are no longer used.
rm(list_of_packages)
rm(new.packages)


##################
#
# Arg parse
#
##################

#Option list for arg parse
option_list = list(
  make_option(c("-f", "--file"), type="character", default=NULL, 
              help="dataset file (.xlsx, .tsv, .csv)", metavar="character"),
  make_option(c("-t", "--template"), type="character", default=NULL, 
              help="dataset template file, CCDI_submission_metadata_template.xlsx", metavar="character")
)

#create list of options and values for file input
opt_parser = OptionParser(option_list=option_list, description = "\nCCDI-CatchERR v1.0.1")
opt = parse_args(opt_parser)

#If no options are presented, return --help, stop and print the following message.
if (is.null(opt$file)&is.null(opt$template)){
  print_help(opt_parser)
  cat("Please supply both the input file (-f) and template file (-t), CCDI_submission_metadata_template.xlsx.\n\n")
  suppressMessages(stop(call.=FALSE))
}


#Data file pathway
file_path=file_path_as_absolute(opt$file)

#Template file pathway
template_path=file_path_as_absolute(opt$template)

###########
#
# File name rework
#
###########

#Rework the file path to obtain a file extension.
file_name=stri_reverse(stri_split_fixed(stri_reverse(basename(file_path)),pattern = ".", n=2)[[1]][2])
ext=tolower(stri_reverse(stri_split_fixed(stri_reverse(basename(file_path)),pattern = ".", n=2)[[1]][1]))
path=paste(dirname(file_path),"/",sep = "")

#Output file name based on input file name and date/time stamped.
output_file=paste(file_name,
                  "_CatchERR",
                  stri_replace_all_fixed(
                    str = Sys.Date(),
                    pattern = "-",
                    replacement = ""),
                  sep="")

##############
#
# Pull Dictionary Page to create node pulls
#
##############

#Read in Dictionary page to obtain the required properties.
df_dict=suppressMessages(read.xlsx(xlsxFile = template_path,sheet = "Dictionary"))
df_dict=remove_empty(df_dict,c('rows','cols'))

#Look for all entries that have a value
all_properties=unique(df_dict$Property)[grep(pattern = '.',unique(df_dict$Property))]
#Remove all entries that are all spaces
all_properties=all_properties[!grepl(pattern = " ",x = all_properties)]
#Pull out required property groups
required_property_groups=unique(df_dict$Required[!is.na(df_dict$Required)])
required_properties=df_dict$Property[!is.na(df_dict$Required)]
#Pull out nodes to read in respective tabs
dict_nodes=unique(df_dict$Node)


################
#
# Read in TaVS page to create value checks
#
################

#Read in Terms and Value sets page to obtain the required value set names.
df_tavs=suppressMessages(read.xlsx(xlsxFile = template_path, sheet = "Terms and Value Sets"))
df_tavs=remove_empty(df_tavs,c('rows','cols'))

#Pull out the positions where the value set names are located
VSN=unique(df_tavs$Value.Set.Name)
VSN=VSN[!is.na(VSN)]

df_all_terms=list()

#Pull the list of values for each controlled vocabulary property.
for (VSN_indv in VSN){
  df_all_terms[[VSN_indv]] = list(filter(df_tavs,Value.Set.Name==VSN_indv)["Term"][[1]])
}


##############
#
# Read in each tab and apply to a data frame list
#
##############

# A bank of NA terms to make sure NAs are brought in correctly
NA_bank=c("NA","na","N/A","n/a")

#Establish the list
workbook_list=list()
incomplete_node=c()

#create a list of all node pages with data
for (node in dict_nodes){
  #read the sheet
  df=suppressMessages(read_xlsx(path = file_path, trim_ws = TRUE, na=NA_bank, sheet = node, guess_max = 1000000, col_types = "text"))
  #df=readWorkbook(xlsxFile = file_path,sheet = node, na.strings = NA_bank)
  #create an emptier version that removes the type and makes everything a character
  df_empty_test=df%>%
    select(-type)%>%
    mutate(across(everything(), as.character))
  #remove empty rows and columns
  df_empty_test=remove_empty(df_empty_test,c("rows","cols"))
  
  #if there are at least one row in the resulting data frame, add it
  if (dim(df_empty_test)[1]>0){
    #if the only columns in the resulting data frame are only linking properties (node.node_id), do not add it.
    if (any(!grepl(pattern = "\\.",x = colnames(df_empty_test)))){
      #add the data frame to the workbook
      workbook_list=append(x = workbook_list,values = list(df))
      names(workbook_list)[length(workbook_list)]<-node
    }else{
      incomplete_node=c(incomplete_node,node)
    }
  }
}

nodes_present=names(workbook_list)


#############
#
# Data frame manipulation
#
#############


#Start write out for log file
sink(paste(path,output_file,".txt",sep = ""))


if (!is.null(incomplete_node)){
  cat("\n\tWARNING: The following node(s), ",paste(incomplete_node, collapse = ", ", sep = ),", did not contain any data except a linking value and type.\n" ,sep = "")
}


##################
#
# Terms and Value sets checks
#
##################

cat("\n\nThe following columns have controlled vocabulary on the 'Terms and Value Sets' page of the template file:\n----------")

for (node in nodes_present){
  cat("\n",node,"\n",sep = "")
  #initialize data frames and properties for tests
  df=workbook_list[node][[1]]
  properties=colnames(df)
  #Enumerated Array properties
  enum_arrays=c('therapeutic_agents',"treatment_type","study_data_types","morphology","primary_site","race")
  
  #For the '_id' properties, make sure there are no illegal characters and it only has "Only the following characters can be included in the ID: English letters, Arabic numerals, period (.), hyphen (-), underscore (_), at symbol (@), and the pound sign (#)."
  for (property in properties){
    if (property %in% names(df_all_terms)){
      if (property %in% enum_arrays){
        unique_values=unique(df[property][[1]])
        unique_values=unique(trimws(unlist(stri_split_fixed(str = unique_values,pattern = ";"))))
        unique_values=unique_values[!is.na(unique_values)]
        if (length(unique_values)>0){
          if (!all(unique_values%in%df_all_terms[property][[1]][[1]])){
            for (x in 1:length(unique_values)){
              check_value=unique_values[x]
              if (!is.na(check_value)){
                if (!as.character(check_value)%in%df_all_terms[property][[1]][[1]]){
                  cat(paste("\tERROR: ",property," property contains a value that is not recognized: ", check_value,"\n",sep = ""))
                  #NEW ADDITION to push the most correct value into the proper case
                  if (tolower(as.character(check_value))%in%tolower(df_all_terms[property][[1]][[1]])){
                    pv_pos=grep(pattern = TRUE, x = tolower(df_all_terms[property][[1]][[1]])%in%tolower(as.character(check_value)))
                    value_pos=grep(pattern = TRUE, x = df[property][[1]]%in%as.character(check_value))
                    df[value_pos,property]<-df_all_terms[property][[1]][[1]][pv_pos]
                    cat(paste("\t\tThe value in ",property," was changed: ", check_value," ---> ",df_all_terms[property][[1]][[1]][pv_pos],"\n",sep = ""))
                  }
                }
              }
            }
          }else{
            cat(paste("\tPASS:",property,"property contains all valid values.\n"))
          }
        }
      }else{
        unique_values=unique(df[property][[1]])
        unique_values=unique_values[!is.na(unique_values)]
        if (length(unique_values)>0){
          if (!all(unique_values%in%df_all_terms[property][[1]][[1]])){
            for (x in 1:length(unique_values)){
              check_value=unique_values[x]
              if (!is.na(check_value)){
                if (!as.character(check_value)%in%df_all_terms[property][[1]][[1]]){
                  cat(paste("\tERROR: ",property," property contains a value that is not recognized: ", check_value,"\n",sep = ""))
                  #NEW ADDITION to push the most correct value into the proper case
                  if (tolower(as.character(check_value))%in%tolower(df_all_terms[property][[1]][[1]])){
                    pv_pos=grep(pattern = TRUE, x = tolower(df_all_terms[property][[1]][[1]])%in%tolower(as.character(check_value)))
                    value_pos=grep(pattern = TRUE, x = df[property][[1]]%in%as.character(check_value))
                    df[value_pos,property]<-df_all_terms[property][[1]][[1]][pv_pos]
                    cat(paste("\t\tThe value in ",property," was changed: ", check_value," ---> ",df_all_terms[property][[1]][[1]][pv_pos],"\n",sep = ""))
                  }
                }
              }
            }
          }else{
            cat(paste("\tPASS:",property,"property contains all valid values.\n"))
          }
        }
      }
    }
  }
  workbook_list[node][[1]]=df
}




##############
#
# Fix URL paths
#
##############

cat("\n\nThe following url columns (file_url_in_cds), check to make sure the full file url is present:\n----------")

for (node in nodes_present){
  
  #initialize data frames and properties for tests
  df=workbook_list[node][[1]]
  properties=colnames(df)
  
  
  if ("file_url_in_cds" %in% colnames(df)){
    cat("\n",node,sep = "")
    
    #Fix urls if the url does not contain the file name but only the base url
    #If full file path is in the url
    for (bucket_loc in 1:dim(df)[1]){
      bucket_url=df$file_url_in_cds[bucket_loc]
      bucket_file=df$file_name[bucket_loc]
      #skip if bucket_url is NA (no associated url for file)
      if (!is.na(bucket_url)){
        #see if the file name is found in the bucket_url
        if (grepl(pattern = bucket_file,x = bucket_url)){
          #download file, run md5sum, copy to data frame and delete file
          file_name=basename(bucket_url)
          if (bucket_file!=file_name){
            cat(paste("\n\tERROR: There is an unresolvable issue with the file url for file: ",bucket_file,sep = ""))
          }
        #if the file url has to be reworked to include the file with the base directory.
        }else{
          if (substr(bucket_url,start = nchar(bucket_url),stop = nchar(bucket_url))=="/"){
            #fix the 'file_url_in_cds' section to have the full file location
            bucket_url_change=paste(bucket_url,bucket_file,sep = "")
            #double check changes made
            file_name=basename(bucket_url_change)
            if (bucket_file!=file_name){
              cat(paste("\n\tERROR: There is an unresolvable issue with the file url for file: ",bucket_file,sep = ""))
            }else{
              #if file name is still found, then change the url
              df$file_url_in_cds[bucket_loc]=bucket_url_change
              cat(paste("\n\tWARNING: The file location for the file, ", bucket_file,", has been changed:\n\t\t", bucket_url, " ---> ", bucket_url_change,sep = ""))
            }
          }else{
            #fix the 'file_url_in_cds' section to have the full file location
            bucket_url_change=paste(bucket_url,"/",bucket_file,sep = "")
            #double check changes made
            file_name=basename(bucket_url_change)
            if (bucket_file!=file_name){
              cat(paste("\n\tERROR: There is an unresolvable issue with the file url for file: ",bucket_file,sep = ""))
            }else{
              #if file name is still found, then change the url
              df$file_url_in_cds[bucket_loc]=bucket_url_change
              cat(paste("\n\tWARNING: The file location for the file, ", bucket_file,", has been changed:\n\t\t", bucket_url, " ---> ", bucket_url_change,sep = ""))
            }
          }
        }
      }
    }
    cat("\n")
  }
  workbook_list[node][[1]]=df
}


#close log file write out
sink()


###############
#
# Assign GUIDS to files
#
###############

cat("The file based nodes will now have a GUID assigned to each unique file.")


#Function to determine if operating system is OS is mac or linux, to run the UUID generation.
get_os <- function(){
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "osx"
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  tolower(os)
}

for (node in nodes_present){
  df=workbook_list[node][[1]]
  properties=colnames(df)
  if ("file_url_in_cds" %in% properties & "file_name" %in% properties & "file_size" %in% properties & "md5sum" %in% properties & "dcf_indexd_guid" %in% properties){
    df_index=df%>%
      select(file_url_in_cds, file_name, file_size, md5sum, dcf_indexd_guid)%>%
      mutate(GUID=NA)
    df_index=unique(df_index)
    #For each unique file, apply a uuid to the GUID column. There is logic to handle this in both OSx and Linux, as the UUID call is different from R to the console.
    pb=txtProgressBar(min=0,max=dim(df_index)[1],style = 3)
    cat("\nGUID creation for the following node: ", node,"\n", sep = "")
    for (x in 1:dim(df_index)[1]){
      setTxtProgressBar(pb,x)
      if (get_os()=="osx"){
        uuid=tolower(system(command = "uuidgen", intern = T))
      }else{
        uuid=system(command = "uuid", intern = T)
      }
      df_index$GUID[x]=uuid
    }
    
    #Take the uuids in the GUID column and paste on the 'dg.4DCF/' prefix to create GUIDs for all the files.
    df_index=mutate(df_index,GUID=paste("dg.4DFC/",GUID,sep = ""))
    
    df=suppressMessages(left_join(df,df_index))
    
    df=df%>%
      mutate(dcf_indexd_guid=GUID)%>%
      select(-GUID)
    
    workbook_list[node][[1]] = df
  }
}


###############
#
# Write out
#
###############

#Write out file

  wb=openxlsx::loadWorkbook(file = file_path)

for (node in nodes_present){
  df=workbook_list[node][[1]]
  openxlsx::deleteData(wb, sheet = node,rows = 1:(dim(df)[1]+1),cols=1:(dim(df)[2]+1),gridExpand = TRUE)
  openxlsx::writeData(wb=wb, sheet=node, df)
  openxlsx::saveWorkbook(wb = wb,file = paste(path,output_file,".xlsx",sep = ""), overwrite = T)
}



cat(paste("\n\nProcess Complete.\n\nThe output file can be found here: ",path,"\n\n",sep = "")) 
